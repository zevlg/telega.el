#include <stdbool.h>
#include <pthread.h>
#include <assert.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include <td/telegram/td_log.h>

#include "telega-engine.h"
#include "telega-dat.h"

#ifdef WITH_VOIP
extern const char* telega_voip_version(void);
extern int telega_voip_cmd(const char* json);
#endif /* WITH_VOIP */

void pngext_usage(char* prog);
void pngext_main(int ac, char** av);

/*
 * Input/Output Protocol:
 * ~~~~~~~~~~~~~~
 *   <COMMAND> <SPACE> <PLIST-LEN> <NEWLINE>
 *   <PLIST of PLIST-LEN length> <NEWLINE>
 *
 * COMMAND is one of `send', `event' or `error'
 * `event' and `error' is used for output
 *
 * If VOIP support is compiled in (make WITH_VOIP=true) then also
 * `voip' command available
 *
 * For example:
 *   event 105
 *   (:@type "updateAuthorizationState" :authorization_state (:@type "authorizationStateWaitTdlibParameters"))
 *
 *   send 109
 *   (:@type "getTextEntities" :text "@telegram /test_command https://telegram.org telegram.me" :@extra ["5" 7.0])
 *
 */

struct telega_engine* engine = NULL;
char* logfile = NULL;
int verbosity = 5;
const char* version = "0.6.0";

int parse_mode = 0;
#define PARSE_MODE_JSON 1
#define PARSE_MODE_PLIST 2

void
usage(char* prog)
{
        printf("Version %s", version);
#ifdef WITH_VOIP
        printf(", with VOIP tgvoip v%s", telega_voip_version());
#endif /* WITH_VOIP */
#ifdef WITH_TON
        printf(", with TON libton v%s", "0");
#endif /* WITH_TON */
        printf("\n");
        printf("usage: %s [-jp] [-l FILE] [-v LVL] [-h]\n", prog);
        printf("\t-l FILE    Log to FILE (default=stderr)\n");
        printf("\t-v LVL     Verbosity level (default=5)\n");
        printf("\t-j         Parse json from stdin and exit\n");
        printf("\t-p         Parse plist from stdin and exit\n");

        printf("\n---- PNG extracting functionality ----\n");
        pngext_usage(prog);
        exit(0);
}

void
stdout_output(struct telega_engine* engine, const char* otype,
              struct telega_dat* plist)
{
        printf("%s %zu\n%s\n", otype, tdat_len(plist) - 1, tdat_start(plist));
        fflush(stdout);
}

static void
on_error_cb(const char* errmsg)
{
        fprintf(stderr, "ERROR: %s\n", errmsg);
        telega_engine_output(engine, "error", errmsg);
}

/*
 * NOTE: Emacs sends HUP when associated buffer is killed
 * kind of graceful exit
 */
static void
on_sighup(int sig)
{
        close(0);
}

static void
stdin_loop(struct telega_engine* engine)
{
        struct telega_dat plist_src = TDAT_INIT;
        char cmdline[33];

        signal(SIGHUP, on_sighup);
        while (fgets(cmdline, 33, stdin)) {
                cmdline[32] = '\0';

                char cmd[33];
                size_t cmdsz;
                if (2 != sscanf(cmdline, "%s %zu\n", cmd, &cmdsz)) {
                        telega_engine_log(
                                engine, 1, "[telega-server] "
                                "Unexpected cmdline format: %s\n", cmdline);
                        continue;
                }

                tdat_ensure(&plist_src, cmdsz);

                /* read including newline */
                size_t rc = fread(plist_src.data, 1, cmdsz + 1, stdin);
                if (rc != cmdsz + 1) {
                        /* EOF or error */
                        telega_engine_log(
                                engine, 1, "[telega-server] "
                                "fread() error: %d\n", ferror(stdin));
                        break;
                }
                plist_src.end = cmdsz + 1;

                telega_engine_send(engine, cmd, &plist_src);

                tdat_reset(&plist_src);
        }

        tdat_drop(&plist_src);
}

static void
parse_stdin(void)
{
        struct telega_dat src = TDAT_INIT;

#define RDSIZE 1024
        tdat_ensure(&src, RDSIZE);

        ssize_t rlen;
        while ((rlen = read(0, &src.data[src.end], RDSIZE)) > 0) {
                src.end += rlen;
                tdat_ensure(&src, RDSIZE);
        }
#undef RDSIZE
        tdat_append1(&src, "\0");

        struct telega_dat dst = TDAT_INIT;
        if (parse_mode == PARSE_MODE_JSON)
                tdat_json_value(&src, &dst);
        else
                tdat_plist_value(&src, &dst);
        tdat_append1(&dst, "\0");

        printf("%s\n", dst.data);

        tdat_drop(&src);
        tdat_drop(&dst);
}

int
main(int ac, char** av)
{
        int ch;
        while ((ch = getopt(ac, av, "E:R:jpl:v:h")) != -1) {
                switch (ch) {
                case 'v':
                        verbosity = atoi(optarg);
                        break;
                case 'l':
                        logfile = optarg;
                        break;
                case 'j':
                        parse_mode = PARSE_MODE_JSON;
                        break;
                case 'p':
                        parse_mode = PARSE_MODE_PLIST;
                        break;
                case 'E':
                case 'R':
                        pngext_main(ac, av);
                        return 0;
                        /* NOT REACHED */
                case 'h':
                case '?':
                default:
                        usage(av[0]);
                        /* NOT REACHED */
                }
        }

        if (parse_mode) {
                parse_stdin();
                return 0;
                /* NOT REACHED */
        }

        td_set_log_fatal_error_callback(on_error_cb);
        /* NOTE: disable TDLib logging to stderr on engine creation */
        telega_engine_set_logging(NULL, verbosity, NULL);

        engine = telega_engine_new(stdout_output, NULL);
        assert(engine != NULL);

        telega_engine_set_logging(engine, verbosity, logfile);

        telega_engine_start(engine);
        stdin_loop(engine);
        telega_engine_stop(engine);

        telega_engine_destroy(engine);
        return 0;
}
