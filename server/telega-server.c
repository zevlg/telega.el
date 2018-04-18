#include <stdbool.h>
#include <pthread.h>
#include <assert.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include <td/telegram/td_json_client.h>
#include <td/telegram/td_log.h>

/*
 * Input/Output Protocol:
 * ~~~~~~~~~~~~~~
 *   <COMMAND> <SPACE> <JSON-LEN> <NEWLINE>
 *   <JSON of JSON-LEN length> <NEWLINE>
 *
 * COMMAND is one of `exec', `send', `event' or `error'
 * `event' and `error' is used for output
 *
 * For example:
 *   event 108
 *   {"@type":"updateAuthorizationState","authorization_state":{"@type":"authorizationStateWaitTdlibParameters"}}
 *
 *   exec 118
 *   {"@type": "getTextEntities", "text": "@telegram /test_command https://telegram.org telegram.me", "@extra": ["5", 7.0]}
 *
 */

char* logfile = NULL;
int verbosity = 5;
const char* version = "0.1.2";


void
usage(char* prog)
{
        printf("Version %s\n", version);
        printf("usage: %s [-l FILE] [-v LVL] [-h]\n", prog);
        printf("\t-l FILE    Log to FILE (default=stderr)\n");
        printf("\t-v LVL     Verbosity level (default=5)\n");
        exit(0);
}

static void
on_error_cb(const char* errmsg)
{
        printf("error %zu\n%s\n", strlen(errmsg), errmsg);
        fflush(stdout);
}

static void*
tdlib_loop(void* cln)
{
        while (true) {
                const char *res = td_json_client_receive(cln, 1);
                if (res) {
                        printf("event %zu\n%s\n", strlen(res), res);
                        fflush(stdout);
                }
        }
        return NULL;
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
stdin_loop(void* cln)
{
        char cmdline[33];
        char* jsonv = NULL;
        size_t jsonsz = 0;

        signal(SIGHUP, on_sighup);
        while (fgets(cmdline, 33, stdin)) {
                cmdline[32] = '\0';

                char cmd[33];
                size_t cmdsz;
                if (2 != sscanf(cmdline, "%s %zu\n", cmd, &cmdsz)) {
                        fprintf(stderr, "Unexpected cmdline format: %s", cmdline);
                        continue;
                }

                if (jsonsz < cmdsz) {
                        jsonv = (char*)realloc(jsonv, cmdsz + 1);
                        assert(jsonv != NULL);
                        jsonsz = cmdsz;
                }

                /* read including newline */
                ssize_t rc = read(0, jsonv, jsonsz + 1);
                if (rc != jsonsz + 1) {
                        fprintf(stderr, "read(0, %zu) failed: rc =%zd",
                                jsonsz + 1, rc);
                        break;  /* EOF */
                }

                jsonv[jsonsz] = '\0';
                if (!strcmp(cmd, "send"))
                        td_json_client_send(cln, jsonv);
                else if (!strcmp(cmd, "exec"))
                        td_json_client_execute(cln, jsonv);
                else
                        fprintf(stderr, "Unknown command: %s", cmd);
        }

        free(jsonv);
}

int
main(int ac, char** av)
{
        int ch;
        while ((ch = getopt(ac, av, "l:v:h")) != -1) {
                switch (ch) {
                case 'v':
                        verbosity = atoi(optarg);
                        td_set_log_verbosity_level(verbosity);
                        break;
                case 'l':
                        logfile = optarg;
                        td_set_log_file_path(logfile);
                        break;
                case 'h':
                case '?':
                default:
                        usage(av[0]);
                }
        }
        td_set_log_fatal_error_callback(on_error_cb);

        void *client = td_json_client_create();

        pthread_t td_thread;
        int rc = pthread_create(&td_thread, NULL, tdlib_loop, client);
        assert(rc == 0);

        stdin_loop(client);

        td_json_client_destroy(client);

        return 0;
}
