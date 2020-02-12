/* Engine for telega-server */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "telega-engine.h"

struct telega_engine*
telega_engine_new(engine_output_func* output, void* user_data)
{
        struct telega_engine* engine = malloc(sizeof(struct telega_engine));
        assert(engine != NULL);

        engine->running = false;
        engine->output = output;
        engine->user_data = user_data;
        engine->user_data2 = NULL;

        engine->td_cln = td_json_client_create();
        assert(engine->td_cln != NULL);

#ifdef WITH_TON
        engine->ton_cln = tonlib_client_json_create();
        assert(engine->ton_cln != NULL);
#endif /* WITH_TON */

        sem_init(&engine->next_sem, 0, 0);
        memset(engine->next_cmd, '\0', 16 + 1);
        memset(&engine->next_plist, 0, sizeof(struct telega_dat));

        return engine;
}

static void* engine_tdlib_loop(void* engine_ptr);
#ifdef WITH_TON
static void* engine_tonlib_loop(void* engine_ptr);
#endif /* WITH_TON */

void
telega_engine_start(struct telega_engine* engine)
{
        engine->running = true;

        int rc = pthread_create(
                &engine->td_thread, NULL, engine_tdlib_loop, engine);
        assert(rc == 0);

#ifdef WITH_TON
        rc = pthread_create(
                &engine->ton_thread, NULL, engine_tonlib_loop, engine);
        assert(rc == 0);
#endif /* WITH_TON */
}

void
telega_engine_stop(struct telega_engine* engine)
{
        if (!engine->running)
                return;

        /* Gracefully stop the engine_tdlib_loop & engine_tonlib_loop */
        engine->running = false;

        int rc = pthread_join(engine->td_thread, NULL);
        assert(rc == 0);

#ifdef WITH_TON
        rc = pthread_join(engine->ton_thread, NULL);
        assert(rc == 0);
#endif  /* WITH_TON */
}

void
telega_engine_destroy(struct telega_engine* engine)
{
        assert(!engine->running);

        td_json_client_destroy(engine->td_cln);

#ifdef WITH_TON
        tonlib_client_json_destroy(engine->ton_cln);
#endif /* WITH_TON */

        sem_destroy(&engine->next_sem);
        tdat_drop(&engine->next_plist);

        free(engine);
}

void
telega_engine_output(struct telega_engine* engine, const char* cmd,
                     const char* json)
{
        telega_engine_log(engine, 5, "[telega-server] "
                          "OUTPUT %s: %s\n", cmd, json);

        strncpy(engine->next_cmd, cmd, 16);

        struct telega_dat json_src = TDAT_INIT;
        struct telega_dat* plist_dst = &engine->next_plist;
        tdat_reset(plist_dst);

        tdat_append(&json_src, json, strlen(json));
        tdat_json_value(&json_src, plist_dst);
        tdat_append1(plist_dst, "\0");
        tdat_drop(&json_src);

        assert(tdat_len(plist_dst) > 0);
        engine->output(engine, engine->next_cmd, plist_dst);
}

void
telega_engine_send(struct telega_engine* engine, const char* cmd,
                   struct telega_dat* plist_data)
{
        struct telega_dat json_dst = TDAT_INIT;
        tdat_plist_value(plist_data, &json_dst);
        tdat_append1(&json_dst, "\0");

        telega_engine_log(engine, 5, "[telega-server] INPUT: %s\n",
                          json_dst.data);

        if (!strcmp(cmd, "send"))
                td_json_client_send(engine->td_cln, json_dst.data);
#ifdef WITH_TON
        else if (!strcmp(cmd, "ton"))
                tonlib_client_json_send(engine->ton_cln, json_dst.data);
#endif /* WITH_TON */
#ifdef WITH_VOIP
        /* TODO */
        else if (!strcmp(cmd, "voip"))
                telega_voip_cmd(json_dst.data);
#endif /* WITH_VOIP */
        else {
                telega_engine_log(
                        engine, 1, "[telega-server] Unknown command: %s", cmd);

                char error[128];
                snprintf(error, 128, "(:message \"Unknown cmd `%s'\")", cmd);
                telega_engine_output(engine, "telega-error", error);
        }

        tdat_reset(&json_dst);
}


static void*
engine_tdlib_loop(void* engine_ptr)
{
        struct telega_engine* engine = (struct telega_engine*)engine_ptr;

        while (engine->running) {
                const char *res = td_json_client_receive(engine->td_cln, 0.5);
                if (res)
                        telega_engine_output(engine, "event", res);
        }
        return NULL;
}

#ifdef WITH_TON
static void*
engine_tonlib_loop(void* engine_ptr)
{
        struct telega_engine* engine = (struct telega_engine*)engine_ptr;

        while (engine->running) {
                const char *res = tonlib_client_json_receive(engine->ton_cln, 0.5);
                if (res)
                        telega_engine_output(engine, "ton-event", res);
        }
        return NULL;
}
#endif /* WITH_TON */

void
telega_engine_set_logging(struct telega_engine* engine,
                          int verbosity, const char* logfile)
{
        if (engine)
                engine->verbosity = verbosity;

        /* TDLib logging */
        char req[1024];
        snprintf(req, 1024, "{\"@type\":\"setLogVerbosityLevel\","
                 "\"new_verbosity_level\":%d}", verbosity);
        td_json_client_execute(engine ? engine->td_cln : NULL, req);

        if (logfile) {
                snprintf(req, 1024, "{\"@type\":\"setLogStream\","
                         "\"log_stream\":{\"@type\":\"logStreamFile\","
                         "\"path\":\"%s\", \"max_file_size\":2097152}}", logfile);
        } else {
                snprintf(req, 1024, "{\"@type\":\"setLogStream\","
                        "\"log_stream\":{\"@type\":\"%s\"}}",
                         "logStreamEmpty");
        }
        td_json_client_execute(engine ? engine->td_cln : NULL, req);
}

#if 0
static void
engine_log_fmt(struct telega_dat* dst, const char* fmt, va_list ap)
{
        const char* fmtstart = fmt;
        char* fmtok;

        while ((fmtok = strchr(fmtstart, '%'))) {
                switch (fmtok[1]) {
                case 's':
                        tdat_append_str_esc_2quote(dst, va_arg(ap, const char*));
                        break;
                default:
                        /* Only %s is supported in engine_log() */
                        assert(NULL);
                }

                fmtstart = &fmtok[2];
        }

        /* Append rest of the FMT after last % */
        tdat_append(dst, fmtstart, fmt + strlen(fmt) - fmtstart);
}
#endif /* 0 */

void
telega_engine_log(struct telega_engine* engine, int verbosity,
                  const char* fmt, ...)
{
        if (verbosity > engine->verbosity)
                return;

        va_list ap;
        va_start(ap, fmt);

#if 0
        struct telega_dat req_data = TDAT_INIT;
        char req[64];
        snprintf(req, 64, "{\"@type\":\"addLogMessage\","
                 "\"verbosity_level\":%d, \"text\":\"", verbosity);
        tdat_append(&req_data, req, strlen(req));

        engine_log_fmt(&req_data, fmt, ap);

        tdat_append(&req_data, "\"}", 2);
        tdat_append1(&req_data, "\0");

        td_json_client_execute(engine ? engine->td_cln : NULL,
                               tdat_start(&req_data));
        tdat_drop(&req_data);
#endif /* 0 */

        vfprintf(stderr, fmt, ap);
        va_end(ap);
}
