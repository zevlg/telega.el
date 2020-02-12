#ifndef __TELEGA_ENGINE_H__
#define __TELEGA_ENGINE_H__

#include <stdbool.h>
#include <pthread.h>
#include <semaphore.h>

#include <td/telegram/td_json_client.h>
#ifdef WITH_TON
#include <tonlib/tonlib_client_json.h>
#endif  /* WITH_TON */

#include "telega-dat.h"

struct telega_engine;
typedef void engine_output_func(struct telega_engine* engine,
                                const char* otype, struct telega_dat* plist);

struct telega_engine {
        /* Engine params */
        engine_output_func* output;

        /** Engine state **/
        /* true if engine is running */
        volatile bool running;

        /* Verbosity for engine itself and for the tdlib */
        int verbosity;

        void* td_cln;
        pthread_t td_thread;

#ifdef WITH_TON
        void* ton_cln;
        pthread_t ton_thread;
#endif /* WITH_TON */

#ifdef WITH_VOIP
        VoIPController* voip;
#endif /* WITH_VOIP */

        void* user_data;
        void* user_data2;

        /** Next command to process in OUTPUT func
         * TODO: lock-free commands queue
         */
        sem_t next_sem;
        char next_cmd[16 + 1];
        struct telega_dat next_plist;
};

struct telega_engine* telega_engine_new(engine_output_func* output, void* user_data);
void telega_engine_start(struct telega_engine* engine);
void telega_engine_stop(struct telega_engine* engine);
void telega_engine_destroy(struct telega_engine* engine);

/* Output CMD's JSON to ENGINE's output function as plist */
void telega_engine_output(struct telega_engine* engine, const char* cmd,
                          const char* json);

/* Send command CMD to the ENGINE with PLIST_DATA */
void telega_engine_send(struct telega_engine* engine, const char* cmd,
                        struct telega_dat* plist_data);

/* Log message to the ENGINE */
void telega_engine_log(struct telega_engine* engine, int verbosity,
                       const char* fmt,...);

/*
 * Setup TDLib logging for the ENGINE
 * It is possible to call this func with ENGINE=NULL 
 * If LOGFILE is NULL, then disable logging.
 */
void telega_engine_set_logging(struct telega_engine* engine,
                               int verbosity, const char* logfile);

#endif  /* __TELEGA_ENGINE_H__ */
