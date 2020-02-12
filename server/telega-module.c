/* Module to run telega-server inside Emacs */
#include <string.h>
#include <stdio.h>

#include "emacs-module.h"

#include "telega-engine.h"

int plugin_is_GPL_compatible;

struct telega_event {
        struct telega_engine* engine;
        char cmd[16];
        char* plist_str;
};
        
struct telega_event telega_event;
sem_t telega_event_sem;

static emacs_value Qnil;
static emacs_value Qtelega_event;
static emacs_value Qmain_thread;
static emacs_value Fsymbol_value;
static emacs_value Fthread_signal;

static void
signal_main_thread(emacs_env* env, emacs_value event_str)
{
        emacs_value main_thread = env->funcall(
                env, Fsymbol_value, 1, (emacs_value[]){Qmain_thread});

        env->funcall(env, Fthread_signal, 3, (emacs_value[]){main_thread, Qtelega_event, event_str});
}

void
module_engine_finalizer(void* arg)
{
        struct telega_engine* engine = arg;
        emacs_env* env = engine->user_data;
        emacs_value callback = engine->user_data2;
        env->free_global_ref(env, callback);

        telega_engine_destroy(engine);
}

static void
module_engine_output(struct telega_engine* engine, const char* otype,
                     const char* plist)
{
        emacs_env* env = engine->user_data;
        emacs_value callback = engine->user_data2;

        fprintf(stderr, "OUTPUT in thread: %lu\n", pthread_self());
        /* signal_main_thread(env, callback); */
        /* emacs_value otype_str = env->make_string(env, otype, strlen(otype)); */
        /* emacs_value plist_str = env->make_string(env, plist, strlen(plist)); */
        /* env->funcall(env, callback, 2, (emacs_value[]){otype_str, plist_str}); */
        /* env->non_local_exit_check(env); */
}

static const char* engine_create_docstr = "\
Create telega engine.\n\
\n\
\(fn EVENT-CALLBACK-FUN)";

static emacs_value
Fengine_create(emacs_env* env, ptrdiff_t nargs, emacs_value* args, void* ignore)
{
        struct telega_engine* engine = telega_engine_new(
                module_engine_output, env);
        assert(engine != NULL);

        engine->user_data = env;
        engine->user_data2 = env->make_global_ref(env, args[0]);

        fprintf(stderr, "CREATE in thread: %lu\n", pthread_self());
        return env->make_user_ptr(env, module_engine_finalizer, engine);
}

static const char* engine_start_docstr = "\
Start telega engine.\n\
\n\
\(fn ENGINE)";

static emacs_value
Fengine_start(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *ignore)
{
        struct telega_engine* engine = env->get_user_ptr(env, args[0]);
        if (env->non_local_exit_check(env))
                return Qnil;

        telega_engine_start(engine);
        return Qnil;
}

static const char* engine_stop_docstr = "\
Stop telega engine.\n\
\n\
\(fn ENGINE)";

static emacs_value
Fengine_stop(emacs_env* env, ptrdiff_t nargs, emacs_value* args, void* ignore)
{
        struct telega_engine* engine = env->get_user_ptr(env, args[0]);
        if (env->non_local_exit_check(env))
                return Qnil;

        telega_engine_stop(engine);
        return Qnil;
}

static const char* engine_send_docstr = "\
Send plist string to the telega engine.\n\
\n\
\(fn ENGINE CMD PLIST-STR)";

static emacs_value
Fengine_send(emacs_env* env, ptrdiff_t nargs, emacs_value* args, void* ignore)
{
        struct telega_engine* engine = env->get_user_ptr(env, args[0]);
        if (env->non_local_exit_check(env))
                return Qnil;

        char cmd[128];
        ptrdiff_t dlen = 128;
        env->copy_string_contents(env, args[1], cmd, &dlen);
        cmd[128] = '\0';

        struct telega_dat plist_data = TDAT_INIT;
        env->copy_string_contents(env, args[2], NULL, &dlen);
        tdat_ensure(&plist_data, dlen);
        env->copy_string_contents(env, args[2], plist_data.data, &dlen);

        telega_engine_send(engine, cmd, &plist_data);
        tdat_reset(&plist_data);

        return Qnil;
}

int
emacs_module_init(struct emacs_runtime* ert)
{
        emacs_env* env = ert->get_environment(ert);

        if (env->size < sizeof(struct emacs_env_26))
                /* At least Emacs 26.0 is required */
                return 2;

        /* Symbols */
        Qnil = env->make_global_ref(
                env, env->intern(env, "nil"));
        Qmain_thread = env->make_global_ref(
                env, env->intern(env, "main-thread"));
        Qtelega_event = env->make_global_ref(
                env, env->intern(env, "telega-event"));
        Fsymbol_value = env->make_global_ref(
                env, env->intern(env, "symbol-value"));
        Fthread_signal = env->make_global_ref(
                env, env->intern(env, "thread-signal"));

        emacs_value fset = env->intern(env, "fset");

        /* Exports */
        emacs_value fun, sym;
        fun = env->make_function(
                env, 1, 1, Fengine_create, engine_create_docstr, NULL);
        sym = env->intern(env, "telega-engine--create");
        env->funcall(env, fset, 2, (emacs_value[]){sym, fun});

        fun = env->make_function(
                env, 1, 1, Fengine_start, engine_start_docstr, NULL);
        sym = env->intern(env, "telega-engine--start");
        env->funcall(env, fset, 2, (emacs_value[]){sym, fun});

        fun = env->make_function(
                env, 1, 1, Fengine_stop, engine_stop_docstr, NULL);
        sym = env->intern(env, "telega-engine--stop");
        env->funcall(env, fset, 2, (emacs_value[]){sym, fun});

        fun = env->make_function(
                env, 3, 3, Fengine_send, engine_send_docstr, NULL);
        sym = env->intern(env, "telega-engine--send");
        env->funcall(env, fset, 2, (emacs_value[]){sym, fun});

        fun = env->make_function(
                env, 0, 0, Fengine_recv, engine_recv_docstr, NULL);
        sym = env->intern(env, "telega-engine--recv");
        env->funcall(env, fset, 2, (emacs_value[]){sym, fun});

        if (env->non_local_exit_check(env))
                return -1;

        /* Disable TDLib logging to stderr on engine creations */
        telega_engine_set_logging(NULL, 0, NULL);

        return 0;
}
