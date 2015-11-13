#include <assert.h>
#include <stdio.h>
#include <emacs_module.h>

int plugin_is_GPL_compatible;

static emacs_value Fmodt_non_local_exit_signal (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  env->non_local_exit_signal (env, env->intern (env, "error"), env->make_integer (env, 56));
  return NULL;
}

static emacs_value Fmodt_non_local_exit_throw (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  env->non_local_exit_throw (env, env->intern (env, "tag"), env->make_integer (env, 65));
  return NULL;
}

static emacs_value Fmodt_non_local_exit_funcall (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (nargs == 1);
  const emacs_value result = env->funcall (env, args[0], 0, NULL);
  emacs_value non_local_exit_symbol, non_local_exit_data;
  enum emacs_funcall_exit code = env->non_local_exit_get (env, &non_local_exit_symbol, &non_local_exit_data);
  switch (code)
    {
    case emacs_funcall_exit_return:
      return result;
    case emacs_funcall_exit_signal:
      {
        env->non_local_exit_clear (env);
        const emacs_value Flist = env->intern (env, "list");
        emacs_value list_args[] = {env->intern (env, "signal"), non_local_exit_symbol, non_local_exit_data};
        return env->funcall (env, Flist, 3, list_args);
      }
    case emacs_funcall_exit_throw:
      {
        env->non_local_exit_clear (env);
        const emacs_value Flist = env->intern (env, "list");
        emacs_value list_args[] = {env->intern (env, "throw"), non_local_exit_symbol, non_local_exit_data};
        return env->funcall (env, Flist, 3, list_args);
      }
    }
  /* never reached */
  return env->intern (env, "nil");;
}

/* Binds NAME to FUN */
static void bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs */
static void provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

int emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

  bind_function (env, "modt-non-local-exit-signal", env->make_function (env, 0, 0, Fmodt_non_local_exit_signal, NULL));
  bind_function (env, "modt-non-local-exit-throw", env->make_function (env, 0, 0, Fmodt_non_local_exit_throw, NULL));
  bind_function (env, "modt-non-local-exit-funcall", env->make_function (env, 1, 1, Fmodt_non_local_exit_funcall, NULL));
  provide (env, "modt-error");
  return 0;
}
