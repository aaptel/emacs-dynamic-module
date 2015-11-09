#include <assert.h>
#include <stdio.h>
#include <emacs_module.h>

int plugin_is_GPL_compatible;

static emacs_value Fmodt_error_signal (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (env->error_check (env) == emacs_funcall_exit_return);
  env->error_signal (env, env->intern (env, "error"), env->make_fixnum (env, 56));
  return NULL;
}

static emacs_value Fmodt_error_throw (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (env->error_check (env) == emacs_funcall_exit_return);
  env->error_throw (env, env->intern (env, "tag"), env->make_fixnum (env, 65));
  return NULL;
}

static emacs_value Fmodt_error_funcall (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (nargs == 1);
  const emacs_value result = env->funcall (env, args[0], 0, NULL);
  emacs_value error_symbol, error_data;
  enum emacs_funcall_exit code = env->error_get (env, &error_symbol, &error_data);
  switch (code)
    {
    case emacs_funcall_exit_return:
      return result;
    case emacs_funcall_exit_signal:
      {
        env->error_clear (env);
        const emacs_value Flist = env->intern (env, "list");
        emacs_value list_args[] = {env->intern (env, "signal"), error_symbol, error_data};
        return env->funcall (env, Flist, 3, list_args);
      }
    case emacs_funcall_exit_throw:
      {
        env->error_clear (env);
        const emacs_value Flist = env->intern (env, "list");
        emacs_value list_args[] = {env->intern (env, "throw"), error_symbol, error_data};
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

  bind_function (env, "modt-error-signal", env->make_function (env, 0, 0, Fmodt_error_signal, NULL));
  bind_function (env, "modt-error-throw", env->make_function (env, 0, 0, Fmodt_error_throw, NULL));
  bind_function (env, "modt-error-funcall", env->make_function (env, 1, 1, Fmodt_error_funcall, NULL));
  provide (env, "modt-error");
  return 0;
}
