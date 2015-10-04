#include <assert.h>
#include <stdio.h>
#include <emacs_module.h>

int plugin_is_GPL_compatible;

static emacs_value Qnil;
static emacs_value Qt;

static emacs_value Fmodt_error_signal (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (!env->error_check (env));
  env->error_signal (env, env->intern (env, "error"), env->make_fixnum (env, 56));
  return Qt;
}

static emacs_value Fmodt_error_funcall (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  assert (nargs == 1);
  const emacs_value result = env->funcall (env, args[0], 0, NULL);
  emacs_value error_symbol, error_data;
  if (env->error_get (env, &error_symbol, &error_data))
    {
      env->error_clear (env);
      const emacs_value Flist = env->intern (env, "list");
      emacs_value list_args[] = {env->intern (env, "signal"), error_symbol, error_data};
      return env->funcall (env, Flist, 3, list_args);
    }
  else
    {
      return result;
    }
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
  Qnil = env->intern (env, "nil");
  Qt = env->intern (env, "t");
  bind_function (env, "modt-error-signal", env->make_function (env, 0, 0, Fmodt_error_signal, NULL));
  bind_function (env, "modt-error-funcall", env->make_function (env, 1, 1, Fmodt_error_funcall, NULL));
  provide (env, "modt-error");
  return 0;
}
