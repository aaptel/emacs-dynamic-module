/*

  basic.c - sample module

  This module provides a simple `basic-sum' function.

  I've used the following prefixes throughout the code:
  - Sfoo: function value (Subr)
  - Qfoo: symbol value (Quote)
  - Ffoo: function callable by Emacs

*/

#include <emacs_module.h>

int plugin_is_GPL_compatible;

/* C function we want to expose to emacs */
static int64_t sum (int64_t a, int64_t b)
{
  return a + b;
}

/* Proper module function that wraps the C function */
static emacs_value Fsum (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  int64_t a = env->extract_integer (env, args[0]);
  int64_t b = env->extract_integer (env, args[1]);

  int64_t r = sum(a, b);

  return env->make_integer (env, r);
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
  emacs_value Ssum = env->make_function (env, 2, 2, Fsum, "Return A + B", NULL);

  bind_function (env, "modt-basic-sum", Ssum);
  provide (env, "modt-basic");

  return 0;
}
