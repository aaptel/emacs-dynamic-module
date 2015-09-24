
#include <emacs_module.h>

int plugin_is_GPL_compatible;

static emacs_value Qnil;
static emacs_value Qt;

static emacs_value Fmodt_globref_make (emacs_env *env, int nargs, emacs_value args[])
{
  /* make a big string and make it global */
  size_t i;
  char str[26*100];

  for (i = 0; i < sizeof (str); i++)
    {
      str[i] = 'a' + (i % 26);
    }

  /* we don't need to null-terminate str */
  emacs_value lisp_str = env->make_string (env, str, sizeof (str));
  return env->make_global_ref (env, lisp_str);
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
  bind_function (env, "modt-globref-make", env->make_function (env, 1, 1, Fmodt_globref_make));
  provide (env, "modt-globref");
  return 0;
}
