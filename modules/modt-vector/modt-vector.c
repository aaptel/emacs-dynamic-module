
#include <emacs_module.h>

int plugin_is_GPL_compatible;

static emacs_value Fmodt_vector_fill (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  size_t i;
  emacs_value vec = args[0];
  emacs_value val = args[1];
  const size_t size = env->vec_size (env, vec);
  for (i = 0; i < size; i++)
    env->vec_set (env, vec, i, val);
  return env->intern (env, "t");
}

static emacs_value Fmodt_vector_eq (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  size_t i;
  emacs_value vec = args[0];
  emacs_value val = args[1];
  const size_t size = env->vec_size (env, vec);
  for (i = 0; i < size; i++)
    if (!env->eq (env, env->vec_get (env, vec, i), val))
        return env->intern (env, "nil");
  return env->intern (env, "t");
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
  bind_function (env, "modt-vector-fill", env->make_function (env, 2, 2, Fmodt_vector_fill, NULL, NULL));
  bind_function (env, "modt-vector-eq", env->make_function (env, 2, 2, Fmodt_vector_eq, NULL, NULL));
  provide (env, "modt-vector");
  return 0;
}
