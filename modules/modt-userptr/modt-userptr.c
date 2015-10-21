
#include <emacs_module.h>
#include <stdlib.h>

int plugin_is_GPL_compatible;

static emacs_value Qnil;
static emacs_value Qt;

struct super_struct
{
  int amazing_int;
  char large_unused_buffer[512];
};

static void finalizer (void *p)
{
  if (p)
    free (p);
}

static emacs_value Fmodt_userptr_make (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (nargs != 1)
    return Qnil;

  struct super_struct *p = calloc (1, sizeof(*p));
  p->amazing_int = env->fixnum_to_int (env, args[0]);
  return env->make_user_ptr (env, finalizer, p);
}

static emacs_value Fmodt_userptr_get (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  if (nargs != 1)
    return Qnil;

  struct super_struct *p = env->get_user_ptr_ptr (env, args[0]);
  return env->make_fixnum (env, p->amazing_int);
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
  bind_function (env, "modt-userptr-make", env->make_function (env, 1, 1, Fmodt_userptr_make, NULL));
  bind_function (env, "modt-userptr-get", env->make_function (env, 1, 1, Fmodt_userptr_get, NULL));
  provide (env, "modt-userptr");
  return 0;
}
