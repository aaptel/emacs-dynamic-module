#include <emacs_module.h>

int plugin_is_GPL_compatible;

static emacs_value Qnil;

static emacs_value Fmodt_string_a_to_b (emacs_env *env, int nargs, emacs_value args[], void* data)
{
  if (nargs != 1)
    {
      return Qnil;
    }

  emacs_value lisp_str = args[0];
  size_t size = 0;
  char * buf = NULL;
  size_t i;

  env->copy_string_contents (env, lisp_str, buf, &size);
  buf = malloc (size);
  env->copy_string_contents (env, lisp_str, buf, &size);

  for (i = 0; i+1 < size; i++) {
    if (buf[i] == 'a')
      buf[i] = 'b';
  }

  return env->make_string (env, buf, size-1);
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
  bind_function (env, "modt-string-a-to-b", env->make_function (env, 1, 1, Fmodt_string_a_to_b, NULL));
  provide (env, "modt-string");
  return 0;
}
