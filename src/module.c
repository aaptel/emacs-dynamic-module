/*
  module.c - Module loading and runtime implementation
  Copyright (C) 2015 Free Software Foundation, Inc.

  This file is part of GNU Emacs.

  GNU Emacs is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  GNU Emacs is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stddef.h>

#include <config.h>
#include "lisp.h"
#include "emacs_module.h"
#include "dynlib.h"
#include "coding.h"

struct emacs_value_tag { Lisp_Object v; };

static const size_t value_frame_size = 512;

struct emacs_value_frame {
  struct emacs_value_tag objects[value_frame_size];
  size_t offset;
  struct emacs_value_frame *next;
};

static void initialize_frame (struct emacs_value_frame *frame)
{
  frame->offset = 0;
  frame->next = 0;
}

struct emacs_env_private {
  struct emacs_value_frame initial_frame;
  struct emacs_value_frame *current_frame;
};

void syms_of_module (void);
static emacs_env* module_get_environment (struct emacs_runtime *ert);
static bool module_error_check (emacs_env *env);
static void module_error_clear (emacs_env *env);
static bool module_error_get (emacs_env *emv, emacs_value *sym, emacs_value *data);
static void module_error_signal (emacs_env *env, emacs_value sym, emacs_value data);
static emacs_value module_make_fixnum (emacs_env *env, int64_t n);
static int64_t module_fixnum_to_int (emacs_env *env, emacs_value n);
static emacs_value module_intern (emacs_env *env, const char *name);
static emacs_value module_make_function (emacs_env *env,
                                         int min_arity,
                                         int max_arity,
                                         emacs_subr subr,
                                         void *data);
static emacs_value module_funcall (emacs_env *env,
                                   emacs_value fun,
                                   int nargs,
                                   emacs_value args[]);
static emacs_value module_make_global_ref (emacs_env *env,
                                           emacs_value ref);
static void module_free_global_ref (emacs_env *env,
                                    emacs_value ref);

static emacs_value module_make_string (emacs_env *env, const char *str, size_t lenght);
static bool module_copy_string_contents (emacs_env *env,
                                         emacs_value value,
                                         char *buffer,
                                         size_t* length);
static enum emacs_type module_type_of (emacs_env *env, emacs_value value);
static emacs_value module_make_float (emacs_env *env, double d);
static double module_float_to_c_double (emacs_env *env, emacs_value f);

/*
 * Each instance of emacs_env get its own id from a simple counter
 */
static int32_t next_module_id = 1;

static inline Lisp_Object value_to_lisp (emacs_value v)
{
  return v->v;
}

static inline emacs_value lisp_to_value (emacs_env *env, Lisp_Object o)
{
  struct emacs_env_private *const p = env->private_members;
  eassert (p->current_frame);
  eassert (p->current_frame->offset < value_frame_size);
  eassert (! p->current_frame->next);
  if (p->current_frame->offset == value_frame_size - 1)
    {
      p->current_frame->next = malloc (sizeof *p->current_frame->next);
      if (! p->current_frame->next) return 0;
      initialize_frame (p->current_frame->next);
      p->current_frame = p->current_frame->next;
    }
  emacs_value value = p->current_frame->objects + p->current_frame->offset;
  value->v = o;
  ++p->current_frame->offset;
  return value;
}

struct env_storage {
  emacs_env pub;
  struct emacs_env_private priv;
};

static emacs_env *initial_environment;

static emacs_env* module_get_environment (struct emacs_runtime *ert)
{
  return initial_environment;
}

static void initialize_environment (struct env_storage *env)
{
  initialize_frame (&env->priv.initial_frame);
  env->priv.current_frame = &env->priv.initial_frame;
  env->pub.size            = sizeof env->pub;
  env->pub.module_id       = next_module_id++;
  env->pub.make_global_ref = module_make_global_ref;
  env->pub.free_global_ref = module_free_global_ref;
  env->pub.type_of         = module_type_of;
  env->pub.error_check     = module_error_check;
  env->pub.error_clear     = module_error_clear;
  env->pub.error_get       = module_error_get;
  env->pub.error_signal    = module_error_signal;
  env->pub.make_fixnum     = module_make_fixnum;
  env->pub.fixnum_to_int   = module_fixnum_to_int;
  env->pub.make_float      = module_make_float;
  env->pub.float_to_c_double = module_float_to_c_double;
  env->pub.intern          = module_intern;
  env->pub.make_function   = module_make_function;
  env->pub.funcall         = module_funcall;
  env->pub.make_string     = module_make_string;
  env->pub.copy_string_contents = module_copy_string_contents;
  env->pub.private_members = &env->priv;
}

static void finalize_environment (struct env_storage *env)
{
  for (struct emacs_value_frame *frame = env->priv.initial_frame.next; frame; frame = frame->next)
    free (frame);
}

/*
 * To make global refs (GC-protected global values) we keep a hash
 * that maps module-id to a list of their global values.
 */

static emacs_value module_make_global_ref (emacs_env *env,
                                           emacs_value ref)
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (Vmodule_refs_hash);
  Lisp_Object mid = make_number (env->module_id);
  Lisp_Object new_obj = value_to_lisp (ref);
  EMACS_UINT hashcode;
  ptrdiff_t i = hash_lookup (h, mid, &hashcode);

  if (i >= 0)
    {
      Lisp_Object v = HASH_VALUE (h, i);
      set_hash_value_slot (h, i, Fcons (new_obj, v));
    }
  else
    {
      hash_put (h, mid, Fcons (new_obj, Qnil), hashcode);
    }

  return ref;
}

static void module_free_global_ref (emacs_env *env,
                                    emacs_value ref)
{
  struct Lisp_Hash_Table *h = XHASH_TABLE (Vmodule_refs_hash);
  Lisp_Object mid = make_number (env->module_id);
  EMACS_UINT hashcode;
  ptrdiff_t i = hash_lookup (h, mid, &hashcode);

  if (i >= 0)
    {
      set_hash_value_slot (h, i,
                           Fdelq (value_to_lisp (ref),
                                  HASH_VALUE (h, i)));
    }
}

static bool module_pending_error;
static Lisp_Object module_error_symbol;
static Lisp_Object module_error_data;

static bool module_error_check (emacs_env *env)
{
  return module_pending_error;
}

static void module_error_clear (emacs_env *env)
{
  module_pending_error = false;
}

static bool module_error_get (emacs_env *env, emacs_value *sym, emacs_value *data)
{
  if (! module_pending_error) return false;
  *sym = lisp_to_value (env, module_error_symbol);
  *data = lisp_to_value (env, module_error_data);
  return true;
}

static void module_error_signal_1 (Lisp_Object sym, Lisp_Object data)
{
  eassert (! module_pending_error);
  module_pending_error = true;
  module_error_symbol = sym;
  module_error_data = data;
}

/*
 * Like for `signal', DATA must be a list
 */
static void module_error_signal (emacs_env *env, emacs_value sym, emacs_value data)
{
  module_error_signal_1 (value_to_lisp (sym), value_to_lisp (data));
}

static void module_wrong_type (Lisp_Object predicate, Lisp_Object value)
{
  module_error_signal_1 (Qwrong_type_argument, list2 (predicate, value));
}

static Lisp_Object module_handle_error_ptr (Lisp_Object err, const void *ptr)
{
  module_error_signal_1 (XCAR (err), XCDR (err));
  return Qnil;
}

static emacs_value module_make_fixnum (emacs_env *env, int64_t n)
{
  if (n < MOST_NEGATIVE_FIXNUM)
    {
      module_error_signal_1 (Qunderflow_error, Qnil);
      return NULL;
    }
  if (n > MOST_POSITIVE_FIXNUM)
    {
      module_error_signal_1 (Qoverflow_error, Qnil);
      return NULL;
    }
  return lisp_to_value (env, make_number (n));
}

static int64_t module_fixnum_to_int (emacs_env *env, emacs_value n)
{
  const Lisp_Object l = value_to_lisp (n);
  if (! INTEGERP (l))
    {
      module_wrong_type (Qintegerp, l);
      return 0;
    }
  return (int64_t) XINT (l);
}

static emacs_value module_make_float (emacs_env *env, double d)
{
  return lisp_to_value (env, make_float (d));
}

static double module_float_to_c_double (emacs_env *env, emacs_value f)
{
  const Lisp_Object lisp = value_to_lisp (f);
  if (! FLOATP (lisp))
    {
      module_wrong_type (Qfloatp, lisp);
      return 0;
    }
  return (double) XFLOAT_DATA (lisp);
}

static Lisp_Object module_intern_1 (const void *name)
{
  return intern ((const char *) name);
}

static emacs_value module_intern (emacs_env *env, const char *name)
{
  return lisp_to_value (env, internal_condition_case_ptr (module_intern_1, name, Qt, module_handle_error_ptr));
}

struct module_make_string_args {
  const char *str;
  size_t length;
};

static Lisp_Object module_make_string_1 (const void *args)
{
  const struct module_make_string_args *const a = (const struct module_make_string_args *) args;
  return make_string (a->str, a->length);
}

static emacs_value module_make_string (emacs_env *env, const char *str, size_t length)
{
  /* Assume STR is utf8 encoded */
  const struct module_make_string_args args = {str, length};
  return lisp_to_value (env, internal_condition_case_ptr (module_make_string_1, &args, Qt, module_handle_error_ptr));
}

static bool module_copy_string_contents (emacs_env *env,
                                         emacs_value value,
                                         char *buffer,
                                         size_t* length)
{
  Lisp_Object lisp_str = value_to_lisp (value);
  if (! STRINGP (lisp_str))
    {
      module_wrong_type (Qstringp, lisp_str);
      return false;
    }

  size_t raw_size = SBYTES (lisp_str);

  /*
   * Emacs internal encoding is more-or-less UTF8, let's assume utf8
   * encoded emacs string are the same byte size.
   */

  if (!buffer || length == 0 || *length-1 < raw_size)
    {
      *length = raw_size + 1;
      return false;
    }

  Lisp_Object lisp_str_utf8 = ENCODE_UTF_8 (lisp_str);
  eassert (raw_size == SBYTES (lisp_str_utf8));
  *length = raw_size + 1;
  memcpy (buffer, SDATA (lisp_str_utf8), SBYTES (lisp_str_utf8));
  buffer[raw_size] = 0;

  return true;
}

static enum emacs_type module_type_of (emacs_env *env, emacs_value value)
{
  Lisp_Object obj = value_to_lisp (value);

  /* Module writers probably don't care about internal types which are
     subject to change anyway... */

  switch (XTYPE (obj))
    {
    case_Lisp_Int:
      return EMACS_FIXNUM;
    case Lisp_Symbol:
      return EMACS_SYMBOL;
    case Lisp_Float:
      return EMACS_FLOAT;
    case Lisp_String:
      return EMACS_STRING;
    case Lisp_Cons:
      return EMACS_CONS;
    case Lisp_Vectorlike:
      if (HASH_TABLE_P (obj))
        return EMACS_HASHTABLE;
      if (VECTORP (obj))
        return EMACS_VECTOR;
    /* FALLTHROUGH */
    default:
      return EMACS_OTHER;
    }
}

struct module_fun_env
{
  emacs_subr subr;
  void *data;
};

/*
 * A module function is lambda function that calls `module-call',
 * passing the function pointer of the module function along with the
 * module emacs_env pointer as arguments.
 *
 *   (function
 *    (lambda
 *     (&rest arglist)
 *     (module-call
 *      envobj
 *      arglist)))
 *
 */
static emacs_value module_make_function (emacs_env *env,
                                         int min_arity,
                                         int max_arity,
                                         emacs_subr subr,
                                         void *data)
{
  Lisp_Object envobj;
  Lisp_Object Qrest = intern ("&rest");
  Lisp_Object Qarglist = intern ("arglist");
  Lisp_Object Qmodule_call = intern ("module-call");

  /* XXX: This should need to be freed when envobj is GC'd */
  struct module_fun_env *envptr = xzalloc (sizeof (*envptr));
  envptr->subr = subr;
  envptr->data = data;
  envobj = make_save_ptr ((void*) envptr);

  Lisp_Object form = list2 (Qfunction,
                            list3 (Qlambda,
                                   list2 (Qrest, Qarglist),
                                   list3 (Qmodule_call,
                                          envobj,
                                          Qarglist)));

  Lisp_Object ret = Feval (form, Qnil);

  return lisp_to_value (env, ret);
}

static Lisp_Object module_handle_signal (Lisp_Object err, ptrdiff_t nargs, Lisp_Object *args)
{
  module_error_signal_1 (XCAR (err), XCDR (err));
  return Qnil;
}

static Lisp_Object module_handle_throw (Lisp_Object tag, Lisp_Object value, ptrdiff_t nargs, Lisp_Object *args)
{
  module_error_signal_1 (Qno_catch, list2 (tag, value));
  return Qnil;
}

static Lisp_Object module_funcall_2 (ptrdiff_t nargs, Lisp_Object *args)
{
  return catch_all_n (Ffuncall, nargs, args, module_handle_throw);
}

static emacs_value module_funcall (emacs_env *env,
                                   emacs_value fun,
                                   int nargs,
                                   emacs_value args[])
{
  /*
   *  Make a new Lisp_Object array starting with the function as the
   *  first arg, because that's what Ffuncall takes
   */
  int i;
  Lisp_Object *newargs = xmalloc ((nargs+1) * sizeof (*newargs));

  newargs[0] = value_to_lisp (fun);
  for (i = 0; i < nargs; i++)
    newargs[1 + i] = value_to_lisp (args[i]);

  Lisp_Object ret = internal_condition_case_n (module_funcall_2, nargs+1, newargs, Qt, module_handle_signal);

  xfree (newargs);
  return lisp_to_value (env, ret);
}

DEFUN ("module-call", Fmodule_call, Smodule_call, 2, 2, 0,
       doc: /* Internal function to call a module function.
SUBRPTR is the module function pointer (emacs_subr prototype) to call.
DATAPTR is the data pointer passed to make_function.
ARGLIST is a list of argument passed to SUBRPTR. */)
  (Lisp_Object envobj, Lisp_Object arglist)
{
  struct env_storage env;
  initialize_environment (&env);

  int len = XINT (Flength (arglist));
  emacs_value *args = xzalloc (len * sizeof (*args));
  int i;

  for (i = 0; i < len; i++)
    {
      args[i] = lisp_to_value (&env.pub, XCAR (arglist));
      if (! args[i]) memory_full (sizeof *args[i]);
      arglist = XCDR (arglist);
    }

  struct module_fun_env *envptr = (struct module_fun_env*) XSAVE_POINTER (envobj, 0);
  module_pending_error = false;
  emacs_value ret = envptr->subr (&env.pub, len, args, envptr->data);
  xfree (args);
  finalize_environment (&env);
  if (module_pending_error)
    Fsignal (module_error_symbol, module_error_data);
  return value_to_lisp (ret);
}

DEFUN ("module-load", Fmodule_load, Smodule_load, 1, 1, 0,
       doc: /* Load module FILE.  */)
  (Lisp_Object file)
{
  dynlib_handle_ptr handle;
  emacs_init_function module_init;
  void *gpl_sym;
  Lisp_Object doc_name, args[2];

  CHECK_STRING (file);
  handle = dynlib_open (SDATA (file));
  if (!handle)
    error ("Cannot load file %s", SDATA (file));

  gpl_sym = dynlib_sym (handle, "plugin_is_GPL_compatible");
  if (!gpl_sym)
    error ("Module %s is not GPL compatible", SDATA (file));

  module_init = (emacs_init_function) dynlib_sym (handle, "emacs_module_init");
  if (!module_init)
    error ("Module %s does not have an init function.", SDATA (file));

  struct emacs_runtime runtime = {
    .size = sizeof runtime,
    .get_environment = module_get_environment,
  };
  struct env_storage env;
  initialize_environment (&env);
  initial_environment = &env.pub;
  int r = module_init (&runtime);
  initial_environment = 0;
  finalize_environment (&env);

  return Qt;
}

void syms_of_module (void)
{
  DEFVAR_LISP ("module-refs-hash", Vmodule_refs_hash,
	       doc: /* Module global referrence table.  */);

  Vmodule_refs_hash = make_hash_table (hashtest_eql, make_number (DEFAULT_HASH_SIZE),
                                       make_float (DEFAULT_REHASH_SIZE),
                                       make_float (DEFAULT_REHASH_THRESHOLD),
                                       Qnil);

  defsubr (&Smodule_call);
  defsubr (&Smodule_load);
}
