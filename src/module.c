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

#include <config.h>
#include "lisp.h"
#include "emacs_module.h"
#include "dynlib.h"
#include "coding.h"

#if defined(HAVE_PTHREAD)
#include <pthread.h>
static pthread_t main_thread;
#elif defined(WINDOWSNT)
#include <windows.h>
/* On Windows, we store a handle to the main thread instead of the
   thread ID because the latter can be reused when a thread terminates. */
static HANDLE main_thread;
#endif

struct emacs_value_tag { Lisp_Object v; };

void syms_of_module (void);
static struct emacs_runtime* module_get_runtime (void);
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
static void check_main_thread ();

emacs_value module_make_user_ptr (emacs_env *env,
                                  emacs_finalizer_function fin,
                                  void *ptr);

void* module_get_user_ptr_ptr (emacs_env *env, emacs_value uptr);
void module_set_user_ptr_ptr (emacs_env *env, emacs_value uptr, void *ptr);

emacs_finalizer_function module_get_user_ptr_finalizer (emacs_env *env, emacs_value uptr);
void module_set_user_ptr_finalizer (emacs_env *env,
                                    emacs_value uptr,
                                    emacs_finalizer_function fin);

/*
 * Each instance of emacs_env get its own id from a simple counter
 */
static int32_t next_module_id = 1;

static inline Lisp_Object value_to_lisp (emacs_value v)
{
  return (Lisp_Object) v;
}

static inline emacs_value lisp_to_value (Lisp_Object o)
{
  return (emacs_value) o;
}

static struct emacs_runtime* module_get_runtime (void)
{
  check_main_thread ();
  struct emacs_runtime *ert = xzalloc (sizeof *ert);

  ert->size = sizeof *ert;
  ert->get_environment = module_get_environment;

  return ert;
}

static emacs_env* module_get_environment (struct emacs_runtime *ert)
{
  check_main_thread ();
  emacs_env *env = xzalloc (sizeof *env);

  env->size            = sizeof *env;
  env->module_id       = next_module_id++;
  env->make_global_ref = module_make_global_ref;
  env->free_global_ref = module_free_global_ref;
  env->type_of         = module_type_of;
  env->error_check     = module_error_check;
  env->error_clear     = module_error_clear;
  env->error_get       = module_error_get;
  env->error_signal    = module_error_signal;
  env->make_fixnum     = module_make_fixnum;
  env->fixnum_to_int   = module_fixnum_to_int;
  env->make_float      = module_make_float;
  env->float_to_c_double = module_float_to_c_double;
  env->intern          = module_intern;
  env->make_function   = module_make_function;
  env->funcall         = module_funcall;
  env->make_string     = module_make_string;
  env->copy_string_contents = module_copy_string_contents;
  env->make_user_ptr = module_make_user_ptr;
  env->get_user_ptr_ptr = module_get_user_ptr_ptr;
  env->set_user_ptr_ptr = module_set_user_ptr_ptr;
  env->get_user_ptr_finalizer = module_get_user_ptr_finalizer;
  env->set_user_ptr_finalizer = module_set_user_ptr_finalizer;
  return env;
}

/*
 * To make global refs (GC-protected global values) we keep a hash
 * that maps module-id to a list of their global values.
 */

static emacs_value module_make_global_ref (emacs_env *env,
                                           emacs_value ref)
{
  check_main_thread ();
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
  check_main_thread ();
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
  *sym = lisp_to_value (module_error_symbol);
  *data = lisp_to_value (module_error_data);
  return true;
}

/*
 * Like for `signal', DATA must be a list
 */
static void module_error_signal (emacs_env *env, emacs_value sym, emacs_value data)
{
  check_main_thread ();
  module_pending_error = true;
  module_error_symbol = value_to_lisp (sym);
  module_error_data = value_to_lisp (data);
}

static emacs_value module_make_fixnum (emacs_env *env, int64_t n)
{
  check_main_thread ();
  return lisp_to_value (make_number (n));
}

static int64_t module_fixnum_to_int (emacs_env *env, emacs_value n)
{
  check_main_thread ();
  return (int64_t) XINT (value_to_lisp (n));
}

static emacs_value module_make_float (emacs_env *env, double d)
{
  check_main_thread ();
  return lisp_to_value (make_float (d));
}

static double module_float_to_c_double (emacs_env *env, emacs_value f)
{
  check_main_thread ();
  return (double) XFLOAT_DATA (value_to_lisp (f));
}

static emacs_value module_intern (emacs_env *env, const char *name)
{
  check_main_thread ();
  return lisp_to_value (intern (name));
}

static emacs_value module_make_string (emacs_env *env, const char *str, size_t length)
{
  check_main_thread ();
  /* Assume STR is utf8 encoded */
  return lisp_to_value (make_string (str, length));
}

static bool module_copy_string_contents (emacs_env *env,
                                         emacs_value value,
                                         char *buffer,
                                         size_t* length)
{
  check_main_thread ();
  Lisp_Object lisp_str = value_to_lisp (value);
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
  check_main_thread ();
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

emacs_value module_make_user_ptr (emacs_env *env,
                                  emacs_finalizer_function fin,
                                  void *ptr)
{
  return lisp_to_value (make_user_ptr (env->module_id, fin, ptr));
}


void* module_get_user_ptr_ptr (emacs_env *env, emacs_value uptr)
{
  return XUSER_PTR (value_to_lisp (uptr))->p;
}

void module_set_user_ptr_ptr (emacs_env *env, emacs_value uptr, void *ptr)
{
  XUSER_PTR (value_to_lisp (uptr))->p = ptr;
}


emacs_finalizer_function module_get_user_ptr_finalizer (emacs_env *env, emacs_value uptr)
{
  return XUSER_PTR (value_to_lisp (uptr))->finalizer;
}

void module_set_user_ptr_finalizer (emacs_env *env,
                                    emacs_value uptr,
                                    emacs_finalizer_function fin)
{
  XUSER_PTR (value_to_lisp (uptr))->finalizer = fin;
}

struct module_fun_env
{
  emacs_env *env;
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
  check_main_thread();
  Lisp_Object envobj;
  Lisp_Object Qrest = intern ("&rest");
  Lisp_Object Qarglist = intern ("arglist");
  Lisp_Object Qmodule_call = intern ("module-call");

  /* XXX: This should need to be freed when envobj is GC'd */
  struct module_fun_env *envptr = xzalloc (sizeof (*envptr));
  envptr->env = env;
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

  return lisp_to_value (ret);
}

static Lisp_Object module_handle_signal (Lisp_Object err, ptrdiff_t nargs, Lisp_Object *args)
{
  module_pending_error = true;
  module_error_symbol = XCAR (err);
  module_error_data = XCDR (err);
  return Qnil;
}

static Lisp_Object module_handle_throw (Lisp_Object tag, Lisp_Object value, ptrdiff_t nargs, Lisp_Object *args)
{
  module_pending_error = true;
  module_error_symbol = Qno_catch;
  module_error_data = list2 (tag, value);
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
  check_main_thread();
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
  return lisp_to_value (ret);
}

static void check_main_thread ()
{
#if defined(HAVE_PTHREAD)
  eassert (pthread_equal (pthread_self (), main_thread));
#elif defined(WINDOWSNT)
  /* CompareObjectHandles would be perfect, but is only available in
     Windows 10.  Also check whether the thread is still running to
     protect against thread identifier reuse. */
  eassert (GetCurrentThreadID () == GetThreadID (main_thread) &&
           WaitForSingleObject (main_thread, 0) == WAIT_TIMEOUT);
#endif
}

DEFUN ("module-call", Fmodule_call, Smodule_call, 2, 2, 0,
       doc: /* Internal function to call a module function.
ENVPTR is the emacs_env pointer to pass the module function.
SUBRPTR is the module function pointer (emacs_subr prototype) to call.
DATAPTR is the data pointer passed to make_function.
ARGLIST is a list of argument passed to SUBRPTR. */)
  (Lisp_Object envobj, Lisp_Object arglist)
{
  int len = XINT (Flength (arglist));
  emacs_value *args = xzalloc (len * sizeof (*args));
  int i;

  for (i = 0; i < len; i++)
    {
      args[i] = lisp_to_value (XCAR (arglist));
      arglist = XCDR (arglist);
    }

  struct module_fun_env *envptr = (struct module_fun_env*) XSAVE_POINTER (envobj, 0);
  module_pending_error = false;
  emacs_value ret = envptr->subr (envptr->env, len, args, envptr->data);
  xfree (args);
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

  int r = module_init (module_get_runtime ());

  return Qt;
}

void syms_of_module (void)
{
  /* It is not guaranteed that dynamic initializers run in the main thread,
     therefore we detect the main thread here. */
#if defined(HAVE_PTHREAD)
  main_thread = pthread_self ();
#elif defined(WINDOWSNT)
  /* GetCurrentProcess returns a pseudohandle, which we have to duplicate. */
  if (! DuplicateHandle (GetCurrentProcess(), GetCurrentThread(),
                         GetCurrentProcess(), &main_thread,
                         SYNCHRONIZE | THREAD_QUERY_LIMITED_INFORMATION,
                         FALSE, 0))
    emacs_abort ();
#endif

  DEFVAR_LISP ("module-refs-hash", Vmodule_refs_hash,
	       doc: /* Module global referrence table.  */);

  Vmodule_refs_hash = make_hash_table (hashtest_eql, make_number (DEFAULT_HASH_SIZE),
                                       make_float (DEFAULT_REHASH_SIZE),
                                       make_float (DEFAULT_REHASH_THRESHOLD),
                                       Qnil);

  defsubr (&Smodule_call);
  defsubr (&Smodule_load);
}
