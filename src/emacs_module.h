/*
  emacs_module.h - Module API
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

#ifndef EMACS_MODULE_H
#define EMACS_MODULE_H

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#ifdef __cplusplus
#define EMACS_EXTERN_C_BEGIN extern "C" {
#define EMACS_EXTERN_C_END }
#else
#define EMACS_EXTERN_C_BEGIN
#define EMACS_EXTERN_C_END
#endif

#if defined(__cplusplus) && __cplusplus >= 201103L
#define EMACS_NOEXCEPT noexcept
#else
#define EMACS_NOEXCEPT
#endif

EMACS_EXTERN_C_BEGIN

/* Current environement */
typedef struct emacs_env_25 emacs_env;
typedef struct emacs_value_tag* emacs_value;

enum emacs_type {
  EMACS_FIXNUM,
  EMACS_SYMBOL,
  EMACS_FLOAT,
  EMACS_STRING,
  EMACS_CONS,
  EMACS_VECTOR,
  EMACS_HASHTABLE,

  EMACS_OTHER = 255,
};

/* Struct passed to a module init function (emacs_module_init) */
struct emacs_runtime {
  size_t size;
  emacs_env* (*get_environment)(struct emacs_runtime *ert);
  struct emacs_runtime_private *private_members;
};


/* Function prototype for the module init function */
typedef int (*emacs_init_function)(struct emacs_runtime *ert);

/* Function prototype for the module Lisp functions */
typedef emacs_value (*emacs_subr)(emacs_env *env,
                                  int nargs,
                                  emacs_value args[],
                                  void *data);
struct emacs_env_25 {
  /*
   * Structure size (for version checking)
   */

  size_t size;
  int32_t module_id;

  /*
   * Memory management
   */


  emacs_value (*make_global_ref)(emacs_env *env,
                                 emacs_value any_reference);

  void (*free_global_ref)(emacs_env *env,
                          emacs_value global_reference);

  /*
   * Error handling
   */

  bool (*error_check)(emacs_env *env);

  void (*error_clear)(emacs_env *env);

  bool (*error_get)(emacs_env *env,
                    emacs_value *error_symbol_out,
                    emacs_value *error_data_out);

  void (*error_signal)(emacs_env *env,
                       emacs_value error_symbol,
                       emacs_value error_data);

  /*
   * Function registration
   */

  emacs_value (*make_function)(emacs_env *env,
                               int min_arity,
                               int max_arity,
                               emacs_value (*function)(emacs_env*, int, emacs_value*, void*) EMACS_NOEXCEPT,
                               void *data);

  emacs_value (*funcall)(emacs_env *env,
                         emacs_value function,
                         int nargs,
                         emacs_value args[]);

  emacs_value (*intern)(emacs_env *env,
                        const char *symbol_name);

  /*
   * Type conversion
   */

  enum emacs_type (*type_of)(emacs_env *env,
                             emacs_value value);

  int64_t (*fixnum_to_int)(emacs_env *env,
                           emacs_value value);

  emacs_value (*make_fixnum)(emacs_env *env,
                             int64_t value);

  double (*float_to_c_double)(emacs_env *env,
                              emacs_value value);

  emacs_value (*make_float)(emacs_env *env,
                            double value);

  /*
   * Copy the content of the lisp string VALUE to BUFFER as an utf8
   * null-terminated string.
   *
   * SIZE must point to the total size of the buffer.  If BUFFER is
   * NULL or if SIZE is not big enough, write the required buffer size
   * to SIZE and return false.
   *
   * Note that SIZE must include the last null byte (e.g. "abc" needs
   * a buffer of size 4).
   *
   * Returns true if the string was successfully copied.
   */

  bool (*copy_string_contents)(emacs_env *env,
                               emacs_value value,
                               char *buffer,
                               size_t *size_inout);

  /*
   * Create a lisp string from a utf8 encoded string.
   */
  emacs_value (*make_string)(emacs_env *env,
                             const char *contents, size_t length);

  struct emacs_env_private *private_members;
};

EMACS_EXTERN_C_END

#endif /* EMACS_MODULE_H */
