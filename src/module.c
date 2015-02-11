/* Dynamic modules related functions for GNU Emacs

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
   along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#include <config.h>
#include <limits.h>
#include "lisp.h"

EXFUN (Fmodule_available_p, 0);
DEFUN ("module-available-p", Fmodule_available_p, Smodule_available_p, 0, 0, 0,
       doc: "Doc")
  (void)
{
#ifdef HAVE_LTDL
  return Qt;
#else
  return Qnil;
#endif
}

/* Module functions */
#ifdef HAVE_LTDL

/* Return a unique id for a new module opaque type. */
module_id_t
module_make_id (void)
{
  static module_id_t module_count = 0;

  eassert (module_count < MODULE_ID_MAX);
  return module_count++;
}

#endif

void syms_of_module (void)
{
#ifdef HAVE_LTDL
  /* Nothing yet! */
#endif

  defsubr(&Smodule_available_p);
}
