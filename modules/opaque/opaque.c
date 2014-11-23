#include <config.h>
#include <lisp.h>

int plugin_is_GPL_compatible;

struct opaque
{
  int a, b, c;
};

static Lisp_Object Qa, Qb, Qc;

EXFUN (Fopaque_make, 3);
DEFUN ("opaque-make", Fopaque_make, Sopaque_make, 3, 3, 0,
       doc: "Make opaque type.")
  (Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
  struct opaque *p = malloc (sizeof (*p));
  p->a = XINT (a);
  p->b = XINT (b);
  p->c = XINT (c);
  return make_save_ptr ((void*)p);
}

EXFUN (Fopaque_free, 1);
DEFUN ("opaque-free", Fopaque_free, Sopaque_free, 1, 1, 0,
       doc: "Free opaque type.")
  (Lisp_Object p)
{
  free (XSAVE_POINTER (p, 0));
  return Qnil;
}

EXFUN (Fopaque_get, 2);
DEFUN ("opaque-get", Fopaque_get, Sopaque_get, 2, 2, 0,
       doc: "Return the field F (`a', `b', `c') of the opaque object OBJ.")
  (Lisp_Object obj, Lisp_Object f)
{
  struct opaque *p = XSAVE_POINTER (obj, 0);
  int val = EQ (f, Qa) ? p->a : EQ (f, Qb) ? p->b : EQ (f, Qc) ? p->c : -1;
  return make_number (val);
}

void init ()
{
  DEFSYM (Qa, "a");
  DEFSYM (Qb, "b");
  DEFSYM (Qc, "c");

  defsubr (&Sopaque_make);
  defsubr (&Sopaque_free);
  defsubr (&Sopaque_get);
}