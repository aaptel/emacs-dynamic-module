#include <config.h>
#include <lisp.h>

int plugin_is_GPL_compatible;
static Lisp_Object MQopaque;

struct opaque
{
  int a, b, c;
};

static Lisp_Object MQopaque, MQa, MQb, MQc;

EXFUN (Fopaque_make, 3);
DEFUN ("opaque-make", Fopaque_make, Sopaque_make, 3, 3, 0,
       doc: "Make opaque type.")
  (Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
  struct opaque *p = malloc (sizeof (*p));
  p->a = XINT (a);
  p->b = XINT (b);
  p->c = XINT (c);

  /*
    store p as a the first slot (index 0) of a Lisp_Save_Value (which
    is a Lisp_Misc)
  */
  return make_save_ptr ((void*)p);
}

EXFUN (Fopaque_free, 1);
DEFUN ("opaque-free", Fopaque_free, Sopaque_free, 1, 1, 0,
       doc: "Free opaque object OBJ.")
  (Lisp_Object obj)
{
  /* the pointer is in the first slot (index 0) */
  free (XSAVE_POINTER (obj, 0));
  return Qnil;
}

EXFUN (Fopaque_get, 2);
DEFUN ("opaque-get", Fopaque_get, Sopaque_get, 2, 2, 0,
       doc: "Return the field F (`a', `b', `c') of the opaque object OBJ.")
  (Lisp_Object obj, Lisp_Object f)
{
  struct opaque *p = XSAVE_POINTER (obj, 0);
  int val = EQ (f, MQa) ? p->a : EQ (f, MQb) ? p->b : EQ (f, MQc) ? p->c : -1;
  return make_number (val);
}

void init ()
{
  MQopaque = intern ("opaque");

  MQa = intern ("a");
  MQb = intern ("b");
  MQc = intern ("c");

  defsubr (&Sopaque_make);
  defsubr (&Sopaque_free);
  defsubr (&Sopaque_get);

  Fprovide (MQopaque, Qnil);
}
