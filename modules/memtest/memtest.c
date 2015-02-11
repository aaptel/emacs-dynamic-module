#include <config.h>
#include <lisp.h>

int plugin_is_GPL_compatible;

static module_id_t module_id;
static Lisp_Object MQmemtest;

static int free_count = 0;

struct int_buffer
{
  int size;
  int capacity;
  int *buf;
};

#define MXBUF(x) ((struct int_buffer*)(XMODULE (x)->p))

static void buf_init (struct int_buffer *b, int size)
{
  b->size = size;
  b->capacity = (size == 0 ? 1 : size);
  b->buf = malloc (sizeof (*b->buf) * b->capacity);
}

static void buf_add (struct int_buffer *b, int val)
{
  if (b->size >= b->capacity)
    {
      b->capacity *= 2;
      b->buf = realloc (b->buf, sizeof (*b->buf) * b->capacity);
    }

  b->buf[b->size++] = val;
}

static void memtest_destructor (void *p)
{
  struct int_buffer *b = p;
  free (b->buf);
  free (b);
  free_count++;
}

EXFUN (Fmemtest_make, 1);
DEFUN ("memtest-make", Fmemtest_make, Smemtest_make, 0, 1, 0,
       doc: "Return an int buffer in the form of a Lisp_Module object.")
  (Lisp_Object size)
{
  struct int_buffer *b;

  b = malloc (sizeof (*b));
  buf_init (b, NILP (size) ? 0 : XINT (size));

  return module_make_object (module_id, memtest_destructor, (void*)b);
}

EXFUN (Fmemtest_get, 2);
DEFUN ("memtest-get", Fmemtest_get, Smemtest_get, 2, 2, 0,
       doc: "Get value at index N of a memtest buffer.")
  (Lisp_Object buf, Lisp_Object n)
{
  return make_number (MXBUF (buf)->buf[XINT (n)]);
}

EXFUN (Fmemtest_set, 3);
DEFUN ("memtest-set", Fmemtest_set, Smemtest_set, 3, 3, 0,
       doc: "Doc")
  (Lisp_Object buf, Lisp_Object n, Lisp_Object value)
{
  MXBUF (buf)->buf[XINT (n)] = XINT (value);
  return value;
}

EXFUN (Fmemtest_size, 1);
DEFUN ("memtest-size", Fmemtest_size, Smemtest_size, 1, 1, 0,
       doc: "Doc")
  (Lisp_Object buf)
{
  return make_number (MXBUF (buf)->size);
}

EXFUN (Fmemtest_add, 2);
DEFUN ("memtest-add", Fmemtest_add, Smemtest_add, 2, 2, 0,
       doc: "Doc")
  (Lisp_Object buf, Lisp_Object value)
{
  buf_add (MXBUF (buf), XINT (value));
  return Qnil;
}


EXFUN (Fmemtest_free_count, 0);
DEFUN ("memtest-free-count", Fmemtest_free_count, Smemtest_free_count, 0, 0, 0,
       doc: "Doc")
  (void)
{
  return make_number (free_count);
}


void init ()
{
  module_id = module_make_id ();
  MQmemtest = intern ("memtest");

  defsubr (&Smemtest_make);
  defsubr (&Smemtest_set);
  defsubr (&Smemtest_get);
  defsubr (&Smemtest_add);
  defsubr (&Smemtest_size);
  defsubr (&Smemtest_free_count);

  Fprovide (MQmemtest, Qnil);
}
