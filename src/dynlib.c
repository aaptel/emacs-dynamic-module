/*
 *              Portable API for dynamic loading
 *
 *  Assuming modules are enabled on modern systems...  *Yes*, the
 *  preprocessor macro checks could be more precise.  I don't care.
 *
 *  If you think the abstraction is too leaky use libltdl (libtool),
 *  don't reinvent the wheel by fixing this one.
 */

#include "dynlib.h"

/*
 *  Windows systems
 */
#if defined(_WIN32)

#include <windows.h>

dynlib_handle_ptr dynlib_open (const char * path)
{

  return (dynlib_handle_ptr) LoadLibrary (path);
}

void * dynlib_sym (dynlib_handle_ptr h, const char * sym)
{
  return GetProcAddress ((HMODULE) h, sym);
}

const char * dynlib_error (void)
{
  /* TODO: use GetLastError(), FormatMessage(), ... */
  return "Can't load DLL";
}

int dynlib_close (dynlib_handle_ptr h)
{
  return FreeLibrary ((HMODULE) h) != 0;
}


/*
 *  POSIX systems
 */
#elif defined(HAVE_UNISTD_H)

#include <dlfcn.h>

dynlib_handle_ptr dynlib_open (const char * path)
{
  return dlopen (path, RTLD_LAZY);
}

void * dynlib_sym (dynlib_handle_ptr h, const char * sym)
{
  return dlsym (h, sym);
}

const char * dynlib_error (void)
{
  return dlerror ();
}

int dynlib_close (dynlib_handle_ptr h)
{
  return dlclose (h) == 0;
}

#else

#error "No dynamic loading for this system"

#endif