/* xlconfig.h.  Generated automatically by configure.  */
#define HAVE_FINITE 1
#define HAVE_ISNAN 1
#if HAVE_FINITE && HAVE_ISNAN
#  define IEEEFP
#endif

#define HAVE_MEMMOVE 1
#if ! HAVE_MEMMOVE
#  define NOMEMMOVE
#endif

#define HAVE_DIFFTIME 1
#if ! HAVE_DIFFTIME
#  define NODIFFTIME
#endif

#define HAVE_MATHERR 1
#if HAVE_MATHERR
#  define USEMATHERR
#endif

#define HAVE_SIGSETJMP 1
#if HAVE_SIGSETJMP
#  define XL_SETJMP(env) sigsetjmp(env,1)
#  define XL_LONGJMP(env,val) siglongjmp(env,val)
#  define XL_JMP_BUF sigjmp_buf
#endif

#define HAVE_FOREIGN 1
#define HAVE_DLOPEN 1
#if HAVE_FOREIGN
#  define FOREIGNCALL
#endif

#ifdef __STDC__
#  define ANSI
#  if HAVE_DLOPEN
#    define SHAREDLIBS
#  endif
#endif

#define UNIX

#ifdef _AIX
#  ifndef _BSD
#    define _BSD
#  endif
#endif

#ifdef __hpux
#  ifndef _HPUX_SOURCE
#    define _HPUX_SOURCE
#  endif
#endif

/* This is to bring in finite and isnan on newer versions of solaris */
#ifdef sun
#  ifndef __EXTENSIONS__
#    define __EXTENSIONS__
#  endif
#endif
