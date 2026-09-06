/* C11 7.16  Variable arguments <stdarg.h>
 *
 * va_list is the compiler's built-in type (a one-element array of the
 * System V register-save-area descriptor, ABI 3.5.7).  The macros expand
 * to builtins because their expansion needs the callee's frame layout. */

/* glibc's <stdio.h> and <wchar.h> include this header with
 * __need___va_list defined, wanting only the __gnuc_va_list name and no
 * pollution of the user's namespace. */
#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef __builtin_va_list __gnuc_va_list;
#endif

#ifdef __need___va_list
#undef __need___va_list
#else

#ifndef _OCC_STDARG_H
#define _OCC_STDARG_H

typedef __builtin_va_list va_list;

#define va_start(ap, last) __builtin_va_start(ap, last)
#define va_arg(ap, type)   __builtin_va_arg(ap, type)
#define va_end(ap)         __builtin_va_end(ap)
#define va_copy(dst, src)  __builtin_va_copy(dst, src)

#endif
#endif
