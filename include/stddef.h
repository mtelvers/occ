/* C11 7.19  Common definitions <stddef.h>
 *
 * The compiler predefines __SIZE_TYPE__, __PTRDIFF_TYPE__ and
 * __WCHAR_TYPE__ for the target, the same names gcc uses, so this header
 * is target independent. */
#ifndef _OCC_STDDEF_H
#define _OCC_STDDEF_H

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__ size_t;
typedef __WCHAR_TYPE__ wchar_t;

/* 7.19p2: an object type whose alignment is the greatest fundamental
 * alignment (6.2.8).  On x86-64 that is 16, for long double. */
typedef struct { long long __ll; long double __ld; } max_align_t;

#define NULL ((void *)0)

/* 7.19p3: an integer constant expression of type size_t. */
#define offsetof(type, member) __builtin_offsetof(type, member)

#endif
