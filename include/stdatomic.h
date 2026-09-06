/* C11 7.17  Atomics <stdatomic.h>
 *
 * This header is the interface between the standard's generic functions
 * and the compiler.  Each generic function becomes one __occ_atomic_*
 * builtin taking the memory order as an argument; the elaborator types
 * them from the pointer argument (7.17.7p1: "A" is the type the pointer
 * points to) and [Lower] emits the matching Ir.Atomic_* instruction.
 *
 * Unlike gcc's header, nothing here needs __typeof__, __auto_type or
 * statement expressions, so the preprocessed output stays C11. */
#ifndef _OCC_STDATOMIC_H
#define _OCC_STDATOMIC_H

#include <stddef.h>
#include <stdint.h>

/* 7.17.1p3 */
#define ATOMIC_BOOL_LOCK_FREE     2
#define ATOMIC_CHAR_LOCK_FREE     2
#define ATOMIC_CHAR16_T_LOCK_FREE 2
#define ATOMIC_CHAR32_T_LOCK_FREE 2
#define ATOMIC_WCHAR_T_LOCK_FREE  2
#define ATOMIC_SHORT_LOCK_FREE    2
#define ATOMIC_INT_LOCK_FREE      2
#define ATOMIC_LONG_LOCK_FREE     2
#define ATOMIC_LLONG_LOCK_FREE    2
#define ATOMIC_POINTER_LOCK_FREE  2

/* 7.17.2 */
#define ATOMIC_VAR_INIT(value) (value)
#define atomic_init(obj, value) __occ_atomic_store((obj), (value), memory_order_relaxed)

/* 7.17.3  The enumeration constants are the values Ir.memory_order uses. */
typedef enum memory_order {
  memory_order_relaxed = 0,
  memory_order_consume = 1,
  memory_order_acquire = 2,
  memory_order_release = 3,
  memory_order_acq_rel = 4,
  memory_order_seq_cst = 5
} memory_order;

/* 7.17.3.1 */
#define kill_dependency(y) (y)

/* 7.17.4 */
#define atomic_thread_fence(order) __occ_atomic_thread_fence(order)
#define atomic_signal_fence(order) __occ_atomic_signal_fence(order)

/* 7.17.5 */
#define atomic_is_lock_free(obj) 1

/* 7.17.6 */
typedef _Atomic _Bool               atomic_bool;
typedef _Atomic char                atomic_char;
typedef _Atomic signed char         atomic_schar;
typedef _Atomic unsigned char       atomic_uchar;
typedef _Atomic short               atomic_short;
typedef _Atomic unsigned short      atomic_ushort;
typedef _Atomic int                 atomic_int;
typedef _Atomic unsigned int        atomic_uint;
typedef _Atomic long                atomic_long;
typedef _Atomic unsigned long       atomic_ulong;
typedef _Atomic long long           atomic_llong;
typedef _Atomic unsigned long long  atomic_ullong;
typedef _Atomic __CHAR16_TYPE__     atomic_char16_t;
typedef _Atomic __CHAR32_TYPE__     atomic_char32_t;
typedef _Atomic wchar_t             atomic_wchar_t;
typedef _Atomic int_least8_t        atomic_int_least8_t;
typedef _Atomic uint_least8_t       atomic_uint_least8_t;
typedef _Atomic int_least16_t       atomic_int_least16_t;
typedef _Atomic uint_least16_t      atomic_uint_least16_t;
typedef _Atomic int_least32_t       atomic_int_least32_t;
typedef _Atomic uint_least32_t      atomic_uint_least32_t;
typedef _Atomic int_least64_t       atomic_int_least64_t;
typedef _Atomic uint_least64_t      atomic_uint_least64_t;
typedef _Atomic int_fast8_t         atomic_int_fast8_t;
typedef _Atomic uint_fast8_t        atomic_uint_fast8_t;
typedef _Atomic int_fast16_t        atomic_int_fast16_t;
typedef _Atomic uint_fast16_t       atomic_uint_fast16_t;
typedef _Atomic int_fast32_t        atomic_int_fast32_t;
typedef _Atomic uint_fast32_t       atomic_uint_fast32_t;
typedef _Atomic int_fast64_t        atomic_int_fast64_t;
typedef _Atomic uint_fast64_t       atomic_uint_fast64_t;
typedef _Atomic intptr_t            atomic_intptr_t;
typedef _Atomic uintptr_t           atomic_uintptr_t;
typedef _Atomic size_t              atomic_size_t;
typedef _Atomic ptrdiff_t           atomic_ptrdiff_t;
typedef _Atomic intmax_t            atomic_intmax_t;
typedef _Atomic uintmax_t           atomic_uintmax_t;

/* 7.17.7  Operations on atomic types.  The non-_explicit forms use
 * memory_order_seq_cst (7.17.7p2 and following). */
#define atomic_store_explicit(obj, desired, order) \
  __occ_atomic_store((obj), (desired), (order))
#define atomic_store(obj, desired) \
  atomic_store_explicit((obj), (desired), memory_order_seq_cst)

#define atomic_load_explicit(obj, order) __occ_atomic_load((obj), (order))
#define atomic_load(obj) atomic_load_explicit((obj), memory_order_seq_cst)

#define atomic_exchange_explicit(obj, desired, order) \
  __occ_atomic_exchange((obj), (desired), (order))
#define atomic_exchange(obj, desired) \
  atomic_exchange_explicit((obj), (desired), memory_order_seq_cst)

#define atomic_compare_exchange_strong_explicit(obj, expected, desired, succ, fail) \
  __occ_atomic_compare_exchange_strong((obj), (expected), (desired), (succ), (fail))
#define atomic_compare_exchange_strong(obj, expected, desired) \
  atomic_compare_exchange_strong_explicit((obj), (expected), (desired), \
                                          memory_order_seq_cst, memory_order_seq_cst)
#define atomic_compare_exchange_weak_explicit(obj, expected, desired, succ, fail) \
  __occ_atomic_compare_exchange_weak((obj), (expected), (desired), (succ), (fail))
#define atomic_compare_exchange_weak(obj, expected, desired) \
  atomic_compare_exchange_weak_explicit((obj), (expected), (desired), \
                                        memory_order_seq_cst, memory_order_seq_cst)

#define atomic_fetch_add_explicit(obj, arg, order) __occ_atomic_fetch_add((obj), (arg), (order))
#define atomic_fetch_sub_explicit(obj, arg, order) __occ_atomic_fetch_sub((obj), (arg), (order))
#define atomic_fetch_or_explicit(obj, arg, order)  __occ_atomic_fetch_or((obj), (arg), (order))
#define atomic_fetch_xor_explicit(obj, arg, order) __occ_atomic_fetch_xor((obj), (arg), (order))
#define atomic_fetch_and_explicit(obj, arg, order) __occ_atomic_fetch_and((obj), (arg), (order))
#define atomic_fetch_add(obj, arg) atomic_fetch_add_explicit((obj), (arg), memory_order_seq_cst)
#define atomic_fetch_sub(obj, arg) atomic_fetch_sub_explicit((obj), (arg), memory_order_seq_cst)
#define atomic_fetch_or(obj, arg)  atomic_fetch_or_explicit((obj), (arg), memory_order_seq_cst)
#define atomic_fetch_xor(obj, arg) atomic_fetch_xor_explicit((obj), (arg), memory_order_seq_cst)
#define atomic_fetch_and(obj, arg) atomic_fetch_and_explicit((obj), (arg), memory_order_seq_cst)

/* 7.17.8  Atomic flag type and operations. */
typedef struct atomic_flag { atomic_bool _Value; } atomic_flag;
#define ATOMIC_FLAG_INIT { 0 }
#define atomic_flag_test_and_set_explicit(obj, order) \
  atomic_exchange_explicit(&(obj)->_Value, 1, (order))
#define atomic_flag_test_and_set(obj) \
  atomic_flag_test_and_set_explicit((obj), memory_order_seq_cst)
#define atomic_flag_clear_explicit(obj, order) \
  atomic_store_explicit(&(obj)->_Value, 0, (order))
#define atomic_flag_clear(obj) atomic_flag_clear_explicit((obj), memory_order_seq_cst)

#endif
