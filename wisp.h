/*
 * This file is part of Wisp.
 *
 * Wisp is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * Wisp is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public
 * License for more details.

 * You should have received a copy of the GNU Affero General Public
 * License along with Wisp. If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef WISP_H
#define WISP_H

#ifdef EMSCRIPTEN
# include <emscripten/emscripten.h>
# define WISP_EXPORT EMSCRIPTEN_KEEPALIVE
#else
# define WISP_EXPORT
#endif

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint32_t wisp_word_t;

typedef enum wisp_lowtag {
  /* 0 */ WISP_LOWTAG_FIXNUM_0,
  /* 1 */ WISP_LOWTAG_FUNCTION_PTR,
  /* 2 */ WISP_LOWTAG_OTHER_IMMEDIATE_0,
  /* 3 */ WISP_LOWTAG_LIST_PTR,
  /* 4 */ WISP_LOWTAG_FIXNUM_1,
  /* 5 */ WISP_LOWTAG_STRUCT_PTR,
  /* 6 */ WISP_LOWTAG_OTHER_IMMEDIATE_1,
  /* 7 */ WISP_LOWTAG_OTHER_PTR,
} wisp_lowtag_t;

typedef enum wisp_widetag {
  WISP_WIDETAG_INSTANCE = 0xC2,
  WISP_WIDETAG_STRING = 0x32,
  WISP_WIDETAG_SYMBOL = 0xAE,
  WISP_WIDETAG_BUILTIN = 0xA2,
} wisp_widetag_t;

typedef struct {
  bool value;
  wisp_word_t term;
  wisp_word_t scopes;
  wisp_word_t plan;
} wisp_machine_t;

extern wisp_machine_t *wisp_machine;

typedef struct __attribute__ ((__packed__)) {
  wisp_word_t function;
  wisp_word_t values;
  wisp_word_t terms;
  wisp_word_t scopes;
  wisp_word_t next;
} wisp_apply_plan_t;

typedef struct __attribute__ ((__packed__)) {
  wisp_word_t params;
  wisp_word_t body;
  wisp_word_t scopes;
  wisp_word_t macro;
} wisp_closure_t;

typedef union wisp_fun_ptr {
  wisp_word_t (*a0) (void);
  wisp_word_t (*a1) (wisp_word_t);
  wisp_word_t (*a2) (wisp_word_t, wisp_word_t);
  wisp_word_t (*a3) (wisp_word_t, wisp_word_t, wisp_word_t);
} wisp_fun_ptr_t;

typedef struct wisp_defun {
  uint32_t id;
  const char *name;
  int n_args;
  bool evaluate_arguments;
  bool evaluate_result;
  wisp_word_t params;
  wisp_fun_ptr_t function;
} wisp_defun_t;

extern wisp_defun_t wisp_builtins[];

#define WISP_DEF(lisp_name, c_name, n_args_, evalargs, evalresult) \
  wisp_word_t c_name FUNARGS_##n_args_;                            \
  static wisp_defun_t c_name##_spec = {                            \
    .id = c_name##_tag,                                            \
    .name = lisp_name,                                             \
    .evaluate_arguments = evalargs,                                \
    .evaluate_result = evalresult,                                 \
    .n_args = n_args_,                                             \
    .params = NIL,                                                 \
    .function = { .a ## n_args_ = c_name }                         \
  };                                                               \
  wisp_word_t c_name

#define WISP_DEFUN(lisp_name, c_name, n_args) \
  WISP_DEF (lisp_name, c_name, n_args, true, false)

#define WISP_DEFMACRO(lisp_name, c_name, n_args) \
  WISP_DEF (lisp_name, c_name, n_args, false, true)

#define WISP_DEFEVAL(lisp_name, c_name, n_args) \
  WISP_DEF (lisp_name, c_name, n_args, true, true)

#define WISP_DEFQUOTE(lisp_name, c_name, n_args) \
  WISP_DEF (lisp_name, c_name, n_args, false, false)

#define FUNARGS_0 (void)
#define FUNARGS_1 (wisp_word_t)
#define FUNARGS_2 (wisp_word_t, wisp_word_t)
#define FUNARGS_3 (wisp_word_t, wisp_word_t, wisp_word_t)

#define WISP_REGISTER(c_name, ...) {                            \
  c_name##_spec.params =                                        \
    wisp_simple_params ((c_name##_spec).n_args, __VA_ARGS__);   \
  wisp_register_builtin_defun (c_name##_spec);                  \
}

#define WISP_DEBUG(...) (fprintf (stderr, __VA_ARGS__))

#define WISP_LOWTAG_BITS 3
#define WISP_WIDETAG_BITS 8

#define WISP_LOWTAG_MASK  ((1 << WISP_LOWTAG_BITS) - 1)
#define WISP_WIDETAG_MASK ((1 << WISP_WIDETAG_BITS) - 1)

#define WISP_LOWTAG(x)  ((x) & WISP_LOWTAG_MASK)
#define WISP_WIDETAG(x) ((x) & WISP_WIDETAG_MASK)

#define wisp_align(x) \
  (((x) + WISP_LOWTAG_MASK) & ~WISP_LOWTAG_MASK)

#define WISP_WORD_SIZE 4
#define WISP_CONS_SIZE (2 * WISP_WORD_SIZE)
#define WISP_SYMBOL_SIZE (6)

#define WISP_IS_FIXNUM(x) (((x) & 3) == 0)
#define WISP_IS_OTHER_IMMEDIATE(x) (((x) & 3) == 2)
#define WISP_IS_PTR(x) ((x) & 1)

#define WISP_IS_OTHER_PTR(x) \
  (WISP_LOWTAG(x) == WISP_LOWTAG_OTHER_PTR)
#define WISP_IS_LIST_PTR(x) \
  (WISP_LOWTAG(x) == WISP_LOWTAG_LIST_PTR)
#define WISP_IS_STRUCT_PTR(x) \
  (WISP_LOWTAG(x) == WISP_LOWTAG_STRUCT_PTR)

#define WISP_ALIGNED_PROPERLY(x) \
  (((x) & WISP_LOWTAG_MASK) == 0)

#define WISP_INSTANCE_HEADER(n) \
  (wisp_header_word ((n) + 1, WISP_WIDETAG_INSTANCE))

#define WISP_IMMEDIATE_DATA(x) ((x) >> WISP_WIDETAG_BITS)

#define WISP_SYMBOL_HEADER \
  (wisp_header_word (WISP_SYMBOL_SIZE, WISP_WIDETAG_SYMBOL))

#define WISP_STATIC_SPACE_SIZE 40

extern void *wisp_heap;
extern size_t heap_size;

extern int wisp_old_heap;
extern int wisp_new_heap;
extern int wisp_heap_used;
extern int wisp_new_heap_scan;

extern float wisp_gc_fraction;

static const wisp_word_t NIL =
  WISP_LOWTAG_LIST_PTR;

enum {
  WISP_CACHED_APPLY,
  WISP_CACHED_CLOSURE,
  WISP_CACHED_EVAL,
  WISP_CACHED_LAMBDA,
  WISP_CACHED_MACRO,
  WISP_CACHED_PARAMS,
  WISP_CACHED_QUOTE,
  WISP_CACHED_SCOPE,
  WISP_CACHED_SET_SYMBOL_FUNCTION,
  WISP_CACHED_T,
  WISP_CACHED_WISP,
  WISP_CACHED_PACKAGE,
  WISP_CACHED_PROGN,

  wisp_cache_size
};

extern wisp_word_t wisp_cache[wisp_cache_size];

#define WISP_CACHE(x) (wisp_cache[WISP_CACHED_##x])

__attribute__ ((noreturn))
void
wisp_not_implemented ();

__attribute__ ((noreturn))
void
wisp_crash (const char *error);


wisp_word_t *
wisp_deref (wisp_word_t ptr);

wisp_word_t
wisp_alloc_raw (wisp_word_t size, wisp_lowtag_t tag);

void
wisp_dump (FILE *stream, wisp_word_t x);

wisp_word_t
wisp_cons (wisp_word_t car, wisp_word_t cdr);

wisp_word_t
wisp_car (wisp_word_t list);

wisp_word_t
wisp_cdr (wisp_word_t list);

int
wisp_length (wisp_word_t list);

wisp_word_t
wisp_intern_symbol (wisp_word_t name,
                    wisp_word_t package);

wisp_word_t
wisp_read (const char **s);

wisp_word_t
wisp_string_n (const char *source, int length);

char *
wisp_string_buffer (wisp_word_t *header);

wisp_word_t
wisp_fixnum (int32_t x);

wisp_word_t *
wisp_is_symbol (wisp_word_t value);

bool
wisp_is_quote (wisp_word_t word);

wisp_word_t
wisp_header_word (uint32_t data,
                  uint8_t widetag);

wisp_word_t
wisp_header_word_data (uint32_t header);

wisp_word_t
wisp_struct_header_type (wisp_word_t *header);

wisp_word_t
wisp_make_instance_with_slots (wisp_word_t type,
                               int n_slots,
                               wisp_word_t *slots);

wisp_word_t
wisp_make_instance_va (wisp_word_t type,
                       int n_slots,
                       ...);

wisp_word_t *
wisp_is_instance (wisp_word_t word,
                  wisp_word_t type);

wisp_word_t
wisp_lambda_list_to_params (wisp_word_t lambda_list);

wisp_word_t
wisp_set_symbol_function (wisp_word_t symbol,
                          wisp_word_t value);

bool
wisp_step (wisp_machine_t *machine);


wisp_word_t
wisp_save_heap (wisp_word_t pathname);

wisp_word_t
wisp_simple_params (int count, ...);

wisp_word_t
wisp_intern_lisp (const char *name);

void
wisp_builtin_function (wisp_word_t builtin_name,
                       wisp_word_t builtin_id,
                       wisp_word_t params);

void
wisp_tidy (void);

#endif
