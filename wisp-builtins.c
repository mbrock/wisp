/*
 * This file is part of Wisp.
 *
 * Wisp is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * Foobar is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public
 * License for more details.

 * You should have received a copy of the GNU Affero General Public
 * License along with Wisp. If not, see <https://www.gnu.org/licenses/>.
 */

#include "wisp.h"
#include "wisp-builtins.h"

wisp_defun_t wisp_builtins[WISP_N_BUILTINS];

WISP_DEFQUOTE ("QUOTE", wisp_quote, 1)
(wisp_word_t term)
{
  return term;
}

WISP_DEFEVAL ("EVAL", wisp_eval, 1)
(wisp_word_t term)
{
  return term;
}

WISP_DEFMACRO ("LAMBDA", wisp_lambda, 2)
(wisp_word_t lambda_list, wisp_word_t body)
{
  wisp_word_t params = wisp_lambda_list_to_params (lambda_list);

  wisp_word_t closure = wisp_make_instance_va
    (WISP_CACHE (CLOSURE), 4,
     params, body, wisp_machine->scopes, NIL);

  return closure;
}

WISP_DEFMACRO ("MACRO", wisp_macro, 2)
(wisp_word_t lambda_list, wisp_word_t body)
{
  wisp_word_t params = wisp_lambda_list_to_params (lambda_list);

  wisp_word_t closure = wisp_make_instance_va
    (WISP_CACHE (CLOSURE), 4,
     params, body, wisp_machine->scopes, WISP_CACHE (MACRO));

  return closure;
}

WISP_DEFUN ("CONS", wisp_cons, 2)
(wisp_word_t car, wisp_word_t cdr)
{
  wisp_word_t cons
    = wisp_alloc_raw (WISP_CONS_SIZE, WISP_LOWTAG_LIST_PTR);

  wisp_word_t *data = wisp_deref (cons);
  data[0] = car;
  data[1] = cdr;

  return cons;
}

WISP_DEFUN ("CAR", wisp_car, 1)
(wisp_word_t list)
{
  assert (WISP_IS_LIST_PTR (list));
  return (wisp_deref (list))[0];
}

WISP_DEFUN ("CDR", wisp_cdr, 1)
(wisp_word_t list)
{
  assert (WISP_IS_LIST_PTR (list));
  return (wisp_deref (list))[1];
}

WISP_DEFUN ("MAKE-INSTANCE", wisp_make_instance, 2)
(wisp_word_t klass, wisp_word_t initargs)
{
  assert (wisp_is_symbol (klass));

  int length = wisp_length (initargs);
  wisp_word_t slots[length];

  wisp_word_t cons = initargs;
  for (int i = 0; i < length; i++) {
    slots[i] = wisp_car (cons);
    cons = wisp_cdr (cons);
  }

  return wisp_make_instance_with_slots
    (klass, length, slots);
}

WISP_DEFUN ("SET-SYMBOL-FUNCTION", wisp_set_symbol_function, 2)
(wisp_word_t symbol, wisp_word_t value)
{
  wisp_word_t *header = wisp_is_symbol (symbol);
  header[6] = value;

  WISP_DEBUG ("Function ");
  wisp_dump (stderr, symbol);
  fprintf (stderr, " ← ");
  wisp_dump (stderr, value);
  fprintf (stderr, "\n");

  return value;
}

WISP_DEFUN ("GC", wisp_collect_garbage, 0)
(void)
{
  wisp_tidy ();
  return NIL;
}

WISP_DEFUN ("PRINT", wisp_print, 1)
(wisp_word_t x)
{
  wisp_dump (stdout, x);
  putchar ('\n');
  return x;
}

WISP_DEFUN ("GET/CC", wisp_getcc, 0)
(void)
{
  return wisp_machine->plan;
}

WISP_DEFUN ("SAVE-HEAP", wisp_save_heap, 1)
(wisp_word_t pathname)
{
  char *path = wisp_string_buffer (wisp_deref (pathname));

  WISP_DEBUG ("saving heap to %s\n", path);

  FILE *f = fopen (path, "w+");

  fprintf (f, "WISP 0 %d\n", WISP_CACHE (WISP));

  if (fwrite (wisp_heap, 1, wisp_heap_used, f) != wisp_heap_used)
    wisp_crash ("heap save write failed");
  WISP_DEBUG ("saved heap to %s\n", path);
  fclose (f);

#ifdef EMSCRIPTEN
  EM_ASM ({
    FS.syncfs (err => {
      if (err)
        {
          Module.printErr ('syncfs error');
          console.error (err);
        }
      else
        {
          console.error ('syncfs done');
        }
    });
  });
#endif

  return WISP_CACHE (T);
}

WISP_DEFUN ("+", wisp_add, 2)
(wisp_word_t x, wisp_word_t y)
{
  if (!WISP_IS_FIXNUM (x))
    wisp_crash ("not a number");

  if (!WISP_IS_FIXNUM (y))
    wisp_crash ("not a number");

  return x + y;
}

WISP_DEFUN ("-", wisp_subtract, 2)
(wisp_word_t x, wisp_word_t y)
{
  if (!WISP_IS_FIXNUM (x))
    wisp_crash ("not a number");

  if (!WISP_IS_FIXNUM (y))
    wisp_crash ("not a number");

  return x - y;
}

WISP_DEFUN ("*", wisp_multiply, 2)
(wisp_word_t x, wisp_word_t y)
{
  if (!WISP_IS_FIXNUM (x))
    wisp_crash ("not a number");

  if (!WISP_IS_FIXNUM (y))
    wisp_crash ("not a number");

  return ((x >> 2) * (y >> 2)) << 2;
}

void
wisp_register_builtin_defun
(wisp_defun_t defun)
{
  wisp_builtins[defun.id] = defun;

  wisp_set_symbol_function
    (wisp_intern_lisp (defun.name),
     (defun.id << 8) | WISP_WIDETAG_BUILTIN);
}

void
wisp_defs (void)
{
  WISP_REGISTER (wisp_quote, "TERM");
  WISP_REGISTER (wisp_eval, "TERM");
  WISP_REGISTER (wisp_lambda, "LAMBDA-LIST", "BODY");
  WISP_REGISTER (wisp_macro, "LAMBDA-LIST", "BODY");
  WISP_REGISTER (wisp_cons, "CAR", "CDR");
  WISP_REGISTER (wisp_car, "CONS");
  WISP_REGISTER (wisp_cdr, "CONS");
  WISP_REGISTER (wisp_set_symbol_function, "SYMBOL", "FUNCTION");
  WISP_REGISTER (wisp_save_heap, "HEAP-PATH");
  WISP_REGISTER (wisp_collect_garbage, NULL);
  WISP_REGISTER (wisp_print, "X");
  WISP_REGISTER (wisp_make_instance, "CLASS", "SLOTS");
  WISP_REGISTER (wisp_add, "X", "Y");
  WISP_REGISTER (wisp_subtract, "X", "Y");
  WISP_REGISTER (wisp_multiply, "X", "Y");
  WISP_REGISTER (wisp_getcc, NULL);
}
