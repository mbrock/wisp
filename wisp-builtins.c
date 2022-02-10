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

#include "wisp.h"
#include "wisp-builtins.h"

wisp_defun_t wisp_builtins[WISP_N_BUILTINS];

WISP_DEFQUOTE ("QUOTE", wisp_quote, 1, false)
(wisp_word_t term, wisp_word_t rest)
{
  return term;
}

WISP_DEFEVAL ("EVAL", wisp_eval, 1, false)
(wisp_word_t term, wisp_word_t rest)
{
  return term;
}

WISP_DEFMACRO ("LAMBDA", wisp_lambda, 1, true)
(wisp_word_t lambda_list, wisp_word_t rest)
{
  wisp_word_t params = wisp_lambda_list_to_params (lambda_list);

  wisp_word_t closure = wisp_make_instance_va
    (WISP_CACHE (CLOSURE), 4,
     params,
     wisp_cons (WISP_CACHE (PROGN), rest),
     wisp_machine->scopes,
     NIL);

  return closure;
}

WISP_DEFMACRO ("MACRO", wisp_macro, 1, true)
(wisp_word_t lambda_list, wisp_word_t rest)
{
  wisp_word_t params = wisp_lambda_list_to_params (lambda_list);

  wisp_word_t closure = wisp_make_instance_va
    (WISP_CACHE (CLOSURE), 4,
     params,
     wisp_cons (WISP_CACHE (PROGN), rest),
     wisp_machine->scopes,
     WISP_CACHE (MACRO));

  return closure;
}

WISP_DEFUN ("CONS", wisp_CONS, 2, false)
(wisp_word_t car, wisp_word_t cdr, wisp_word_t rest)
{
  wisp_word_t cons
    = wisp_alloc_raw (WISP_CONS_SIZE, WISP_LOWTAG_LIST_PTR);

  wisp_word_t *data = wisp_deref (cons);
  data[0] = car;
  data[1] = cdr;

  return cons;
}

WISP_DEFUN ("CAR", wisp_CAR, 1, false)
(wisp_word_t list, wisp_word_t rest)
{
  assert (WISP_IS_LIST_PTR (list));
  return (wisp_deref (list))[0];
}

WISP_DEFUN ("CDR", wisp_CDR, 1, false)
(wisp_word_t list, wisp_word_t rest)
{
  assert (WISP_IS_LIST_PTR (list));
  return (wisp_deref (list))[1];
}

WISP_DEFUN ("MAKE-INSTANCE", wisp_make_instance, 2, false)
(wisp_word_t klass, wisp_word_t initargs, wisp_word_t rest)
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

WISP_DEFUN ("SET-SYMBOL-FUNCTION", wisp_SET_SYMBOL_FUNCTION, 2, false)
(wisp_word_t symbol, wisp_word_t value, wisp_word_t rest)
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

WISP_DEFUN ("GC", wisp_collect_garbage, 0, false)
(wisp_word_t rest)
{
  wisp_tidy ();
  return NIL;
}

WISP_DEFUN ("PRINT", wisp_print, 1, false)
(wisp_word_t x, wisp_word_t rest)
{
  wisp_dump (stdout, x);
  putchar ('\n');
  return x;
}

WISP_DEFUN ("GET/CC", wisp_getcc, 0, false)
(wisp_word_t rest)
{
  return wisp_machine->plan;
}

WISP_DEFUN ("SAVE-HEAP", wisp_SAVE_HEAP, 1, false)
(wisp_word_t pathname, wisp_word_t rest)
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

WISP_DEFUN ("+", wisp_add, 0, true)
(wisp_word_t args)
{
  wisp_word_t sum = wisp_fixnum (0);

  while (args != NIL)
    {
      wisp_word_t x = wisp_pop (&args);

      if (!WISP_IS_FIXNUM (x))
        wisp_crash ("not a number");

      sum += x;
    }

  return sum;
}

WISP_DEFUN ("-", wisp_subtract, 1, true)
(wisp_word_t x, wisp_word_t args)
{
  int result = -x;

  while (args != NIL)
    {
      wisp_word_t y = wisp_pop (&args);

      if (!WISP_IS_FIXNUM (y))
        wisp_crash ("not a number");

      result -= y;
    }

  return x;
}

WISP_DEFUN ("*", wisp_multiply, 0, true)
(wisp_word_t args)
{
  wisp_word_t product = 1;

  while (args != NIL)
    {
      wisp_word_t x = wisp_pop (&args);

      if (!WISP_IS_FIXNUM (x))
        wisp_crash ("not a number");

      product *= x >> 2;
    }

  return wisp_fixnum (product);
}

#ifdef EMSCRIPTEN

EM_JS(int, wisp_js_fetch, (const char *url), {
  let urlString = UTF8ToString(url);
  let promise = fetch(urlString).then(x => x.text());
  let id = window.wisp.promise(promise);
  return id;
});

#else

int
wisp_js_fetch (const char *url)
{
  return 1;
}

#endif

WISP_DEFUN ("FETCH", wisp_FETCH, 1, false)
(wisp_word_t url, wisp_word_t rest)
{
  const char *string =
    wisp_string_buffer (wisp_deref (url));

  wisp_word_t promise =
    wisp_fixnum (wisp_js_fetch (string));

  return wisp_make_instance_va
    (wisp_intern_lisp ("PROMISE"), 1, promise);
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

wisp_word_t
wisp_cons (wisp_word_t car, wisp_word_t cdr)
{
  return wisp_CONS (car, cdr, NIL);
}

wisp_word_t
wisp_car (wisp_word_t cons)
{
  return wisp_CAR (cons, NIL);
}

wisp_word_t
wisp_cdr (wisp_word_t cons)
{
  return wisp_CDR (cons, NIL);
}

wisp_word_t
wisp_set_symbol_function (wisp_word_t symbol, wisp_word_t function)
{
  return wisp_SET_SYMBOL_FUNCTION (symbol, function, NIL);
}

wisp_word_t
wisp_save_heap (wisp_word_t path)
{
  return wisp_SAVE_HEAP (path, NIL);
}

void
wisp_defs (void)
{
  WISP_REGISTER (wisp_quote, "TERM");
  WISP_REGISTER (wisp_eval, "TERM");
  WISP_REGISTER (wisp_lambda, "LAMBDA-LIST", "BODY");
  WISP_REGISTER (wisp_macro, "LAMBDA-LIST", "BODY");
  WISP_REGISTER (wisp_CONS, "CAR", "CDR");
  WISP_REGISTER (wisp_CAR, "CONS");
  WISP_REGISTER (wisp_CDR, "CONS");
  WISP_REGISTER (wisp_SET_SYMBOL_FUNCTION, "SYMBOL", "FUNCTION");
  WISP_REGISTER (wisp_SAVE_HEAP, "HEAP-PATH");
  WISP_REGISTER (wisp_collect_garbage, NULL);
  WISP_REGISTER (wisp_print, "X");
  WISP_REGISTER (wisp_make_instance, "CLASS", "SLOTS");
  WISP_REGISTER (wisp_add, "X", "Y");
  WISP_REGISTER (wisp_subtract, "X", "Y");
  WISP_REGISTER (wisp_multiply, "X", "Y");
  WISP_REGISTER (wisp_getcc, NULL);
  WISP_REGISTER (wisp_FETCH, "URL");
}
