/*
 * This file is part of Wisp.
 *
 * Wisp is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * Wisp is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
 * Public License for more details.

 * You should have received a copy of the GNU Affero General Public
 * License along with Wisp. If not, see <https://www.gnu.org/licenses/>.
 */

#include "wisp.h"

wisp_machine_t
wisp_initial_machine (wisp_word_t term)
{
  return (wisp_machine_t){ .term = term, .plan = NIL, .scopes = NIL };
}

WISP_EXPORT
wisp_word_t
wisp_eval_code (const char *code)
{
  /* WISP_DEBUG ("evaluating %s\n", code); */

  wisp_word_t term = wisp_read (&code);

  wisp_machine_t machine = wisp_initial_machine (term);
  wisp_machine = &machine;

  while (wisp_step (&machine))
    if (wisp_heap_used >= heap_size * wisp_gc_fraction)
      wisp_tidy ();

  return machine.term;
}

wisp_word_t
wisp_reverse (wisp_word_t list)
{
  wisp_word_t result = NIL;

  while (list != NIL)
    {
      wisp_word_t car = wisp_pop (&list);
      result = wisp_cons (car, result);
    }

  return result;
}

bool
wisp_step_into_call (wisp_machine_t *machine,
                     wisp_word_t callee,
                     wisp_word_t args);

bool
wisp_find_binding_in_scope (wisp_word_t scope,
                            wisp_word_t symbol,
                            wisp_word_t *result)
{
  wisp_word_t *header = wisp_deref (scope);
  wisp_word_t *slots = header + 2;
  int size = wisp_header_word_data (header[0]) / 2;
  for (int i = 0; i < size; i++)
    {
      wisp_word_t variable = slots[i * 2];
      if (variable == symbol)
        {
          *result = slots[i * 2 + 1];
          return true;
        }
    }

  return false;
}

bool
wisp_find_binding (wisp_word_t scopes,
                   wisp_word_t symbol,
                   wisp_word_t *result)
{
  if (scopes == NIL)
    return false;

  wisp_word_t *scopes_cons = wisp_deref (scopes);

  wisp_word_t scopes_car = scopes_cons[0];
  wisp_word_t scopes_cdr = scopes_cons[1];

  if (wisp_find_binding_in_scope (scopes_car, symbol, result))
    return true;

  return wisp_find_binding (scopes_cdr, symbol, result);
}

wisp_word_t
wisp_make_apply_plan (wisp_word_t function,
                      wisp_word_t values,
                      wisp_word_t terms,
                      wisp_word_t scopes,
                      wisp_word_t next)
{
  return wisp_make_instance_va
    (WISP_CACHE (APPLY), 5, function, values, terms, scopes, next);
}

#define WISP_DEFINE_PLAN_TYPE(name)                \
  wisp_##name##_t *                                \
  wisp_get_##name (wisp_word_t *header)            \
  {                                                \
    return (wisp_##name##_t *) (header + 2);       \
  }

WISP_DEFINE_PLAN_TYPE (apply_plan);
WISP_DEFINE_PLAN_TYPE (progn_plan);
WISP_DEFINE_PLAN_TYPE (if_plan);
WISP_DEFINE_PLAN_TYPE (funcall_plan);
WISP_DEFINE_PLAN_TYPE (await_plan);

bool
wisp_term_irreducible (wisp_word_t term)
{
  switch (term & WISP_LOWTAG_MASK)
    {
    case WISP_LOWTAG_FIXNUM_0:
    case WISP_LOWTAG_FIXNUM_1:
      return true;

    default:
      if (term == NIL)
        return true;

      if (WISP_IS_LIST_PTR (term))
        return false;

      if (WISP_IS_STRUCT_PTR (term))
        return true;

      if (wisp_is_symbol (term))
        return false;

      if (WISP_LOWTAG (term) == WISP_LOWTAG_OTHER_PTR)
        {
          wisp_word_t *header = wisp_deref (term);
          if (WISP_WIDETAG (header[0]) == WISP_WIDETAG_STRING)
            return true;
        }

      wisp_crash ("strange term");
    }
}

wisp_closure_t *
wisp_get_closure (wisp_word_t value)
{
  wisp_word_t *symbol_header = wisp_is_symbol (value);

  wisp_word_t *closure_header =
    symbol_header
    ? wisp_is_instance (symbol_header[6], WISP_CACHE (CLOSURE))
    : wisp_is_instance (value, WISP_CACHE (CLOSURE));

  if (closure_header)
    return (wisp_closure_t *) (closure_header + 2);
  else
    wisp_crash ("not a function");
}

wisp_word_t
wisp_lambda_list_to_params (wisp_word_t lambda_list)
{
  int length = 0;
  wisp_word_t rest_symbol = NIL;

  wisp_word_t lambda_cons = lambda_list;
  for (;;)
    {
      if (lambda_cons == NIL)
        break;

      if (!WISP_IS_LIST_PTR (lambda_cons))
        {
          rest_symbol = lambda_cons;
          break;
        }

      wisp_word_t car = wisp_pop (&lambda_cons);
      ++length;
    }

  wisp_word_t slots[1 + length];

  for (int i = 0; i < length; i++)
    slots[i] = wisp_pop (&lambda_list);

  slots[length] = rest_symbol;

  wisp_word_t params =
    wisp_make_instance_with_slots
    (WISP_CACHE (PARAMS), 1 + length, slots);

  return params;
}

wisp_word_t
wisp_make_args_scope (wisp_word_t params,
                      wisp_word_t values,
                      bool backwards)
{
  wisp_word_t *parameter_struct =
    wisp_deref (params);

  wisp_word_t parameter_count =
    wisp_header_word_data (parameter_struct[0]) - 1;

  wisp_word_t *parameter_names =
    parameter_struct + 2;

  bool varargs =
    parameter_names[parameter_count - 1] != NIL;

  int slot_count =
    varargs
    ? parameter_count
    : parameter_count - 1;

  wisp_word_t scope_slots[2 * slot_count];

  if (varargs && backwards)
    {
      backwards = false;
      values = wisp_reverse (values);
    }

  for (int i = 0; i < slot_count; i++)
    {
      int parameter_index = backwards
        ? slot_count - i - 1
        : i;

      scope_slots[parameter_index * 2] =
        parameter_names[parameter_index];

      if (varargs && (parameter_index == slot_count - 1))
        scope_slots[parameter_index * 2 + 1] = values;
      else
        {
          wisp_word_t car = wisp_car (values);
          wisp_word_t cdr = wisp_cdr (values);

          scope_slots[parameter_index * 2 + 1] = car;
          values = cdr;
        }
    }

  wisp_word_t args_scope =
    wisp_make_instance_with_slots
    (WISP_CACHE (SCOPE), 2 * slot_count, scope_slots);

  return args_scope;
}

wisp_machine_t *wisp_machine = NULL;

void
wisp_do_call (wisp_machine_t *machine,
              bool backwards,
              wisp_apply_plan_t *plan)
{
  wisp_word_t function = plan->function;

  if (WISP_WIDETAG (function) == WISP_WIDETAG_BUILTIN)
    {
      uint32_t id = WISP_IMMEDIATE_DATA (function);
      wisp_defun_t builtin = wisp_builtins[id];

      wisp_word_t values = plan->values;
      int args_length = wisp_length (values);

      if (args_length < builtin.n_args)
        wisp_crash ("too few arguments");

      if (args_length > builtin.n_args && !builtin.varargs)
        wisp_crash ("too many arguments");

      wisp_word_t args[builtin.n_args];

      for (int i = 0; i < builtin.n_args; i++)
        {
          int parameter_index = backwards
            ? args_length - i - 1
            : i;

          assert (values != NIL);

          wisp_word_t *cons = wisp_deref (values);
          wisp_word_t car = cons[0];
          wisp_word_t cdr = cons[1];

          args[parameter_index] = car;

          values = cdr;
        }

      wisp_word_t x;
      wisp_fun_ptr_t f = builtin.function;

      switch (builtin.n_args)
        {
        case 0: x = f.a0 (values); break;
        case 1: x = f.a1 (args[0], values); break;
        case 2: x = f.a2 (args[0], args[1], values); break;
        case 3: x = f.a3 (args[0], args[1], args[2], values); break;
        default: wisp_not_implemented ();
        }

      if (builtin.evaluate_result)
        {
          *machine = (wisp_machine_t) {
            .plan = plan->next,
            .term = x,
            .value = false,
            .scopes = plan->scopes
          };
        }
      else
        {
          *machine = (wisp_machine_t) {
            .plan = plan->next,
            .term = x,
            .value = true,
            .scopes = plan->scopes
          };
        }
    }
  else
    {
      wisp_closure_t *closure =
        wisp_get_closure (function);

      /* WISP_DEBUG ("body "); */
      /* wisp_dump (stderr, closure->body); */
      /* WISP_DEBUG ("\n"); */

      wisp_word_t args_scope =
        wisp_make_args_scope (closure->params, plan->values, backwards);

      /* WISP_DEBUG ("made args scope\n"); */
      /* WISP_DEBUG ("body "); */
      /* wisp_dump (stderr, closure->body); */
      /* WISP_DEBUG ("\n"); */

      if (closure->macro == NIL)
        *machine = (wisp_machine_t) {
          .term = closure->body,
          .value = false,
          .scopes = wisp_cons (args_scope, closure->scopes),
          .plan = plan->next
        };

      else
        {
          wisp_word_t eval_plan =
            wisp_make_instance_va (WISP_CACHE (EVAL), 2,
                                   machine->scopes,
                                   plan->next);

          *machine = (wisp_machine_t) {
            .term = closure->body,
            .value = false,
            .scopes = wisp_cons (args_scope, closure->scopes),
            .plan = eval_plan
          };
        }
    }
}

bool
wisp_follow_plan (wisp_machine_t *machine)
{
  wisp_word_t value = machine->term;
  wisp_word_t scopes = machine->scopes;
  wisp_word_t plan = machine->plan;

  if (!WISP_IS_STRUCT_PTR (plan))
    wisp_crash ("bad plan");

  wisp_word_t *header = wisp_deref (plan);
  wisp_word_t type = wisp_struct_header_type (header);

  if (type == WISP_CACHE (APPLY))
    {
      wisp_apply_plan_t *apply_plan =
        wisp_get_apply_plan (header);

      wisp_word_t terms = apply_plan->terms;

      if (terms == NIL)
        {
          wisp_apply_plan_t new_apply_plan = {
            .function = apply_plan->function,
            .terms = NIL,
            .values = wisp_cons (value, apply_plan->values),
            .scopes = apply_plan->scopes,
            .next = apply_plan->next
          };

          wisp_do_call (machine, true, &new_apply_plan);

          return true;
        }
      else
        {
          wisp_word_t *cons =
            wisp_deref (terms);

          wisp_word_t car = cons[0];
          wisp_word_t cdr = cons[1];

          wisp_word_t next_apply_plan =
            wisp_make_apply_plan
            (apply_plan->function,
             wisp_cons (value, apply_plan->values),
             cdr,
             apply_plan->scopes,
             apply_plan->next);

          machine->scopes = apply_plan->scopes;
          machine->term = car;
          machine->value = false;
          machine->plan = next_apply_plan;

          return true;
        }
    }

  else if (type == WISP_CACHE (FUNCALL))
    {
      wisp_funcall_plan_t *funcall_plan =
        wisp_get_funcall_plan (header);

      machine->scopes = funcall_plan->scopes;
      machine->plan = funcall_plan->next;

      return wisp_step_into_call
        (machine,
         value,
         funcall_plan->terms);
    }

  else if (type == WISP_CACHE (EVAL))
    {
      /* fprintf (stderr, "; eval\n"); */
      machine->value = false;
      machine->scopes = header[2];
      machine->plan = header[3];
      return true;
    }

  else if (type == WISP_CACHE (PROGN))
    {
      wisp_progn_plan_t *progn_plan =
        wisp_get_progn_plan (header);

      wisp_word_t terms = progn_plan->terms;

      if (terms == NIL)
        {
          machine->scopes = progn_plan->scopes;
          machine->plan = progn_plan->next;

          return true;
        }
      else
        {
          wisp_word_t car = wisp_car (terms);
          wisp_word_t cdr = wisp_cdr (terms);

          wisp_word_t new_plan = wisp_make_instance_va
            (WISP_CACHE (PROGN), 3,
             cdr,
             progn_plan->scopes,
             progn_plan->next);

          machine->scopes = progn_plan->scopes;
          machine->term = car;
          machine->value = false;
          machine->plan = new_plan;

          return true;
        }
    }

  else if (type == WISP_CACHE (IF))
    {
      wisp_if_plan_t *if_plan =
        wisp_get_if_plan (header);

      machine->value = false;

      if (value != NIL)
        machine->term = if_plan->true_case;
      else
        machine->term = if_plan->false_case;

      machine->scopes = if_plan->scopes;
      machine->plan = if_plan->next;

      return true;
    }

  else if (type == WISP_CACHE (AWAIT))
    {
      wisp_await_plan_t *await_plan =
        wisp_get_await_plan (header);

      machine->term = wisp_make_instance_va
        (WISP_CACHE (AWAIT), 1, value);
      machine->value = false;
      machine->scopes = await_plan->scopes;
      machine->plan = await_plan->next;

      // We return false to say that evaluation has stopped, but we
      // also set the machine value to false to say that we are
      // waiting for a value from the outer runtime.
      return false;
    }

  wisp_crash ("bad plan");
}

bool
wisp_step_into_nullary_call (wisp_machine_t *machine,
                             wisp_word_t function)
{
  wisp_word_t scopes = machine->scopes;
  wisp_word_t plan = machine->plan;

  // XXX: It's not necessary to allocate a plan on the heap here.

  wisp_apply_plan_t apply_plan = {
    .function = function,
    .terms = NIL,
    .values = NIL,
    .scopes = scopes,
    .next = plan
  };

  wisp_do_call (machine, false, &apply_plan);

  return true;
}

bool
wisp_start_evaluating_arguments (wisp_machine_t *machine,
                                 wisp_word_t function,
                                 wisp_word_t args)
{
  wisp_word_t *term_list = wisp_deref (args);
  wisp_word_t first_term = term_list[0];
  wisp_word_t remaining_terms = term_list[1];

  wisp_word_t apply_plan = wisp_make_apply_plan
    (function, NIL, remaining_terms,
     machine->scopes, machine->plan);

  machine->term = first_term;
  machine->value = false;
  machine->plan = apply_plan;

  return true;
}

bool
wisp_step_into_macro_call (wisp_machine_t *machine,
                           wisp_word_t function,
                           wisp_word_t args)
{
  wisp_apply_plan_t apply_plan = {
    .function = function,
    .terms = NIL,
    .values = args,
    .scopes = machine->scopes,
    .next = machine->plan
  };

  wisp_do_call (machine, false, &apply_plan);

  return true;
}

bool
wisp_function_evaluates_arguments (wisp_word_t function)
{
  if (WISP_WIDETAG (function) == WISP_WIDETAG_BUILTIN)
    {
      uint32_t id = WISP_IMMEDIATE_DATA (function);
      return wisp_builtins[id].evaluate_arguments;
    }
  else
    {
      wisp_closure_t *closure = wisp_get_closure (function);
      return closure->macro == NIL;
    }
}

bool
wisp_step_into_call (wisp_machine_t *machine,
                     wisp_word_t function,
                     wisp_word_t args)
{
  if (args == NIL)
    return wisp_step_into_nullary_call
      (machine, function);

  else if (wisp_function_evaluates_arguments (function))
    return wisp_start_evaluating_arguments
      (machine, function, args);

  else
    return wisp_step_into_macro_call
      (machine, function, args);
}

bool
wisp_step_into_symbol_call (wisp_machine_t *machine,
                            wisp_word_t callee,
                            wisp_word_t args)
{
  wisp_word_t *symbol = wisp_is_symbol (callee);

  if (symbol)
    {
      wisp_word_t function = symbol[6];
      return wisp_step_into_call (machine, function, args);
    }

  wisp_crash ("bad call");
}

bool
wisp_step_into_progn (wisp_machine_t *machine,
                      wisp_word_t sequence)
{
  assert (WISP_IS_PTR (sequence));

  if (sequence == NIL)
    {
      machine->term = NIL;
      machine->value = true;
      return wisp_follow_plan (machine);
    }
  else
    {
      wisp_word_t car = wisp_car (sequence);
      wisp_word_t cdr = wisp_cdr (sequence);

      wisp_word_t plan = wisp_make_instance_va
        (WISP_CACHE (PROGN), 3,
         cdr,
         machine->scopes,
         machine->plan);

      machine->term = car;
      machine->value = false;
      machine->plan = plan;

      return true;
    }
}

wisp_word_t
wisp_pop (wisp_word_t *list)
{
  wisp_word_t car = wisp_car (*list);
  *list = wisp_cdr (*list);

  return car;
}

bool
wisp_step_into_if (wisp_machine_t *machine,
                   wisp_word_t args)
{
  wisp_word_t condition = wisp_pop (&args);
  wisp_word_t true_case = wisp_pop (&args);
  wisp_word_t false_case = wisp_pop (&args);

  assert (args == NIL);

  wisp_word_t plan = wisp_make_instance_va
    (WISP_CACHE (IF), 4,
     true_case,
     false_case,
     machine->scopes,
     machine->plan);

  machine->term = condition;
  machine->value = false;
  machine->plan = plan;

  return true;
}

bool
wisp_step_into_funcall (wisp_machine_t *machine,
                        wisp_word_t args)
{
  wisp_word_t function = wisp_pop (&args);

  wisp_word_t plan = wisp_make_instance_va
    (WISP_CACHE (FUNCALL), 3,
     args,
     machine->scopes,
     machine->plan);

  machine->term = function;
  machine->value = false;
  machine->plan = plan;

  return true;
}

bool
wisp_step_into_await (wisp_machine_t *machine,
                      wisp_word_t args)
{
  wisp_word_t promise = wisp_pop (&args);

  wisp_word_t plan = wisp_make_instance_va
    (WISP_CACHE (AWAIT), 2,
     machine->scopes,
     machine->plan);

  machine->term = promise;
  machine->value = false;
  machine->plan = plan;

  return true;
}

bool
wisp_step (wisp_machine_t *machine)
{
  wisp_word_t term = machine->term;
  wisp_word_t scopes = machine->scopes;
  wisp_word_t plan = machine->plan;

  /* fprintf (stderr, "step: "); */
  /* wisp_dump (stderr, term); */
  /* fprintf (stderr, "\n"); */

  if (machine->value || wisp_term_irreducible (term))
    {
      machine->value = true;

      if (plan == NIL)
        return false;
      else
        return wisp_follow_plan (machine);
    }

  else if (WISP_IS_LIST_PTR (term))
    {
      wisp_word_t *cons = wisp_deref (term);
      wisp_word_t car = cons[0];
      wisp_word_t cdr = cons[1];

      if (car == WISP_CACHE (PROGN))
        return wisp_step_into_progn (machine, cdr);
      else if (car == WISP_CACHE (IF))
        return wisp_step_into_if (machine, cdr);
      else if (car == WISP_CACHE (FUNCALL))
        return wisp_step_into_funcall (machine, cdr);
      else if (car == WISP_CACHE (AWAIT))
        return wisp_step_into_await (machine, cdr);
      else
        return wisp_step_into_symbol_call (machine, car, cdr);
    }

  else if (wisp_is_symbol (term))
    {
      wisp_word_t binding;

      if (wisp_find_binding (machine->scopes, term, &binding))
        {
          machine->term = binding;
          machine->value = true;
          return true;
        }

      else
        wisp_crash ("unbound variable");
    }

  wisp_crash ("bad term");
}

wisp_machine_t the_machine;

WISP_EXPORT
bool
wisp_eval_code_async (const char *code)
{
  wisp_word_t term = wisp_read (&code);

  the_machine = wisp_initial_machine (term);
  wisp_machine = &the_machine;

  while (wisp_step (&the_machine))
    if (wisp_heap_used >= heap_size * wisp_gc_fraction)
      wisp_tidy ();

  return !the_machine.value;
}

WISP_EXPORT
wisp_word_t
wisp_get_machine_term ()
{
  assert (wisp_machine != NULL);
  return wisp_machine->term;
}

WISP_EXPORT
uint32_t
wisp_get_promise_id ()
{
  assert (wisp_machine != NULL);

  wisp_word_t *await = wisp_deref (wisp_machine->term);

  assert (await[0] == WISP_INSTANCE_HEADER (1));
  assert (await[1] == WISP_CACHE (AWAIT));
  assert (WISP_IS_STRUCT_PTR (await[2]));

  wisp_word_t *promise = wisp_deref (await[2]);
  assert (promise[0] == WISP_INSTANCE_HEADER (1));
  assert (promise[1] == WISP_CACHE (PROMISE));
  assert (WISP_IS_FIXNUM (promise[2]));

  return promise[2] >> 2;
}

WISP_EXPORT
bool
wisp_resume_await (wisp_word_t result)
{
  assert (wisp_machine != NULL);
  assert (wisp_machine->value == false);

  wisp_machine->term = result;
  wisp_machine->value = true;

  while (wisp_step (wisp_machine))
    if (wisp_heap_used >= heap_size * wisp_gc_fraction)
      wisp_tidy ();

  return !wisp_machine->value;
}
