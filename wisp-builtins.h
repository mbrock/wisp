#include "wisp.h"

typedef enum {
  wisp_quote_tag,
  wisp_eval_tag,
  wisp_lambda_tag,
  wisp_macro_tag,
  wisp_cons_tag,
  wisp_car_tag,
  wisp_cdr_tag,
  wisp_make_instance_tag,
  wisp_set_symbol_function_tag,
  wisp_save_heap_tag,
  wisp_add_tag,
  wisp_subtract_tag,
  wisp_multiply_tag,
  wisp_collect_garbage_tag,
  wisp_print_tag,
  wisp_getcc_tag,

  WISP_N_BUILTINS
} wisp_builtin_tag_t;
