#include "wisp.h"

typedef enum {
  wisp_quote_tag,
  wisp_eval_tag,
  wisp_lambda_tag,
  wisp_macro_tag,
  wisp_CONS_tag,
  wisp_CAR_tag,
  wisp_CDR_tag,
  wisp_make_instance_tag,
  wisp_SET_SYMBOL_FUNCTION_tag,
  wisp_SAVE_HEAP_tag,
  wisp_add_tag,
  wisp_subtract_tag,
  wisp_multiply_tag,
  wisp_collect_garbage_tag,
  wisp_print_tag,
  wisp_getcc_tag,

  WISP_N_BUILTINS
} wisp_builtin_tag_t;
