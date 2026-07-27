#define _POSIX_C_SOURCE 200809L

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef enum { INTEGER, SYMBOL, PAIR } Kind;
typedef enum { X, A, B, PLUS, MINUS, TIMES, DIVIDE } Symbol;

typedef struct Value Value;
struct Value {
    Kind kind;
    union {
        int integer;
        Symbol symbol;
        struct {
            Value *car;
            Value *cdr;
        } pair;
    } as;
};

typedef struct {
    Value *values;
    size_t used;
    size_t capacity;
} Arena;

static Value integer_0 = {.kind = INTEGER, .as.integer = 0};
static Value integer_1 = {.kind = INTEGER, .as.integer = 1};
static Value integer_3 = {.kind = INTEGER, .as.integer = 3};
static Value integer_5 = {.kind = INTEGER, .as.integer = 5};
static Value symbol_x = {.kind = SYMBOL, .as.symbol = X};
static Value symbol_a = {.kind = SYMBOL, .as.symbol = A};
static Value symbol_b = {.kind = SYMBOL, .as.symbol = B};
static Value symbol_plus = {.kind = SYMBOL, .as.symbol = PLUS};
static Value symbol_minus = {.kind = SYMBOL, .as.symbol = MINUS};
static Value symbol_times = {.kind = SYMBOL, .as.symbol = TIMES};
static Value symbol_divide = {.kind = SYMBOL, .as.symbol = DIVIDE};

static Value *allocate(Arena *arena) {
    if (arena->used == arena->capacity) {
        fprintf(stderr, "native benchmark arena exhausted\n");
        exit(2);
    }
    return &arena->values[arena->used++];
}

static Value *cons(Arena *arena, Value *car, Value *cdr) {
    Value *value = allocate(arena);
    value->kind = PAIR;
    value->as.pair.car = car;
    value->as.pair.cdr = cdr;
    return value;
}

static Value *list(Arena *arena, Value **items, size_t length) {
    Value *result = NULL;
    while (length > 0) {
        length--;
        result = cons(arena, items[length], result);
    }
    return result;
}

static Value *list3(
    Arena *arena,
    Value *a,
    Value *b,
    Value *c
) {
    Value *items[] = {a, b, c};
    return list(arena, items, 3);
}

static int equal(Value *a, Value *b) {
    if (a == b) return 1;
    if (a == NULL || b == NULL || a->kind != b->kind) return 0;
    if (a->kind == INTEGER)
        return a->as.integer == b->as.integer;
    if (a->kind == SYMBOL)
        return a->as.symbol == b->as.symbol;
    return equal(a->as.pair.car, b->as.pair.car) &&
        equal(a->as.pair.cdr, b->as.pair.cdr);
}

static int tak(int x, int y, int z) {
    if (!(y < x)) return z;
    return tak(
        tak(x - 1, y, z),
        tak(y - 1, z, x),
        tak(z - 1, x, y)
    );
}

static Value *deriv(Arena *arena, Value *value);

static Value *map_deriv(Arena *arena, Value *values) {
    if (values == NULL) return NULL;
    return cons(
        arena,
        deriv(arena, values->as.pair.car),
        map_deriv(arena, values->as.pair.cdr)
    );
}

static Value *map_quotients(Arena *arena, Value *values) {
    if (values == NULL) return NULL;
    Value *argument = values->as.pair.car;
    return cons(
        arena,
        list3(
            arena,
            &symbol_divide,
            deriv(arena, argument),
            argument
        ),
        map_quotients(arena, values->as.pair.cdr)
    );
}

static Value *deriv(Arena *arena, Value *value) {
    if (value->kind != PAIR)
        return value == &symbol_x ? &integer_1 : &integer_0;

    Value *op = value->as.pair.car;
    Value *arguments = value->as.pair.cdr;
    if (op == &symbol_plus || op == &symbol_minus)
        return cons(arena, op, map_deriv(arena, arguments));
    if (op == &symbol_times) {
        Value *sum = cons(
            arena,
            &symbol_plus,
            map_quotients(arena, arguments)
        );
        return list3(arena, &symbol_times, value, sum);
    }
    if (op == &symbol_divide) {
        Value *numerator = arguments->as.pair.car;
        Value *denominator =
            arguments->as.pair.cdr->as.pair.car;
        Value *first = list3(
            arena,
            &symbol_divide,
            deriv(arena, numerator),
            denominator
        );
        Value *product_items[] = {
            &symbol_times,
            denominator,
            denominator,
            deriv(arena, denominator),
        };
        Value *second = list3(
            arena,
            &symbol_divide,
            numerator,
            list(arena, product_items, 4)
        );
        return list3(arena, &symbol_minus, first, second);
    }
    fprintf(stderr, "no derivation method available\n");
    exit(2);
}

static Value *make_deriv_input(Arena *arena) {
    Value *term1_items[] = {
        &symbol_times, &integer_3, &symbol_x, &symbol_x,
    };
    Value *term2_items[] = {
        &symbol_times, &symbol_a, &symbol_x, &symbol_x,
    };
    Value *term3_items[] = {
        &symbol_times, &symbol_b, &symbol_x,
    };
    Value *input_items[] = {
        &symbol_plus,
        list(arena, term1_items, 4),
        list(arena, term2_items, 4),
        list(arena, term3_items, 3),
        &integer_5,
    };
    return list(arena, input_items, 5);
}

static Value *quotient(
    Arena *arena,
    Value *derivative,
    Value *argument
) {
    return list3(
        arena,
        &symbol_divide,
        derivative,
        argument
    );
}

static Value *expected_term(
    Arena *arena,
    Value *term,
    Value **quotients,
    size_t quotient_count
) {
    Value *sum = cons(
        arena,
        &symbol_plus,
        list(arena, quotients, quotient_count)
    );
    return list3(arena, &symbol_times, term, sum);
}

static Value *make_deriv_expected(
    Arena *arena,
    Value *input
) {
    Value *term1 = input->as.pair.cdr->as.pair.car;
    Value *term2 =
        input->as.pair.cdr->as.pair.cdr->as.pair.car;
    Value *term3 =
        input->as.pair.cdr->as.pair.cdr->as.pair.cdr
            ->as.pair.car;

    Value *q1[] = {
        quotient(arena, &integer_0, &integer_3),
        quotient(arena, &integer_1, &symbol_x),
        quotient(arena, &integer_1, &symbol_x),
    };
    Value *q2[] = {
        quotient(arena, &integer_0, &symbol_a),
        quotient(arena, &integer_1, &symbol_x),
        quotient(arena, &integer_1, &symbol_x),
    };
    Value *q3[] = {
        quotient(arena, &integer_0, &symbol_b),
        quotient(arena, &integer_1, &symbol_x),
    };
    Value *items[] = {
        &symbol_plus,
        expected_term(arena, term1, q1, 3),
        expected_term(arena, term2, q2, 3),
        expected_term(arena, term3, q3, 2),
        &integer_0,
    };
    return list(arena, items, 5);
}

static Value *make_nil_list(Arena *arena, size_t length) {
    Value *result = NULL;
    while (length > 0) {
        result = cons(arena, NULL, result);
        length--;
    }
    return result;
}

static Value *diviter(Arena *arena, Value *value) {
    Value *result = NULL;
    while (value != NULL) {
        result = cons(arena, value->as.pair.car, result);
        value = value->as.pair.cdr->as.pair.cdr;
    }
    return result;
}

static Value *divrec(Arena *arena, Value *value) {
    if (value == NULL) return NULL;
    return cons(
        arena,
        value->as.pair.car,
        divrec(arena, value->as.pair.cdr->as.pair.cdr)
    );
}

static size_t list_length(Value *value) {
    size_t result = 0;
    while (value != NULL) {
        result++;
        value = value->as.pair.cdr;
    }
    return result;
}

static uint64_t nanoseconds(void) {
    struct timespec value;
    clock_gettime(CLOCK_MONOTONIC, &value);
    return (uint64_t)value.tv_sec * 1000000000 +
        (uint64_t)value.tv_nsec;
}

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: native BENCHMARK ITERATIONS [WARMUP]\n");
        return 2;
    }

    const char *name = argv[1];
    size_t iterations = strtoull(argv[2], NULL, 10);
    size_t warmup = argc > 3 ? strtoull(argv[3], NULL, 10) : 0;
    if (iterations == 0) {
        fprintf(stderr, "iterations must be positive\n");
        return 2;
    }

    Arena arena = {
        .values = calloc(1000000, sizeof(Value)),
        .used = 0,
        .capacity = 1000000,
    };
    if (arena.values == NULL) return 2;

    Value *deriv_input = make_deriv_input(&arena);
    Value *deriv_expected =
        make_deriv_expected(&arena, deriv_input);
    Value *dividend = make_nil_list(&arena, 1000);
    Value *result = NULL;
    int integer_result = 0;

    for (size_t i = 0; i < warmup; i++) {
        if (strcmp(name, "tak") == 0)
            integer_result = tak(18, 12, 6);
        else if (strcmp(name, "deriv") == 0)
            result = deriv(&arena, deriv_input);
        else if (strcmp(name, "diviter") == 0)
            result = diviter(&arena, dividend);
        else if (strcmp(name, "divrec") == 0)
            result = divrec(&arena, dividend);
        else {
            fprintf(stderr, "unknown benchmark: %s\n", name);
            return 2;
        }
    }

    uint64_t started = nanoseconds();
    for (size_t i = 0; i < iterations; i++) {
        if (strcmp(name, "tak") == 0)
            integer_result = tak(18, 12, 6);
        else if (strcmp(name, "deriv") == 0)
            result = deriv(&arena, deriv_input);
        else if (strcmp(name, "diviter") == 0)
            result = diviter(&arena, dividend);
        else
            result = divrec(&arena, dividend);
    }
    uint64_t elapsed = nanoseconds() - started;

    int correct = strcmp(name, "tak") == 0
        ? integer_result == 7
        : strcmp(name, "deriv") == 0
            ? equal(result, deriv_expected)
            : list_length(result) == 500;
    if (!correct) {
        fprintf(stderr, "wrong result for %s\n", name);
        return 2;
    }

    printf(
        "{\"benchmark\":\"%s\",\"iterations\":%zu,"
        "\"elapsed_ns\":%llu,\"ns_per_iteration\":%llu}\n",
        name,
        iterations,
        (unsigned long long)elapsed,
        (unsigned long long)(elapsed / iterations)
    );
    free(arena.values);
    return 0;
}
