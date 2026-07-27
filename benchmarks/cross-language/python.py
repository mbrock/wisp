#!/usr/bin/env python3

import json
import sys
import time


TAK_INPUT = (18, 12, 6)
DERIV_INPUT = ["+", ["*", 3, "x", "x"], ["*", "a", "x", "x"],
               ["*", "b", "x"], 5]
DERIV_EXPECTED = [
    "+",
    ["*", ["*", 3, "x", "x"],
     ["+", ["/", 0, 3], ["/", 1, "x"], ["/", 1, "x"]]],
    ["*", ["*", "a", "x", "x"],
     ["+", ["/", 0, "a"], ["/", 1, "x"], ["/", 1, "x"]]],
    ["*", ["*", "b", "x"],
     ["+", ["/", 0, "b"], ["/", 1, "x"]]],
    0,
]


def tak(x, y, z):
    if y >= x:
        return z
    return tak(tak(x - 1, y, z),
               tak(y - 1, z, x),
               tak(z - 1, x, y))


def deriv(value):
    if not isinstance(value, list):
        return 1 if value == "x" else 0
    op, *arguments = value
    if op in ("+", "-"):
        return [op, *(deriv(argument) for argument in arguments)]
    if op == "*":
        return ["*", value,
                ["+", *(["/", deriv(argument), argument]
                         for argument in arguments)]]
    if op == "/":
        return ["-", ["/", deriv(value[1]), value[2]],
                ["/", value[1],
                 ["*", value[2], value[2], deriv(value[2])]]]
    raise ValueError("no derivation method available")


class Pair:
    __slots__ = ("car", "cdr")

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr


def make_list(length):
    result = None
    for _ in range(length):
        result = Pair(None, result)
    return result


def diviter(value):
    result = None
    while value is not None:
        result = Pair(value.car, result)
        value = value.cdr.cdr
    return result


def divrec(value):
    if value is None:
        return None
    return Pair(value.car, divrec(value.cdr.cdr))


def list_length(value):
    result = 0
    while value is not None:
        result += 1
        value = value.cdr
    return result


DIVIDEND = make_list(1000)


def run_once(name):
    if name == "tak":
        return tak(*TAK_INPUT)
    if name == "deriv":
        return deriv(DERIV_INPUT)
    if name == "diviter":
        return diviter(DIVIDEND)
    if name == "divrec":
        return divrec(DIVIDEND)
    raise ValueError(f"unknown benchmark: {name}")


def correct(name, result):
    if name == "tak":
        return result == 7
    if name == "deriv":
        return result == DERIV_EXPECTED
    return list_length(result) == 500


def main():
    name = sys.argv[1]
    iterations = int(sys.argv[2])
    warmup = int(sys.argv[3]) if len(sys.argv) > 3 else 0
    if iterations < 1:
        raise ValueError("iterations must be positive")

    for _ in range(warmup):
        run_once(name)

    started = time.perf_counter_ns()
    result = None
    for _ in range(iterations):
        result = run_once(name)
    elapsed = time.perf_counter_ns() - started

    if not correct(name, result):
        raise RuntimeError(f"wrong result for {name}")
    print(json.dumps({
        "benchmark": name,
        "iterations": iterations,
        "elapsed_ns": elapsed,
        "ns_per_iteration": elapsed // iterations,
    }, separators=(",", ":")))


if __name__ == "__main__":
    main()
