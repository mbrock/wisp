import "os" for Process

class Pair {
  construct new(car, cdr) {
    _car = car
    _cdr = cdr
  }

  car { _car }
  cdr { _cdr }
}

class Benchmark {
  static tak(x, y, z) {
    if (!(y < x)) return z
    return tak(
      tak(x - 1, y, z),
      tak(y - 1, z, x),
      tak(z - 1, x, y)
    )
  }

  static deriv(value) {
    if (!(value is List)) return value == "x" ? 1 : 0

    var op = value[0]
    if (op == "+" || op == "-") {
      var result = [op]
      for (index in 1...value.count) {
        result.add(deriv(value[index]))
      }
      return result
    }
    if (op == "*") {
      var sum = ["+"]
      for (index in 1...value.count) {
        var argument = value[index]
        sum.add(["/", deriv(argument), argument])
      }
      return ["*", value, sum]
    }
    if (op == "/") {
      return [
        "-",
        ["/", deriv(value[1]), value[2]],
        ["/", value[1],
          ["*", value[2], value[2], deriv(value[2])]]
      ]
    }
    Fiber.abort("no derivation method available")
  }

  static makeList(length) {
    var result = null
    for (ignored in 0...length) {
      result = Pair.new(null, result)
    }
    return result
  }

  static diviter(value) {
    var result = null
    while (value != null) {
      result = Pair.new(value.car, result)
      value = value.cdr.cdr
    }
    return result
  }

  static divrec(value) {
    if (value == null) return null
    return Pair.new(value.car, divrec(value.cdr.cdr))
  }

  static listLength(value) {
    var result = 0
    while (value != null) {
      result = result + 1
      value = value.cdr
    }
    return result
  }

  static deepEqual(left, right) {
    if (left is List) {
      if (!(right is List) || left.count != right.count) {
        return false
      }
      for (index in 0...left.count) {
        if (!deepEqual(left[index], right[index])) return false
      }
      return true
    }
    return left == right
  }

  static runOnce(name) {
    if (name == "tak") return tak(18, 12, 6)
    if (name == "deriv") return deriv(__derivInput)
    if (name == "diviter") return diviter(__dividend)
    if (name == "divrec") return divrec(__dividend)
    Fiber.abort("unknown benchmark: %(name)")
  }

  static correct(name, result) {
    if (name == "tak") return result == 7
    if (name == "deriv") return deepEqual(result, __derivExpected)
    return listLength(result) == 500
  }

  static initialize() {
    __derivInput = [
      "+",
      ["*", 3, "x", "x"],
      ["*", "a", "x", "x"],
      ["*", "b", "x"],
      5
    ]
    __derivExpected = [
      "+",
      ["*", ["*", 3, "x", "x"],
        ["+", ["/", 0, 3], ["/", 1, "x"], ["/", 1, "x"]]],
      ["*", ["*", "a", "x", "x"],
        ["+", ["/", 0, "a"], ["/", 1, "x"], ["/", 1, "x"]]],
      ["*", ["*", "b", "x"],
        ["+", ["/", 0, "b"], ["/", 1, "x"]]],
      0
    ]
    __dividend = makeList(1000)
  }
}

Benchmark.initialize()

var arguments = Process.arguments
var name = arguments[0]
var iterations = Num.fromString(arguments[1])
var warmup = arguments.count > 2 ? Num.fromString(arguments[2]) : 0
if (iterations < 1) Fiber.abort("iterations must be positive")

for (ignored in 0...warmup) Benchmark.runOnce(name)

var started = System.clock
var result = null
for (ignored in 0...iterations) {
  result = Benchmark.runOnce(name)
}
var elapsedNs = ((System.clock - started) * 1000000000).floor

if (!Benchmark.correct(name, result)) {
  Fiber.abort("wrong result for %(name)")
}

System.print(
  "{\"benchmark\":\"%(name)\",\"iterations\":%(iterations)," +
  "\"elapsed_ns\":%(elapsedNs)," +
  "\"ns_per_iteration\":%((elapsedNs / iterations).floor)}"
)
