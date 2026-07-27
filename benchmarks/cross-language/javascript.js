"use strict";

const TAK_INPUT = [18, 12, 6];
const DERIV_INPUT = [
  "+", ["*", 3, "x", "x"], ["*", "a", "x", "x"],
  ["*", "b", "x"], 5,
];
const DERIV_EXPECTED = [
  "+",
  ["*", ["*", 3, "x", "x"],
    ["+", ["/", 0, 3], ["/", 1, "x"], ["/", 1, "x"]]],
  ["*", ["*", "a", "x", "x"],
    ["+", ["/", 0, "a"], ["/", 1, "x"], ["/", 1, "x"]]],
  ["*", ["*", "b", "x"],
    ["+", ["/", 0, "b"], ["/", 1, "x"]]],
  0,
];
class Pair {
  constructor(car, cdr) {
    this.car = car;
    this.cdr = cdr;
  }
}

function makeList(length) {
  let result = null;
  for (let i = 0; i < length; i += 1) {
    result = new Pair(null, result);
  }
  return result;
}

const DIVIDEND = makeList(1000);

function tak(x, y, z) {
  if (!(y < x)) return z;
  return tak(
    tak(x - 1, y, z),
    tak(y - 1, z, x),
    tak(z - 1, x, y),
  );
}

function deriv(value) {
  if (!Array.isArray(value)) return value === "x" ? 1 : 0;
  const [op, ...args] = value;
  if (op === "+" || op === "-") {
    return [op, ...args.map(deriv)];
  }
  if (op === "*") {
    return ["*", value,
      ["+", ...args.map((arg) => ["/", deriv(arg), arg])]];
  }
  if (op === "/") {
    return ["-", ["/", deriv(value[1]), value[2]],
      ["/", value[1],
        ["*", value[2], value[2], deriv(value[2])]]];
  }
  throw new Error("no derivation method available");
}

function diviter(value) {
  let result = null;
  while (value !== null) {
    result = new Pair(value.car, result);
    value = value.cdr.cdr;
  }
  return result;
}

function divrec(value) {
  if (value === null) return null;
  return new Pair(value.car, divrec(value.cdr.cdr));
}

function listLength(value) {
  let result = 0;
  while (value !== null) {
    result += 1;
    value = value.cdr;
  }
  return result;
}

function runOnce(name) {
  switch (name) {
    case "tak": return tak(...TAK_INPUT);
    case "deriv": return deriv(DERIV_INPUT);
    case "diviter": return diviter(DIVIDEND);
    case "divrec": return divrec(DIVIDEND);
    default: throw new Error(`unknown benchmark: ${name}`);
  }
}

function correct(name, result) {
  if (name === "tak") return result === 7;
  if (name === "deriv") {
    return JSON.stringify(result) === JSON.stringify(DERIV_EXPECTED);
  }
  return listLength(result) === 500;
}

const name = process.argv[2];
const iterations = Number.parseInt(process.argv[3], 10);
const warmup = Number.parseInt(process.argv[4] || "0", 10);
if (iterations < 1) throw new Error("iterations must be positive");

for (let i = 0; i < warmup; i += 1) runOnce(name);
const started = process.hrtime.bigint();
let result = null;
for (let i = 0; i < iterations; i += 1) result = runOnce(name);
const elapsed = process.hrtime.bigint() - started;

if (!correct(name, result)) throw new Error(`wrong result for ${name}`);
console.log(JSON.stringify({
  benchmark: name,
  iterations,
  elapsed_ns: Number(elapsed),
  ns_per_iteration: Number(elapsed / BigInt(iterations)),
}));
