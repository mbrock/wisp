#!/usr/bin/env ruby

require "json"

TAK_INPUT = [18, 12, 6].freeze
DERIV_INPUT = [
  :+, [:*, 3, :x, :x], [:*, :a, :x, :x],
  [:*, :b, :x], 5
].freeze
DERIV_EXPECTED = [
  :+,
  [:*, [:*, 3, :x, :x],
   [:+, [:/, 0, 3], [:/, 1, :x], [:/, 1, :x]]],
  [:*, [:*, :a, :x, :x],
   [:+, [:/, 0, :a], [:/, 1, :x], [:/, 1, :x]]],
  [:*, [:*, :b, :x],
   [:+, [:/, 0, :b], [:/, 1, :x]]],
  0
].freeze
Pair = Struct.new(:car, :cdr)

def make_list(length)
  result = nil
  length.times { result = Pair.new(nil, result) }
  result
end

DIVIDEND = make_list(1000)

def tak(x, y, z)
  return z unless y < x

  tak(tak(x - 1, y, z),
      tak(y - 1, z, x),
      tak(z - 1, x, y))
end

def deriv(value)
  return value == :x ? 1 : 0 unless value.is_a?(Array)

  op = value[0]
  arguments = value.drop(1)
  return [op, *arguments.map { |argument| deriv(argument) }] \
    if op == :+ || op == :-
  if op == :*
    return [:*, value,
            [:+, *arguments.map { |argument|
              [:/, deriv(argument), argument]
            }]]
  end
  if op == :/
    return [:-, [:/, deriv(value[1]), value[2]],
            [:/, value[1],
             [:*, value[2], value[2], deriv(value[2])]]]
  end
  raise "no derivation method available"
end

def diviter(value)
  result = nil
  until value.nil?
    result = Pair.new(value.car, result)
    value = value.cdr.cdr
  end
  result
end

def divrec(value)
  return nil if value.nil?

  Pair.new(value.car, divrec(value.cdr.cdr))
end

def list_length(value)
  result = 0
  until value.nil?
    result += 1
    value = value.cdr
  end
  result
end

def run_once(name)
  case name
  when "tak" then tak(*TAK_INPUT)
  when "deriv" then deriv(DERIV_INPUT)
  when "diviter" then diviter(DIVIDEND)
  when "divrec" then divrec(DIVIDEND)
  else raise "unknown benchmark: #{name}"
  end
end

def correct?(name, result)
  return result == 7 if name == "tak"
  return result == DERIV_EXPECTED if name == "deriv"

  list_length(result) == 500
end

name = ARGV.fetch(0)
iterations = Integer(ARGV.fetch(1))
warmup = Integer(ARGV.fetch(2, "0"))
raise "iterations must be positive" if iterations < 1

warmup.times { run_once(name) }
started = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
result = nil
iterations.times { result = run_once(name) }
elapsed = Process.clock_gettime(
  Process::CLOCK_MONOTONIC, :nanosecond
) - started

raise "wrong result for #{name}" unless correct?(name, result)

puts JSON.generate({
  benchmark: name,
  iterations: iterations,
  elapsed_ns: elapsed,
  ns_per_iteration: elapsed / iterations
})
