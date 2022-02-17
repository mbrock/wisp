const std = @import("std");
const assert = std.debug.assert;

const wisp = @import("./base.zig");
const read = @import("./read.zig").read;
const print = @import("./print.zig").print;

const W = wisp.W;
const NIL = wisp.NIL;
const Wisp = wisp.Wisp;
const Machine = wisp.Machine;

pub const Error = error{
    NotImplemented,
    UnknownTerm,
    VariableNotFound,
    TooManySteps,
};

const TermType = enum {
    number,
    string,
    symbol,
    list,
    nil,
};

fn termType(ctx: *Wisp, term: W) !TermType {
    switch (term.lowtag()) {
        .fixnum0, .fixnum1 => {
            return .number;
        },

        .otherptr => {
            const data = try ctx.heap.deref(term);
            if (data[0].raw == wisp.symbolHeader.raw) {
                return .symbol;
            } else if (data[0].widetag() == .string) {
                return .string;
            } else {
                return Error.UnknownTerm;
            }
        },

        .listptr => {
            if (term.isNil()) {
                return .nil;
            } else {
                return .list;
            }
        },

        else => {
            return Error.UnknownTerm;
        },
    }
}

pub fn irreducible(ctx: *Wisp, term: W) !bool {
    return switch (try termType(ctx, term)) {
        .number, .string, .nil => true,
        else => false,
    };
}

pub fn step(ctx: *Wisp, machine: Machine) !Machine {
    _ = ctx;

    const term = machine.term;

    if (try irreducible(ctx, term)) {
        var next = machine;
        next.value = true;
        return next;
    } else if (term.isOtherPointer()) {
        const data = try ctx.heap.deref(term);
        if (data[0].raw == wisp.symbolHeader.raw) {
            return stepVariable(ctx, machine);
        } else {
            return Error.UnknownTerm;
        }
    } else if (term.isListPointer()) {
        const cons = try ctx.getCons(term);
        if (cons.car.eq(ctx.symbol(.QUOTE))) {
            var next = machine;
            next.value = true;
            next.term = (try ctx.getCons(cons.cdr)).car;
            return next;
        } else {
            return stepIntoSymbolCall(ctx, machine, cons.car, cons.cdr);
        }
    } else {
        return Error.UnknownTerm;
    }
}

fn stepIntoSymbolCall(
    ctx: *Wisp,
    machine: Machine,
    car: W,
    cdr: W,
) !Machine {
    const symbolData = try ctx.getSymbolData(car);
    std.log.warn("symbol {any}", .{symbolData});
    return stepIntoCall(ctx, machine, symbolData.function, cdr);
}

const Direction = enum { backward, foreward };

fn stepIntoCall(
    ctx: *Wisp,
    machine: Machine,
    function: W,
    arglist: W,
) !Machine {
    if (arglist.eq(NIL)) {
        return Error.NotImplemented;
    } else if (try evaluatesArguments(ctx, function)) {
        return stepIntoArgumentList(ctx, machine, function, arglist);
    } else {
        return doCall(ctx, machine, .foreward, wisp.Plan{
            .next = machine.plan,
            .scopes = machine.scopes,
            .data = wisp.PlanData{
                .APPLY = .{
                    .terms = NIL,
                    .values = arglist,
                    .function = function,
                },
            },
        });
    }
}

fn stepIntoArgumentList(
    ctx: *Wisp,
    machine: Machine,
    function: W,
    arglist: W,
) !Machine {
    const termList = try ctx.getCons(arglist);

    var next = machine;
    next.term = termList.car;
    next.value = false;
    next.plan = try makeApplyPlan(
        ctx,
        function,
        NIL,
        termList.cdr,
        machine.scopes,
        machine.plan,
    );

    return next;
}

fn makeApplyPlan(
    ctx: *Wisp,
    function: W,
    values: W,
    terms: W,
    scopes: W,
    next: W,
) !W {
    var slots: [5]W = .{ function, values, terms, scopes, next };
    return ctx.makeInstance(ctx.symbol(.APPLY), slots);
}

fn doCall(
    ctx: *Wisp,
    machine: Machine,
    direction: Direction,
    plan: wisp.Plan,
) !Machine {
    const function = plan.data.APPLY.function;

    if (function.widetag() == .builtin) {
        if (ctx.builtins[function.immediate()]) |builtin| {
            _ = builtin;
            _ = direction;
            _ = machine;
            return Error.NotImplemented;
        } else {
            return Error.NotImplemented;
        }
    } else {
        return Error.NotImplemented;
    }
}

fn evaluatesArguments(ctx: *Wisp, function: W) !bool {
    if (function.widetag() == .builtin) {
        if (ctx.builtins[function.immediate()]) |builtin| {
            return builtin.evalArgs;
        } else {
            return Error.NotImplemented;
        }
    } else {
        const closureData = try ctx.getDataPointer(.closure, function);
        return closureData.macro.eq(NIL);
    }
}

fn searchScope(ctx: *Wisp, scope: W, variable: W) !?W {
    var slice = try ctx.scopeDataAsSlice(scope);

    assert(@mod(slice.len, 2) == 0);

    while (slice.len != 0) {
        const symbol = slice[0];
        const value = slice[1];

        slice = slice[2..slice.len];

        if (symbol.raw == variable.raw) {
            return value;
        }
    }

    return null;
}

fn stepVariable(ctx: *Wisp, machine: Machine) !Machine {
    var scopes = machine.scopes;
    var variable = machine.term;

    while (!scopes.isNil()) {
        const entry = try ctx.getCons(scopes);
        const scope = entry.car;

        if (try searchScope(ctx, scope, variable)) |value| {
            var next = machine;
            next.value = true;
            next.term = value;
            return next;
        } else {
            scopes = entry.cdr;
        }
    }

    return Error.VariableNotFound;
}

pub fn initialMachine(term: W) Machine {
    return Machine{
        .value = false,
        .term = term,
        .scopes = NIL,
        .plan = NIL,
    };
}

pub fn initialMachineForCode(ctx: *Wisp, code: []const u8) !Machine {
    const term = try read(ctx, code);
    return initialMachine(term);
}

fn expectSelfEvaluating(code: []const u8) !void {
    var ctx = try wisp.testWisp();
    defer ctx.heap.free();

    const m1 = try initialMachineForCode(&ctx, code);
    const m2 = try step(&ctx, m1);

    try std.testing.expectEqual(
        Machine{
            .value = true,
            .term = m1.term,
            .scopes = m1.scopes,
            .plan = m1.plan,
        },
        m2,
    );
}

test "self-evaluating" {
    try expectSelfEvaluating("NIL");
    try expectSelfEvaluating("()");
    try expectSelfEvaluating("123");
    try expectSelfEvaluating("\"foo\"");
}

fn makeScope(ctx: *Wisp, names: []const W, values: []const W) !W {
    assert(names.len == values.len);

    var slots = try ctx.heap.allocator.alloc(W, 2 * names.len);
    defer ctx.heap.allocator.free(slots);

    for (names) |name, i| {
        slots[i * 2] = name;
        slots[i * 2 + 1] = values[i];
    }

    return wisp.makeInstance(ctx, .scope, slots);
}

test "existing variable" {
    var ctx = try wisp.testWisp();
    defer ctx.heap.free();

    const t = try read(&ctx, "FOO");
    var m1 = initialMachine(t);

    var names = [_]W{t};
    var values = [_]W{wisp.fixnum(123)};

    m1.scopes = try ctx.cons(
        try makeScope(&ctx, &names, &values),
        NIL,
    );

    const m2 = try step(&ctx, m1);

    try std.testing.expectEqual(
        Machine{
            .value = true,
            .term = wisp.fixnum(123),
            .scopes = m1.scopes,
            .plan = m1.plan,
        },
        m2,
    );
}

test "nonexisting variable" {
    var ctx = try wisp.testWisp();
    defer ctx.heap.free();

    const t = try read(&ctx, "FOO");
    var m1 = initialMachine(t);

    try std.testing.expectError(
        Error.VariableNotFound,
        step(&ctx, m1),
    );
}

fn expectEval(code: []const u8, expected: []const u8) !void {
    var ctx = try wisp.testWisp();
    defer ctx.heap.free();

    const m1 = try initialMachineForCode(&ctx, code);
    const m2 = try step(&ctx, m1);

    try std.testing.expectEqual(true, m2.value);

    const t = try read(&ctx, expected);

    var s1 = std.ArrayList(u8).init(std.testing.allocator);
    var s2 = std.ArrayList(u8).init(std.testing.allocator);

    defer s1.deinit();
    defer s2.deinit();

    try print(&ctx, s1.writer(), t);
    try print(&ctx, s2.writer(), m2.term);

    try std.testing.expectEqualStrings(
        s1.items,
        s2.items,
    );
}

test "(quote (foo bar))" {
    try expectEval(
        "(quote (foo bar))",
        "(foo bar)",
    );
}

pub fn evalTopLevel(ctx: *Wisp, term: W, max_steps: u32) !W {
    var i: u32 = 0;

    var machine = initialMachine(term);

    while (i < max_steps) : (i += 1) {
        machine = try step(ctx, machine);
        if (machine.value) {
            return machine.term;
        }
    }

    return Error.TooManySteps;
}

// test "(+ 1 2)" {
//     try expectEval("(+ 1 2)", "3");
// }
