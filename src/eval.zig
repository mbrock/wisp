const std = @import("std");
const assert = std.debug.assert;

const wisp = @import("./base.zig");
const read = @import("./read.zig").read;

const W = wisp.W;
const NIL = wisp.NIL;
const Wisp = wisp.Wisp;
const Machine = wisp.Machine;

pub const Error = error{
    UnknownTerm,
    VariableNotFound,
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
    } else {
        return Error.UnknownTerm;
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
