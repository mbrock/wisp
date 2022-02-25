const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const dump = @import("./print.zig").dump;
const Primops = @import("./primops.zig");

const Eval = @This();

const Word = wisp.Word;
const Heap = wisp.Heap;
const Immediate = wisp.Immediate;
const Pointer = wisp.Pointer;

const Status = enum { done, work };

const Term = union(Status) {
    done: u32,
    work: u32,
};

heap: *Heap,
term: Term,
scopes: u32,
plan: u32,

pub const Error = error{
    Nope,
    EvaluationLimitExceeded,
};

pub fn doneWithTerm(this: *Eval, x: u32) void {
    this.term = .{ .done = x };
}

pub fn step(this: *Eval) !void {
    switch (this.term) {
        .done => |x| {
            return this.proceed(x);
        },

        .work => |t| {
            switch (Word.from(t)) {
                .immediate => this.doneWithTerm(t),

                .pointer => |x| {
                    switch (x) {
                        .string => this.doneWithTerm(t),
                        .symbol => return this.findVariable(t),
                        .cons => return this.stepCons(t),
                        else => return Error.Nope,
                    }
                },
            }
        },
    }
}

fn findVariable(this: *Eval, word: u32) !void {
    const value = (try this.heap.symbolValue(word)).*;
    if (value == wisp.ZAP) {
        return Error.Nope;
    } else {
        this.doneWithTerm(value);
    }
}

fn stepCons(this: *Eval, p: u32) !void {
    const cons = try this.heap.deref(.cons, p);
    const cdr = try this.heap.deref(.cons, cons.cdr);
    const callee = (try this.heap.symbolFunction(cons.car)).*;

    switch (Word.from(callee)) {
        .immediate => |imm| {
            switch (imm) {
                .primfun => {
                    this.* = .{
                        .heap = this.heap,
                        .scopes = this.scopes,
                        .term = .{ .work = cdr.car },
                        .plan = try this.heap.append(.argsPlan, wisp.ArgsPlan{
                            .next = this.plan,
                            .scopes = this.scopes,
                            .callee = callee,
                            .values = wisp.NIL,
                            .terms = cdr.cdr,
                        }),
                    };
                },

                .primmac => |i| {
                    const result = try callPrimop(
                        this,
                        Primops.primmacs.values[i],
                        false,
                        cons.cdr,
                    );

                    this.* = .{
                        .heap = this.heap,
                        .scopes = this.scopes,
                        .term = .{ .work = result },
                        .plan = this.plan,
                    };
                },

                else => return Error.Nope,
            }
        },
        else => return Error.Nope,
    }
}

pub fn proceed(this: *Eval, x: u32) !void {
    if (this.plan == wisp.NIL) {
        this.term = .{ .done = x };
        return;
    }

    switch (Word.from(this.plan)) {
        .pointer => |pointer| {
            switch (pointer) {
                .argsPlan => |i| {
                    try this.doArgsPlan(try this.heap.get(.argsPlan, i));
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

fn doArgsPlan(this: *Eval, argsPlan: wisp.ArgsPlan) !void {
    const values = try this.heap.append(.cons, .{
        .car = this.term.done,
        .cdr = argsPlan.values,
    });

    if (argsPlan.terms == wisp.NIL) {
        // Done with evaluating subterms.
        switch (Word.from(argsPlan.callee)) {
            .immediate => |imm| {
                switch (imm) {
                    .primfun => |i| {
                        const result = try this.callPrimop(
                            Primops.primfuns.values[i],
                            true,
                            values,
                        );

                        this.* = .{
                            .heap = this.heap,
                            .plan = argsPlan.next,
                            .scopes = argsPlan.scopes,
                            .term = .{ .done = result },
                        };
                    },
                    else => return Error.Nope,
                }
            },
            else => return Error.Nope,
        }
    } else {
        const cons = try this.heap.deref(.cons, argsPlan.terms);
        this.* = .{
            .heap = this.heap,
            .term = .{ .work = cons.car },
            .scopes = this.scopes,
            .plan = try this.heap.append(.argsPlan, wisp.ArgsPlan{
                .next = argsPlan.next,
                .scopes = argsPlan.scopes,
                .callee = argsPlan.callee,
                .terms = cons.cdr,
                .values = values,
            }),
        };
    }
}

pub fn scanList(heap: *Heap, buffer: []u32, reverse: bool, list: u32) ![]u32 {
    var i: usize = 0;
    var cur = list;
    while (cur != wisp.NIL) {
        const cons = try heap.deref(.cons, cur);
        buffer[i] = cons.car;
        cur = cons.cdr;
        i += 1;
    }

    var slice = buffer[0..i];
    if (reverse) {
        std.mem.reverse(u32, slice);
    }
    return slice;
}

fn callPrimop(this: *Eval, primop: Primops.Primop, reverse: bool, values: u32) !u32 {
    switch (primop.info.tag) {
        .f0x => {
            var xs: [31]u32 = undefined;
            const slice = try scanList(this.heap, &xs, reverse, values);
            const f = Primops.FnTag.f0x.cast(primop.func);
            return try f(this.heap, slice);
        },

        .f2 => {
            var xs: [2]u32 = undefined;
            const slice = try scanList(this.heap, &xs, reverse, values);
            const f = Primops.FnTag.f2.cast(primop.func);
            return try f(this.heap, slice[0], slice[1]);
        },
    }
}

pub fn evaluate(this: *Eval, limit: u32) !u32 {
    var i: u32 = 0;
    while (i < limit) : (i += 1) {
        if (this.plan == wisp.NIL) {
            switch (this.term) {
                .done => |x| return x,
                else => {},
            }
        }

        try this.step();
    }

    return Error.EvaluationLimitExceeded;
}

fn newTestHeap() !Heap {
    var heap = try Heap.init(std.testing.allocator);
    try heap.loadPrimops();
    return heap;
}

pub fn init(heap: *Heap, term: u32) Eval {
    return Eval{
        .heap = heap,
        .plan = wisp.NIL,
        .scopes = wisp.NIL,
        .term = Term{ .work = term },
    };
}

test "step evaluates string" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const term = try heap.addString("foo");
    var ctx = init(&heap, term);

    try ctx.step();
    try expectEqual(Term{ .done = term }, ctx.term);
}

test "step evaluates variable" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const x = try heap.internStringInBasePackage("X");
    const foo = try heap.addString("foo");

    var ctx = init(&heap, x);

    (try heap.symbolValue(x)).* = foo;

    try ctx.step();
    try expectEqual(Term{ .done = foo }, ctx.term);
}

test "(+ 1 2 3) => 6" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const term = try read(&heap, "(+ 1 2 3)");
    var ctx = init(&heap, term);
    const value = try ctx.evaluate(10);

    try expectEqual(@as(u32, 6 * 4), wisp.encodeFixnum(1 + 2 + 3));

    try expectEqual(wisp.encodeFixnum(1 + 2 + 3), value);
}

test "(FOO + 1) => 2" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const term = try read(&heap, "(FOO + 1)");
    var ctx = init(&heap, term);
    const value = try ctx.evaluate(10);

    try expectEqual(wisp.encodeFixnum(2), value);
}
