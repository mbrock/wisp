const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

const wisp = @import("./wisp.zig");

const Eval = @This();

const Word = wisp.Word;
const Heap = wisp.Heap;
const Pointer = wisp.Pointer;

const Status = enum { done, work };

const Term = union(Status) {
    done: Word,
    work: Word,
};

heap: *Heap,
plan: Word,
scopes: Word,
term: Term,

pub const Error = error{
    Nope,
};

pub fn doneWithTerm(this: *Eval, x: Word) void {
    this.term = .{ .done = x };
}

pub fn step(this: *Eval) !void {
    switch (this.term) {
        .done => |x| {
            return this.proceed(x);
        },

        .work => |t| {
            switch (t) {
                .immediate => this.doneWithTerm(t),

                .pointer => |x| {
                    switch (x) {
                        .string => this.doneWithTerm(t),
                        else => return Error.Nope,
                    }
                },
            }
        },
    }
}

pub fn proceed(this: *Eval, word: Word) !void {
    std.log.warn("proceeding with plan {any} {any}", .{ this, word });
}

fn newTestHeap() !Heap {
    return try Heap.init(std.testing.allocator);
}

fn newTestEval(heap: *Heap, term: Word) Eval {
    return Eval{
        .heap = heap,
        .plan = Word.nil,
        .scopes = Word.nil,
        .term = Term{ .work = term },
    };
}

test "eval string" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const term = Word.from(try heap.addString("foo"));
    var ctx = newTestEval(&heap, term);

    try ctx.step();
    try expectEqual(Term{ .done = term }, ctx.term);
}
