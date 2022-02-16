const std = @import("std");
const builtin = @import("builtin");

const base = @import("./base.zig");
const read = @import("./read.zig").read;
const print = @import("./print.zig").print;
const eval = @import("./eval.zig");

const W = base.W;
const Wisp = base.Wisp;
const Result = base.Result;

test {
    std.testing.refAllDecls(@This());
}

fn expectParsingRoundtrip(text: []const u8) !void {
    var ctx = try base.testWisp();
    defer ctx.heap.free();

    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    const x = try read(&ctx, text);

    try print(&ctx, &writer, x);
    try std.testing.expectEqualStrings(text, list.items);
}

test "read roundtrips" {
    try expectParsingRoundtrip("NIL");
    try expectParsingRoundtrip("123");
    try expectParsingRoundtrip("FOO");
    try expectParsingRoundtrip("FÖÖ");
    try expectParsingRoundtrip("\"Hello, world!\"");
}

test "read list roundtrips" {
    try expectParsingRoundtrip("(FOO)");
    try expectParsingRoundtrip("(FOO (1 2 3) BAR)");
    try expectParsingRoundtrip("(1 . 2)");
}

test "read code roundtrips" {
    try expectParsingRoundtrip("(DEFUN FOO (X Y) (+ X Y))");
}

pub fn main() anyerror!void {
    if (builtin.os.tag == .freestanding) {
        return;
    } else {
        return repl();
    }
}

fn allocateWisp(heapSize: i32) !*Wisp {
    var heap = try base.emptyHeap(
        std.heap.page_allocator,
        @intCast(u29, heapSize),
    );

    var wisp = try base.start(heap);

    var place = try wisp.heap.allocator.create(Wisp);
    place.* = wisp;

    return place;
}

pub export fn wispStart(heapSize: i32) ?*Wisp {
    return allocateWisp(heapSize) catch null;
}

pub fn alloc(comptime T: type, ctx: *Wisp, x: anyerror!T) ?*T {
    if (x) |it| {
        if (ctx.heap.allocator.create(T)) |place| {
            place.* = it;
            return place;
        } else |_| {
            return null;
        }
    } else |_| {
        return null;
    }
}

pub export fn wispRead(ctx: *Wisp, string: [*:0]const u8) u32 {
    if (read(ctx, std.mem.span(string))) |w| {
        return w.raw;
    } else |_| {
        return errorValue.raw;
    }
}

pub export fn wispAllocString(ctx: *Wisp, size: u32) ?[*:0]u8 {
    if (ctx.heap.allocator.alloc(u8, size)) |slice| {
        return @ptrCast(?[*:0]u8, slice);
    } else |_| {
        return null;
    }
}

pub export fn wispFreeString(ctx: *Wisp, ptr: [*:0]u8) void {
    ctx.heap.allocator.free(std.mem.span(ptr));
}

pub export fn wispFreeValue(ctx: *Wisp, ptr: *W) void {
    ctx.heap.allocator.destroy(ptr);
}

pub export fn wispToString(ctx: *Wisp, term: u32) ?[*:0]u8 {
    var buffer = std.ArrayList(u8).init(ctx.heap.allocator);
    if (print(ctx, buffer.writer(), W.from(term))) |_| {
        return buffer.toOwnedSliceSentinel(0) catch null;
    } else |_| {
        return null;
    }
}

pub export fn strlen(s: [*:0]u8) u32 {
    return @intCast(u32, std.mem.len(s));
}

const errorValue = W{ .raw = 7 };

pub export fn wispEvalTopLevel(
    ctx: *Wisp,
    term: u32,
    max_steps: u32,
) u32 {
    const w = eval.evalTopLevel(
        ctx,
        W.from(term),
        max_steps,
    ) catch errorValue;

    return w.raw;
}

pub fn repl() anyerror!void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var heap = try base.emptyHeap(std.heap.page_allocator, 1024 * 1024 * 16);
    var ctx = try base.start(heap);

    while (true) {
        try stdout.writeAll("wisp> ");

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        const lineOrEof = try stdin.readUntilDelimiterOrEofAlloc(
            allocator,
            '\n',
            4096,
        );

        if (lineOrEof) |line| {
            const term = try read(&ctx, line);
            const result = try eval.evalTopLevel(&ctx, term, 100);
            try print(&ctx, stdout, result);
            try stdout.writeByte('\n');
        } else {
            try stdout.writeByte('\n');
            return;
        }
    }
}
