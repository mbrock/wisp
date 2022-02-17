const std = @import("std");
const assert = std.debug.assert;

const wisp = @import("./base.zig");
const printAlloc = @import("./print.zig").printAlloc;

const Wisp = wisp.Wisp;
const W = wisp.W;

const Error = error{
    UnexpectedWidetag,
};

test "gc" {
    var heap = try wisp.emptyHeap(std.testing.allocator, 4096);
    defer heap.free();

    var ctx = try wisp.start(heap);

    try std.testing.expectEqual(
        @as(u29, 0),
        ctx.heap.area.new,
    );

    const usedBefore = ctx.heap.used;
    try tidy(&ctx);
    const usedNow = ctx.heap.used;

    try std.testing.expectEqual(
        @as(u29, 2048),
        ctx.heap.area.new,
    );

    // Some garbage is created in the startup process.
    try std.testing.expect(usedNow < usedBefore);
}

test "gc several times" {
    var heap = try wisp.emptyHeap(std.testing.allocator, 4096);
    defer heap.free();

    var ctx = try wisp.start(heap);

    try tidy(&ctx);
    try tidy(&ctx);
    try tidy(&ctx);
}

test "intern symbol after gc" {
    var heap = try wisp.emptyHeap(std.testing.allocator, 4096);
    defer heap.free();

    var ctx = try wisp.start(heap);

    try tidy(&ctx);

    var symbol = try ctx.internString("FOOBAR", ctx.basePackage);

    var s = try printAlloc(ctx.heap.allocator, &ctx, symbol);
    defer ctx.heap.allocator.free(s);
    try std.testing.expectEqualStrings("FOOBAR", s);
}

pub fn flip(ctx: *Wisp) void {
    const heap: wisp.Heap = ctx.heap;

    ctx.heap.area.new = heap.area.old;
    ctx.heap.area.old = heap.area.new;

    ctx.heap.used = wisp.staticSpaceSize;
    ctx.heap.scan = wisp.staticSpaceSize;
}

pub fn tidy(ctx: *Wisp) !void {
    flip(ctx);

    ctx.basePackage = try copy(ctx, ctx.basePackage);

    for (ctx.symbolCache) |symbol, i| {
        ctx.symbolCache[i] = try copy(ctx, symbol);
    }

    // XXX: We will need to copy the words in the builtins.

    while (ctx.heap.scan < ctx.heap.used) {
        try scavenge(ctx);
    }

    std.mem.set(u8, ctx.heap.oldAreaWithoutStaticSpace(), 0);
}

pub fn copy(ctx: *Wisp, ptr: W) !W {
    if (!ptr.isPointer() or ptr.offset() < wisp.staticSpaceSize) {
        return ptr;
    }

    var data = try ctx.heap.deref(ptr);

    if (alreadyCopied(ctx, data)) {
        assert(data[0].lowtag() == ptr.lowtag());
        return data[0];
    }

    const n = objectSize(data);

    const newptr = wisp.makePointer(
        ctx.heap.area.new,
        ctx.heap.used,
        ptr.lowtag(),
    );

    var newdata = try ctx.heap.deref(newptr);

    // Copy all the words into the new heap area.
    const words = n / 4;
    std.mem.copy(W, newdata[0..words], data[0..words]);

    // Replace the old header with the forwarding pointer.
    data[0] = newptr;

    // Now we've used some more of the new heap area.
    ctx.heap.used += n;

    return newptr;
}

fn scavenge(ctx: *Wisp) !void {
    var ptr = wisp.makePointer(
        ctx.heap.area.new,
        ctx.heap.scan,
        .otherptr,
    );

    var data = try ctx.heap.deref(ptr);

    const header = data[0];

    if (header.raw == 0) {
        ctx.heap.scan += 8;
        return;
    }

    if (header.isImmediate()) {
        switch (header.widetag()) {
            .instance, .symbol => {
                const n = 1 + header.immediate();
                try scavengeWords(ctx, data[0..n]);
            },

            .string => {
                ctx.heap.scan += stringSize(data);
            },

            else => {
                return Error.UnexpectedWidetag;
            },
        }
    } else {
        try scavengeWords(ctx, data[0..2]);
    }
}

fn scavengeWords(ctx: *Wisp, data: []W) !void {
    for (data[0..data.len]) |word, i| {
        data[i] = try copy(ctx, word);
    }

    ctx.heap.scan += wisp.alignToDoubleWord(
        @intCast(u29, 4 * data.len),
    );
}

fn objectSize(data: [*]W) u29 {
    if (data[0].isImmediate()) {
        return switch (data[0].widetag()) {
            .instance, .symbol => instanceSize(data),
            .string => stringSize(data),
            else => 8,
        };
    } else {
        return 8;
    }
}

fn instanceSize(data: [*]W) u29 {
    return wisp.alignToDoubleWord(4 * (1 + data[0].immediate()));
}

fn stringSize(data: [*]W) u29 {
    return wisp.alignToDoubleWord(4 + data[0].immediate() + 1);
}

fn alreadyCopied(ctx: *Wisp, data: [*]W) bool {
    const header = data[0];

    if (!header.isPointer())
        return false;

    if (header.offset() < ctx.heap.area.new)
        return false;

    if (header.offset() > ctx.heap.area.new + ctx.heap.size / 2)
        return false;

    return true;
}
