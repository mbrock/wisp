// Segmented, composable continuations. Ordinary frames never point
// across a dynamic boundary. Captured frames are frozen in O(1) and
// copied lazily on writes; lexical environments remain shared store.
const std = @import("std");
const Wisp = @import("./wisp.zig");
const Profile = @import("./profile.zig");

const Heap = Wisp.Heap;
const top = Wisp.top;
const nil = Wisp.nil;
const Frame = Wisp.Row(.ktx);

pub const Context = struct {
    way: u32 = top,
    meta: u32 = top,

    pub fn fromRun(run: Wisp.Row(.run)) Context {
        return .{ .way = run.way, .meta = run.meta };
    }

    pub fn install(ctx: Context, run: *Wisp.Row(.run)) void {
        run.way = ctx.way;
        run.meta = ctx.meta;
    }
};

// Meta entries are KTX rows of kind PROMPT, BINDING or RESUME.
// ARG is [handler-or-binding-value, suspended-outer-segment].
// ENV is the lexical environment at the boundary; HOP is the next
// meta entry. RESUME is an invisible composition boundary.
pub fn boundary(heap: *Heap, run: Wisp.Row(.run), kind: u32, key: u32, val: u32) !u32 {
    return heap.new(.ktx, .{
        .hop = run.meta,
        .env = run.env,
        .fun = kind,
        .acc = key,
        .arg = try heap.newv32(&.{ val, run.way }),
    });
}

pub fn outer(heap: *Heap, entry: Frame) !Context {
    return .{ .way = (try heap.v32slice(entry.arg))[1], .meta = entry.hop };
}

pub fn value(heap: *Heap, entry: u32) !u32 {
    return (try heap.v32slice(try heap.get(.ktx, .arg, entry)))[0];
}

// First-class contexts reuse the continuation tag, but have a
// distinct kind from both ordinary frames and meta entries.
pub fn snapshot(heap: *Heap, ctx: Context) !u32 {
    heap.freezeContinuations();
    if (ctx.meta == top) return ctx.way;
    return heap.new(.ktx, .{
        .hop = top,
        .env = nil,
        .fun = heap.kwd.CONTINUATION,
        .acc = ctx.way,
        .arg = ctx.meta,
    });
}

pub fn context(heap: *Heap, ptr: u32) !Context {
    if (ptr == top) return .{};
    const frame = try heap.row(.ktx, ptr);
    if (frame.fun == heap.kwd.CONTINUATION)
        return .{ .way = frame.acc, .meta = frame.arg };
    return .{ .way = ptr };
}

// Only the meta spine is copied. No ordinary frame, argument
// accumulator, or lexical environment is traversed or copied.
fn appendPrefix(heap: *Heap, meta: u32, stop: u32, tail: u32) !u32 {
    var entries: std.ArrayList(u32) = .empty;
    defer entries.deinit(heap.orb);
    var cur = meta;
    while (cur != stop) {
        try entries.append(heap.orb, cur);
        cur = try heap.get(.ktx, .hop, cur);
    }
    var result = tail;
    var i = entries.items.len;
    while (i > 0) {
        i -= 1;
        var entry = try heap.row(.ktx, entries.items[i]);
        entry.hop = result;
        result = try heap.new(.ktx, entry);
    }
    return result;
}

pub fn compose(heap: *Heap, captured: Context, caller: Wisp.Row(.run)) !Context {
    if (captured.way == top and captured.meta == top)
        return Context.fromRun(caller);
    heap.freezeContinuations();
    // A tail resume needs no return boundary. In particular, deep
    // handlers must not accumulate empty RESUME entries per effect.
    const tail = if (caller.way == top)
        caller.meta
    else
        try boundary(heap, caller, heap.kwd.RESUME, nil, nil);
    return .{
        .way = captured.way,
        .meta = try appendPrefix(heap, captured.meta, top, tail),
    };
}

pub const Capture = struct {
    handler: u32,
    outside: Context,
    inside: Context,
};

pub fn capture(heap: *Heap, ctx: Context, tag: u32) !?Capture {
    var cur = ctx.meta;
    var boundaries: u32 = 0;
    while (cur != top) {
        const entry = try heap.row(.ktx, cur);
        boundaries += 1;
        if (entry.fun == heap.kwd.PROMPT and entry.acc == tag) {
            heap.freezeContinuations();
            Profile.recordContinuationSearch(boundaries, true);
            return .{
                .handler = try value(heap, cur),
                .outside = try outer(heap, entry),
                .inside = .{
                    .way = ctx.way,
                    .meta = try appendPrefix(heap, ctx.meta, cur, top),
                },
            };
        }
        cur = entry.hop;
    }
    Profile.recordContinuationSearch(boundaries, false);
    return null;
}

pub fn findBinding(heap: *Heap, meta: u32, name: u32) !?u32 {
    var cur = meta;
    var hops: u32 = 0;
    while (cur != top) {
        const entry = try heap.row(.ktx, cur);
        hops += 1;
        if (entry.fun == heap.kwd.BINDING and entry.acc == name) {
            Profile.recordDynamicLookup(hops, true);
            return cur;
        }
        cur = entry.hop;
    }
    Profile.recordDynamicLookup(hops, false);
    return null;
}

pub fn setBinding(heap: *Heap, meta: u32, binding: u32, val: u32) !u32 {
    var entry = try heap.row(.ktx, binding);
    const segment = (try heap.v32slice(entry.arg))[1];
    entry.arg = try heap.newv32(&.{ val, segment });
    return appendPrefix(heap, meta, binding, try heap.new(.ktx, entry));
}

// The public KTX accessors expose the old flattened view. Walking
// it allocates context wrappers, but execution/capture never does
// this walk. Composition boundaries are invisible in that view.
pub fn view(heap: *Heap, ptr: u32) !Frame {
    var ctx = try context(heap, ptr);
    while (ctx.way == top and ctx.meta != top) {
        const entry = try heap.row(.ktx, ctx.meta);
        const next = try outer(heap, entry);
        if (entry.fun != heap.kwd.RESUME) {
            return .{
                .hop = try snapshot(heap, next),
                .env = entry.env,
                .fun = entry.fun,
                .acc = entry.acc,
                .arg = try value(heap, ctx.meta),
            };
        }
        ctx = next;
    }
    var frame = try heap.row(.ktx, ctx.way);
    frame.hop = try snapshot(heap, .{ .way = frame.hop, .meta = ctx.meta });
    return frame;
}

// Dynamic SET! path-copies meta entries, without changing the
// control destination. Debugger breakpoints ignore binding values.
pub fn same(heap: *Heap, a: Context, b: Context) !bool {
    if (a.way != b.way) return false;
    var x = a.meta;
    var y = b.meta;
    while (x != y) {
        if (x == top or y == top) return false;
        const ex = try heap.row(.ktx, x);
        const ey = try heap.row(.ktx, y);
        if (ex.fun != ey.fun or ex.acc != ey.acc or ex.env != ey.env)
            return false;
        if (ex.fun == heap.kwd.BINDING) {
            if ((try heap.v32slice(ex.arg))[1] != (try heap.v32slice(ey.arg))[1])
                return false;
        } else if (ex.arg != ey.arg) return false;
        x = ex.hop;
        y = ey.hop;
    }
    return true;
}

test "capture and composition share segments independent of frame depth" {
    var heap = try Heap.init(std.testing.allocator, std.testing.io, .e0);
    defer heap.deinit();
    var run = @import("./step.zig").initRun(nil);
    const target = try boundary(&heap, run, heap.kwd.PROMPT, heap.kwd.ERROR, 17);
    run.meta = target;
    for (0..128) |_| {
        run.way = try heap.new(.ktx, .{
            .hop = run.way,
            .env = nil,
            .fun = heap.kwd.IF,
            .acc = nil,
            .arg = nil,
        });
    }
    const outer_segment = run.way;
    run.meta = try boundary(&heap, run, heap.kwd.PROMPT, heap.kwd.PROMPT, 23);
    run.way = top;
    run.meta = try boundary(&heap, run, heap.kwd.BINDING, heap.kwd.ERROR, 31);
    run.way = outer_segment;
    const before = heap.tab(.ktx).list.len;
    const split = (try capture(&heap, Context.fromRun(run), heap.kwd.ERROR)).?;
    try std.testing.expectEqual(before + 2, heap.tab(.ktx).list.len);
    try std.testing.expectEqual(outer_segment, split.inside.way);
    try std.testing.expectEqual(@as(u32, 17), split.handler);
    try std.testing.expectEqual(top, split.outside.way);
    try std.testing.expectEqual(top, split.outside.meta);
    const captured_binding = try heap.row(.ktx, split.inside.meta);
    const captured_prompt = try heap.row(.ktx, captured_binding.hop);
    try std.testing.expectEqual(outer_segment, (try outer(&heap, captured_prompt)).way);

    const composed = try compose(&heap, split.inside, run);
    try std.testing.expectEqual(before + 5, heap.tab(.ktx).list.len);
    try std.testing.expectEqual(outer_segment, composed.way);
    const count = heap.tab(.ktx).list.len;
    try std.testing.expect((try capture(&heap, Context.fromRun(run), nil)) == null);
    try std.testing.expectEqual(count, heap.tab(.ktx).list.len);
}

test "tail resumption does not accumulate composition boundaries" {
    var heap = try Heap.init(std.testing.allocator, std.testing.io, .e0);
    defer heap.deinit();
    var caller = @import("./step.zig").initRun(nil);
    caller.meta = try boundary(&heap, caller, heap.kwd.PROMPT, nil, 19);
    const segment = try heap.new(.ktx, .{
        .hop = top,
        .env = nil,
        .fun = heap.kwd.IF,
        .acc = nil,
        .arg = nil,
    });
    const before = heap.tab(.ktx).list.len;
    var captured: Context = .{ .way = segment };
    for (0..256) |_| {
        const resumed = try compose(&heap, captured, caller);
        try std.testing.expectEqual(caller.meta, resumed.meta);
        captured = (try capture(&heap, resumed, nil)).?.inside;
    }
    try std.testing.expectEqual(segment, captured.way);
    try std.testing.expectEqual(top, captured.meta);
    try std.testing.expectEqual(before, heap.tab(.ktx).list.len);
}

test "debugger destinations ignore binding updates but distinguish new prompts" {
    var heap = try Heap.init(std.testing.allocator, std.testing.io, .e0);
    defer heap.deinit();
    var run = @import("./step.zig").initRun(nil);
    const binding = try boundary(&heap, run, heap.kwd.BINDING, heap.kwd.ERROR, 7);
    run.meta = binding;
    const prompt = try boundary(&heap, run, heap.kwd.PROMPT, heap.kwd.ERROR, 19);
    const original: Context = .{ .meta = prompt };
    const updated: Context = .{ .meta = try setBinding(&heap, prompt, binding, 9) };
    try std.testing.expect(try same(&heap, original, updated));
    const replacement: Context = .{
        .meta = try boundary(&heap, run, heap.kwd.PROMPT, heap.kwd.ERROR, 19),
    };
    try std.testing.expect(!try same(&heap, original, replacement));
}
