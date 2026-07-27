// -*- fill-column: 64; -*-
//
// Cheap semantic profiling for the evaluator.  The ordinary Wisp
// executables do not declare `wisp_profile`, so all of the recording
// calls in hot paths are compiled away.

const std = @import("std");
const root = @import("root");
const Word = @import("./word.zig");

const Tag = Word.Tag;

pub const enabled: bool = if (@hasDecl(root, "wisp_profile"))
    root.wisp_profile
else
    false;

const histogram_len = 17;
const tag_count = 32;

pub const CallKind = enum {
    jet,
    fun,
    mac,
    continuation,
};

pub const Stats = struct {
    evaluator_steps: u64 = 0,

    jet_calls: u64 = 0,
    function_calls: u64 = 0,
    macro_calls: u64 = 0,
    continuation_calls: u64 = 0,
    continuation_searches: u64 = 0,
    continuation_captures: u64 = 0,
    continuation_frames: u64 = 0,
    call_arity: [histogram_len]u64 = @splat(0),

    arguments_accumulated: u64 = 0,
    lists_scanned: u64 = 0,
    list_cells_scanned: u64 = 0,
    lists_reversed: u64 = 0,
    list_cells_reversed: u64 = 0,

    lexical_lookups: u64 = 0,
    lexical_frames: u64 = 0,
    lexical_comparisons: u64 = 0,
    lexical_global_fallbacks: u64 = 0,
    lexical_depth: [histogram_len]u64 = @splat(0),

    dynamic_lookups: u64 = 0,
    dynamic_hops: u64 = 0,
    dynamic_hits: u64 = 0,

    allocations: [tag_count]u64 = @splat(0),
    gc_copies: [tag_count]u64 = @splat(0),
    v08_bytes: u64 = 0,
    v32_words: u64 = 0,
    gc_v08_bytes: u64 = 0,
    gc_v32_words: u64 = 0,

    gc_count: u64 = 0,
    gc_nanoseconds: u64 = 0,
    gc_bytes_before: u64 = 0,
    gc_bytes_after: u64 = 0,
};

pub var stats: Stats = .{};
var in_gc = false;

pub inline fn reset() void {
    if (enabled) {
        stats = .{};
        in_gc = false;
    }
}

pub inline fn evaluatorStep() void {
    if (enabled) stats.evaluator_steps += 1;
}

pub inline fn recordCall(kind: CallKind) void {
    if (!enabled) return;

    switch (kind) {
        .jet => stats.jet_calls += 1,
        .fun => stats.function_calls += 1,
        .mac => stats.macro_calls += 1,
        .continuation => stats.continuation_calls += 1,
    }
}

fn histogramIndex(n: anytype) usize {
    return @min(@as(usize, @intCast(n)), histogram_len - 1);
}

pub inline fn recordCallArity(arity: usize) void {
    if (enabled) stats.call_arity[histogramIndex(arity)] += 1;
}

pub inline fn recordContinuationSearch(
    frames: u32,
    captured: bool,
) void {
    if (!enabled) return;
    stats.continuation_searches += 1;
    stats.continuation_frames += frames;
    if (captured) stats.continuation_captures += 1;
}

pub inline fn recordArgument() void {
    if (enabled) stats.arguments_accumulated += 1;
}

pub inline fn recordListScan(cells: usize) void {
    if (!enabled) return;
    stats.lists_scanned += 1;
    stats.list_cells_scanned += cells;
}

pub inline fn recordListReverse(cells: usize) void {
    if (!enabled) return;
    stats.lists_reversed += 1;
    stats.list_cells_reversed += cells;
}

pub inline fn recordLexicalLookup(
    frames: u32,
    comparisons: u32,
    global_fallback: bool,
) void {
    if (!enabled) return;
    stats.lexical_lookups += 1;
    stats.lexical_frames += frames;
    stats.lexical_comparisons += comparisons;
    stats.lexical_depth[histogramIndex(frames)] += 1;
    if (global_fallback) stats.lexical_global_fallbacks += 1;
}

pub inline fn recordDynamicLookup(hops: u32, hit: bool) void {
    if (!enabled) return;
    stats.dynamic_lookups += 1;
    stats.dynamic_hops += hops;
    if (hit) stats.dynamic_hits += 1;
}

pub inline fn recordAllocation(comptime tag: Tag) void {
    if (!enabled) return;
    const index = @backingInt(tag);
    if (in_gc)
        stats.gc_copies[index] += 1
    else
        stats.allocations[index] += 1;
}

pub inline fn recordV08Bytes(bytes: usize) void {
    if (!enabled) return;
    if (in_gc)
        stats.gc_v08_bytes += bytes
    else
        stats.v08_bytes += bytes;
}

pub inline fn recordV32Words(words: usize) void {
    if (!enabled) return;
    if (in_gc)
        stats.gc_v32_words += words
    else
        stats.v32_words += words;
}

pub inline fn beginGc(io: std.Io, bytes_before: usize) i96 {
    if (!enabled) return 0;
    in_gc = true;
    stats.gc_count += 1;
    stats.gc_bytes_before += bytes_before;
    return std.Io.Clock.awake.now(io).nanoseconds;
}

pub inline fn finishGc(
    io: std.Io,
    started: i96,
    bytes_after: usize,
) void {
    if (!enabled) return;
    const elapsed =
        std.Io.Clock.awake.now(io).nanoseconds - started;
    stats.gc_nanoseconds += @intCast(elapsed);
    stats.gc_bytes_after += bytes_after;
    in_gc = false;
}

pub inline fn leaveGc() void {
    if (enabled) in_gc = false;
}

fn allocationCount(snapshot: Stats, comptime tag: Tag) u64 {
    return snapshot.allocations[@backingInt(tag)];
}

fn gcCopyCount(snapshot: Stats, comptime tag: Tag) u64 {
    return snapshot.gc_copies[@backingInt(tag)];
}

pub fn writeJson(
    writer: anytype,
    snapshot: Stats,
    name: []const u8,
    input: []const u8,
    iterations: u64,
    elapsed_ns: u64,
    heap_bytes_start: usize,
    heap_bytes_end: usize,
) !void {
    try writer.print(
        "{{\"benchmark\":\"{s}\",\"input\":\"{s}\"," ++
            "\"iterations\":{d}," ++
            "\"elapsed_ns\":{d},\"ns_per_iteration\":{d}," ++
            "\"heap_bytes_start\":{d},\"heap_bytes_end\":{d}," ++
            "\"evaluator_steps\":{d}," ++
            "\"jet_calls\":{d},\"function_calls\":{d}," ++
            "\"macro_calls\":{d},\"continuation_calls\":{d}," ++
            "\"continuation_searches\":{d}," ++
            "\"continuation_captures\":{d}," ++
            "\"continuation_frames\":{d}," ++
            "\"arguments_accumulated\":{d}," ++
            "\"lists_scanned\":{d},\"list_cells_scanned\":{d}," ++
            "\"lists_reversed\":{d},\"list_cells_reversed\":{d}," ++
            "\"lexical_lookups\":{d},\"lexical_frames\":{d}," ++
            "\"lexical_comparisons\":{d}," ++
            "\"lexical_global_fallbacks\":{d},",
        .{
            name,
            input,
            iterations,
            elapsed_ns,
            elapsed_ns / @max(iterations, 1),
            heap_bytes_start,
            heap_bytes_end,
            snapshot.evaluator_steps,
            snapshot.jet_calls,
            snapshot.function_calls,
            snapshot.macro_calls,
            snapshot.continuation_calls,
            snapshot.continuation_searches,
            snapshot.continuation_captures,
            snapshot.continuation_frames,
            snapshot.arguments_accumulated,
            snapshot.lists_scanned,
            snapshot.list_cells_scanned,
            snapshot.lists_reversed,
            snapshot.list_cells_reversed,
            snapshot.lexical_lookups,
            snapshot.lexical_frames,
            snapshot.lexical_comparisons,
            snapshot.lexical_global_fallbacks,
        },
    );

    try writer.print(
        "\"dynamic_lookups\":{d},\"dynamic_hops\":{d}," ++
            "\"dynamic_hits\":{d}," ++
            "\"alloc_duo\":{d},\"alloc_ktx\":{d}," ++
            "\"alloc_v32\":{d},\"alloc_fun\":{d}," ++
            "\"v08_bytes\":{d},\"v32_words\":{d}," ++
            "\"gc_count\":{d},\"gc_ns\":{d}," ++
            "\"gc_bytes_before\":{d},\"gc_bytes_after\":{d}," ++
            "\"gc_copy_duo\":{d},\"gc_copy_ktx\":{d}," ++
            "\"gc_copy_v32\":{d},\"gc_v32_words\":{d}," ++
            "\"call_arity\":[",
        .{
            snapshot.dynamic_lookups,
            snapshot.dynamic_hops,
            snapshot.dynamic_hits,
            allocationCount(snapshot, .duo),
            allocationCount(snapshot, .ktx),
            allocationCount(snapshot, .v32),
            allocationCount(snapshot, .fun),
            snapshot.v08_bytes,
            snapshot.v32_words,
            snapshot.gc_count,
            snapshot.gc_nanoseconds,
            snapshot.gc_bytes_before,
            snapshot.gc_bytes_after,
            gcCopyCount(snapshot, .duo),
            gcCopyCount(snapshot, .ktx),
            gcCopyCount(snapshot, .v32),
            snapshot.gc_v32_words,
        },
    );

    for (snapshot.call_arity, 0..) |count, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.print("{d}", .{count});
    }

    try writer.writeAll("],\"lexical_depth\":[");

    for (snapshot.lexical_depth, 0..) |count, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.print("{d}", .{count});
    }

    try writer.writeAll("]}\n");
}
