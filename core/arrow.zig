// -*- fill-column: 64; -*-
//
// Export a Wisp heap through the Arrow C Data Interface.
//
// Zig describes the Vat with borrowed ArrowSchema and ArrowArray
// trees. Nanoarrow consumes their standard C ABI through an
// ArrowArrayStream and writes the Arrow IPC file. Heap buffers
// remain owned by Wisp and are never copied into Arrow arrays.

const std = @import("std");

const Wisp = @import("./wisp.zig");

const ErrorCode = c_int;
const success: ErrorCode = 0;
const io_error: ErrorCode = 5;

const ReleaseSchema = *const fn (
    schema: *ArrowSchema,
) callconv(.c) void;

const ReleaseArray = *const fn (
    array: *ArrowArray,
) callconv(.c) void;

const ArrowSchema = extern struct {
    format: ?[*:0]const u8,
    name: ?[*:0]const u8,
    metadata: ?[*]const u8,
    flags: i64,
    n_children: i64,
    children: ?[*]*ArrowSchema,
    dictionary: ?*ArrowSchema,
    release: ?ReleaseSchema,
    private_data: ?*anyopaque,
};

const ArrowArray = extern struct {
    length: i64,
    null_count: i64,
    offset: i64,
    n_buffers: i64,
    n_children: i64,
    buffers: ?[*]const ?*const anyopaque,
    children: ?[*]*ArrowArray,
    dictionary: ?*ArrowArray,
    release: ?ReleaseArray,
    private_data: ?*anyopaque,
};

const ArrowArrayStream = extern struct {
    get_schema: ?*const fn (
        stream: *ArrowArrayStream,
        out: *ArrowSchema,
    ) callconv(.c) ErrorCode,

    get_next: ?*const fn (
        stream: *ArrowArrayStream,
        out: *ArrowArray,
    ) callconv(.c) ErrorCode,

    get_last_error: ?*const fn (
        stream: *ArrowArrayStream,
    ) callconv(.c) ?[*:0]const u8,

    release: ?*const fn (
        stream: *ArrowArrayStream,
    ) callconv(.c) void,

    private_data: ?*anyopaque,
};

const ArrowError = extern struct {
    message: [1024]u8,
};

const ArrowIpcOutputStream = extern struct {
    write: ?*const fn (
        stream: *ArrowIpcOutputStream,
        bytes: ?*const anyopaque,
        byte_count: i64,
        written: *i64,
        arrow_error: *ArrowError,
    ) callconv(.c) ErrorCode,

    release: ?*const fn (
        stream: *ArrowIpcOutputStream,
    ) callconv(.c) void,

    private_data: ?*anyopaque,
};

const ArrowIpcWriter = extern struct {
    private_data: ?*anyopaque,
};

extern fn ArrowIpcWriterInit(
    writer: *ArrowIpcWriter,
    output: *ArrowIpcOutputStream,
) ErrorCode;

extern fn ArrowIpcWriterReset(
    writer: *ArrowIpcWriter,
) void;

extern fn ArrowIpcWriterStartFile(
    writer: *ArrowIpcWriter,
    arrow_error: *ArrowError,
) ErrorCode;

extern fn ArrowIpcWriterWriteArrayStream(
    writer: *ArrowIpcWriter,
    stream: *ArrowArrayStream,
    arrow_error: *ArrowError,
) ErrorCode;

extern fn ArrowIpcWriterFinalizeFile(
    writer: *ArrowIpcWriter,
    arrow_error: *ArrowError,
) ErrorCode;

const Pair = struct {
    schema: *ArrowSchema,
    array: *ArrowArray,
};

fn releaseSchema(schema: *ArrowSchema) callconv(.c) void {
    schema.release = null;
}

fn releaseArray(array: *ArrowArray) callconv(.c) void {
    array.release = null;
}

fn pointer(items: anytype) ?*const anyopaque {
    return if (items.len == 0)
        null
    else
        @ptrCast(items.ptr);
}

fn node(
    arena: std.mem.Allocator,
    name: [*:0]const u8,
    format: [*:0]const u8,
    length: usize,
    source_buffers: []const ?*const anyopaque,
    source_children: []const Pair,
) !Pair {
    const buffers = try arena.alloc(
        ?*const anyopaque,
        source_buffers.len,
    );
    @memcpy(buffers, source_buffers);

    const schemas = try arena.alloc(
        *ArrowSchema,
        source_children.len,
    );
    const arrays = try arena.alloc(
        *ArrowArray,
        source_children.len,
    );
    for (source_children, 0..) |child, index| {
        schemas[index] = child.schema;
        arrays[index] = child.array;
    }

    const schema = try arena.create(ArrowSchema);
    schema.* = .{
        .format = format,
        .name = name,
        .metadata = null,
        .flags = 0,
        .n_children = @intCast(source_children.len),
        .children = if (schemas.len == 0)
            null
        else
            schemas.ptr,
        .dictionary = null,
        .release = releaseSchema,
        .private_data = null,
    };

    const array = try arena.create(ArrowArray);
    array.* = .{
        .length = @intCast(length),
        .null_count = 0,
        .offset = 0,
        .n_buffers = @intCast(source_buffers.len),
        .n_children = @intCast(source_children.len),
        .buffers = if (buffers.len == 0)
            null
        else
            buffers.ptr,
        .children = if (arrays.len == 0)
            null
        else
            arrays.ptr,
        .dictionary = null,
        .release = releaseArray,
        .private_data = null,
    };

    return .{
        .schema = schema,
        .array = array,
    };
}

fn primitive(
    arena: std.mem.Allocator,
    name: [*:0]const u8,
    format: [*:0]const u8,
    items: anytype,
) !Pair {
    return node(
        arena,
        name,
        format,
        items.len,
        &.{ null, pointer(items) },
        &.{},
    );
}

fn scalar(
    arena: std.mem.Allocator,
    name: [*:0]const u8,
    format: [*:0]const u8,
    value: anytype,
) !Pair {
    const values = try arena.alloc(@TypeOf(value), 1);
    values[0] = value;
    return primitive(arena, name, format, values);
}

fn offsets(
    arena: std.mem.Allocator,
    length: usize,
) ![]i32 {
    const end = std.math.cast(i32, length) orelse
        return error.ArrowOffsetOverflow;
    const values = try arena.alloc(i32, 2);
    values[0] = 0;
    values[1] = end;
    return values;
}

fn binary(
    arena: std.mem.Allocator,
    name: [*:0]const u8,
    bytes: []const u8,
) !Pair {
    const value_offsets = try offsets(arena, bytes.len);
    return node(
        arena,
        name,
        "z",
        1,
        &.{
            null,
            pointer(value_offsets),
            pointer(bytes),
        },
        &.{},
    );
}

fn list(
    arena: std.mem.Allocator,
    name: [*:0]const u8,
    length: usize,
    item: Pair,
) !Pair {
    const value_offsets = try offsets(arena, length);
    return node(
        arena,
        name,
        "+l",
        1,
        &.{ null, pointer(value_offsets) },
        &.{item},
    );
}

fn structure(
    arena: std.mem.Allocator,
    name: [*:0]const u8,
    length: usize,
    children: []const Pair,
) !Pair {
    return node(
        arena,
        name,
        "+s",
        length,
        &.{null},
        children,
    );
}

fn words(
    arena: std.mem.Allocator,
    heap: *Wisp.Heap,
) !Pair {
    const item = try primitive(
        arena,
        "item",
        "I",
        heap.v32.list.items,
    );
    return list(
        arena,
        "words",
        heap.v32.list.items.len,
        item,
    );
}

fn vat(
    arena: std.mem.Allocator,
    heap: *Wisp.Heap,
    comptime tag: Wisp.Tag,
) !Pair {
    const table = heap.tab(tag);
    const names = comptime std.meta.fieldNames(Wisp.Row(tag));
    var columns: [names.len]Pair = undefined;

    inline for (names, 0..) |name, index| {
        columns[index] = try primitive(
            arena,
            name,
            "I",
            table.col(
                @as(
                    Wisp.Col(tag),
                    @fromBackingInt(@intCast(index)),
                ),
            ),
        );
    }

    const item = try structure(
        arena,
        "item",
        table.list.len,
        &columns,
    );
    return list(
        arena,
        @tagName(tag),
        table.list.len,
        item,
    );
}

fn pins(
    arena: std.mem.Allocator,
    heap: *Wisp.Heap,
) !Pair {
    const ids = heap.pins.entries.items(.key);
    const values = heap.pins.entries.items(.value);
    comptime std.debug.assert(
        @sizeOf(@TypeOf(ids[0])) == @sizeOf(u32),
    );

    const columns = [_]Pair{
        try primitive(arena, "id", "I", ids),
        try primitive(arena, "value", "I", values),
    };
    const item = try structure(
        arena,
        "item",
        ids.len,
        &columns,
    );
    return list(arena, "pins", ids.len, item);
}

const Metadata = struct {
    key: []const u8,
    value: []const u8,
};

fn writeI32(
    bytes: []u8,
    cursor: *usize,
    value: usize,
) !void {
    const integer = std.math.cast(i32, value) orelse
        return error.ArrowMetadataOverflow;
    std.mem.writeInt(
        i32,
        bytes[cursor.*..][0..4],
        integer,
        .little,
    );
    cursor.* += 4;
}

fn metadata(
    arena: std.mem.Allocator,
    entries: []const Metadata,
) ![]u8 {
    var byte_count: usize = 4;
    for (entries) |entry| {
        byte_count += 8;
        byte_count += entry.key.len;
        byte_count += entry.value.len;
    }

    const bytes = try arena.alloc(u8, byte_count);
    var cursor: usize = 0;
    try writeI32(bytes, &cursor, entries.len);
    for (entries) |entry| {
        try writeI32(bytes, &cursor, entry.key.len);
        @memcpy(
            bytes[cursor..][0..entry.key.len],
            entry.key,
        );
        cursor += entry.key.len;

        try writeI32(bytes, &cursor, entry.value.len);
        @memcpy(
            bytes[cursor..][0..entry.value.len],
            entry.value,
        );
        cursor += entry.value.len;
    }
    return bytes;
}

fn heapPair(
    arena: std.mem.Allocator,
    heap: *Wisp.Heap,
) !Pair {
    var fields: [
        10 + Wisp.pointerTags.len + 1
    ]Pair = undefined;
    var count: usize = 0;

    fields[count] = try scalar(
        arena,
        "era",
        "C",
        @as(u8, @backingInt(heap.era)),
    );
    count += 1;
    fields[count] = try scalar(
        arena,
        "current_package",
        "I",
        heap.pkg,
    );
    count += 1;
    fields[count] = try scalar(
        arena,
        "base",
        "I",
        heap.base,
    );
    count += 1;
    fields[count] = try scalar(
        arena,
        "keyword_package",
        "I",
        heap.keywordPackage,
    );
    count += 1;
    fields[count] = try scalar(
        arena,
        "key_package",
        "I",
        heap.keyPackage,
    );
    count += 1;
    fields[count] = try scalar(
        arena,
        "nil_string",
        "I",
        heap.commonStrings.NIL,
    );
    count += 1;
    fields[count] = try scalar(
        arena,
        "t_string",
        "I",
        heap.commonStrings.T,
    );
    count += 1;
    fields[count] = try scalar(
        arena,
        "next_pin_id",
        "I",
        @as(u32, heap.nextPinId),
    );
    count += 1;

    fields[count] = try binary(
        arena,
        "bytes",
        heap.v08.items,
    );
    count += 1;
    fields[count] = try words(arena, heap);
    count += 1;

    inline for (Wisp.pointerTags) |tag| {
        fields[count] = try vat(arena, heap, tag);
        count += 1;
    }

    fields[count] = try pins(arena, heap);
    count += 1;

    const root = try structure(
        arena,
        "heap",
        1,
        fields[0..count],
    );
    const root_metadata = try metadata(arena, &.{
        .{ .key = "wisp:format", .value = "heap" },
        .{ .key = "wisp:version", .value = "1" },
    });
    root.schema.metadata = root_metadata.ptr;
    return root;
}

const StreamState = struct {
    schema: *const ArrowSchema,
    array: *const ArrowArray,
    emitted: bool = false,
};

fn state(
    stream: *ArrowArrayStream,
) *StreamState {
    return @ptrCast(@alignCast(stream.private_data.?));
}

fn getSchema(
    stream: *ArrowArrayStream,
    out: *ArrowSchema,
) callconv(.c) ErrorCode {
    out.* = state(stream).schema.*;
    return success;
}

fn getNext(
    stream: *ArrowArrayStream,
    out: *ArrowArray,
) callconv(.c) ErrorCode {
    const value = state(stream);
    if (value.emitted) {
        out.* = std.mem.zeroes(ArrowArray);
    } else {
        out.* = value.array.*;
        value.emitted = true;
    }
    return success;
}

fn getLastError(
    _: *ArrowArrayStream,
) callconv(.c) ?[*:0]const u8 {
    return null;
}

fn releaseStream(
    stream: *ArrowArrayStream,
) callconv(.c) void {
    stream.release = null;
}

fn streamFor(value: *StreamState) ArrowArrayStream {
    return .{
        .get_schema = getSchema,
        .get_next = getNext,
        .get_last_error = getLastError,
        .release = releaseStream,
        .private_data = value,
    };
}

const Output = struct {
    writer: *std.Io.Writer,
    failure: ?anyerror = null,
};

fn setError(
    arrow_error: *ArrowError,
    message: []const u8,
) void {
    const length = @min(
        message.len,
        arrow_error.message.len - 1,
    );
    @memcpy(
        arrow_error.message[0..length],
        message[0..length],
    );
    arrow_error.message[length] = 0;
}

fn writeOutput(
    ipc_stream: *ArrowIpcOutputStream,
    source: ?*const anyopaque,
    byte_count: i64,
    written: *i64,
    arrow_error: *ArrowError,
) callconv(.c) ErrorCode {
    written.* = 0;
    if (byte_count < 0) {
        setError(arrow_error, "negative write length");
        return io_error;
    }

    const output: *Output = @ptrCast(
        @alignCast(ipc_stream.private_data.?),
    );
    const length: usize = @intCast(byte_count);
    if (length > 0) {
        const address = source orelse {
            setError(arrow_error, "null write buffer");
            return io_error;
        };
        const bytes: [*]const u8 = @ptrCast(address);
        output.writer.writeAll(bytes[0..length]) catch |failure| {
            output.failure = failure;
            setError(arrow_error, "Wisp output failed");
            return io_error;
        };
    }

    written.* = byte_count;
    return success;
}

fn releaseOutput(
    ipc_stream: *ArrowIpcOutputStream,
) callconv(.c) void {
    ipc_stream.release = null;
}

fn outputFor(output: *Output) ArrowIpcOutputStream {
    return .{
        .write = writeOutput,
        .release = releaseOutput,
        .private_data = output,
    };
}

fn check(
    status: ErrorCode,
    arrow_error: *ArrowError,
    output: *Output,
) !void {
    if (output.failure) |failure| return failure;
    if (status == success) return;

    const message = std.mem.sliceTo(
        arrow_error.message[0..],
        0,
    );
    if (message.len > 0) {
        std.log.err("nanoarrow: {s}", .{message});
    } else {
        std.log.err(
            "nanoarrow failed with status {d}",
            .{status},
        );
    }
    return error.NanoarrowFailure;
}

pub fn save(heap: *Wisp.Heap, path: []const u8) !void {
    var arena_state = std.heap.ArenaAllocator.init(heap.orb);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const pair = try heapPair(arena, heap);
    var stream_state = StreamState{
        .schema = pair.schema,
        .array = pair.array,
    };
    var array_stream = streamFor(&stream_state);

    const root = try @import("./file.zig").cwd(arena);
    const file = try root.createFile(heap.cap, path, .{});
    defer file.close(heap.cap);

    var write_buffer: [4096]u8 = undefined;
    var file_writer = file.writer(heap.cap, &write_buffer);
    var output = Output{
        .writer = &file_writer.interface,
    };
    var ipc_output = outputFor(&output);
    var ipc_writer = ArrowIpcWriter{
        .private_data = null,
    };
    var arrow_error = std.mem.zeroes(ArrowError);

    try check(
        ArrowIpcWriterInit(
            &ipc_writer,
            &ipc_output,
        ),
        &arrow_error,
        &output,
    );
    defer ArrowIpcWriterReset(&ipc_writer);

    try check(
        ArrowIpcWriterStartFile(
            &ipc_writer,
            &arrow_error,
        ),
        &arrow_error,
        &output,
    );
    try check(
        ArrowIpcWriterWriteArrayStream(
            &ipc_writer,
            &array_stream,
            &arrow_error,
        ),
        &arrow_error,
        &output,
    );
    try check(
        ArrowIpcWriterFinalizeFile(
            &ipc_writer,
            &arrow_error,
        ),
        &arrow_error,
        &output,
    );
    try file_writer.interface.flush();
}

test "Arrow metadata describes a Wisp heap" {
    var arena_state = std.heap.ArenaAllocator.init(
        std.testing.allocator,
    );
    defer arena_state.deinit();

    const bytes = try metadata(
        arena_state.allocator(),
        &.{
            .{ .key = "wisp:format", .value = "heap" },
            .{ .key = "wisp:version", .value = "1" },
        },
    );
    try std.testing.expectEqual(
        @as(i32, 2),
        std.mem.readInt(i32, bytes[0..4], .little),
    );
    try std.testing.expect(
        std.mem.indexOf(u8, bytes, "wisp:format") != null,
    );
    try std.testing.expect(
        std.mem.indexOf(u8, bytes, "wisp:version") != null,
    );
}
