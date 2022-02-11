const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

pub fn main() anyerror!void {
    std.log.info("Wisp Zig", .{});
}

const Lowtag = enum(u3) {
    const mask: u32 = @as(u32, 7);

    fixnum0 = 0,
    funptr = 1,
    immediate0 = 2,
    listptr = 3,
    fixnum1 = 4,
    structptr = 5,
    immediate1 = 6,
    otherptr = 7,
};

const Widetag = enum(u8) {
    instance = 0xC2,
    string = 0x32,
    symbol = 0xAE,
    builtin = 0xA2,
};

const W = packed struct {
    raw: u32,

    pub fn lowtag(self: W) Lowtag {
        return @intToEnum(Lowtag, @bitCast(Pointer, self).lowtag);
    }

    pub fn offset(self: W) u29 {
        return @bitCast(Pointer, self).offset;
    }

    pub fn fixnum(self: W) u30 {
        return @bitCast(Fixnum, self).value;
    }
};

test "W" {
    const w1 = W{ .raw = 3 };
    try expectEqual(Lowtag.listptr, w1.lowtag());
    try expectEqual(@as(u29, 0), w1.offset());
    try expectEqual(@as(u30, 123), (W{ .raw = 123 << 2 }).fixnum());
}

const Pointer = packed struct {
    lowtag: u3,
    offset: u29,
};

const Fixnum = packed struct {
    zero: u2,
    value: u30,
};

const Immediate = packed struct {
    value: u24,
    widetag: u8,
};

const Machine = struct {
    value: bool,
    term: W,
    scopes: W,
    plan: W,
};

const NIL = W{ .raw = 3 };

test "NIL" {
    try expectEqual(NIL.raw, 3);
}

var machine = Machine{
    .value = false,
    .term = NIL,
    .scopes = NIL,
    .plan = NIL,
};

const PlanData = packed union(enum) {
    APPLY: struct {
        terms: W,
        values: W,
        function: W,
    },

    FUNCALL: struct {
        terms: W,
    },

    PROGN: struct {
        terms: W,
    },

    IF: struct {
        true_case: W,
        false_case: W,
    },

    AWAIT: struct {},
};

const Plan = packed struct {
    next: W,
    scopes: W,
    data: PlanData,
};

const Closure = packed struct {
    params: W,
    body: W,
    scopes: W,
    macro: W,
};

const Builtin = struct {
    tag: u24,
    argumentCount: u32,
    varargs: bool,
    evalArgs: bool,
    evalResult: bool,
    params: W,
    function: *anyopaque,
};

const SymbolCacheTag = enum {
    APPLY,
    CLOSURE,
    EVAL,
    LAMBDA,
    PARAMS,
    QUOTE,
    SCOPE,
    @"SET-SYMBOL-FUNCTION",
    T,
    WISP,
    PACKAGE,
    PROGN,
    IF,
    FUNCALL,
    PROMISE,
    AWAIT,
};

const BuiltinTag = enum(u24) {
    QUOTE,
    EVAL,
    LAMBDA,
    MACRO,
    CONS,
    CAR,
    CDR,
    @"MAKE-INSTANCE",
    @"SET-SYMBOL-FUNCTION",
    @"SAVE-HEAP",
    @"+",
    @"-",
    @"*",
    GC,
    PRINT,
    @"GET/CC",
    FETCH,
};

const symbolCacheSize = @typeInfo(SymbolCacheTag).Enum.fields.len;
const builtinCount = @typeInfo(BuiltinTag).Enum.fields.len;

fn FunctionTypeN(
    comptime n: comptime_int,
    comptime varargs: bool,
) type {
    if (varargs) {
        return switch (n) {
            0 => fn ([]W) W,
            1 => fn (W, []W) W,
            2 => fn (W, W, []W) W,
            3 => fn (W, W, W, []W) W,
            else => @compileError("fix me"),
        };
    } else {
        return switch (n) {
            0 => fn () W,
            1 => fn (W) W,
            2 => fn (W, W) W,
            3 => fn (W, W, W) W,
            else => @compileError("fix me"),
        };
    }
}

const SliceSize = std.builtin.TypeInfo.Pointer.Size.Slice;

fn isVarargsFunction(comptime t: std.builtin.TypeInfo) bool {
    if (t.args.len == 0) {
        return false;
    } else {
        const last = t.args[t.args.len - 1];
        return switch (@typeInfo(last)) {
            .Pointer => |ptr| ptr.child == W and ptr.size == SliceSize,
        };
    }
}

fn FunctionType(comptime f: anytype) type {
    return switch (@typeInfo(f)) {
        .Fn => |info| if (info.args.len == 0)
            FunctionTypeN(0, false)
        else if (isVarargsFunction(@typeInfo(f)))
            FunctionTypeN(info.args.len - 1, true)
        else
            FunctionTypeN(info.args.len, false),
        else => @compileError("not a function"),
    };
}

const Heap = struct {
    const staticSpaceSize: u32 = 40;

    size: u29,
    used: u29,
    data: []u8,
    area: struct {
        old: u29,
        new: u29,
    },

    allocator: std.mem.Allocator,

    pub fn free(self: Heap) void {
        self.allocator.free(self.data);
    }

    pub fn deref(self: Heap, ptr: W) [*]W {
        assert(isPointer(ptr.lowtag()));
        return @ptrCast([*]W, self.data[ptr.offset()..]);
    }

    fn allocate(heap: *Heap, size: u29, tag: Lowtag) !W {
        assert(isPointer(tag));

        const alignedSize = alignToDoubleWord(size);

        if (heap.used + size >= heap.size / 2) {
            return error.HeapFull;
        } else {
            const i = heap.used;
            heap.used += @intCast(u29, alignedSize);

            return makePointer(heap.area.new, i, tag);
        }
    }
};

fn alignToDoubleWord(x: u32) u32 {
    return (x + Lowtag.mask) & ~Lowtag.mask;
}

test "align to double word" {
    try expectEqual(@as(u32, 0), alignToDoubleWord(@as(u32, 0)));
    try expectEqual(@as(u32, 8), alignToDoubleWord(@as(u32, 1)));
    try expectEqual(@as(u32, 8), alignToDoubleWord(@as(u32, 2)));
    try expectEqual(@as(u32, 16), alignToDoubleWord(@as(u32, 9)));
}

fn fixnum(x: u32) W {
    return x << 2;
}

fn isAligned(x: u32) bool {
    return x & 7 == 0;
}

fn isPointer(x: anytype) bool {
    return switch (@TypeOf(x)) {
        W => x.raw & 1 == 1,
        u32 => x & 1 == 1,
        Lowtag => @enumToInt(x) & 1 == 1,
        else => @compileError("unknown type"),
    };
}

fn emptyHeap(allocator: std.mem.Allocator, size: u29) !Heap {
    return Heap{
        .allocator = allocator,
        .size = size,
        .used = Heap.staticSpaceSize,
        .data = try allocator.alloc(u8, size),
        .area = .{
            .old = size / 2,
            .new = 0,
        },
    };
}

fn makeHeader(data: u24, widetag: Widetag) W {
    return @bitCast(W, Immediate{
        .value = data,
        .widetag = @enumToInt(widetag),
    });
}

fn makePointer(area: u29, offset: u29, lowtag: Lowtag) W {
    return @bitCast(W, Pointer{
        .offset = area + offset,
        .lowtag = @enumToInt(lowtag),
    });
}

test "allocate" {
    var heap = try emptyHeap(std.testing.allocator, 1024);
    defer heap.free();

    const foo = try heap.allocate(16, Lowtag.listptr);

    try expectEqual(Heap.staticSpaceSize, foo.offset());
    try expectEqual(Lowtag.listptr, foo.lowtag());
}

const Wisp = struct {
    heap: *Heap,
    builtins: [builtinCount]?Builtin,
    symbolCache: [symbolCacheSize]W,

    pub fn symbol(self: Wisp, tag: SymbolCacheTag) W {
        return self.symbolCache[tag];
    }
};

const symbolSlots = 6;

const Headers = struct {
    const symbol = makeHeader(symbolSlots, .symbol);
};

fn start(heap: *Heap) !Wisp {
    var nil: [*]W = heap.deref(NIL);
    nil[0] = Headers.symbol;
    nil[1] = NIL;
    nil[2] = NIL;
    nil[3] = NIL;
    nil[4] = try makeString(heap, "NIL");
    nil[5] = NIL;
    nil[6] = NIL;

    return Wisp{
        .heap = heap,
        .builtins = [_]?Builtin{null} ** builtinCount,
        .symbolCache = [_]W{W{ .raw = 3 }} ** symbolCacheSize,
    };
}

test "start" {
    var heap = try emptyHeap(std.testing.allocator, 1024);
    defer heap.free();

    _ = try start(&heap);
}

fn stringBuffer(data: [*]W) [*]u8 {
    return @ptrCast([*]u8, data + 1);
}

fn makeString(heap: *Heap, text: []const u8) !W {
    const stringPtr = try heap.allocate(
        4 + 1 + @intCast(u29, text.len),
        .otherptr,
    );

    const data = heap.deref(stringPtr);
    const buffer = stringBuffer(data);

    data[0] = makeHeader(
        @intCast(u24, text.len),
        Widetag.string,
    );

    for (text) |x, i| {
        buffer[i] = x;
    }

    buffer[text.len] = 0;

    return stringPtr;
}
