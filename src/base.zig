const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

pub const Lowtag = enum(u3) {
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

pub const Widetag = enum(u8) {
    instance = 0xC2,
    string = 0x32,
    symbol = 0xAE,
    builtin = 0xA2,
};

pub const W = packed struct {
    raw: u32,

    pub fn isNil(self: W) bool {
        return self.raw == 3;
    }

    pub fn lowtag(self: W) Lowtag {
        return @intToEnum(Lowtag, self.raw & 7);
    }

    pub fn widetag(self: W) Widetag {
        return @intToEnum(Widetag, self.raw & 0xff);
    }

    pub fn offset(self: W) u29 {
        return @intCast(u29, self.raw & ~Lowtag.mask);
    }

    pub fn fixnum(self: W) u30 {
        return @intCast(u30, self.raw / 4);
    }

    pub fn immediate(self: W) u24 {
        return @intCast(u24, std.math.shr(u32, self.raw, 8));
    }

    pub fn isFixnum(self: W) bool {
        return self.raw & 3 == 0;
    }

    pub fn isListPointer(self: W) bool {
        return self.lowtag() == .listptr;
    }

    pub fn isOtherPointer(self: W) bool {
        return self.lowtag() == .otherptr;
    }
};

test "W" {
    const w1 = W{ .raw = 3 };
    try expectEqual(Lowtag.listptr, w1.lowtag());
    try expectEqual(@as(u29, 0), w1.offset());
    try expectEqual(@as(u30, 1234), (W{ .raw = 1234 << 2 }).fixnum());
    try expectEqual(@as(u29, 1024), (W{ .raw = 1024 }).offset());
}

pub const Cons = packed struct {
    car: W,
    cdr: W,
};

pub const Machine = struct {
    value: bool,
    term: W,
    scopes: W,
    plan: W,
};

pub const NIL = W{ .raw = 3 };

test "NIL" {
    try expectEqual(NIL.raw, 3);
}

pub var machine = Machine{
    .value = false,
    .term = NIL,
    .scopes = NIL,
    .plan = NIL,
};

pub const PlanData = packed union(enum) {
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

pub const Plan = packed struct {
    next: W,
    scopes: W,
    data: PlanData,
};

pub const Closure = packed struct {
    params: W,
    body: W,
    scopes: W,
    macro: W,
};

pub const Builtin = struct {
    tag: u24,
    argumentCount: u32,
    varargs: bool,
    evalArgs: bool,
    evalResult: bool,
    params: W,
    function: *anyopaque,
};

pub const SymbolCacheTag = enum {
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
    SYMBOL,
    STRING,
};

pub const BuiltinTag = enum(u24) {
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

pub const symbolCacheSize = @typeInfo(SymbolCacheTag).Enum.fields.len;
pub const builtinCount = @typeInfo(BuiltinTag).Enum.fields.len;

pub fn FunctionTypeN(
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

pub const SliceSize = std.builtin.TypeInfo.Pointer.Size.Slice;

pub fn isVarargsFunction(comptime t: std.builtin.TypeInfo) bool {
    if (t.args.len == 0) {
        return false;
    } else {
        const last = t.args[t.args.len - 1];
        return switch (@typeInfo(last)) {
            .Pointer => |ptr| ptr.child == W and ptr.size == SliceSize,
        };
    }
}

pub fn FunctionType(comptime f: anytype) type {
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

pub const Heap = struct {
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

    pub fn deref(self: Heap, ptr: W) ![*]W {
        return if (isPointer(ptr.lowtag()))
            @ptrCast([*]W, self.data[ptr.offset()..])
        else
            Error.NotAPointer;
    }

    pub fn derefCons(self: Heap, ptr: W) !*Cons {
        return if (ptr.isListPointer())
            @ptrCast(*Cons, try self.deref(ptr))
        else
            Error.NotACons;
    }

    pub fn allocate(heap: *Heap, size: u29, tag: Lowtag) !W {
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

    pub fn allocateWords(heap: *Heap, n: usize, lowtag: Lowtag) !W {
        return heap.allocate(@intCast(u29, 4 * n), lowtag);
    }
};

pub fn alignToDoubleWord(x: u32) u29 {
    return @intCast(u29, (x + Lowtag.mask) & ~Lowtag.mask);
}

test "align to double word" {
    try expectEqual(@as(u32, 0), alignToDoubleWord(@as(u32, 0)));
    try expectEqual(@as(u32, 8), alignToDoubleWord(@as(u32, 1)));
    try expectEqual(@as(u32, 8), alignToDoubleWord(@as(u32, 2)));
    try expectEqual(@as(u32, 16), alignToDoubleWord(@as(u32, 9)));
}

pub fn fixnum(x: u32) W {
    return W{ .raw = x << 2 };
}

pub fn isAligned(x: u32) bool {
    return x & 7 == 0;
}

pub fn isPointer(x: anytype) bool {
    return switch (@TypeOf(x)) {
        W => x.raw & 1 == 1,
        u32 => x & 1 == 1,
        Lowtag => @enumToInt(x) & 1 == 1,
        else => @compileError("unknown type"),
    };
}

pub fn emptyHeap(allocator: std.mem.Allocator, size: u29) !Heap {
    return Heap{
        .allocator = allocator,
        .size = size,
        .used = 0,
        .data = try allocator.alloc(u8, size),
        .area = .{
            .old = size / 2,
            .new = 0,
        },
    };
}

pub fn makeHeader(data: u24, widetag: Widetag) W {
    return W{ .raw = (data << 8) | @enumToInt(widetag) };
}

pub fn makePointer(area: u29, offset: u29, lowtag: Lowtag) W {
    assert(isAligned(area + offset));
    return W{ .raw = (area + offset) | @enumToInt(lowtag) };
}

test "allocate" {
    var heap = try emptyHeap(std.testing.allocator, 1024);
    defer heap.free();

    const foo = try heap.allocate(16, Lowtag.listptr);

    try expectEqual(@as(u29, 0), foo.offset());
    try expectEqual(Lowtag.listptr, foo.lowtag());
}

pub const Error = error{
    NotASymbol,
    NotACons,
    NotAPointer,
    NotAnInstance,
    NotAString,
    ReadError,
    EOF,
};

const StringStruct = packed struct {
    header: W,
    firstByte: u8,

    fn slice(self: *StringStruct) []u8 {
        var bytes = @ptrCast([*]u8, &self.firstByte);
        var length = self.header.immediate();
        return bytes[0..length];
    }
};

pub const PackageStruct = packed struct {
    const slotCount = 3;
    header: W,
    typeDescriptor: W,
    name: W,
    symbols: W,
};

pub const ScopeStruct = packed struct {
    header: W,
    typeDescriptor: W,
    firstWord: W,

    fn scopeSlice(self: *ScopeStruct) []W {
        var words = @ptrCast([*]W, &self.firstWord);
        var length = self.header.immediate() - 1;
        return words[0..length];
    }
};

pub const SymbolStruct = packed struct {
    const slotCount = 5;
    header: W,
    value: W,
    unused: W,
    plist: W,
    name: W,
    package: W,
    function: W,
};

pub const BasicType = enum {
    string,
    symbol,
    package,
    scope,

    fn widetag(self: BasicType) Widetag {
        return switch (self) {
            .string => .string,
            .symbol => .symbol,
            .package => .instance,
            .scope => .instance,
        };
    }

    fn Struct(comptime self: BasicType) type {
        return switch (self) {
            .string => StringStruct,
            .symbol => SymbolStruct,
            .package => PackageStruct,
            .scope => ScopeStruct,
        };
    }

    pub fn castDataPointer(
        comptime self: BasicType,
        pointer: [*]W,
    ) *self.Struct() {
        return @ptrCast(*self.Struct(), pointer);
    }
};

pub const staticSpaceSize: u32 = 40;

pub const Wisp = struct {
    heap: Heap,
    builtins: [builtinCount]?Builtin,
    symbolCache: [symbolCacheSize]W,
    basePackage: W,

    pub fn symbol(self: Wisp, tag: SymbolCacheTag) W {
        return self.symbolCache[@enumToInt(tag)];
    }

    pub fn getSymbolData(self: Wisp, ptr: W) !*SymbolStruct {
        if (!ptr.isOtherPointer() and !ptr.isNil()) {
            return Error.NotASymbol;
        }

        return self.getDataPointer(.symbol, ptr);
    }

    pub fn getDataPointer(
        wisp: Wisp,
        comptime t: BasicType,
        w: W,
    ) !*t.Struct() {
        const pointer = try wisp.heap.deref(w);
        return t.castDataPointer(pointer);
    }

    pub fn typeSymbol(self: Wisp, t: BasicType) W {
        return switch (t) {
            .symbol => self.symbol(.SYMBOL),
            .package => self.symbol(.PACKAGE),
            .string => self.symbol(.STRING),
            .scope => self.symbol(.SCOPE),
        };
    }

    pub fn stringsEqual(self: Wisp, x: W, y: W) !bool {
        return std.mem.eql(
            u8,
            try self.stringBufferAsSlice(x),
            try self.stringBufferAsSlice(y),
        );
    }

    pub fn stringBufferAsSlice(self: Wisp, word: W) ![]u8 {
        const data = try self.getDataPointer(.string, word);
        if (data.header.widetag() != .string) {
            return Error.NotAString;
        } else {
            return data.slice();
        }
    }

    pub fn scopeDataAsSlice(self: *Wisp, word: W) ![]W {
        const data: *ScopeStruct = try self.getDataPointer(.scope, word);
        if (data.header.widetag() != .instance) {
            return Error.NotAnInstance;
        } else {
            return data.scopeSlice();
        }
    }

    pub fn cons(self: *Wisp, car: W, cdr: W) !W {
        var pointer = try self.heap.allocateWords(2, .listptr);

        (try self.heap.derefCons(pointer)).* = Cons{
            .car = car,
            .cdr = cdr,
        };

        return pointer;
    }

    pub fn list(self: *Wisp, items: anytype) !W {
        var result = NIL;
        var i: usize = 0;

        while (i < items.len) : (i += 1) {
            result = try self.cons(items[items.len - 1 - i], result);
        }

        return result;
    }

    pub fn getCons(self: *Wisp, ptr: W) !*Cons {
        return self.heap.derefCons(ptr);
    }
};

pub fn makeInstance(wisp: *Wisp, comptime t: BasicType, slots: anytype) !W {
    var pointer = try wisp.heap.allocateWords(2 + slots.len, Lowtag.structptr);
    var data = try wisp.heap.deref(pointer);

    data[0] = makeHeader(1 + @intCast(u24, slots.len), Widetag.instance);
    data[1] = wisp.typeSymbol(t);

    for (slots) |slot, i| {
        data[2 + i] = slot;
    }

    return pointer;
}

pub fn makePackage(wisp: *Wisp, name: W) !W {
    return makeInstance(wisp, .package, [_]W{ name, NIL });
}

pub const symbolHeader = makeHeader(
    BasicType.symbol.Struct().slotCount,
    .symbol,
);

pub fn makeSymbol(wisp: *Wisp, name: W, package: W) !W {
    var symbol = try wisp.heap.allocateWords(
        1 + BasicType.symbol.Struct().slotCount,
        .otherptr,
    );

    var symbolData = try wisp.getSymbolData(symbol);

    symbolData.* = SymbolStruct{
        .header = symbolHeader,
        .name = name,
        .package = package,
        .value = NIL,
        .function = NIL,
        .plist = NIL,
        .unused = NIL,
    };

    return symbol;
}

pub fn internSymbol(wisp: *Wisp, name: W, package: W) !W {
    var packageData = try wisp.getDataPointer(BasicType.package, package);

    assert(packageData.header.widetag() == BasicType.package.widetag());
    assert(packageData.typeDescriptor.raw == wisp.typeSymbol(.package).raw);

    var cur = packageData.symbols;

    while (!cur.isNil()) {
        assert(cur.isListPointer());

        var cons = try wisp.heap.derefCons(cur);
        var symbolData = try wisp.getSymbolData(cons.car);
        var symbolName = symbolData.name;

        if (try wisp.stringsEqual(symbolName, name)) {
            return cons.car;
        }

        cur = cons.cdr;
    }

    // No symbol found; create it.
    var symbol = try makeSymbol(wisp, name, package);

    // Push it onto the package's symbol list.
    packageData.symbols = try wisp.cons(symbol, packageData.symbols);

    return symbol;
}

pub fn start(heap: Heap) !Wisp {
    var nil = @ptrCast(*SymbolStruct, try heap.deref(NIL));
    var theHeap = heap;

    theHeap.used = alignToDoubleWord(7 * 4);

    nil.* = SymbolStruct{
        .header = symbolHeader,
        .name = try makeString(&theHeap, "NIL"),
        .package = NIL,
        .value = NIL,
        .function = NIL,
        .plist = NIL,
        .unused = NIL,
    };

    var symbolCache = [_]W{W{ .raw = 3 }} ** symbolCacheSize;
    var wisp = Wisp{
        .heap = theHeap,
        .builtins = [_]?Builtin{null} ** builtinCount,
        .symbolCache = symbolCache,
        .basePackage = NIL,
    };

    var packageSymbol = try makeSymbol(&wisp, try makeString(&wisp.heap, "PACKAGE"), NIL);
    var packageSymbolData = try wisp.getDataPointer(BasicType.symbol, packageSymbol);

    symbolCache[@enumToInt(SymbolCacheTag.PACKAGE)] = packageSymbol;

    var wispPackage = try makePackage(&wisp, try makeString(&wisp.heap, "WISP"));
    var wispPackageData = try wisp.getDataPointer(BasicType.package, wispPackage);

    wispPackageData.symbols = try wisp.list([_]W{ packageSymbol, NIL });
    packageSymbolData.package = wispPackage;
    wisp.basePackage = wispPackage;

    return wisp;
}

test "start" {
    var heap = try emptyHeap(std.testing.allocator, 1024);
    defer heap.free();

    _ = try start(heap);
}

pub fn stringBuffer(data: [*]W) [*]u8 {
    return @ptrCast([*]u8, data + 1);
}

pub fn makeString(heap: *Heap, text: []const u8) !W {
    const stringPtr = try heap.allocate(
        4 + 1 + @intCast(u29, text.len),
        .otherptr,
    );

    const data = try heap.deref(stringPtr);
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

pub fn testWisp() !Wisp {
    var heap = try emptyHeap(std.testing.allocator, 1024);
    return try start(heap);
}
