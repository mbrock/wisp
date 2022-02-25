const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

const read = @import("./read.zig").read;
const print = @import("./print.zig").print;
const GC = @import("./gc.zig");
const Primops = @import("./primops.zig");

pub const NIL: u32 = 0b00000000000000000000000000001111;
pub const ZAP: u32 = 0b11111111111111111111111111111111;

pub const Tag0 = enum {
    immediate,
    pointer,
};

pub fn uintType(comptime bits: comptime_int) type {
    return @Type(.{
        .Int = .{
            .bits = bits,
            .signedness = .unsigned,
        },
    });
}

pub fn payloadBits(comptime this: anytype) comptime_int {
    return 32 - this.tagBits();
}

pub fn tagType(comptime this: anytype) type {
    return uintType(this.tagBits());
}

pub fn payloadType(comptime this: anytype) type {
    return uintType(payloadBits(this));
}

pub const ImmediateTag = enum {
    nil,
    fixnum,
    primfun,
    primmac,
    glyph,
    zap,

    pub fn tagBits(comptime this: ImmediateTag) comptime_int {
        return switch (this) {
            .nil,
            .zap,
            => 0,

            .fixnum => 2,

            .primfun,
            .primmac,
            .glyph,
            => 11,
        };
    }
};

pub const Immediate = union(ImmediateTag) {
    nil: void,
    zap: void,
    fixnum: u30,
    primfun: u21,
    primmac: u21,
    glyph: u21,

    pub fn make(comptime tag: ImmediateTag, x: u32) Word {
        return Word{
            .immediate = @unionInit(
                @This(),
                @tagName(tag),
                @intCast(payloadType(tag), x >> tag.tagBits()),
            ),
        };
    }

    pub fn raw(x: Immediate) u32 {
        return switch (x) {
            .nil => NIL,
            .zap => ZAP,
            .fixnum => encodeFixnum(x.fixnum),
            .primfun => |i| (i << 11) | 0b011111,
            .primmac => |i| (i << 11) | 0b101111,
            .glyph => unreachable,
        };
    }
};

pub const Pointer = union(Kind) {
    cons: u29,
    symbol: u29,
    string: u29,
    package: u21,
    argsPlan: u21,

    pub fn make(comptime this: Kind, x: u32) Word {
        return Word{
            .pointer = @unionInit(
                @This(),
                @tagName(this),
                @intCast(payloadType(this), x >> this.tagBits()),
            ),
        };
    }

    pub fn raw(ptr: Pointer) u32 {
        return switch (ptr) {
            .cons => |x| Kind.castOffsetToWord(.cons, x),
            .symbol => |x| Kind.castOffsetToWord(.symbol, x),
            .string => |x| Kind.castOffsetToWord(.string, x),
            .package => |x| Kind.castOffsetToWord(.package, x),
            .argsPlan => |x| Kind.castOffsetToWord(.argsPlan, x),
        };
    }

    pub fn semispace(x: Pointer) Semispace {
        return Semispace.of(x.raw());
    }

    pub fn offset(
        this: Pointer,
        comptime kind: Kind,
        space: Semispace,
    ) payloadType(kind) {
        assert(this.semispace() == space);
        const x = @field(this, @tagName(kind));
        const t = payloadType(kind);
        const semispaceBit = (@as(t, 1) << (@typeInfo(t).Int.bits - 1));
        return x & ~semispaceBit;
    }
};

pub const Word = union(Tag0) {
    pub const zap = Word.from(ZAP);
    pub const nil = Word.from(NIL);

    immediate: Immediate,
    pointer: Pointer,

    pub fn from(x: u32) Word {
        if (x == NIL) {
            return Word{ .immediate = .{ .nil = .{} } };
        }

        if (x == ZAP) {
            return Word{ .immediate = .{ .zap = .{} } };
        }

        return switch (@intCast(u3, x & 0b111)) {
            0b000 => Immediate.make(.fixnum, x),
            0b001 => Pointer.make(.symbol, x),
            0b010 => Pointer.make(.cons, x),
            0b011 => unreachable,
            0b100 => Immediate.make(.fixnum, x),
            0b101 => unreachable,
            0b110 => Pointer.make(.string, x),
            0b111 => switch (x & ~@as(u11, 0)) {
                0b000111 => Immediate.make(.glyph, x),
                0b010111 => Pointer.make(.package, x),
                0b011111 => Immediate.make(.primfun, x),
                0b101111 => Immediate.make(.primmac, x),
                0b100111 => Pointer.make(.argsPlan, x),
                else => {
                    std.log.warn("weird {b}", .{x});
                    unreachable;
                },
            },
        };
    }

    pub fn raw(x: Word) u32 {
        return switch (x) {
            .immediate => x.immediate.raw(),
            .pointer => x.pointer.raw(),
        };
    }

    pub fn eql(x: Word, y: Word) bool {
        return x.raw() == y.raw();
    }
};

pub const Kind = enum {
    cons,
    symbol,
    package,
    string,
    argsPlan,

    pub fn valueType(kind: Kind) type {
        return switch (kind) {
            .cons => Cons,
            .symbol => Symbol,
            .package => Package,
            .string => String,
            .argsPlan => ArgsPlan,
        };
    }

    pub fn tagBits(comptime this: Kind) comptime_int {
        return switch (this) {
            .cons,
            .symbol,
            .string,
            => 3,

            .package,
            .argsPlan,
            => 11,
        };
    }

    pub fn tag(comptime this: Kind) tagType(this) {
        return switch (this) {
            .cons => 0b010,
            .symbol => 0b001,
            .string => 0b110,
            .package => 0b10111,
            .argsPlan => 0b100111,
        };
    }

    pub fn castOffsetToWord(
        comptime this: Kind,
        offset: payloadType(this),
    ) u32 {
        return (@intCast(u32, offset) << this.tagBits()) | this.tag();
    }

    pub fn containerType(kind: Kind) type {
        return std.MultiArrayList(kind.valueType());
    }

    pub fn emptyContainer(
        comptime kind: Kind,
    ) std.MultiArrayList(kind.valueType()) {
        return switch (kind) {
            .cons => std.MultiArrayList(Cons){},
            .symbol => std.MultiArrayList(Symbol){},
            .package => std.MultiArrayList(Package){},
            .string => std.MultiArrayList(String){},
            .argsPlan => std.MultiArrayList(ArgsPlan){},
        };
    }
};

test "tag bit stuff" {
    try expectEqual(u29, payloadType(Kind.cons));
    try expectEqual(u3, tagType(Kind.cons));

    try expectEqual(
        @as(u32, 0b10000000000000000000000000000010),
        Kind.castOffsetToWord(.cons, 0b10000000000000000000000000000),
    );

    const x = Pointer.make(.cons, 0b10000000000000000000000000000010);
    try expectEqual(@as(u32, 0b10000000000000000000000000000010), x.raw());
    try expectEqual(x.pointer.semispace(), Semispace.space1);
}

pub const allKinds = std.enums.values(Kind);

pub const String = struct {
    offset: u32,
    length: u32,
};

pub const Symbol = struct {
    name: u32,
    package: u32,
    value: u32 = ZAP,
    function: u32 = NIL,
};

pub const Package = struct {
    name: u32,
    symbols: u32 = NIL,
};

pub const Cons = struct {
    car: u32,
    cdr: u32,
};

pub const ArgsPlan = struct {
    next: u32,
    scopes: u32,
    callee: u32,
    values: u32,
    terms: u32,
};

fn SliceOf(comptime t: type) type {
    return std.MultiArrayList(t).Slice;
}

pub fn EnumFieldMultiArrays(comptime E: type) type {
    const StructField = std.builtin.TypeInfo.StructField;
    var fields: []const StructField = &[_]StructField{};
    inline for (std.meta.fields(E)) |field| {
        const e = @intToEnum(E, field.value);
        const fieldType = std.MultiArrayList(e.valueType());
        fields = fields ++ &[_]StructField{.{
            .name = field.name,
            .field_type = fieldType,
            .default_value = @as(fieldType, e.emptyContainer()),
            .is_comptime = false,
            .alignment = @alignOf(fieldType),
        }};
    }

    return @Type(.{ .Struct = .{
        .layout = .Auto,
        .fields = fields,
        .decls = &[_]std.builtin.TypeInfo.Declaration{},
        .is_tuple = false,
    } });
}

pub const Heap = struct {
    gpa: std.mem.Allocator,
    semispace: Semispace = .space0,
    stringBytes: std.ArrayListUnmanaged(u8) = .{},
    data: EnumFieldMultiArrays(Kind) = .{},

    pub const Slices = struct {
        symbols: SliceOf(Symbol),
        packages: SliceOf(Package),
        conses: SliceOf(Cons),
        strings: SliceOf(String),
    };

    pub fn kindContainer(
        this: *Heap,
        comptime kind: Kind,
    ) *std.MultiArrayList(kind.valueType()) {
        return &@field(this.data, @tagName(kind));
    }

    pub fn slices(this: Heap) Slices {
        return Slices{
            .symbols = this.symbols.slice(),
            .packages = this.packages.slice(),
            .conses = this.conses.slice(),
            .strings = this.strings.slice(),
        };
    }

    pub fn init(gpa: std.mem.Allocator) !Heap {
        var this = Heap{
            .gpa = gpa,
        };

        try this.data.package.append(gpa, Package{
            .name = try this.addString("WISP"),
        });

        return this;
    }

    pub fn loadPrimops(this: *Heap) !void {
        inline for (Primops.primfuns.values) |primop, i| {
            const symbol = try this.internStringInBasePackage(primop.info.name);
            const function = try this.symbolFunction(symbol);
            function.* = Immediate.make(.primfun, i).raw();
        }

        inline for (Primops.primmacs.values) |primop, i| {
            const symbol = try this.internStringInBasePackage(primop.info.name);
            const function = try this.symbolFunction(symbol);
            assert(function.* == NIL);
            function.* = Immediate.make(.primmac, i).raw();
        }
    }

    pub fn makePointer(this: Heap, x: Pointer) u32 {
        return this.semispace.makePointer(x);
    }

    pub fn alloc(this: *Heap, comptime t: Kind, x: t.valueType()) !payloadType(t) {
        var container = this.kindContainer(t);
        const i = @intCast(payloadType(t), container.len);
        try container.append(this.gpa, x);
        return i;
    }

    pub fn append(this: *Heap, comptime t: Kind, x: t.valueType()) !u32 {
        const pointer = @unionInit(Pointer, @tagName(t), try this.alloc(t, x));
        return this.makePointer(pointer);
    }

    pub fn deinit(this: *Heap) void {
        inline for (std.meta.fields(Kind)) |field| {
            @field(this.data, field.name).deinit(this.gpa);
        }

        this.stringBytes.deinit(this.gpa);
        this.* = .{ .gpa = this.gpa };
    }

    pub fn allocString(this: *Heap, text: []const u8) !u29 {
        const offset = @intCast(u30, this.stringBytes.items.len);
        const length = @intCast(u30, text.len);

        try this.stringBytes.appendSlice(this.gpa, text);
        return this.alloc(.string, .{
            .offset = encodeFixnum(offset),
            .length = encodeFixnum(length),
        });
    }

    pub fn addString(this: *Heap, text: []const u8) !u32 {
        const idx = try this.allocString(text);
        return this.makePointer(.{ .string = idx });
    }

    pub fn stringSlice(this: *const Heap, ptr: u32) []const u8 {
        const idx = Word.from(ptr).pointer.offset(.string, this.semispace);
        const string: String = this.data.string.get(idx);
        const offset0 = decodeFixnum(string.offset);
        const offset1 = offset0 + decodeFixnum(string.length);
        return this.stringBytes.items[offset0..offset1];
    }

    pub fn internStringInBasePackage(this: *Heap, name: []const u8) !u32 {
        return this.internString(name, this.basePackage());
    }

    pub fn basePackage(this: *Heap) u32 {
        const x = this.makePointer(.{ .package = 0 });
        return x;
    }

    pub fn internString(
        this: *Heap,
        name: []const u8,
        package: u32,
    ) !u32 {
        const packageIdx = Word.from(package).pointer.offset(.package, this.semispace);
        var symbols = &this.data.package.items(.symbols)[packageIdx];
        const symbolNames = this.data.symbol.items(.name);

        var cur = symbols.*;
        while (cur != NIL) {
            const it = try this.car(cur);
            const symbolIdx = Word.from(it).pointer.offset(.symbol, this.semispace);
            const itsName = symbolNames[symbolIdx];
            const s = this.stringSlice(itsName);

            if (std.mem.eql(u8, s, name)) {
                return it;
            } else {
                cur = try this.cdr(cur);
            }
        }

        const stringIdx = try this.allocString(name);
        const symbolIdx = @intCast(u29, this.data.symbol.len);

        try this.data.symbol.append(this.gpa, .{
            .name = this.makePointer(.{ .string = stringIdx }),
            .package = packageIdx,
        });

        const ptr = this.makePointer(.{ .symbol = symbolIdx });

        symbols.* = try this.append(.cons, .{
            .car = ptr,
            .cdr = symbols.*,
        });

        return ptr;
    }

    pub fn symbolValue(this: *Heap, ptr: u32) !*u32 {
        const i = this.getOffset(.symbol, ptr);
        return &this.data.symbol.items(.value)[i];
    }

    pub fn symbolFunction(this: *Heap, ptr: u32) !*u32 {
        const i = this.getOffset(.symbol, ptr);
        return &this.data.symbol.items(.function)[i];
    }

    fn pointerWord(this: *const Heap, ptr: u32) Word {
        const word = Word.from(ptr);
        assert(word.pointer.semispace() == this.semispace);
        return word;
    }

    pub fn deref(
        this: *Heap,
        comptime kind: Kind,
        x: u32,
    ) !kind.valueType() {
        return this.kindContainer(kind).get(this.getOffset(kind, x));
    }

    pub fn get(
        this: *Heap,
        comptime kind: Kind,
        i: payloadType(kind),
    ) !kind.valueType() {
        return this.kindContainer(kind).get(i);
    }

    pub fn getField(
        this: *Heap,
        comptime kind: Kind,
        comptime field: kind.containerType().Field,
        i: payloadType(kind),
    ) !std.meta.fieldInfo(kind.valueType(), field).field_type {
        return this.kindContainer(kind).items(field)[i];
    }

    pub fn getOffset(this: *Heap, comptime kind: Kind, x: u32) payloadType(kind) {
        return Word.from(x).pointer.offset(kind, this.semispace);
    }

    pub fn car(this: *Heap, ptr: u32) !u32 {
        return (try this.deref(.cons, ptr)).car;
    }

    pub fn cdr(this: *Heap, ptr: u32) !u32 {
        return (try this.deref(.cons, ptr)).cdr;
    }

    pub fn pointerDeref(
        this: *Heap,
        comptime kind: Kind,
        p: Pointer,
    ) !kind.valueType() {
        return this.kindContainer(kind).get(
            p.offset(kind, this.semispace),
        );
    }

    pub fn list(this: *Heap, xs: anytype) !u32 {
        var result = NIL;
        var i: usize = 0;
        while (i < xs.len) : (i += 1) {
            result = try this.append(.cons, .{
                .car = xs[xs.len - i - 1],
                .cdr = result,
            });
        }

        return result;
    }
};

pub const Semispace = enum(u1) {
    space0 = 0,
    space1 = 1,

    pub const mask = @as(u32, 1) << 31;

    pub fn other(this: Semispace) Semispace {
        return switch (this) {
            .space0 => .space1,
            .space1 => .space0,
        };
    }

    pub fn word(this: Semispace) u32 {
        return switch (this) {
            .space0 => 0,
            .space1 => @as(u32, 1) << 31,
        };
    }

    pub fn makePointer(semispace: Semispace, x: Pointer) u32 {
        return semispace.word() | x.raw();
    }

    pub fn of(x: u32) Semispace {
        return if (x & mask == 0) .space0 else .space1;
    }
};

pub fn encodeFixnum(x: u30) u32 {
    return x << 2;
}

pub fn decodeFixnum(x: u32) u30 {
    return @intCast(u30, x >> 2);
}

fn expectParsingRoundtrip(text: []const u8) !void {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    const x = try read(&heap, text);
    // try dumpDataStderr(&heap);

    try print(&heap, &writer, x);
    try std.testing.expectEqualStrings(text, list.items);
}

pub fn dumpMultiArrayList(out: anytype, name: []const u8, x: anytype) !void {
    try out.print("* {s}\n", .{name});
    var i: usize = 0;
    while (i < x.len) : (i += 1) {
        try out.print("  - {any}\n", .{x.get(i)});
    }
}

pub fn dumpArrayList(out: anytype, name: []const u8, x: anytype) !void {
    try out.print("* {s}\n", .{name});
    var i: usize = 0;
    while (i < x.items.len) : (i += 1) {
        try out.print("  - {s}\n", .{x.items[i]});
    }
}

pub fn dumpHashMap(out: anytype, name: []const u8, x: anytype) !void {
    try out.print("* {s}\n", .{name});
    var it = x.iterator();
    while (it.next()) |kv| {
        try out.print("  {any} = {any}\n", .{
            kv.key_ptr.*,
            kv.value_ptr.*,
        });
    }
}

pub fn dumpDataStderr(this: *Heap) !void {
    try dumpData(this, std.io.getStdErr().writer());
}

pub fn dumpData(this: *Heap, out: anytype) !void {
    try dumpMultiArrayList(out, "symbols", this.data.symbol);
    try dumpMultiArrayList(out, "packages", this.data.package);
    try dumpMultiArrayList(out, "conses", this.data.cons);
    try dumpMultiArrayList(out, "strings", this.data.string);

    try out.print(
        "* stringBytes\n  {s}\n",
        .{this.stringBytes.items},
    );
}

test {
    std.testing.refAllDecls(@This());
}

test "initialize wisp" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();
}

test "intern string" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    const x = Word.from(
        try heap.internStringInBasePackage("FOO"),
    );

    try expectEqual(@as(u29, 0), x.pointer.symbol);
}

test "cons" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    const x = try heap.append(.cons, .{
        .car = encodeFixnum(1),
        .cdr = encodeFixnum(2),
    });

    try expectEqual(encodeFixnum(1), try heap.car(x));
    try expectEqual(encodeFixnum(2), try heap.cdr(x));
}

test "fixnum lowtag" {
    try expectEqual(
        Word{ .immediate = .{ .fixnum = 0 } },
        Word.from(encodeFixnum(0)),
    );

    try expectEqual(
        Word{ .immediate = .{ .fixnum = 1 } },
        Word.from(encodeFixnum(1)),
    );
}

test "fixnum roundtrip" {
    try expectEqual(@as(u30, 123), decodeFixnum(encodeFixnum(123)));
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
