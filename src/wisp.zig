const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

const read = @import("./read.zig").read;
const print = @import("./print.zig").print;
const GC = @import("./gc.zig");

pub const NIL: u32 = 0b00000000000000000000000000001111;
pub const ZAP: u32 = 0b11111111111111111111111111111111;

pub const Tag0 = enum {
    immediate,
    pointer,
};

pub const ImmediateTag = enum {
    nil,
    fixnum,
    primop,
    glyph,
    zap,
};

pub const Immediate = union(ImmediateTag) {
    nil: void,
    zap: void,
    fixnum: u30,
    primop: u29,
    glyph: u21,

    pub fn raw(x: Immediate) u32 {
        return switch (x) {
            .nil => NIL,
            .zap => ZAP,
            .fixnum => encodeFixnum(x.fixnum),
            .primop => unreachable,
            .glyph => unreachable,
        };
    }
};

pub const Pointer = union(Kind) {
    cons: u29,
    symbol: u29,
    string: u29,
    package: u21,

    pub fn raw(x: Pointer) u32 {
        return switch (x) {
            .cons => (@intCast(u32, x.cons) << 3) | 0b010,
            .symbol => (@intCast(u32, x.symbol) << 3) | 0b001,
            .string => (@intCast(u32, x.string) << 3) | 0b110,
            .package => (@intCast(u32, x.package) << 11) | 0b10111,
        };
    }

    pub fn semispace(x: Pointer) Semispace {
        return Semispace.of(x.raw());
    }

    pub fn offset(
        self: Pointer,
        comptime kind: Kind,
        space: Semispace,
    ) kind.offsetType() {
        assert(self.semispace() == space);
        const x = @field(self, @tagName(kind));
        const t = kind.offsetType();
        const semispaceBit = (@as(t, 1) << (@typeInfo(t).Int.bits - 1));
        return x & ~semispaceBit;
    }
};

pub const Word = union(Tag0) {
    immediate: Immediate,
    pointer: Pointer,

    pub fn from(x: u32) Word {
        if (x == NIL) {
            return Word{ .immediate = .{ .nil = .{} } };
        }

        return switch (@intCast(u3, x & 0b111)) {
            0b000 => Word{ .immediate = .{ .fixnum = decodeFixnum(x) } },
            0b001 => Word{ .pointer = .{ .symbol = @intCast(u29, x >> 3) } },
            0b010 => Word{ .pointer = .{ .cons = @intCast(u29, x >> 3) } },
            0b011 => unreachable,
            0b100 => Word{ .immediate = .{ .fixnum = decodeFixnum(x) } },
            0b101 => Word{ .immediate = .{ .primop = @intCast(u29, x >> 3) } },
            0b110 => Word{ .pointer = .{ .string = @intCast(u29, x >> 3) } },
            0b111 => switch (x & ~@as(u11, 0)) {
                0b00111 => Word{ .immediate = .{ .glyph = @intCast(u21, x >> 11) } },
                0b10111 => Word{ .pointer = .{ .package = @intCast(u21, x >> 11) } },
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
};

pub const Kind = enum {
    cons,
    symbol,
    package,
    string,

    pub fn valueType(kind: Kind) type {
        return switch (kind) {
            .cons => Cons,
            .symbol => Symbol,
            .package => Package,
            .string => String,
        };
    }

    pub fn offsetType(kind: Kind) type {
        return switch (kind) {
            .cons, .symbol, .string => u29,
            .package => u21,
        };
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
        };
    }
};

pub const allKinds = std.enums.values(Kind);

pub const String = struct {
    offset: u32,
    length: u32,
};

pub const Symbol = struct {
    name: u32,
    package: u32,
    value: u32 = NIL,
};

pub const Package = struct {
    name: u32,
    symbols: u32 = NIL,
};

pub const Cons = struct {
    car: u32,
    cdr: u32,
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
        self: *Heap,
        comptime kind: Kind,
    ) *std.MultiArrayList(kind.valueType()) {
        return &@field(self.data, @tagName(kind));
    }

    pub fn slices(self: Heap) Slices {
        return Slices{
            .symbols = self.symbols.slice(),
            .packages = self.packages.slice(),
            .conses = self.conses.slice(),
            .strings = self.strings.slice(),
        };
    }

    pub fn init(gpa: std.mem.Allocator) !Heap {
        var self = Heap{
            .gpa = gpa,
        };

        try self.data.package.append(gpa, Package{
            .name = try self.addString("WISP"),
        });

        return self;
    }

    pub fn makePointer(self: Heap, x: Pointer) u32 {
        return self.semispace.makePointer(x);
    }

    pub fn alloc(self: *Heap, comptime t: Kind, x: t.valueType()) !t.offsetType() {
        var container = self.kindContainer(t);
        const i = @intCast(t.offsetType(), container.len);
        try container.append(self.gpa, x);
        return i;
    }

    pub fn append(self: *Heap, comptime t: Kind, x: t.valueType()) !u32 {
        const pointer = @unionInit(Pointer, @tagName(t), try self.alloc(t, x));
        return self.makePointer(pointer);
    }

    pub fn deinit(self: *Heap) void {
        inline for (std.meta.fields(Kind)) |field| {
            @field(self.data, field.name).deinit(self.gpa);
        }

        self.stringBytes.deinit(self.gpa);
        self.* = .{ .gpa = self.gpa };
    }

    pub fn allocString(self: *Heap, text: []const u8) !u29 {
        const offset = @intCast(u30, self.stringBytes.items.len);
        const length = @intCast(u30, text.len);

        try self.stringBytes.appendSlice(self.gpa, text);
        return self.alloc(.string, .{
            .offset = encodeFixnum(offset),
            .length = encodeFixnum(length),
        });
    }

    pub fn addString(self: *Heap, text: []const u8) !u32 {
        const idx = try self.allocString(text);
        return self.makePointer(.{ .string = idx });
    }

    pub fn stringSlice(self: *const Heap, ptr: u32) []const u8 {
        const idx = Word.from(ptr).pointer.offset(.string, self.semispace);
        const string: String = self.data.string.get(idx);
        const offset0 = decodeFixnum(string.offset);
        const offset1 = offset0 + decodeFixnum(string.length);
        return self.stringBytes.items[offset0..offset1];
    }

    pub fn internStringInBasePackage(self: *Heap, name: []const u8) !u32 {
        return self.internString(name, self.basePackage());
    }

    pub fn basePackage(self: *Heap) u32 {
        const x = self.makePointer(.{ .package = 0 });
        return x;
    }

    pub fn internString(
        self: *Heap,
        name: []const u8,
        package: u32,
    ) !u32 {
        const packageIdx = Word.from(package).pointer.offset(.package, self.semispace);
        var symbols = &self.data.package.items(.symbols)[packageIdx];
        const symbolNames = self.data.symbol.items(.name);

        var cur = symbols.*;
        while (cur != NIL) {
            const it = try self.car(cur);
            const symbolIdx = Word.from(it).pointer.offset(.symbol, self.semispace);
            const itsName = symbolNames[symbolIdx];
            const s = self.stringSlice(itsName);

            if (std.mem.eql(u8, s, name)) {
                return it;
            } else {
                cur = try self.cdr(cur);
            }
        }

        const stringIdx = try self.allocString(name);
        const symbolIdx = @intCast(u29, self.data.symbol.len);

        try self.data.symbol.append(self.gpa, .{
            .name = self.makePointer(.{ .string = stringIdx }),
            .package = packageIdx,
        });

        const ptr = self.makePointer(.{ .symbol = symbolIdx });

        symbols.* = try self.append(.cons, .{
            .car = ptr,
            .cdr = symbols.*,
        });

        return ptr;
    }

    pub fn symbolValue(self: *Heap, ptr: u32) !*u32 {
        const i = self.getOffset(.symbol, ptr);
        return &self.data.symbol.items(.value)[i];
    }

    fn pointerWord(self: *const Heap, ptr: u32) Word {
        const word = Word.from(ptr);
        assert(word.pointer.semispace() == self.semispace);
        return word;
    }

    pub fn deref(
        self: *Heap,
        comptime kind: Kind,
        x: u32,
    ) !kind.valueType() {
        return self.kindContainer(kind).get(self.getOffset(kind, x));
    }

    pub fn getOffset(self: *Heap, comptime kind: Kind, x: u32) kind.offsetType() {
        return Word.from(x).pointer.offset(kind, self.semispace);
    }

    pub fn car(self: *Heap, ptr: u32) !u32 {
        return (try self.deref(.cons, ptr)).car;
    }

    pub fn cdr(self: *Heap, ptr: u32) !u32 {
        return (try self.deref(.cons, ptr)).cdr;
    }

    pub fn list(self: *Heap, xs: anytype) !u32 {
        var result = NIL;
        var i: usize = 0;
        while (i < xs.len) : (i += 1) {
            result = try self.append(.cons, .{
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

    pub fn other(self: Semispace) Semispace {
        return switch (self) {
            .space0 => .space1,
            .space1 => .space0,
        };
    }

    pub fn word(self: Semispace) u32 {
        return switch (self) {
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
    return @intCast(u30, x / 4);
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

pub fn dumpDataStderr(self: *Heap) !void {
    try dumpData(self, std.io.getStdErr().writer());
}

pub fn dumpData(self: *Heap, out: anytype) !void {
    try dumpMultiArrayList(out, "symbols", self.data.symbol);
    try dumpMultiArrayList(out, "packages", self.data.package);
    try dumpMultiArrayList(out, "conses", self.data.cons);
    try dumpMultiArrayList(out, "strings", self.data.string);

    try out.print(
        "* stringBytes\n  {s}\n",
        .{self.stringBytes.items},
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
