const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

const read = @import("./read.zig").read;
const print = @import("./print.zig").print;
const GC = @import("./gc.zig");

pub const Tag1 = enum {
    fixnum,
    nil,
    symbol,
    cons,
    closure,
    primop,
    string,
    glyph,
};

pub const String = struct {
    offset0: u32,
    offset1: u32,
};

pub const Symbol = struct {
    name: u29,
    package: u29,
};

pub const Package = struct {
    name: u29,
    symbols: u32 = NIL,
};

pub const Cons = struct {
    car: u32,
    cdr: u32,
};

pub const Semispace = enum(u32) {
    space0 = 0,
    space1 = @as(u32, 1) << 31,

    pub fn other(self: Semispace) Semispace {
        return switch (self) {
            .space0 => .space1,
            .space1 => .space0,
        };
    }

    pub fn makePointer(semispace: Semispace, tag: Tag1, idx: u29) u32 {
        const suffix = @as(u32, switch (tag) {
            .symbol => 0b001,
            .cons => 0b010,
            .string => 0b110,
            else => unreachable,
        });

        return @enumToInt(semispace) | (idx << 3) | suffix;
    }

    pub fn pointerIndex(self: Semispace, x: u32) u29 {
        const semispaceMask = @as(u32, 1) << 31;
        assert(x & semispaceMask == @enumToInt(self));
        return @intCast(u29, (x & ~semispaceMask) / 0b1000);
    }
};

pub const Data = struct {
    gpa: std.mem.Allocator,

    semispace: Semispace = .space0,

    symbols: std.MultiArrayList(Symbol) = .{},
    packages: std.MultiArrayList(Package) = .{},
    conses: std.MultiArrayList(Cons) = .{},

    strings: std.ArrayListUnmanaged(String) = .{},

    stringBytes: std.ArrayListUnmanaged(u8) = .{},
    symbolValues: std.AutoHashMapUnmanaged(u32, u32) = .{},

    pub fn init(gpa: std.mem.Allocator) !Data {
        var data = Data{
            .gpa = gpa,
        };

        try data.packages.append(gpa, Package{
            .name = try data.allocString("WISP"),
        });

        _ = try data.internString("NIL", 0);

        return data;
    }

    pub fn makePointer(self: Data, tag: Tag1, idx: u29) u32 {
        return self.semispace.makePointer(tag, idx);
    }

    pub fn pointerToIndex(self: Data, x: u32) u29 {
        return self.semispace.pointerIndex(x);
    }

    pub fn allocCons(self: *Data, x: Cons) !u29 {
        const i = @intCast(u29, self.conses.len);
        try self.conses.append(self.gpa, x);
        return i;
    }

    pub fn addCons(self: *Data, x: Cons) !u32 {
        return self.makePointer(.cons, try self.allocCons(x));
    }

    pub fn deinit(self: *Data) void {
        self.stringBytes.deinit(self.gpa);
        self.strings.deinit(self.gpa);
        self.symbolValues.deinit(self.gpa);
        self.symbols.deinit(self.gpa);
        self.packages.deinit(self.gpa);
        self.conses.deinit(self.gpa);
    }

    pub fn allocString(self: *Data, text: []const u8) !u29 {
        const i = @intCast(u29, self.strings.items.len);

        const offset = @intCast(u32, self.stringBytes.items.len);
        const length = @intCast(u32, text.len);

        try self.stringBytes.appendSlice(self.gpa, text);
        try self.strings.append(self.gpa, String{
            .offset0 = offset,
            .offset1 = offset + length,
        });

        return i;
    }

    pub fn addString(self: *Data, text: []const u8) !u32 {
        const idx = try self.allocString(text);
        return self.makePointer(.string, idx);
    }

    pub fn stringSlice(self: *const Data, idx: u29) []const u8 {
        const string: String = self.strings.items[idx];
        return self.stringBytes.items[string.offset0..string.offset1];
    }

    pub fn internString(
        self: *Data,
        name: []const u8,
        package: u29,
    ) !u32 {
        var symbols = &self.packages.items(.symbols)[package];
        const symbolNames = self.symbols.items(.name);

        var cur = symbols.*;
        while (cur != NIL) {
            const it = try self.car(cur);
            const itsName = symbolNames[self.pointerToIndex(it)];
            const s = self.stringSlice(itsName);

            if (std.mem.eql(u8, s, name)) {
                return it;
            } else {
                cur = try self.cdr(cur);
            }
        }

        const stringIdx = try self.allocString(name);
        const symbolIdx = @intCast(u29, self.symbols.len);

        try self.symbols.append(self.gpa, .{
            .name = stringIdx,
            .package = package,
        });

        const ptr = self.makePointer(.symbol, symbolIdx);

        symbols.* = try self.addCons(.{
            .car = ptr,
            .cdr = symbols.*,
        });

        return ptr;
    }

    pub fn cons(self: *const Data, ptr: u32) !Cons {
        assert(type1(ptr) == .cons);
        return self.conses.get(self.pointerToIndex(ptr));
    }

    pub fn symbol(self: *Data, ptr: u32) !Symbol {
        assert(type1(ptr) == .symbol);
        return self.symbols.get(self.pointerToIndex(ptr));
    }

    pub fn car(self: *Data, ptr: u32) !u32 {
        assert(type1(ptr) == .cons);
        return self.conses.items(.car)[self.pointerToIndex(ptr)];
    }

    pub fn cdr(self: *Data, ptr: u32) !u32 {
        assert(type1(ptr) == .cons);
        return self.conses.items(.cdr)[self.pointerToIndex(ptr)];
    }

    pub fn list(self: *Data, xs: anytype) !u32 {
        var result = NIL;
        var i: usize = 0;
        while (i < xs.len) : (i += 1) {
            result = try self.addCons(.{
                .car = xs[xs.len - i - 1],
                .cdr = result,
            });
        }

        return result;
    }
};

pub const NIL: u32 = Semispace.space0.makePointer(.symbol, 0);
pub const ZAP: u32 = 0xffffffff;

pub fn type1(x: u32) Tag1 {
    if (x == 1) {
        return .nil;
    }

    return switch (@intCast(u3, x & 0b111)) {
        0b000 => .fixnum,
        0b001 => .symbol,
        0b010 => .cons,
        0b011 => .closure,
        0b100 => .fixnum,
        0b101 => .primop,
        0b110 => .string,
        0b111 => switch (x & ~@as(u11, 0)) {
            0b111 => .glyph,
            else => unreachable,
        },
    };
}

pub fn encodeFixnum(x: u30) u32 {
    return x << 2;
}

pub fn decodeFixnum(x: u32) u30 {
    return @intCast(u30, x / 4);
}

fn expectParsingRoundtrip(text: []const u8) !void {
    var ctx = try Data.init(std.testing.allocator);
    defer ctx.deinit();

    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    const x = try read(&ctx, text);

    try print(&ctx, &writer, x);
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

pub fn dumpData(self: *Data, out: anytype) !void {
    try dumpMultiArrayList(out, "symbols", self.symbols);
    try dumpMultiArrayList(out, "packages", self.packages);
    try dumpMultiArrayList(out, "conses", self.conses);
    try dumpArrayList(out, "strings", self.strings);
    try dumpHashMap(out, "symbolValues", self.symbolValues);

    try out.print(
        "* stringBytes\n  {s}\n",
        .{self.stringBytes.items},
    );
}

test {
    std.testing.refAllDecls(@This());
}

test "initialize wisp" {
    var data = try Data.init(std.testing.allocator);
    defer data.deinit();
}

test "intern string" {
    var data = try Data.init(std.testing.allocator);
    defer data.deinit();

    const x = try data.internString("FOO", 0);

    try expectEqual(Tag1.symbol, type1(x));
}

test "cons" {
    var data = try Data.init(std.testing.allocator);
    defer data.deinit();

    const x = try data.addCons(.{
        .car = encodeFixnum(1),
        .cdr = encodeFixnum(2),
    });

    try expectEqual(Tag1.cons, type1(x));
    try expectEqual(encodeFixnum(1), try data.car(x));
    try expectEqual(encodeFixnum(2), try data.cdr(x));
}

test "fixnum lowtag" {
    try expectEqual(Tag1.fixnum, type1(encodeFixnum(0)));
    try expectEqual(Tag1.fixnum, type1(encodeFixnum(1)));
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
