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

pub const Symbol = struct {
    name: u29,
    package: u29,
};

pub const Package = struct {
    name: u29,
    symbolMap: std.StringHashMap(u29),
};

pub const Cons = struct {
    car: u32,
    cdr: u32,
};

pub const Closure = struct {
    body: u32,
    params: u29,
    scopes: u29,
};

pub const Data = struct {
    gpa: std.mem.Allocator,

    symbols: std.MultiArrayList(Symbol) = .{},
    packages: std.MultiArrayList(Package) = .{},
    conses: std.MultiArrayList(Cons) = .{},
    closures: std.MultiArrayList(Closure) = .{},
    strings: std.ArrayListUnmanaged([]const u8) = .{},
    symbolValues: std.AutoHashMapUnmanaged(u32, u32) = .{},

    pub fn init(gpa: std.mem.Allocator) !Data {
        var data = Data{
            .gpa = gpa,
        };

        try data.packages.append(gpa, Package{
            .name = try data.allocString("WISP"),
            .symbolMap = std.StringHashMap(u29).init(gpa),
        });

        _ = try data.internString("NIL", 0);

        return data;
    }

    pub fn addCons(self: *Data, x: Cons) !u32 {
        const i = self.conses.len;
        try self.conses.append(self.gpa, x);
        return (@intCast(u29, i) << 3) + 0b010;
    }

    pub fn deinit(self: *Data) void {
        for (self.strings.items) |x| {
            self.gpa.free(x);
        }

        for (self.packages.items(.symbolMap)) |*x| {
            x.deinit();
        }

        self.strings.deinit(self.gpa);
        self.symbolValues.deinit(self.gpa);
        self.symbols.deinit(self.gpa);
        self.packages.deinit(self.gpa);
        self.conses.deinit(self.gpa);
        self.closures.deinit(self.gpa);
    }

    pub fn allocString(self: *Data, text: []const u8) !u29 {
        const i = @intCast(u29, self.strings.items.len);
        const string = try self.gpa.dupe(u8, text);
        try self.strings.append(self.gpa, string);
        return i;
    }

    pub fn addString(self: *Data, text: []const u8) !u32 {
        const idx = try self.allocString(text);
        return stringPointer(idx);
    }

    pub fn internString(
        self: *Data,
        name: []const u8,
        package: u29,
    ) !u32 {
        var packageSymbolMap = &self.packages.items(.symbolMap)[package];

        if (packageSymbolMap.get(name)) |symbolIndex| {
            return symbolPointer(symbolIndex);
        } else {
            const stringIdx = try self.allocString(name);
            const symbolIdx = @intCast(u29, self.symbols.len);

            try self.symbols.append(self.gpa, .{
                .name = stringIdx,
                .package = package,
            });

            try packageSymbolMap.put(
                self.strings.items[stringIdx],
                symbolIdx,
            );

            return symbolPointer(symbolIdx);
        }
    }

    pub fn cons(self: *Data, ptr: u32) !Cons {
        assert(type1(ptr) == .cons);
        return self.conses.get(ptr / 0b1000);
    }

    pub fn symbol(self: *Data, ptr: u32) !Symbol {
        assert(type1(ptr) == .symbol);
        return self.symbols.get(ptr / 0b1000);
    }

    pub fn car(self: *Data, ptr: u32) !u32 {
        assert(type1(ptr) == .cons);
        const i = ptr / 0b1000;
        return self.conses.items(.car)[i];
    }

    pub fn cdr(self: *Data, ptr: u32) !u32 {
        assert(type1(ptr) == .cons);
        const i = ptr / 0b1000;
        return self.conses.items(.cdr)[i];
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

pub fn stringPointer(idx: u29) u32 {
    return (idx << 3) + 0b110;
}

pub fn symbolPointer(idx: u29) u32 {
    return (idx << 3) + 0b001;
}

pub fn consIndex(x: u32) u29 {
    assert(type1(x) == .cons);
    return @intCast(u29, x / 0b1000);
}

pub fn stringIndex(x: u32) u29 {
    assert(type1(x) == .string);
    return @intCast(u29, x / 0b1000);
}

pub const NIL: u32 = symbolPointer(0);

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
    try dumpMultiArrayList(out, "closures", self.closures);
    try dumpArrayList(out, "strings", self.strings);
    try dumpHashMap(out, "symbolValues", self.symbolValues);
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
