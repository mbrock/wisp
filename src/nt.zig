const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

pub const Tag1 = enum {
    fixnum,
    nil,
    symbol,
    cons,
    closure,
    primop,
    plan,
    glyph,
};

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
        0b110 => .plan,
        0b111 => switch (x & ~@as(u11, 0)) {
            0b111 => .glyph,
            else => unreachable,
        },
    };
}

pub const Symbol = struct {
    name: u29,
    package: u14,
};

pub const Package = struct {
    name: u29,
    symbolMap: std.StringHashMap(u15),
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
    symbols: std.MultiArrayList(Symbol) = .{},
    packages: std.MultiArrayList(Package) = .{},
    conses: std.MultiArrayList(Cons) = .{},
    closures: std.MultiArrayList(Closure) = .{},
    strings: std.ArrayListUnmanaged([]u8) = .{},

    pub fn deinit(self: *Data, gpa: std.mem.Allocator) void {
        inline for (std.meta.fields(Data)) |info| {
            @field(self, info.name).deinit(gpa);
        }
    }
};

test "initialize wisp" {
    var data = Data{};
    defer data.deinit(std.testing.allocator);
}

pub fn encodeFixnum(x: u30) u32 {
    return x << 2;
}

pub fn decodeFixnum(x: u32) u30 {
    return @intCast(u30, x / 4);
}

test "fixnum lowtag" {
    try expectEqual(Tag1.fixnum, type1(encodeFixnum(0)));
    try expectEqual(Tag1.fixnum, type1(encodeFixnum(1)));
}

test "fixnum roundtrip" {
    try expectEqual(@as(u30, 123), decodeFixnum(encodeFixnum(123)));
}
