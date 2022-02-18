const std = @import("std");
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

test "foo" {
    try expectEqual(1, 1);
}

pub const SymbolData = struct {
    name: []const u8,
    package: u14,
};

pub const Package = struct {
    symbolMap: std.StringHashMap(u15),
};

pub const Cons = [2]u32;

pub const Tag1 = enum {
    fixnum,
    nil,
    cons,
    symbol,
    primop,
    glyph,
};

pub fn type1(x: u32) Tag1 {
    if (x & 0b11 == 0) {
        return .fixnum;
    } else if (x == 1) {
        return .nil;
    } else if (x & 3 == 0b11) {
        return .cons;
    } else if (x & ~@as(u21, 0) == ~@as(u21, 0)) {
        return .glyph;
    } else {
        unreachable;
    }
}

pub const Wisp = struct {
    symbolData: std.MultiArrayList(SymbolData),
    packages: std.ArrayList(Package),
};

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
