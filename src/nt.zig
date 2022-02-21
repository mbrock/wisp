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
    string,
    glyph,
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

    pub fn init(gpa: std.mem.Allocator) !Data {
        var data = Data{ .gpa = gpa };

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

        self.strings.deinit(self.gpa);
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

    pub fn internString(
        self: *Data,
        name: []const u8,
        package: u29,
    ) !u32 {
        const stringIndex = try self.allocString(name);
        const symbolIndex = @intCast(u29, self.symbols.len);

        try self.symbols.append(self.gpa, .{
            .name = stringIndex,
            .package = package,
        });

        return symbolPointer(symbolIndex);
    }

    pub fn cons(self: *Data, ptr: u32) !Cons {
        assert(type1(ptr) == .cons);
        const i = ptr / 0b1000;
        return self.conses.get(i);
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
};

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

test "garbage collection of conses" {
    var data1 = try Data.init(std.testing.allocator);

    defer data1.deinit();

    _ = try data1.addCons(.{
        .car = encodeFixnum(1),
        .cdr = encodeFixnum(2),
    });

    const yData = Cons{
        .car = encodeFixnum(3),
        .cdr = encodeFixnum(4),
    };

    const y = try data1.addCons(yData);

    var gc = try GC.init(&data1);
    defer gc.new.deinit();
    defer gc.deinit();

    const y2 = try gc.copy(y);

    try gc.scavenge();

    try expectEqual(gc.new.conses.len, 1);
    try expectEqual(yData, try gc.new.cons(y2));
}

const GC = struct {
    old: *Data,
    new: Data,

    consProgress: std.DynamicBitSet,

    pub fn init(old: *Data) !GC {
        return GC{
            .old = old,
            .new = Data{
                .gpa = old.gpa,
                .symbols = old.symbols.toOwnedSlice().toMultiArrayList(),
                .packages = old.packages.toOwnedSlice().toMultiArrayList(),
                .conses = .{},
                .closures = .{},
                .strings = .{},
            },
            .consProgress = try std.DynamicBitSet.initEmpty(
                old.gpa,
                old.conses.len,
            ),
        };
    }

    pub fn deinit(self: *GC) void {
        self.consProgress.deinit();
    }

    pub fn copy(self: *GC, x: u32) !u32 {
        return switch (type1(x)) {
            .cons => try self.copyCons(x),
            else => x,
        };
    }

    pub fn scavenge(self: *GC) !void {
        _ = self;
    }

    fn copyCons(self: *GC, oldPtr: u32) !u32 {
        const oldIdx = consIndex(oldPtr);
        const oldCons = self.old.conses.get(oldIdx);

        if (self.consProgress.isSet(oldIdx)) {
            return oldCons.car;
        } else {
            self.consProgress.set(oldIdx);
            return try self.new.addCons(oldCons);
        }
    }
};
