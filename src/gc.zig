const std = @import("std");
const expectEqual = std.testing.expectEqual;

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const print = @import("./print.zig").print;

const Data = wisp.Data;

const GC = @This();

old: *Data,
new: Data,

consProgress: std.DynamicBitSet,
stringProgress: std.DynamicBitSet,

pub fn init(old: *Data) !GC {
    var new = GC{
        .old = old,
        .new = Data{
            .gpa = old.gpa,
            .symbols = old.symbols,
            .symbolValues = old.symbolValues,
            .packages = old.packages,
            .closures = old.closures,
            .strings = .{},
            .conses = .{},
        },
        .consProgress = try std.DynamicBitSet.initEmpty(
            old.gpa,
            old.conses.len,
        ),
        .stringProgress = try std.DynamicBitSet.initEmpty(
            old.gpa,
            old.strings.items.len,
        ),
    };

    return new;
}

pub fn finalize(self: *GC) void {
    const gpa = self.old.gpa;

    for (self.old.strings.items) |x| {
        self.old.gpa.free(x);
    }

    self.old.conses.deinit(gpa);
    self.old.strings.deinit(gpa);
    self.old.* = .{ .gpa = gpa };
}

pub fn deinit(self: *GC) void {
    self.consProgress.deinit();
    self.stringProgress.deinit();
}

pub fn copyRoots(self: *GC) !void {
    try self.copyPackageNames();
    try self.copySymbolNames();
    try self.copySymbolValues();
}

pub fn copySymbolNames(self: *GC) !void {
    for (self.old.symbols.items(.name)) |*x| {
        const newPtr = try self.copyString(wisp.stringPointer(x.*));
        x.* = wisp.stringIndex(newPtr);
    }
}

pub fn copyPackageNames(self: *GC) !void {
    for (self.old.packages.items(.name)) |*x| {
        const newPtr = try self.copyString(wisp.stringPointer(x.*));
        x.* = wisp.stringIndex(newPtr);
    }
}

pub fn copySymbolValues(self: *GC) !void {
    var it = self.old.symbolValues.iterator();
    while (it.next()) |kv| {
        kv.value_ptr.* = try self.copy(kv.value_ptr.*);
    }
}

pub fn copy(self: *GC, x: u32) !u32 {
    return switch (wisp.type1(x)) {
        .cons => try self.copyCons(x),
        .string => try self.copyString(x),
        else => x,
    };
}

pub fn scavenge(self: *GC) !void {
    _ = self;

    var scan: usize = 0;
    while (scan < self.new.conses.len) : (scan += 1) {
        const slice = self.new.conses.slice();
        const car = &slice.items(.car)[scan];
        const cdr = &slice.items(.cdr)[scan];

        car.* = try self.copy(car.*);
        cdr.* = try self.copy(cdr.*);
    }
}

fn copyCons(self: *GC, oldPtr: u32) !u32 {
    const oldIdx = wisp.consIndex(oldPtr);
    const oldCons = self.old.conses.get(oldIdx);

    if (self.consProgress.isSet(oldIdx)) {
        return oldCons.car;
    } else {
        self.consProgress.set(oldIdx);
        return try self.new.addCons(oldCons);
    }
}

fn copyString(self: *GC, oldPtr: u32) !u32 {
    const oldIdx = wisp.stringIndex(oldPtr);
    const oldString: []const u8 = self.old.strings.items[oldIdx];

    if (self.stringProgress.isSet(oldIdx)) {
        return @intCast(u32, @ptrToInt(oldString.ptr));
    } else {
        self.stringProgress.set(oldIdx);
        return self.new.addString(oldString);
    }
}

test "garbage collection of conses" {
    var data1 = try Data.init(std.testing.allocator);

    defer data1.deinit();

    _ = try data1.addCons(.{
        .car = wisp.encodeFixnum(1),
        .cdr = wisp.encodeFixnum(2),
    });

    const yData = wisp.Cons{
        .car = wisp.encodeFixnum(3),
        .cdr = wisp.encodeFixnum(4),
    };

    const y = try data1.addCons(yData);

    var gc = try GC.init(&data1);
    defer gc.new.deinit();
    defer gc.deinit();
    defer gc.finalize();

    const y2 = try gc.copy(y);

    try gc.scavenge();

    try expectEqual(gc.new.conses.len, 1);
    try expectEqual(yData, try gc.new.cons(y2));
}

test "read and gc" {
    var data1 = try Data.init(std.testing.allocator);

    defer data1.deinit();

    const x1 = try read(&data1, "(foo (bar (baz 1 2 3)))");

    const foo = try data1.internString("X", 0);
    try data1.symbolValues.put(
        data1.gpa,
        foo,
        x1,
    );

    var gc = try GC.init(&data1);
    defer gc.new.deinit();
    defer gc.deinit();
    defer gc.finalize();

    try gc.copyRoots();
    try gc.scavenge();

    var data = gc.new;

    const bar = try data.internString("X", 0);
    const x2 = data.symbolValues.get(bar).?;

    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    try print(&data, &writer, x2);
    try std.testing.expectEqualStrings(
        "(FOO (BAR (BAZ 1 2 3)))",
        list.items,
    );
}

test "gc ephemeral strings" {
    var data1 = try Data.init(std.testing.allocator);

    defer data1.deinit();

    const x = try read(&data1,
        \\ ("foo" "bar" "baz")
    );

    const foo = try data1.car(x);
    try data1.symbolValues.put(
        data1.gpa,
        try data1.internString("X", 0),
        foo,
    );

    const stringCount1 = data1.strings.items.len;

    var gc = try GC.init(&data1);
    defer gc.new.deinit();
    defer gc.deinit();
    defer gc.finalize();

    try gc.copyRoots();
    try gc.scavenge();

    const stringCount2 = gc.new.strings.items.len;

    try expectEqual(stringCount1 - 2, stringCount2);
}
