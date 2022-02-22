const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const print = @import("./print.zig").print;
const dump = @import("./print.zig").dump;

const Data = wisp.Data;

const GC = @This();

fn SliceOf(comptime t: type) type {
    return std.MultiArrayList(t).Slice;
}

const DataSlices = struct {
    symbols: SliceOf(wisp.Symbol),
    packages: SliceOf(wisp.Package),
    conses: SliceOf(wisp.Cons),
    closures: SliceOf(wisp.Closure),
};

old: *Data,
new: Data,
oldSlices: DataSlices,

pub fn init(old: *Data) !GC {
    var new = GC{
        .old = old,
        .oldSlices = DataSlices{
            .symbols = old.symbols.slice(),
            .packages = old.packages.slice(),
            .conses = old.conses.slice(),
            .closures = old.closures.slice(),
        },
        .new = Data{
            .gpa = old.gpa,
            .packages = old.packages,
            .symbols = old.symbols,
            .symbolValues = old.symbolValues,
            .closures = old.closures,
        },
    };

    return new;
}

pub fn finalize(self: *GC) Data {
    const gpa = self.old.gpa;

    self.old.stringBytes.deinit(gpa);
    self.old.conses.deinit(gpa);
    self.old.strings.deinit(gpa);
    self.old.* = .{ .gpa = gpa };

    return self.new;
}

pub fn deinit(self: *GC) void {
    _ = self;
}

pub fn copyRoots(self: *GC) !void {
    try self.copySymbolNames();
    try self.copySymbolValues();
    try self.copyPackageNames();
    try self.copyPackageSymbols();
}

pub fn copySymbolNames(self: *GC) !void {
    for (self.oldSlices.symbols.items(.name)) |*x| {
        x.* = try self.copyString(x.*);
    }
}

pub fn copySymbolValues(self: *GC) !void {
    var it = self.old.symbolValues.iterator();
    while (it.next()) |kv| {
        kv.value_ptr.* = try self.copy(kv.value_ptr.*);
    }
}

pub fn copyPackageNames(self: *GC) !void {
    for (self.oldSlices.packages.items(.name)) |*x| {
        x.* = try self.copyString(x.*);
    }
}

pub fn copyPackageSymbols(self: *GC) !void {
    for (self.oldSlices.packages.items(.symbols)) |*x| {
        x.* = try self.copy(x.*);
    }
}

pub fn copy(self: *GC, x: u32) !u32 {
    return switch (wisp.type1(x)) {
        .cons => try self.copyPointer(.cons, x),
        .string => try self.copyPointer(.string, x),
        else => x,
    };
}

pub fn copyPointer(
    self: *GC,
    tag: wisp.Tag1,
    ptr: u32,
) !u32 {
    assert(ptr != wisp.ZAP);

    const idx0 = wisp.pointerToIndex(ptr);
    const idx1 = switch (tag) {
        .cons => try self.copyCons(idx0),
        .string => try self.copyString(idx0),
        else => unreachable,
    };

    return wisp.makePointer(tag, idx1);
}

pub fn scavenge(self: *GC) !void {
    _ = self;

    var scan: usize = 0;
    while (scan < self.new.conses.len) : (scan += 1) {
        std.log.warn("scavenging {d} {d}", .{
            scan,
            self.new.conses.len,
        });

        const slice = self.new.conses.slice();
        const car = &slice.items(.car)[scan];
        const cdr = &slice.items(.cdr)[scan];

        car.* = try self.copy(car.*);
        cdr.* = try self.copy(cdr.*);

        std.log.warn("now {d}", .{self.new.conses.len});
    }
}

fn copyCons(self: *GC, oldIdx: u29) !u29 {
    std.log.warn("copy cons {d}", .{oldIdx});

    const oldCar = &self.oldSlices.conses.items(.car)[oldIdx];
    const oldCdr = &self.oldSlices.conses.items(.cdr)[oldIdx];

    if (oldCdr.* == wisp.ZAP) {
        return @intCast(u29, oldCar.*);
    } else {
        const cons = wisp.Cons{
            .car = oldCar.*,
            .cdr = oldCdr.*,
        };

        std.log.warn("cons {any}", .{cons});

        const newIdx = try self.new.allocCons(cons);

        std.log.warn("{any} {any}", .{
            self.new.conses.items(.car),
            self.new.conses.items(.cdr),
        });

        oldCar.* = @intCast(u32, newIdx);
        oldCdr.* = wisp.ZAP;

        return newIdx;
    }
}

fn copyString(self: *GC, oldIdx: u29) !u29 {
    const oldString = &self.old.strings.items[oldIdx];

    if (oldString.offset1 == wisp.ZAP) {
        return @intCast(u29, oldString.offset0);
    } else {
        const newIdx = try self.new.allocString(
            self.old.stringSlice(oldIdx),
        );

        oldString.* = .{
            .offset0 = newIdx,
            .offset1 = wisp.ZAP,
        };

        return newIdx;
    }
}

test "garbage collection of conses" {
    var data = try Data.init(std.testing.allocator);

    defer data.deinit();

    _ = try data.addCons(.{
        .car = wisp.encodeFixnum(1),
        .cdr = wisp.encodeFixnum(2),
    });

    const yData = wisp.Cons{
        .car = wisp.encodeFixnum(3),
        .cdr = wisp.encodeFixnum(4),
    };

    const y = try data.addCons(yData);

    var gc = try GC.init(&data);
    defer gc.deinit();

    const y2 = try gc.copy(y);

    try gc.scavenge();

    data = gc.finalize();

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
    defer gc.deinit();

    try gc.copyRoots();
    try gc.scavenge();

    var data = gc.finalize();

    try wisp.dumpData(&data, std.io.getStdErr().writer());

    const bar = try data.internString("X", 0);

    try expectEqual(foo, bar);

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
    var data = try Data.init(std.testing.allocator);

    defer data.deinit();

    const x = try read(&data,
        \\ ("foo" "bar" "baz")
    );

    const foo = try data.car(x);
    try data.symbolValues.put(
        data.gpa,
        try data.internString("X", 0),
        foo,
    );

    const stringCount1 = data.strings.items.len;

    var gc = try GC.init(&data);
    defer gc.new.deinit();
    defer gc.deinit();

    try gc.copyRoots();
    try gc.scavenge();

    data = gc.finalize();

    const stringCount2 = gc.new.strings.items.len;

    try expectEqual(stringCount1 - 2, stringCount2);
}
