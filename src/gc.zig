const std = @import("std");
const EnumArray = std.enums.EnumArray;
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const testGpa = std.testing.allocator;

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const printer = @import("./print.zig");

const Data = wisp.Data;

const GC = @This();

const Kind = std.meta.FieldEnum(Data.Slices);
const allKinds = std.enums.values(Kind);

pub fn tidy(data: *Data) !void {
    var gc = try GC.init(data);
    defer gc.deinit();
    try gc.copyRoots();
    try gc.scavenge();
    data.* = gc.finalize();
}

old: *Data,
new: Data,

scans: EnumArray(Kind, usize) = EnumArray(Kind, usize).initFill(0),

pub fn init(old: *Data) !GC {
    var new = Data{
        .semispace = old.semispace.other(),
        .gpa = old.gpa,
        .stringBytes = old.stringBytes,
    };

    return GC{
        .old = old,
        .new = new,
    };
}

pub fn finalize(self: *GC) Data {
    self.old.stringBytes = .{};
    self.old.deinit();
    return self.new;
}

pub fn deinit(self: *GC) void {
    _ = self;
}

pub fn copyRoots(self: *GC) !void {
    self.new.packages = try self.old.packages.clone(self.new.gpa);
}

/// Go through all the values in the new data and look at every
/// pointer field.  When we see a pointer to an old value, we copy
/// that value into the new data and update the pointer.  Loop until
/// there are no more new values.
pub fn scavenge(self: *GC) !void {
    var keepGoing = true;
    while (keepGoing) {
        keepGoing = false;
        inline for (allKinds) |x| {
            if (try self.scavengeKind(x)) {
                keepGoing = true;
            }
        }
    }
}

/// Go through all the new values of a specific kind added since the
/// last scavenge round, updating every field of every value.
fn scavengeKind(self: *GC, comptime kind: Kind) !bool {
    const t = kindType(kind);
    const newContainer = kindContainer(&self.new, kind);
    var i = self.scans.get(kind);

    if (i == newContainer.len) {
        return false;
    }

    while (i < newContainer.len) {
        inline for (std.meta.fields(t)) |_, j| {
            const field = @intToEnum(std.meta.FieldEnum(t), j);
            const new = try self.copy(newContainer.items(field)[i]);

            // The copying may have reallocated the container, so we
            // can't cache the field pointer.
            newContainer.items(field)[i] = new;
        }

        i += 1;
    }

    self.scans.set(kind, i);

    return true;
}

fn copy(self: *GC, x: u32) !u32 {
    const tag = wisp.type1(x);
    return switch (tag) {
        .nil, .fixnum => x,
        .symbol => self.copyValueOfKind(.symbol, x),
        .string => self.copyValueOfKind(.string, x),
        .cons => self.copyValueOfKind(.cons, x),

        else => {
            std.log.warn("unexpected {any}", .{tag});
            unreachable;
        },
    };
}

fn kindType(comptime kind: Kind) type {
    return switch (kind) {
        .conses => wisp.Cons,
        .symbols => wisp.Symbol,
        .packages => wisp.Package,
        .strings => wisp.String,
    };
}

fn kindContainer(
    data: *Data,
    comptime kind: Kind,
) *std.MultiArrayList(kindType(kind)) {
    return &@field(data.*, @tagName(kind));
}

fn copyValueOfKind(self: *GC, comptime tag: wisp.Tag1, ptr: u32) !u32 {
    if (wisp.Semispace.of(ptr) == self.new.semispace) {
        return ptr;
    }

    const kind: Kind = switch (tag) {
        .cons => .conses,
        .symbol => .symbols,
        .package => .packages,
        .string => .strings,
        else => unreachable,
    };

    const oldContainer = kindContainer(self.old, kind);

    const Field = std.meta.FieldEnum(kindType(kind));
    const field0 = @intToEnum(Field, 0);
    const field1 = @intToEnum(Field, 1);

    const i = self.old.pointerToIndex(ptr);
    const oldSlice = oldContainer.slice();

    if (oldSlice.items(field1)[i] == wisp.ZAP) {
        return oldSlice.items(field0)[i];
    }

    const newContainer = kindContainer(&self.new, kind);
    const oldValue = oldContainer.get(i);
    try newContainer.append(self.new.gpa, oldValue);

    const newPointer = self.new.makePointer(
        tag,
        @intCast(u29, newContainer.len) - 1,
    );

    oldSlice.items(field0)[i] = newPointer;
    oldSlice.items(field1)[i] = wisp.ZAP;

    return newPointer;
}

test "garbage collection of conses" {
    var data = try Data.init(testGpa);

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
    var data = try Data.init(testGpa);
    defer data.deinit();

    const t1 = try read(&data, "(foo (bar (baz)))");
    const v1 = try data.internString("X", data.makePointer(.package, 0));

    (try data.symbolValue(v1)).* = t1;
    try tidy(&data);

    const v2 = try data.internString("X", data.makePointer(.package, 0));
    const t2 = (try data.symbolValue(v2)).*;

    try printer.expect("(FOO (BAR (BAZ)))", data, t2);
}

test "gc ephemeral strings" {
    var data = try Data.init(testGpa);

    defer data.deinit();

    const x = try read(&data,
        \\ ("foo" "bar" "baz")
    );

    const foo = try data.car(x);
    (try data.symbolValue(
        try data.internString("X", 0),
    )).* = foo;

    const stringCount1 = data.strings.len;

    try tidy(&data);

    const stringCount2 = data.strings.len;

    try expectEqual(stringCount1 - 2, stringCount2);
}
