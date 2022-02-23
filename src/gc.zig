const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const testGpa = std.testing.allocator;

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const printer = @import("./print.zig");

const Data = wisp.Data;

const GC = @This();

const Thing = std.meta.FieldEnum(Data.Slices);
const thingCount = @typeInfo(Thing).Enum.fields.len;

pub fn tidy(data: *Data) !void {
    var gc = try GC.init(data);
    defer gc.deinit();
    try gc.copyRoots();
    try gc.scavenge();
    data.* = gc.finalize();
}

old: *Data,
new: Data,

scans: [thingCount]usize = .{0} ** thingCount,

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

pub fn scavenge(self: *GC) !void {
    var done = false;
    while (!done) {
        done = true;
        done = (try self.scavengeThing(.symbols)) and done;
        done = (try self.scavengeThing(.packages)) and done;
        done = (try self.scavengeThing(.conses)) and done;
    }
}

fn scavengeThing(self: *GC, comptime thing: Thing) !bool {
    const t = thingType(thing);
    const scan: *usize = &self.scans[@enumToInt(thing)];
    const newContainer = thingContainer(&self.new, thing);

    var done = true;

    var i = scan.*;

    while (i < newContainer.len) : (i += 1) {
        done = false;

        inline for (std.meta.fields(t)) |_, j| {
            const field = @intToEnum(std.meta.FieldEnum(t), j);
            const new = try self.copy(newContainer.items(field)[i]);

            // The copying may have reallocated the container, so we
            // can't cache the field pointer.
            newContainer.items(field)[i] = new;
        }
    }

    scan.* = i;

    return done;
}

fn copy(self: *GC, x: u32) !u32 {
    const tag = wisp.type1(x);
    return switch (tag) {
        .nil, .fixnum => x,
        .symbol => self.copyOldThing(.symbol, x),
        .string => self.copyOldThing(.string, x),
        .cons => self.copyOldThing(.cons, x),

        else => {
            std.log.warn("unexpected {any}", .{tag});
            unreachable;
        },
    };
}

fn thingType(comptime thing: Thing) type {
    return switch (thing) {
        .conses => wisp.Cons,
        .symbols => wisp.Symbol,
        .packages => wisp.Package,
        .strings => wisp.String,
    };
}

fn thingContainer(
    data: *Data,
    comptime thing: Thing,
) *std.MultiArrayList(thingType(thing)) {
    return &@field(data.*, @tagName(thing));
}

fn copyOldThing(self: *GC, comptime tag: wisp.Tag1, ptr: u32) !u32 {
    if (wisp.Semispace.of(ptr) == self.new.semispace) {
        return ptr;
    }

    const thing: Thing = switch (tag) {
        .cons => .conses,
        .symbol => .symbols,
        .package => .packages,
        .string => .strings,
        else => unreachable,
    };

    const oldContainer = thingContainer(self.old, thing);

    const Field = std.meta.FieldEnum(thingType(thing));
    const field0 = @intToEnum(Field, 0);
    const field1 = @intToEnum(Field, 1);

    const i = self.old.pointerToIndex(ptr);
    const oldSlice = oldContainer.slice();

    if (oldSlice.items(field1)[i] == wisp.ZAP) {
        return oldSlice.items(field0)[i];
    }

    const newContainer = thingContainer(&self.new, thing);
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
