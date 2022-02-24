const std = @import("std");
const EnumArray = std.enums.EnumArray;
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const testGpa = std.testing.allocator;

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const printer = @import("./print.zig");

const Heap = wisp.Heap;

const GC = @This();

const Progress = EnumArray(wisp.Kind, usize);

old: *Heap,
new: Heap,
progress: Progress = Progress.initFill(0),

pub fn tidy(heap: *Heap) !void {
    var gc = try GC.init(heap);
    defer gc.deinit();
    try gc.copyRoots();
    try gc.scavenge();
    heap.* = gc.finalize();
}

pub fn init(old: *Heap) !GC {
    var new = Heap{
        .semispace = old.semispace.other(),
        .gpa = old.gpa,
        .stringBytes = old.stringBytes,
    };

    return GC{
        .old = old,
        .new = new,
    };
}

pub fn deinit(self: *GC) void {
    _ = self;
}

fn finalize(self: *GC) Heap {
    self.old.stringBytes = .{};
    self.old.deinit();
    return self.new;
}

fn copyRoots(self: *GC) !void {
    self.new.data.package = try self.old.data.package.clone(self.new.gpa);
}

fn copy(self: *GC, x: u32) !u32 {
    return switch (wisp.Word.from(x)) {
        .immediate => x,
        .pointer => |pointer| switch (pointer) {
            .symbol => self.copyValue(.symbol, x),
            .string => self.copyValue(.string, x),
            .cons => self.copyValue(.cons, x),
            else => |it| {
                std.log.warn("unexpected {any}", .{it});
                unreachable;
            },
        },
    };
}

fn copyValue(self: *GC, comptime kind: wisp.Kind, ptr: u32) !u32 {
    if (wisp.Semispace.of(ptr) == self.new.semispace) {
        return ptr;
    }

    const oldContainer = self.old.kindContainer(kind);

    const Field = std.meta.FieldEnum(kind.valueType());
    const field0 = @intToEnum(Field, 0);
    const field1 = @intToEnum(Field, 1);

    const i = self.old.getOffset(kind, ptr);
    const oldSlice = oldContainer.slice();

    if (oldSlice.items(field1)[i] == wisp.ZAP) {
        return oldSlice.items(field0)[i];
    }

    const newContainer = self.new.kindContainer(kind);
    const oldValue = oldContainer.get(i);
    try newContainer.append(self.new.gpa, oldValue);

    const newOffset = @intCast(wisp.payloadType(kind), newContainer.len) - 1;

    const newPointer = self.new.makePointer(
        @unionInit(wisp.Pointer, @tagName(kind), newOffset),
    );

    oldSlice.items(field0)[i] = newPointer;
    oldSlice.items(field1)[i] = wisp.ZAP;

    return newPointer;
}

pub fn scavenge(self: *GC) !void {
    while (!self.isDone()) {
        inline for (wisp.allKinds) |x| {
            try self.scavengeValues(x);
        }
    }
}

fn isDone(self: *GC) bool {
    inline for (wisp.allKinds) |x| {
        if (self.progress.get(x) < self.new.kindContainer(x).len) {
            return false;
        }
    }

    return true;
}

/// Go through all the new values of a specific kind added since the
/// last scavenge round, updating every field of every value.
fn scavengeValues(self: *GC, comptime kind: wisp.Kind) !void {
    var i = self.progress.get(kind);
    const container = self.new.kindContainer(kind);

    while (i < container.len) {
        try self.scavengeFields(kind, container, i);
        i += 1;
    }

    self.progress.set(kind, i);
}

fn scavengeFields(
    self: *GC,
    comptime kind: wisp.Kind,
    container: *kind.containerType(),
    i: usize,
) !void {
    const t = kind.valueType();

    inline for (std.meta.fields(t)) |_, j| {
        const field = @intToEnum(std.meta.FieldEnum(t), j);
        const new = try self.copy(container.items(field)[i]);

        // The copying may have reallocated the container, so we
        // can't cache the field pointer.
        container.items(field)[i] = new;
    }
}

test "garbage collection of conses" {
    var heap = try Heap.init(testGpa);

    defer heap.deinit();

    _ = try heap.append(.cons, .{
        .car = wisp.encodeFixnum(1),
        .cdr = wisp.encodeFixnum(2),
    });

    const cons = wisp.Cons{
        .car = wisp.encodeFixnum(3),
        .cdr = wisp.encodeFixnum(4),
    };

    const cons1 = try heap.append(.cons, cons);

    var gc = try GC.init(&heap);
    defer gc.deinit();

    const cons2 = try gc.copy(cons1);

    try gc.scavenge();

    heap = gc.finalize();

    try expectEqual(heap.data.cons.len, 1);
    try expectEqual(cons, try heap.deref(.cons, cons2));
}

test "read and gc" {
    var heap = try Heap.init(testGpa);
    defer heap.deinit();

    const t1 = try read(&heap, "(foo (bar (baz)))");
    const v1 = try heap.internStringInBasePackage("X");

    (try heap.symbolValue(v1)).* = t1;
    try tidy(&heap);

    try expectEqual(wisp.Semispace.space1, heap.semispace);

    const v2 = try heap.internStringInBasePackage("X");
    const t2 = (try heap.symbolValue(v2)).*;

    try printer.expect("(FOO (BAR (BAZ)))", &heap, t2);
}

test "gc ephemeral strings" {
    var heap = try Heap.init(testGpa);

    defer heap.deinit();

    const x = try read(&heap,
        \\ ("foo" "bar" "baz")
    );

    const foo = try heap.car(x);
    (try heap.symbolValue(
        try heap.internStringInBasePackage("X"),
    )).* = foo;

    const stringCount1 = heap.data.string.len;

    try tidy(&heap);

    const stringCount2 = heap.data.string.len;

    try expectEqual(stringCount1 - 2, stringCount2);
}
