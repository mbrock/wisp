const std = @import("std");

pub fn main(init: std.process.Init) anyerror!void {
    const orb = init.gpa;
    const cap = init.io;

    var heap = try @import("./main.zig").makeHeap(orb, cap);
    defer heap.deinit();

    try @import("./tidy.zig").gc(&heap, &.{});
    try @import("./tape.zig").save(&heap, "boot.core");
}
