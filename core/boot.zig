const std = @import("std");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var orb = gpa.allocator();

    var heap = try @import("./main.zig").makeHeap(orb);
    defer heap.deinit();

    try @import("./tidy.zig").gc(&heap, &.{});
    try @import("./tape.zig").save(&heap, "boot.core");
}
