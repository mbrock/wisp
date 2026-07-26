const std = @import("std");
const builtin = @import("builtin");

var process_io: std.Io = std.Io.failing;

pub fn setIo(new_io: std.Io) void {
    process_io = new_io;
}

pub fn io() std.Io {
    if (builtin.is_test) return std.testing.io;
    return process_io;
}
