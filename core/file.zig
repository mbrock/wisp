// -*- fill-column: 64; -*-
//
// This file is part of Wisp.
//
// Wisp is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License
// as published by the Free Software Foundation, either version
// 3 of the License, or (at your option) any later version.
//
// Wisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General
// Public License along with Wisp. If not, see
// <https://www.gnu.org/licenses/>.
//

const std = @import("std");

const Wisp = @import("./wisp.zig");

pub fn cwd(allocator: std.mem.Allocator) !std.fs.Dir {
    if (@import("builtin").os.tag == .wasi) {
        var preopens = try std.fs.wasi.preopensAlloc(allocator);

        if (preopens.find(".")) |x| {
            return std.fs.Dir{ .fd = x };
        } else {
            return Wisp.Oof.Err;
        }
    } else {
        return std.fs.cwd();
    }
}

pub fn readLine(
    allocator: std.mem.Allocator,
    stream: *std.Io.Reader,
) !?[]u8 {
    var w = try std.Io.Writer.Allocating.initCapacity(allocator, 80);
    if (stream.streamDelimiter(&w.writer, '\n')) |_| {
        return try w.toOwnedSlice();
    } else |err| switch (err) {
        error.EndOfStream => return null,
        else => |e| return e,
    }
}
