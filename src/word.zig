//
// This file is part of Wisp.
//
// Wisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// Wisp is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
// or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
// Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with Wisp. If not, see
// <https://www.gnu.org/licenses/>.
//

const std = @import("std");
const assert = std.debug.assert;
const same = std.testing.expectEqual;

const util = @import("./util.zig");

pub const Tag = enum(u5) {
    int,

    sys = 0x11,
    chr = 0x12,
    fop = 0x13,
    mop = 0x14,

    duo = 0x15,
    sym = 0x16,
    fun = 0x17,
    vec = 0x18,
    str = 0x19,
    pkg = 0x1a,
    ct0 = 0x1b,
};

pub const Era = enum(u1) { e0, e1 };

pub const Ptr = packed struct {
    era: Era,
    idx: u26,
    tag: Tag,

    pub fn make(tag: Tag, idx: u26, era: Era) Ptr {
        return .{ .era = era, .idx = idx, .tag = tag };
    }

    pub fn from(x: u32) Ptr {
        return @bitCast(Ptr, x);
    }

    pub fn word(ptr: Ptr) u32 {
        return @bitCast(u32, ptr);
    }
};

pub const Imm = packed struct {
    idx: u27,
    tag: Tag,

    pub fn make(tag: Tag, idx: u26) Imm {
        return .{ .idx = idx, .tag = tag };
    }

    pub fn from(x: u32) Imm {
        return @bitCast(Imm, x);
    }

    pub fn word(imm: Imm) u32 {
        return @bitCast(u32, imm);
    }
};

pub const nil = (Imm{ .tag = .sys, .idx = 0 }).word();
pub const zap = (Imm{ .tag = .sys, .idx = 1 }).word();

test "nil, zap" {
    try same(0b10001000000000000000000000000000, nil);
    try same(0b10001000000000000000000000000001, zap);
}

pub fn tagOf(x: u32) Tag {
    return if (x & (1 << 31) == 0)
        .int
    else
        Ptr.from(x).tag;
}
