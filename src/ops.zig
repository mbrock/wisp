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
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const EnumArray = std.enums.EnumArray;

const wisp = @import("./wisp.zig");
const util = @import("./util.zig");

const ref = wisp.ref;
const Vat = wisp.Vat;
const Ptr = wisp.Ptr;
const DeclEnum = util.DeclEnum;

pub const FnTag = enum {
    f0x,
    f2,

    pub fn from(comptime T: type) FnTag {
        return switch (T) {
            fn (*Vat, []u32) anyerror!u32 => .f0x,
            fn (*Vat, u32, u32) anyerror!u32 => .f2,
            else => @compileLog("unhandled op type", T),
        };
    }

    pub fn functionType(comptime self: FnTag) type {
        return switch (self) {
            .f0x => fn (*Vat, []u32) anyerror!u32,
            .f2 => fn (*Vat, u32, u32) anyerror!u32,
        };
    }

    pub fn cast(comptime this: FnTag, x: anytype) this.functionType() {
        return @ptrCast(this.functionType(), x);
    }
};

pub const Op = struct {
    name: []const u8,
    tag: FnTag,
    func: *const anyopaque,
};

pub const Fops = struct {
    pub fn @"+"(vat: *Vat, xs: []u32) anyerror!u32 {
        _ = vat;

        var result: i31 = 0;
        for (xs) |x| {
            result += @intCast(i31, x);
        }

        return @intCast(u32, result);
    }
};

pub const Mops = struct {
    pub fn @"FOO"(vat: *Vat, x: u32, y: u32) anyerror!u32 {
        _ = vat;
        return try vat.new(.duo, .{
            .car = x,
            .cdr = try vat.new(.duo, .{
                .car = y,
                .cdr = try vat.new(.duo, .{
                    .car = 1,
                    .cdr = wisp.nil,
                }),
            }),
        });
    }
};

pub const FopTag = DeclEnum(Fops, u27);
pub const MopTag = DeclEnum(Mops, u27);

pub const fops = makeOpArray(FopTag, Fops);
pub const mops = makeOpArray(MopTag, Mops);

fn makeOpArray(comptime T: type, comptime S: type) EnumArray(T, Op) {
    var ops = EnumArray(T, Op).initUndefined();

    inline for (@typeInfo(T).Enum.fields) |x| {
        const f = @field(S, x.name);
        ops.set(@intToEnum(T, x.value), .{
            .name = x.name,
            .tag = FnTag.from(@TypeOf(f)),
            .func = f,
        });
    }

    return ops;
}

test "ops" {
    try expectEqual(
        @ptrCast(*const anyopaque, Fops.@"+"),
        fops.get(.@"+").func,
    );
}

pub fn load(vat: *Vat) !void {
    inline for (fops.values) |fop, i| {
        var sym = try vat.intern(fop.name, vat.base());
        vat.col(.sym, .fun)[ref(sym)] = wisp.Imm.make(.fop, i).word();
    }

    inline for (mops.values) |mop, i| {
        var sym = try vat.intern(mop.name, vat.base());
        vat.col(.sym, .fun)[ref(sym)] = wisp.Imm.make(.mop, i).word();
    }
}
