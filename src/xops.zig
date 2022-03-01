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

pub const Fops = @import("./fops.zig");
pub const Mops = @import("./mops.zig");

pub const FopTag = DeclEnum(Fops, u27);
pub const MopTag = DeclEnum(Mops, u27);

pub const fops = makeOpArray(FopTag, Fops);
pub const mops = makeOpArray(MopTag, Mops);

const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const EnumArray = std.enums.EnumArray;

const wisp = @import("./wisp.zig");
const util = @import("./util.zig");

const ref = wisp.ref;
const Ctx = wisp.Ctx;
const Ptr = wisp.Ptr;
const DeclEnum = util.DeclEnum;

pub const FnTag = enum {
    f0x,
    f1,
    f2,

    pub fn from(comptime T: type) FnTag {
        return switch (T) {
            fn (*Ctx, []u32) anyerror!u32 => .f0x,
            fn (*Ctx, u32) anyerror!u32 => .f1,
            fn (*Ctx, u32, u32) anyerror!u32 => .f2,
            else => @compileLog("unhandled op type", T),
        };
    }

    pub fn functionType(comptime self: FnTag) type {
        return switch (self) {
            .f0x => fn (*Ctx, []u32) anyerror!u32,
            .f2 => fn (*Ctx, u32, u32) anyerror!u32,
            .f1 => fn (*Ctx, u32) anyerror!u32,
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

pub fn load(ctx: *Ctx) !void {
    inline for (fops.values) |fop, i| {
        var sym = try ctx.intern(fop.name, ctx.base);
        ctx.col(.sym, .fun)[ref(sym)] = wisp.Imm.make(.fop, i).word();
    }

    inline for (mops.values) |mop, i| {
        var sym = try ctx.intern(mop.name, ctx.base);
        ctx.col(.sym, .fun)[ref(sym)] = wisp.Imm.make(.mop, i).word();
    }
}
