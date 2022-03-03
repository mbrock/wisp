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
const wisp = @import("./ff-wisp.zig");

export const wisp_tag_int = wisp.Tag.int;
export const wisp_tag_sys = wisp.Tag.sys;
export const wisp_tag_chr = wisp.Tag.chr;
export const wisp_tag_fop = wisp.Tag.fop;
export const wisp_tag_mop = wisp.Tag.mop;
export const wisp_tag_duo = wisp.Tag.duo;
export const wisp_tag_sym = wisp.Tag.sym;
export const wisp_tag_fun = wisp.Tag.fun;
export const wisp_tag_mac = wisp.Tag.mac;
export const wisp_tag_v32 = wisp.Tag.v32;
export const wisp_tag_v08 = wisp.Tag.v08;
export const wisp_tag_pkg = wisp.Tag.pkg;
export const wisp_tag_ct0 = wisp.Tag.ct0;
export const wisp_tag_ct1 = wisp.Tag.ct1;
export const wisp_tag_ct2 = wisp.Tag.ct2;
export const wisp_tag_ct3 = wisp.Tag.ct3;

export const wisp_sys_t: u32 = wisp.t;
export const wisp_sys_nil: u32 = wisp.nil;
export const wisp_sys_nah: u32 = wisp.nah;
export const wisp_sys_zap: u32 = wisp.zap;
export const wisp_sys_top: u32 = wisp.top;

export fn wisp_ctx_init() ?*wisp.Ctx {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    var orb: std.mem.Allocator = gpa.allocator();

    if (orb.create(wisp.Ctx)) |ctxptr| {
        if (wisp.Ctx.init(orb, .e0)) |ctx| {
            ctxptr.* = ctx;
            return ctxptr;
        } else |_| {
            return null;
        }
    } else |_| {
        return null;
    }
}

export fn wisp_read(ctx: *wisp.Ctx, str: [*:0]const u8) u32 {
    if (wisp.read(ctx, std.mem.span(str))) |x| {
        return x;
    } else |_| {
        return wisp.zap;
    }
}

fn Field(comptime name: []const u8, t: type) std.builtin.TypeInfo.StructField {
    return .{
        .name = name,
        .field_type = t,
        .default_value = null,
        .is_comptime = false,
        .alignment = 0,
    };
}

fn TabDat(tag: wisp.Tag) type {
    const n = std.meta.fields(wisp.Row(tag)).len;
    var fields: [1 + n]std.builtin.TypeInfo.StructField = undefined;

    fields[0] = Field("n", u32);

    for (std.meta.fields(wisp.Row(tag))) |field, i| {
        fields[1 + i] = Field(field.name, usize);
    }

    const t = @Type(std.builtin.TypeInfo{
        .Struct = .{
            .layout = .Packed,
            .fields = &fields,
            .decls = &[_]std.builtin.TypeInfo.Declaration{},
            .is_tuple = false,
        },
    });

    return t;
}

const Dat = packed struct {
    duo: TabDat(.duo),
    sym: TabDat(.sym),
    fun: TabDat(.fun),
    mac: TabDat(.mac),
    v08: TabDat(.v08),
    v32: TabDat(.v32),
    pkg: TabDat(.pkg),
    ct0: TabDat(.ct0),
    ct1: TabDat(.ct1),
    ct2: TabDat(.ct2),
    ct3: TabDat(.ct3),
};

export fn wisp_dat_init(ctx: *wisp.Ctx) ?*Dat {
    return ctx.orb.create(Dat) catch null;
}

export fn wisp_dat_read(ctx: *wisp.Ctx, dat: *Dat) void {
    inline for (wisp.pointerTags) |tag| {
        const E = std.meta.FieldEnum(wisp.Row(tag));
        const tab = ctx.tab(tag);
        var tagdat = &@field(dat, @tagName(tag));
        tagdat.n = @intCast(u32, tab.list.len);
        inline for (std.meta.fields(wisp.Row(tag))) |field, i| {
            const slice = tab.list.slice();
            @field(tagdat, field.name) = @ptrToInt(slice.items(@intToEnum(E, i)).ptr);
        }
    }
}

test "sanity" {
    try std.testing.expectEqual(0x88000000, wisp_sys_nil);
}
