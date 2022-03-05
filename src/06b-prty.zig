const wisp = @import("./ff-wisp.zig");

pub const Box = struct {
    ptr: u32,

    pub fn make(
        ctx: *wisp.Ctx,
        len: u32,
        fin: u32,
        max: u32,
        txt: u32,
    ) !Box {
        return try ctx.newv32(.{ len, fin, max, txt });
    }
};
