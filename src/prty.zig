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

const Wisp = @import("./wisp.zig");

pub const Box = struct {
    ptr: u32,

    pub fn make(
        ctx: *Wisp.Ctx,
        len: u32,
        fin: u32,
        max: u32,
        txt: u32,
    ) !Box {
        return try ctx.newv32(.{ len, fin, max, txt });
    }
};
