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
const assert = std.debug.assert;

pub fn DeclEnum(comptime T: type, Tag: type) type {
    const fieldInfos = @typeInfo(T).Struct.decls;

    var pubs = 0;
    inline for (fieldInfos) |field| {
        if (field.is_pub) {
            pubs += 1;
        }
    }

    var enumFields: [pubs]std.builtin.TypeInfo.EnumField = undefined;
    var decls = [_]std.builtin.TypeInfo.Declaration{};
    var i = 0;
    inline for (fieldInfos) |field| {
        if (field.is_pub) {
            enumFields[i] = .{
                .name = field.name,
                .value = i,
            };
            i += 1;
        }
    }

    return @Type(.{
        .Enum = .{
            .layout = .Auto,
            .tag_type = Tag,
            .fields = &enumFields,
            .decls = &decls,
            .is_exhaustive = true,
        },
    });
}
