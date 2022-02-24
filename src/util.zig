const std = @import("std");
const assert = std.debug.assert;

pub fn DeclEnum(comptime T: type, Tag: type) type {
    const fieldInfos = @typeInfo(T).Struct.decls;
    var enumFields: [fieldInfos.len]std.builtin.TypeInfo.EnumField = undefined;
    var decls = [_]std.builtin.TypeInfo.Declaration{};
    inline for (fieldInfos) |field, i| {
        enumFields[i] = .{
            .name = field.name,
            .value = i,
        };
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
