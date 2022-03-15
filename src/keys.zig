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

pub const zb32AlphabetUpper = "YBNDRFG8EJKMCPQXOT1UWISZA345H769";
pub const zb32AlphabetLower = "ybndrfg8ejkmcpqxot1uwisza345h769";

pub const zb32AlphabetMap: [256]i6 = blk: {
    var map: [256]i6 = .{-1} ** 256;
    for (zb32AlphabetUpper) |c, i| map[c] = i;
    for (zb32AlphabetLower) |c, i| map[c] = i;
    break :blk map;
};

/// Days between 1970-01-01 and 2022-01-01.
pub const epochDaysSince1970 = 18993;

pub const Key = packed struct {
    day: u16,
    rnd: u48,

    pub fn of(day: u16, rnd: u48) Key {
        return .{ .day = day, .rnd = rnd };
    }

    pub fn rndToZB32(key: Key) [10]u8 {
        var rndS: [10]u8 = undefined;
        const rnd = @intCast(u64, key.rnd);
        inline for (&[_]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }) |i| {
            const j: u5 = @truncate(u5, (rnd >> (5 * i)));
            rndS[i] = zb32AlphabetUpper[j];
        }

        return rndS;
    }

    pub fn toZB32(key: Key) [20:0]u8 {
        const unixDay = std.time.epoch.EpochDay{
            .day = epochDaysSince1970 + key.day,
        };

        const yearAndDay = unixDay.calculateYearDay();
        const monthAndDay = yearAndDay.calculateMonthDay();

        const rndS = rndToZB32(key);

        var keyS: [20:0]u8 = undefined;
        _ = std.fmt.bufPrint(
            &keyS,
            "~{d:0>4}{d:0>2}{d:0>2}.{s}",
            .{
                yearAndDay.year,
                monthAndDay.month.numeric(),
                monthAndDay.day_index + 1,
                rndS,
            },
        ) catch unreachable;

        return keyS;
    }
};

pub fn generate(rng: *const std.rand.Random) Key {
    const now: i64 = std.time.timestamp();
    const sec = @intCast(
        u16,
        @divFloor(now, 60 * 60 * 24) - epochDaysSince1970,
    );

    const rnd = rng.int(u48);
    return Key.of(sec, rnd);
}

pub fn parse(str: []const u8) !Key {
    if (str.len != 20) return error.BadKey;
    if (str[0] != '~') return error.BadKey;
    if (str[9] != '.') return error.BadKey;

    // ~20220314.8NJAFJ7WJF
    // 0123456789a

    const day = try parseDate(str[1..9]);
    const rnd = try parseZB32(str[10..]);

    return Key{ .day = day, .rnd = rnd };
}

pub fn parseZB32Char(c: u8) !u5 {
    const x = zb32AlphabetMap[c];
    return if (x >= 0) @intCast(u5, x) else error.BadZB32Char;
}

pub fn parseZB32(str: []const u8) !u48 {
    if (str.len != 10) return error.BadKey;

    var xs: [10]u5 = undefined;

    for (str) |c, i| {
        xs[i] = try parseZB32Char(c);
    }

    var rnd: u48 = 0;
    inline for (&[_]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }) |i| {
        rnd += @intCast(u48, xs[i]) << (5 * i);
    }

    return rnd;
}

pub fn parseDate(str: []const u8) !u16 {
    const yyyy = str[0..4];
    const mm = str[4..6];
    const dd = str[6..8];

    const year = try std.fmt.parseUnsigned(u16, yyyy, 10);

    if (year < 2020) return error.KeyPrecedesWispEpoch;

    const month = try std.fmt.parseUnsigned(u4, mm, 10);
    const day = try std.fmt.parseUnsigned(u5, dd, 10);

    var epochDay: u16 = 0;

    var yearX: u16 = 2022;
    while (yearX < year) : (yearX += 1) {
        std.log.warn("year {d} adding {d}", .{
            yearX,
            std.time.epoch.getDaysInYear(yearX),
        });
        epochDay += std.time.epoch.getDaysInYear(yearX);
    }

    const leap: std.time.epoch.YearLeapKind =
        if (std.time.epoch.isLeapYear(year)) .leap else .not_leap;

    var monthX: u5 = 1;
    while (monthX < month) : (monthX += 1) {
        epochDay += std.time.epoch.getDaysInMonth(
            leap,
            @intToEnum(std.time.epoch.Month, monthX),
        );
    }

    epochDay += day - 1;

    return epochDay;
}

test "key zb32" {
    const keyA = Key{
        .day = 72,
        .rnd = 0xa9a7525c2447,
    };

    try std.testing.expectEqualStrings(
        "~20220314.8NJAFJ7WJF",
        &keyA.toZB32(),
    );

    const parsedKeyA = try parse("~20220314.8njafj7wjf");
    try std.testing.expectEqual(keyA.rnd, parsedKeyA.rnd);
    try std.testing.expectEqual(keyA.day, parsedKeyA.day);

    const keyB = generate(&std.crypto.random);
    const keyC = generate(&std.crypto.random);

    try std.testing.expectApproxEqAbs(
        @intToFloat(f32, keyB.day),
        @intToFloat(f32, keyC.day),
        1,
    );

    try std.testing.expect(keyB.rnd != keyC.rnd);
}
