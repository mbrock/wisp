const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable(
        "wisp",
        "src/main.zig",
    );

    exe.addPackagePath("ziglyph", "vendor/ziglyph/src/ziglyph.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const wasm = b.addSharedLibrary(
        "wisp",
        "src/main.zig",
        .unversioned,
    );

    wasm.setTarget(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });

    wasm.addPackagePath("ziglyph", "vendor/ziglyph/src/ziglyph.zig");
    wasm.setBuildMode(mode);
    wasm.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/main.zig");
    exe_tests.addPackagePath(
        "ziglyph",
        "vendor/ziglyph/src/ziglyph.zig",
    );
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);
}
