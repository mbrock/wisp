const std = @import("std");

fn wispStep(
    mode: std.builtin.Mode,
    target: std.zig.CrossTarget,
    step: *std.build.LibExeObjStep,
) *std.build.LibExeObjStep {
    step.addPackagePath("ziglyph", "vendor/ziglyph/src/ziglyph.zig");
    step.setTarget(target);
    step.setBuildMode(mode);
    return step;
}

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();
    const standardTarget = b.standardTargetOptions(.{});
    const wasiTarget = std.zig.CrossTarget{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    };

    const exe = wispStep(mode, standardTarget, b.addExecutable(
        "wisp",
        "src/repl.zig",
    ));

    const wasmExe = wispStep(mode, wasiTarget, b.addExecutable(
        "wisp",
        "src/repl.zig",
    ));

    const wasmLib = wispStep(mode, wasiTarget, b.addSharedLibrary(
        "wisp",
        "src/wasm.zig",
        .unversioned,
    ));

    const tests = wispStep(mode, standardTarget, b.addTest(
        "src/repl.zig",
    ));

    exe.install();
    wasmExe.install();
    wasmLib.install();

    const testStep = b.step("test", "Run unit tests");
    testStep.dependOn(&tests.step);

    const runCmd = exe.run();
    runCmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        runCmd.addArgs(args);
    }

    const runStep = b.step("run", "Run the Wisp REPL");
    runStep.dependOn(&runCmd.step);
}
