const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const standardTarget = b.standardTargetOptions(.{});
    const wasiTarget = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });

    const boot = b.addExecutable(.{
        .name = "wisp-mkboot",
        .root_source_file = b.path("boot.zig"),
        .target = standardTarget,
    });

    const bootRun = b.addRunArtifact(boot);

    const exe = b.addExecutable(.{
        .name = "wisp",
        .root_source_file = b.path("main.zig"),
        .target = standardTarget,
    });

    const wasmExe = b.addExecutable(.{
        .name = "wisp",
        .root_source_file = b.path("main.zig"),
        .target = wasiTarget,
    });

    const wasmLib = b.addSharedLibrary(.{
        .name = "wisp",
        .root_source_file = b.path("wasm.zig"),
        .target = wasiTarget,
        .optimize = optimize,
    });

    const tests = b.addTest(.{
        .root_source_file = b.path("repl.zig"),
        .target = standardTarget,
    });

    const testsPrty = b.addTest(.{
        .root_source_file = b.path("sexp-prty.zig"),
        .target = standardTarget,
    });

    tests.step.dependOn(&bootRun.step);
    exe.step.dependOn(&bootRun.step);
    wasmExe.step.dependOn(&bootRun.step);

    b.installArtifact(exe);
    b.installArtifact(wasmExe);
    b.installArtifact(wasmLib);

    const testStep = b.step("test", "Run unit tests");
    testStep.dependOn(&tests.step);

    const testPrtyStep = b.step("test-prty", "Run tests for Prty");
    testPrtyStep.dependOn(&testsPrty.step);

    const runCmd = b.addRunArtifact(exe);
    runCmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        runCmd.addArgs(args);
    }

    const runStep = b.step("run", "Run the Wisp REPL");
    runStep.dependOn(&runCmd.step);
}
