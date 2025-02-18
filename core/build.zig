const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const standardTarget = b.standardTargetOptions(.{});
    const wasiTarget = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });

    const ziglyph = b.dependency("ziglyph", .{
        .optimize = optimize,
        .target = standardTarget,
    });

    const boot = b.addExecutable(.{
        .name = "wisp-mkboot",
        .root_source_file = b.path("boot.zig"),
        .target = standardTarget,
    });

    boot.root_module.addImport("ziglyph", ziglyph.module("ziglyph"));

    const bootRun = b.addRunArtifact(boot);

    const exe = b.addExecutable(.{
        .name = "wisp",
        .root_source_file = b.path("main.zig"),
        .target = standardTarget,
    });

    exe.root_module.addImport("ziglyph", ziglyph.module("ziglyph"));

    // const wasmExe = b.addExecutable(.{
    //     .name = "wisp",
    //     .root_source_file = b.path("main.zig"),
    //     .target = wasiTarget,
    // });

    // wasmExe.root_module.addImport("ziglyph", ziglyph.module("ziglyph"));

    const wasmLib = b.addExecutable(.{
        .name = "wisp",
        .root_source_file = b.path("wasm.zig"),
        .target = wasiTarget,
        .optimize = optimize,
    });

    wasmLib.entry = .disabled;
    wasmLib.root_module.export_symbol_names = &[_][]const u8{
        "wisp_sys_t",
        "wisp_sys_nil",
        "wisp_sys_nah",
        "wisp_sys_zap",
        "wisp_sys_top",
        "wisp_heap_init",
        "wisp_read",
        "wisp_read_many",
        "wisp_eval",
        "wisp_run_init",
        "wisp_run_eval",
        "wisp_eval_step",
        "wisp_run_restart",
        "wisp_dat_init",
        "wisp_dat_read",
        "wisp_heap_load_v08",
        "wisp_heap_load_v32",
        "wisp_heap_load_tab_col",
        "wisp_heap_new_ext",
        "wisp_heap_v08_new",
        "wisp_heap_v32_new",
        "wisp_heap_get_ext_idx",
        "wisp_heap_free_pin",
        "wisp_heap_get_v08_ptr",
        "wisp_heap_get_v08_len",
        "wisp_heap_get_v32_ptr",
        "wisp_heap_get_v32_len",
        "wisp_heap_v08_len",
        "wisp_heap_v08_ptr",
        "wisp_heap_v32_len",
        "wisp_heap_v32_ptr",
        "wisp_alloc",
        "wisp_free_0",
        "wisp_free_n",
        "wisp_jet_name",
        "wisp_jet_name_len",
        "wisp_genkey",
        "wisp_tape_save",
        "wisp_call",
        "wisp_cons",
        "wisp_call_package_function",
        "wisp_intern_keyword",
    };

    wasmLib.root_module.addImport("ziglyph", ziglyph.module("ziglyph"));

    const tests = b.addTest(.{
        .root_source_file = b.path("repl.zig"),
        .target = standardTarget,
    });

    tests.root_module.addImport("ziglyph", ziglyph.module("ziglyph"));

    const testsPrty = b.addTest(.{
        .root_source_file = b.path("sexp-prty.zig"),
        .target = standardTarget,
    });

    testsPrty.root_module.addImport("ziglyph", ziglyph.module("ziglyph"));

    tests.step.dependOn(&bootRun.step);
    exe.step.dependOn(&bootRun.step);
    //    wasmExe.step.dependOn(&bootRun.step);

    b.installArtifact(exe);
    //    b.installArtifact(wasmExe);
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
