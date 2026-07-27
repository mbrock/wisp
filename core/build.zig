const std = @import("std");

fn addNanoarrow(
    b: *std.Build,
    name: []const u8,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    c_flags: []const []const u8,
) *std.Build.Step.Compile {
    const module = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    module.addIncludePath(
        b.path("vendor/nanoarrow/include"),
    );
    module.addCSourceFiles(.{
        .root = b.path("vendor/nanoarrow"),
        .files = &.{
            "src/nanoarrow.c",
            "src/nanoarrow_ipc.c",
            "src/flatcc.c",
        },
        .flags = c_flags,
    });
    return b.addLibrary(.{
        .name = name,
        .root_module = module,
    });
}

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const standardTarget = b.standardTargetOptions(.{});
    const wasiTarget = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });

    const boot = b.addExecutable(.{
        .name = "wisp-boot",
        .root_module = b.createModule(.{
            .root_source_file = b.path("boot.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });
    const bootRun = b.addRunArtifact(boot);
    bootRun.setCwd(b.path("."));

    const nanoarrow = addNanoarrow(
        b,
        "nanoarrow",
        standardTarget,
        optimize,
        &.{"-std=c11"},
    );
    const nanoarrowWasi = addNanoarrow(
        b,
        "nanoarrow-wasi",
        wasiTarget,
        optimize,
        &.{
            "-std=c11",
            "-D_WASI_EMULATED_SIGNAL",
            "-DENODATA=120",
        },
    );

    const exe = b.addExecutable(.{
        .name = "wisp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("main.zig"),
            .target = standardTarget,
            .optimize = optimize,
        }),
    });
    exe.root_module.linkLibrary(nanoarrow);

    // const wasmExe = b.addExecutable(.{
    //     .name = "wisp",
    //     .root_source_file = b.path("main.zig"),
    //     .target = wasiTarget,
    // });

    const wasmLib = b.addExecutable(.{
        .name = "wisp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("wasm.zig"),
            .target = wasiTarget,
            .optimize = optimize,
        }),
    });

    wasmLib.entry = .disabled;
    wasmLib.root_module.linkLibrary(nanoarrowWasi);
    wasmLib.root_module.export_symbol_names = &[_][]const u8{
        "wisp_sys_t",
        "wisp_sys_nil",
        "wisp_sys_nah",
        "wisp_sys_zap",
        "wisp_sys_top",
        "wisp_arrow_ipc_check",
        "wisp_heap_init",
        "wisp_heap_deinit",
        "wisp_heap_tidy",
        "wisp_tape_size",
        "wisp_tape_write",
        "wisp_heap_from_tape",
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
        "wisp_heap_bytesize",
        "wisp_heap_era",
        "wisp_heap_table_len",
        "wisp_heap_package_count",
        "wisp_heap_pin_count",
        "wisp_heap_root_count",
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

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("repl.zig"),
            .target = standardTarget,
            .optimize = optimize,
        }),
    });

    const testsPrty = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("sexp-prty.zig"),
            .target = standardTarget,
            .optimize = optimize,
        }),
    });

    const testsArrow = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("arrow.zig"),
            .target = standardTarget,
            .optimize = optimize,
        }),
    });
    testsArrow.root_module.linkLibrary(nanoarrow);
    const testsArrowRun = b.addRunArtifact(testsArrow);

    exe.step.dependOn(&bootRun.step);
    wasmLib.step.dependOn(&bootRun.step);
    tests.step.dependOn(&bootRun.step);
    testsPrty.step.dependOn(&bootRun.step);

    b.installArtifact(exe);
    //    b.installArtifact(wasmExe);
    b.installArtifact(wasmLib);

    const testStep = b.step("test", "Run unit tests");
    testStep.dependOn(&tests.step);
    testStep.dependOn(&testsArrowRun.step);

    const testPrtyStep = b.step("test-prty", "Run tests for Prty");
    testPrtyStep.dependOn(&testsPrty.step);

    const runCmd = b.addRunArtifact(exe);
    runCmd.step.dependOn(b.getInstallStep());
    runCmd.addPassthruArgs();

    const runStep = b.step("run", "Run the Wisp REPL");
    runStep.dependOn(&runCmd.step);
}
