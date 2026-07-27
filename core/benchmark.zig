// -*- fill-column: 64; -*-
//
// Evaluator diagnostics and small program benchmarks.  Setup and
// parsing happen before counters and the monotonic clock are started.

const std = @import("std");
const build_options = @import("build_options");

const Profile = @import("./profile.zig");
const Sexp = @import("./sexp.zig");
const Step = @import("./step.zig");
const Wisp = @import("./wisp.zig");

pub const wisp_profile = build_options.semantic_profile;

const program_source = @embedFile("lisp/benchmarks.wisp");
const repo_source = @embedFile("lisp/repo-benchmarks.wisp");

const Check = union(enum) {
    dump: []const u8,
    list_length: u32,
};

const Case = struct {
    name: []const u8,
    entry: []const u8,
    arguments: []const u8,
    input: []const u8 = "count",
    default_iterations: u64 = 25_000,
    iterations_last: bool = false,
    setup: []const u8,
    check: Check = .{ .dump = "0" },
};

const cases = [_]Case{
    .{
        .name = "call-1",
        .entry = "%BENCH-CALL-1",
        .arguments = "",
        .setup =
        \\(defun %bench-call-1 (count)
        \\  (if (eq? count 0)
        \\      0
        \\      (%bench-call-1 (- count 1))))
        ,
    },
    .{
        .name = "call-2",
        .entry = "%BENCH-CALL-2",
        .arguments = " 0",
        .setup =
        \\(defun %bench-call-2 (count a)
        \\  (if (eq? count 0)
        \\      0
        \\      (%bench-call-2 (- count 1) 0)))
        ,
    },
    .{
        .name = "call-5",
        .entry = "%BENCH-CALL-5",
        .arguments = " 0 0 0 0",
        .setup =
        \\(defun %bench-call-5 (count a b c d)
        \\  (if (eq? count 0)
        \\      0
        \\      (%bench-call-5 (- count 1) 0 0 0 0)))
        ,
    },
    .{
        .name = "call-16",
        .entry = "%BENCH-CALL-16",
        .arguments = " 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
        .setup =
        \\(defun %bench-call-16
        \\    (count a b c d e f g h i j k l m n o)
        \\  (if (eq? count 0)
        \\      0
        \\      (%bench-call-16
        \\       (- count 1)
        \\       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
        ,
    },
    .{
        .name = "jet-add-2",
        .entry = "%BENCH-JET-ADD-2",
        .arguments = "",
        .setup =
        \\(defun %bench-jet-add-2 (count)
        \\  (if (eq? count 0)
        \\      0
        \\      (do
        \\       (+ 1 2)
        \\       (%bench-jet-add-2 (- count 1)))))
        ,
    },
    .{
        .name = "closure-leaf-2",
        .entry = "%BENCH-CLOSURE-LEAF-2",
        .arguments = "",
        .setup =
        \\(defun %bench-leaf-2 (a b) b)
        \\(defun %bench-closure-leaf-2 (count)
        \\  (if (eq? count 0)
        \\      0
        \\      (do
        \\       (%bench-leaf-2 1 2)
        \\       (%bench-closure-leaf-2 (- count 1)))))
        ,
    },
    .{
        .name = "lookup-first-16",
        .entry = "%BENCH-LOOKUP-FIRST-16",
        .arguments = " 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
        .setup =
        \\(defun %bench-lookup-first-16
        \\    (target a b c d e f g h i j k l m n o)
        \\  (if (eq? target 0)
        \\      0
        \\      (%bench-lookup-first-16
        \\       (- target 1)
        \\       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
        ,
    },
    .{
        .name = "lookup-last-16",
        .entry = "%BENCH-LOOKUP-LAST-16",
        .arguments = " 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
        .iterations_last = true,
        .setup =
        \\(defun %bench-lookup-last-16
        \\    (a b c d e f g h i j k l m n o target)
        \\  (if (eq? target 0)
        \\      0
        \\      (%bench-lookup-last-16
        \\       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        \\       (- target 1))))
        ,
    },
    .{
        .name = "lookup-inner-8",
        .entry = "%BENCH-LOOKUP-INNER-8",
        .arguments = "",
        .setup =
        \\(defun %bench-lookup-inner-8-leaf ()
        \\  (let ((a 0))
        \\    (let ((b 0))
        \\      (let ((c 0))
        \\        (let ((d 0))
        \\          (let ((e 0))
        \\            (let ((f 0))
        \\              (let ((g 0))
        \\                (let ((target 1))
        \\                  target)))))))))
        \\(defun %bench-lookup-inner-8 (count)
        \\  (if (eq? count 0)
        \\      0
        \\      (do
        \\       (%bench-lookup-inner-8-leaf)
        \\       (%bench-lookup-inner-8 (- count 1)))))
        ,
    },
    .{
        .name = "lookup-outer-8",
        .entry = "%BENCH-LOOKUP-OUTER-8",
        .arguments = "",
        .setup =
        \\(defun %bench-lookup-outer-8-leaf ()
        \\  (let ((target 1))
        \\    (let ((a 0))
        \\      (let ((b 0))
        \\        (let ((c 0))
        \\          (let ((d 0))
        \\            (let ((e 0))
        \\              (let ((f 0))
        \\                (let ((g 0))
        \\                  target)))))))))
        \\(defun %bench-lookup-outer-8 (count)
        \\  (if (eq? count 0)
        \\      0
        \\      (do
        \\       (%bench-lookup-outer-8-leaf)
        \\       (%bench-lookup-outer-8 (- count 1)))))
        ,
    },
    .{
        .name = "tak",
        .entry = "%BENCHMARK-TAK",
        .arguments = "",
        .input = "18/12/6",
        .default_iterations = 1,
        .setup = program_source,
        .check = .{ .dump = "7" },
    },
    .{
        .name = "deriv",
        .entry = "%BENCHMARK-DERIV",
        .arguments = "",
        .input = "canonical-expression",
        .default_iterations = 100,
        .setup = program_source,
        .check = .{ .dump =
        \\(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
        \\   (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
        \\   (* (* b x) (+ (/ 0 b) (/ 1 x)))
        \\   0)
        },
    },
    .{
        .name = "diviter",
        .entry = "%BENCHMARK-DIVITER",
        .arguments = "",
        .input = "1000-cell-list",
        .default_iterations = 100,
        .setup = program_source,
        .check = .{ .list_length = 500 },
    },
    .{
        .name = "divrec",
        .entry = "%BENCHMARK-DIVREC",
        .arguments = "",
        .input = "1000-cell-list",
        .default_iterations = 100,
        .setup = program_source,
        .check = .{ .list_length = 500 },
    },
    .{
        .name = "stdlib-list",
        .entry = "%BENCHMARK-STDLIB",
        .arguments = "",
        .input = "64-element-list-pipeline",
        .default_iterations = 100,
        .setup = repo_source,
        .check = .{ .dump = "66" },
    },
    .{
        .name = "backquote",
        .entry = "%BENCHMARK-BACKQUOTE",
        .arguments = "",
        .input = "nested-unquote-splice",
        .default_iterations = 100,
        .setup = repo_source,
        .check = .{ .dump =
        \\(append
        \\ (list (quote a))
        \\ (list
        \\  (append
        \\   (list (quote b))
        \\   (list c)
        \\   (quote nil)))
        \\ d
        \\ (list (quote e))
        \\ (quote nil))
        },
    },
    .{
        .name = "router-hit",
        .entry = "%BENCHMARK-ROUTER-HIT",
        .arguments = "",
        .input = "8-pattern-late-hit",
        .default_iterations = 100,
        .setup = repo_source,
        .check = .{ .dump = "(\"alice\")" },
    },
    .{
        .name = "router-miss",
        .entry = "%BENCHMARK-ROUTER-MISS",
        .arguments = "",
        .input = "8-pattern-miss",
        .default_iterations = 100,
        .setup = repo_source,
        .check = .{ .dump = "not-found" },
    },
};

fn printCases(writer: anytype) !void {
    for (cases) |case| {
        try writer.print("{s}\n", .{case.name});
    }
}

fn invocationFor(
    allocator: std.mem.Allocator,
    case: Case,
    iterations: u64,
) ![]u8 {
    return if (case.iterations_last)
        try std.fmt.allocPrint(
            allocator,
            "({s}{s} {d})",
            .{ case.entry, case.arguments, iterations },
        )
    else
        try std.fmt.allocPrint(
            allocator,
            "({s} {d}{s})",
            .{ case.entry, iterations, case.arguments },
        );
}

fn runCase(
    io: std.Io,
    allocator: std.mem.Allocator,
    writer: anytype,
    case: Case,
    iterations: u64,
    warmup_iterations: u64,
) !void {
    if (iterations == 0) return error.InvalidIterationCount;

    var heap = try Wisp.Heap.fromEmbeddedCore(allocator, io);
    defer heap.deinit();

    // DEFUN intentionally prints while compiling.  Benchmark
    // setup should not share stdout with the JSONL report.
    _ = try heap.load(
        "(set-symbol-function! 'print (fn (x &optional stream) x))",
    );
    _ = try heap.load(case.setup);

    if (warmup_iterations > 0) {
        const warmup_invocation = try invocationFor(
            allocator,
            case,
            warmup_iterations,
        );
        defer allocator.free(warmup_invocation);

        var warmup_expression = try Sexp.read(
            &heap,
            warmup_invocation,
        );
        try heap.roots.append(heap.orb, &warmup_expression);
        defer _ = heap.roots.pop();

        var warmup_run = Step.initRun(warmup_expression);
        _ = try Step.evaluate(&heap, &warmup_run, 0);
    }

    const invocation = try invocationFor(
        allocator,
        case,
        iterations,
    );
    defer allocator.free(invocation);

    var expression = try Sexp.read(&heap, invocation);
    try heap.roots.append(heap.orb, &expression);
    defer _ = heap.roots.pop();

    var run = Step.initRun(expression);
    const heap_bytes_start = heap.bytesize();

    Profile.reset();
    const started = std.Io.Clock.awake.now(io).nanoseconds;
    const result = try Step.evaluate(&heap, &run, 0);
    const elapsed: u64 = @intCast(
        std.Io.Clock.awake.now(io).nanoseconds - started,
    );
    const snapshot = Profile.stats;
    const heap_bytes_end = heap.bytesize();

    var checked_result = result;
    try heap.roots.append(heap.orb, &checked_result);
    defer _ = heap.roots.pop();

    switch (case.check) {
        .dump => |expected_source| {
            const expected_word =
                try Sexp.read(&heap, expected_source);
            const expected = try Sexp.printAlloc(
                allocator,
                &heap,
                expected_word,
            );
            defer allocator.free(expected);

            const actual = try Sexp.printAlloc(
                allocator,
                &heap,
                checked_result,
            );
            defer allocator.free(actual);

            if (!std.mem.eql(u8, expected, actual)) {
                try writer.print(
                    "benchmark {s}: expected {s}, got {s}\n",
                    .{ case.name, expected, actual },
                );
                return error.UnexpectedBenchmarkResult;
            }
        },
        .list_length => |expected| {
            const actual = try Wisp.length(&heap, checked_result);
            if (actual != expected) {
                try writer.print(
                    "benchmark {s}: expected list length {d}, " ++
                        "got {d}\n",
                    .{ case.name, expected, actual },
                );
                return error.UnexpectedBenchmarkResult;
            }
        },
    }

    try Profile.writeJson(
        writer,
        snapshot,
        case.name,
        case.input,
        iterations,
        elapsed,
        heap_bytes_start,
        heap_bytes_end,
    );
}

pub fn main(init: std.process.Init) !void {
    var arena = std.heap.ArenaAllocator.init(init.gpa);
    defer arena.deinit();
    const tmp = arena.allocator();

    var args = try init.minimal.args.iterateAllocator(tmp);
    _ = args.skip();

    const selection = args.next() orelse "all";
    const iteration_text = args.next();
    const warmup_text = args.next();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(
        init.io,
        &stdout_buffer,
    );
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch {};

    if (std.mem.eql(u8, selection, "--list")) {
        return printCases(stdout);
    }

    var matched = false;
    for (cases) |case| {
        if (std.mem.eql(u8, selection, "all") or
            std.mem.eql(u8, selection, case.name))
        {
            matched = true;
            const iterations = if (iteration_text) |text|
                try std.fmt.parseInt(u64, text, 10)
            else
                case.default_iterations;
            const warmup_iterations = if (warmup_text) |text|
                try std.fmt.parseInt(u64, text, 10)
            else
                0;
            try runCase(
                init.io,
                init.gpa,
                stdout,
                case,
                iterations,
                warmup_iterations,
            );
            try stdout.flush();
        }
    }

    if (!matched) {
        try stdout.print(
            "unknown benchmark: {s}\n\navailable benchmarks:\n",
            .{selection},
        );
        try printCases(stdout);
        try stdout.flush();
        return error.UnknownBenchmark;
    }
}
