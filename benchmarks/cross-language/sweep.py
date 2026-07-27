#!/usr/bin/env python3

import argparse
import json
import math
import os
from pathlib import Path
import shutil
import statistics
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
BUILD = HERE / ".build"

WORKLOADS = {
    "tak": {
        "input": "18/12/6",
        "iterations": 10,
        "warmup": 1,
    },
    "deriv": {
        "input": "canonical-expression",
        "iterations": 10_000,
        "warmup": 100,
    },
    "diviter": {
        "input": "1000-cell-list",
        "iterations": 1000,
        "warmup": 10,
    },
    "divrec": {
        "input": "1000-cell-list",
        "iterations": 1000,
        "warmup": 10,
    },
    "stdlib-list": {
        "input": "64-element-list-pipeline",
        "iterations": 1000,
        "warmup": 10,
    },
    "backquote": {
        "input": "nested-unquote-splice",
        "iterations": 1000,
        "warmup": 10,
    },
    "router-hit": {
        "input": "8-pattern-late-hit",
        "iterations": 50,
        "warmup": 2,
    },
    "router-miss": {
        "input": "8-pattern-miss",
        "iterations": 50,
        "warmup": 2,
    },
}

GABRIEL_BENCHMARKS = {"tak", "deriv", "diviter", "divrec"}
REPOSITORY_LISP_BENCHMARKS = {"stdlib-list", "backquote"}
ROUTER_BENCHMARKS = {"router-hit", "router-miss"}
ALL_BENCHMARKS = set(WORKLOADS)


def output(command, cwd=ROOT, env=None):
    result = subprocess.run(
        command,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        env=env,
        check=True,
    )
    lines = result.stdout.strip().splitlines()
    return lines[0] if lines else ""


def version(command, env=None):
    try:
        return output(command, env=env)
    except (OSError, subprocess.CalledProcessError):
        return "unknown"


def git_version():
    revision = output(["git", "rev-parse", "--short", "HEAD"])
    dirty = subprocess.run(
        ["git", "diff", "--quiet"],
        cwd=ROOT,
    ).returncode != 0
    untracked = output(
        ["git", "status", "--short", "--untracked-files=normal"]
    )
    return revision + ("-dirty" if dirty or untracked else "")


def require(command):
    return shutil.which(command) is not None


def quiet_run(command, cwd=ROOT, env=None):
    completed = subprocess.run(
        command,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        env=env,
    )
    if completed.returncode != 0:
        raise RuntimeError(
            f"{' '.join(command)} failed:\n{completed.stdout}"
        )


def fetch_interpreters():
    if not require("git"):
        print(
            "warning: git is unavailable; skipping optional interpreters",
            file=sys.stderr,
        )
        return

    wren_root = BUILD / "wren-cli"
    wren_binary = wren_root / "bin/wren_cli"
    if not require("wren_cli") and not wren_binary.exists():
        if wren_root.exists():
            shutil.rmtree(wren_root)
        try:
            quiet_run([
                "git", "clone", "--depth", "1", "--branch", "0.4.0",
                "--recurse-submodules", "--shallow-submodules",
                "https://github.com/wren-lang/wren-cli.git",
                str(wren_root),
            ])
            make_project = (
                "projects/make.mac"
                if sys.platform == "darwin"
                else "projects/make"
            )
            quiet_run(
                ["make", "config=release_64bit"],
                cwd=wren_root / make_project,
            )
        except RuntimeError as error:
            print(f"warning: could not build Wren:\n{error}",
                  file=sys.stderr)

    chibi_root = BUILD / "chibi-scheme"
    chibi_binary = chibi_root / "chibi-scheme"
    if not require("chibi-scheme") and not chibi_binary.exists():
        if chibi_root.exists():
            shutil.rmtree(chibi_root)
        try:
            quiet_run([
                "git", "clone", "--depth", "1", "--branch", "0.11",
                "https://github.com/ashinn/chibi-scheme.git",
                str(chibi_root),
            ])
            quiet_run(
                ["make", f"-j{os.cpu_count() or 2}"],
                cwd=chibi_root,
            )
        except RuntimeError as error:
            print(f"warning: could not build Chibi-Scheme:\n{error}",
                  file=sys.stderr)


def prepare(should_fetch_interpreters):
    BUILD.mkdir(parents=True, exist_ok=True)

    wisp_build = subprocess.run(
        [
            "zig", "build", "bench-build",
            "-Doptimize=ReleaseFast",
            "-Dsemantic-profile=false",
        ],
        cwd=ROOT / "core",
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    if wisp_build.returncode != 0:
        raise RuntimeError(
            "Wisp benchmark build failed:\n" + wisp_build.stdout
        )

    if require("clang"):
        native_build = subprocess.run(
            [
                "clang", "-O3", "-DNDEBUG", "-std=c11",
                "-Wall", "-Wextra", "-Werror",
                str(HERE / "native.c"),
                "-o", str(BUILD / "native"),
            ],
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        if native_build.returncode != 0:
            raise RuntimeError(
                "native benchmark build failed:\n" +
                native_build.stdout
            )
    if should_fetch_interpreters:
        fetch_interpreters()


def runtimes():
    wren_command = shutil.which("wren_cli")
    if wren_command is None:
        local_wren = BUILD / "wren-cli/bin/wren_cli"
        if local_wren.exists():
            wren_command = str(local_wren)

    chibi_command = shutil.which("chibi-scheme")
    chibi_env = None
    if chibi_command is None:
        chibi_root = BUILD / "chibi-scheme"
        local_chibi = chibi_root / "chibi-scheme"
        if local_chibi.exists():
            chibi_command = str(local_chibi)
            chibi_env = os.environ.copy()
            chibi_env.update({
                "CHIBI_IGNORE_SYSTEM_PATH": "1",
                "CHIBI_MODULE_PATH": str(chibi_root / "lib"),
                "DYLD_LIBRARY_PATH": str(chibi_root),
                "LD_LIBRARY_PATH": str(chibi_root),
            })

    candidates = [
        {
            "id": "wisp",
            "language": "Wisp",
            "available": True,
            "version": f"{git_version()} / Zig {version(['zig', 'version'])}",
            "command": [str(ROOT / "core/zig-out/bin/wisp-bench")],
            "benchmarks": ALL_BENCHMARKS,
        },
        {
            "id": "python",
            "language": "Python",
            "available": require("python3"),
            "version": version(["python3", "--version"]),
            "command": ["python3", str(HERE / "python.py")],
            "benchmarks": GABRIEL_BENCHMARKS,
        },
        {
            "id": "ruby",
            "language": "Ruby",
            "available": require("ruby"),
            "version": version(["ruby", "--version"]),
            "command": ["ruby", str(HERE / "ruby.rb")],
            "benchmarks": GABRIEL_BENCHMARKS,
        },
        {
            "id": "tcl",
            "language": "Tcl",
            "available": require("tclsh"),
            "version": version([
                "tclsh", str(HERE / "tcl.tcl"), "--version",
            ]),
            "command": ["tclsh", str(HERE / "tcl.tcl")],
            "benchmarks": GABRIEL_BENCHMARKS,
        },
        {
            "id": "node",
            "language": "JavaScript",
            "available": require("node"),
            "version": f"Node.js {version(['node', '--version'])}",
            "command": ["node", str(HERE / "javascript.js")],
            "benchmarks": GABRIEL_BENCHMARKS,
        },
        {
            "id": "racket",
            "language": "Racket",
            "available": require("racket"),
            "version": version(["racket", "--version"]),
            "command": ["racket", str(HERE / "racket.rkt")],
            "benchmarks": ALL_BENCHMARKS,
        },
        {
            "id": "chibi",
            "language": "Scheme",
            "available": chibi_command is not None,
            "version": (
                version([chibi_command, "-V"], env=chibi_env)
                if chibi_command else "unavailable"
            ),
            "command": (
                [chibi_command, str(HERE / "chibi-scheme.scm")]
                if chibi_command else []
            ),
            "env": chibi_env,
            "benchmarks": (
                GABRIEL_BENCHMARKS | REPOSITORY_LISP_BENCHMARKS
            ),
        },
        {
            "id": "sbcl",
            "language": "Common Lisp",
            "available": require("sbcl"),
            "version": version(["sbcl", "--version"]),
            "command": [
                "sbcl", "--noinform", "--disable-debugger", "--script",
                str(HERE / "common-lisp.lisp"),
            ],
            "benchmarks": (
                GABRIEL_BENCHMARKS | REPOSITORY_LISP_BENCHMARKS
            ),
        },
        {
            "id": "wren",
            "language": "Wren",
            "available": wren_command is not None,
            "version": (
                version([wren_command, "--version"])
                if wren_command else "unavailable"
            ),
            "command": (
                [wren_command, str(HERE / "wren.wren")]
                if wren_command else []
            ),
            "benchmarks": GABRIEL_BENCHMARKS,
        },
        {
            "id": "c",
            "language": "C",
            "available": require("clang"),
            "version": version(["clang", "--version"]),
            "command": [str(BUILD / "native")],
            "benchmarks": GABRIEL_BENCHMARKS,
        },
    ]
    return candidates


def run_sample(runtime, benchmark, workload, sample):
    command = runtime["command"] + [
        benchmark,
        str(workload["iterations"]),
        str(workload["warmup"]),
    ]
    completed = subprocess.run(
        command,
        cwd=ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=runtime.get("env"),
        timeout=120,
    )
    if completed.returncode != 0:
        raise RuntimeError(
            f"{runtime['id']} {benchmark} failed:\n"
            f"{completed.stdout}{completed.stderr}"
        )

    lines = [line for line in completed.stdout.splitlines()
             if line.strip()]
    if not lines:
        raise RuntimeError(
            f"{runtime['id']} {benchmark} produced no result"
        )
    record = json.loads(lines[-1])
    if (record.get("benchmark") != benchmark or
            record.get("iterations") != workload["iterations"]):
        raise RuntimeError(
            f"{runtime['id']} {benchmark} returned bad metadata"
        )
    if record.get("elapsed_ns", 0) <= 0:
        raise RuntimeError(
            f"{runtime['id']} {benchmark} returned a bad duration"
        )

    record.update({
        "runtime": runtime["id"],
        "language": runtime["language"],
        "version": runtime["version"],
        "sample": sample,
        "input": workload["input"],
        "warmup_iterations": workload["warmup"],
    })
    return record


def format_duration(nanoseconds):
    if nanoseconds < 1000:
        return f"{nanoseconds:.0f} ns"
    if nanoseconds < 1_000_000:
        return f"{nanoseconds / 1000:.2f} us"
    return f"{nanoseconds / 1_000_000:.2f} ms"


def summarize(records, selected_benchmarks, selected_runtimes):
    medians = {}
    for benchmark in selected_benchmarks:
        for runtime in selected_runtimes:
            values = [
                record["elapsed_ns"] / record["iterations"]
                for record in records
                if record["benchmark"] == benchmark
                and record["runtime"] == runtime["id"]
            ]
            if values:
                medians[(benchmark, runtime["id"])] = (
                    statistics.median(values)
                )

    print()
    for benchmark in selected_benchmarks:
        print(
            f"{benchmark} "
            f"({WORKLOADS[benchmark]['input']}, "
            f"{WORKLOADS[benchmark]['iterations']} iterations)"
        )
        print(f"{'runtime':<10} {'median/run':>14} {'vs fastest':>12}")
        eligible = [
            runtime for runtime in selected_runtimes
            if (benchmark, runtime["id"]) in medians
        ]
        fastest = min(
            medians[(benchmark, runtime["id"])]
            for runtime in eligible
        )
        ranked = sorted(
            eligible,
            key=lambda runtime: medians[
                (benchmark, runtime["id"])
            ],
        )
        for runtime in ranked:
            value = medians[(benchmark, runtime["id"])]
            print(
                f"{runtime['id']:<10} "
                f"{format_duration(value):>14} "
                f"{value / fastest:>11.2f}x"
            )
        print()

    wisp_comparisons = []
    for benchmark in selected_benchmarks:
        wisp_key = (benchmark, "wisp")
        others = [
            runtime for runtime in selected_runtimes
            if runtime["id"] != "wisp"
            and (benchmark, runtime["id"]) in medians
        ]
        if wisp_key not in medians or not others:
            continue
        slowest = max(
            others,
            key=lambda runtime: medians[
                (benchmark, runtime["id"])
            ],
        )
        wisp_value = medians[wisp_key]
        other_value = medians[(benchmark, slowest["id"])]
        if wisp_value >= other_value:
            relation = f"{wisp_value / other_value:.2f}x slower"
        else:
            relation = f"{other_value / wisp_value:.2f}x faster"
        wisp_comparisons.append((benchmark, slowest, relation))

    if wisp_comparisons:
        print("Wisp versus the slowest other runtime")
        print(f"{'benchmark':<14} {'other':<10} {'Wisp is':>14}")
        for benchmark, slowest, relation in wisp_comparisons:
            print(
                f"{benchmark:<14} {slowest['id']:<10} "
                f"{relation:>14}"
            )
        print()

    complete_runtimes = [
        runtime for runtime in selected_runtimes
        if all(
            (benchmark, runtime["id"]) in medians
            for benchmark in selected_benchmarks
        )
    ]
    scores = []
    for runtime in complete_runtimes:
        ratios = []
        for benchmark in selected_benchmarks:
            fastest = min(
                medians[(benchmark, other["id"])]
                for other in selected_runtimes
                if (benchmark, other["id"]) in medians
            )
            ratios.append(
                medians[(benchmark, runtime["id"])] / fastest
            )
        score = math.exp(
            sum(math.log(ratio) for ratio in ratios) / len(ratios)
        )
        scores.append((score, runtime))

    if scores:
        print("overall geometric mean (complete runtimes only)")
        print(f"{'runtime':<10} {'score':>12}")
        for score, runtime in sorted(scores):
            print(f"{runtime['id']:<10} {score:>11.2f}x")
    else:
        print("overall geometric mean: no runtime supports every case")


def selected_values(text, available, kind):
    if text == "all":
        return available
    wanted = set(text.split(","))
    known = {item["id"] if kind == "runtime" else item
             for item in available}
    unknown = wanted - known
    if unknown:
        raise ValueError(
            f"unknown {kind}{'s' if len(unknown) > 1 else ''}: "
            f"{', '.join(sorted(unknown))}"
        )
    if kind == "runtime":
        return [item for item in available if item["id"] in wanted]
    return [item for item in available if item in wanted]


def main():
    parser = argparse.ArgumentParser(
        description="Run the Wisp cross-language benchmark sweep",
    )
    parser.add_argument("--samples", type=int, default=5)
    parser.add_argument("--runtimes", default="all")
    parser.add_argument("--benchmarks", default="all")
    parser.add_argument(
        "--no-fetch-interpreters",
        action="store_true",
        help="do not fetch pinned Wren and Chibi-Scheme runtimes",
    )
    parser.add_argument(
        "--results",
        type=Path,
        default=HERE / "results/latest.jsonl",
    )
    args = parser.parse_args()
    if args.samples < 1:
        parser.error("--samples must be positive")

    prepare(not args.no_fetch_interpreters)
    available = [item for item in runtimes() if item["available"]]
    selected_runtimes = selected_values(
        args.runtimes, available, "runtime"
    )
    benchmark_items = list(WORKLOADS)
    selected_benchmarks = selected_values(
        args.benchmarks, benchmark_items, "benchmark"
    )

    print(
        "runtimes: " +
        ", ".join(runtime["id"] for runtime in selected_runtimes)
    )
    print(f"samples: {args.samples}")

    records = []
    for benchmark in selected_benchmarks:
        workload = WORKLOADS[benchmark]
        for sample in range(1, args.samples + 1):
            for runtime in selected_runtimes:
                if benchmark not in runtime["benchmarks"]:
                    continue
                print(
                    f"\r{benchmark}: sample {sample}/{args.samples}, "
                    f"{runtime['id']:<8}",
                    end="",
                    flush=True,
                )
                records.append(
                    run_sample(
                        runtime,
                        benchmark,
                        workload,
                        sample,
                    )
                )
    unsupported = [
        benchmark for benchmark in selected_benchmarks
        if not any(
            record["benchmark"] == benchmark for record in records
        )
    ]
    if unsupported:
        raise RuntimeError(
            "no selected runtime supports: " + ", ".join(unsupported)
        )
    print("\r" + " " * 70 + "\r", end="")

    args.results.parent.mkdir(parents=True, exist_ok=True)
    with args.results.open("w", encoding="utf-8") as destination:
        for record in records:
            destination.write(
                json.dumps(record, separators=(",", ":")) + "\n"
            )
    print(f"raw results: {args.results}")
    summarize(records, selected_benchmarks, selected_runtimes)


if __name__ == "__main__":
    try:
        main()
    except (ValueError, RuntimeError, subprocess.CalledProcessError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
