# Wisp cross-language benchmark sweep

This is a small implementation-comparison suite, not a claim about
which language is intrinsically fastest. It combines four
Gabriel-derived workloads with four cases derived from this
repository:

| benchmark | fixed input | measured iterations |
| --- | --- | ---: |
| `tak` | `(18 12 6)` | 10 |
| `deriv` | canonical symbolic expression | 10000 |
| `diviter` | prebuilt 1000-cell list | 1000 |
| `divrec` | prebuilt 1000-cell list | 1000 |
| `stdlib-list` | map/append/reverse/remove over 64 elements | 1000 |
| `backquote` | nested unquote and unquote-splicing form | 1000 |
| `router-hit` | late hit among eight route patterns | 50 |
| `router-miss` | miss among eight route patterns | 50 |

The current ports are Wisp, Python, Ruby, Tcl, JavaScript on
Node.js, Wren, Chibi-Scheme, Racket, Common Lisp on SBCL, and C
compiled by Clang. The Gabriel cases run everywhere. The repository
list and backquote cases run in Wisp, Racket, Chibi-Scheme, and
SBCL. The continuation router runs in Wisp and Racket, whose prompt
and composable-continuation primitives provide a direct comparison.
Every runner warms up in-process, starts its clock after setup,
checks its result after timing, and emits the same small JSON record.

Run the five-sample sweep:

```sh
make bench-sweep
```

On first use, the sweep fetches and locally builds the pinned
Wren CLI 0.4.0 and Chibi-Scheme 0.11 releases under the ignored
`.build` directory. It never installs them system-wide. Pass
`--no-fetch-interpreters` to use only already-available runtimes;
unavailable optional runtimes are then skipped explicitly.

Or select a subset:

```sh
python3 benchmarks/cross-language/sweep.py \
  --samples 9 \
  --runtimes wisp,chibi,wren,python,node,racket,sbcl,c \
  --benchmarks tak,deriv
```

Raw samples go to `results/latest.jsonl`, which is ignored by Git.
The terminal report ranks median time per logical program run and
includes a Wisp-focused comparison with the slowest other runtime
for each case. It then gives a geometric mean of per-benchmark
ratios for runtimes which support every selected case.

## Comparability boundaries

- The algorithms, inputs, repetition counts, and result checks are
  shared. The ports preserve constant-time cons construction;
  Python, Ruby, and JavaScript use explicit pair nodes for the
  division benchmarks rather than quadratic host-array copying.
  Tcl uses nested two-element list objects; its copy-on-write
  representation shares each previously constructed tail.
- `stdlib-list` exercises each Lisp's ordinary list library.
  `backquote` ports Wisp's `bq-completely-process` algorithm rather
  than timing a host macro expander.
- The router case keeps `web/http.wisp`'s matcher and nested
  mismatch/response prompt shape, but replaces Deno request and
  response objects with fixed route data. Each mismatch captures and
  discards a continuation, just as the Wisp route search does.
- Unsupported repository cases are omitted, not emulated with a
  different control mechanism. This is why Chibi-Scheme and SBCL do
  not currently appear in the router ranking and the non-Lisp ports
  remain in the Gabriel tier.
- Wisp is built without semantic counters for the sweep. Core
  loading, source parsing, setup, warmup, and validation are outside
  its clock, as they are in the other runners.
- Node.js and Racket have JITs; SBCL compiles functions to native
  code; C is compiled with `-O3`; Wisp, Python, Ruby, and Tcl take
  their normal execution paths. Chibi-Scheme uses its compact VM and
  Wren uses its bytecode VM. Warmup reduces cold-code noise but does
  not erase these implementation differences.
- The C port uses a fixed arena and does not reclaim timed
  allocations. It is a useful native baseline, not a
  garbage-collector comparison.
- Results are meaningful only for this machine, checkout, and
  runtime versions. Read individual cases before the overall score.
