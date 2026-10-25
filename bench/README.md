# Interpreter benchmarks

Each workload checks its result with assertions:

- `calls.komodo`: recursive pattern dispatch and captured environments.
- `closures.komodo`: closure creation, capture, and anonymous calls.
- `loops.komodo`: arithmetic and mutable bindings without function calls.
- `polyominoes.komodo`: collection-heavy application using the standard library.
- `indexing.komodo`: repeated dictionary and list indexing into a large collection.
- `snapshots.komodo`: nested collection snapshots and indexed assignment isolation.
- `literals.komodo`: repeated arbitrary-precision integer and decimal literals.
- `blocks.komodo`: short nested branches, operand stacks, and integer arithmetic.
- `scope-mutation.komodo`: mutation lookup with a shared captured scope.
- `tail-calls.komodo`: repeated tail recursion at a depth runnable by both binaries.
- `capture-retention.komodo`: returned closures created alongside unused large lists.
- `recursive-retention.komodo`: create and discard local recursive functions.

Inspect runtime structure sizes with
`nix run .#runtime-layout`.

On Linux, `nix run .#bench-memory -- /absolute/baseline /absolute/candidate`
samples `/proc` high-water RSS for the capture, recursion, and snapshot cases,
five runs each. Optional workload names select other cases. The CSV output
records every sample and the median. Samples are lower bounds on peak RSS;
short-lived peaks can be missed. These are whole-process memory measurements,
including compilation, rather than allocation counts or retained-heap profiles.

Build each revision with `nix build .#komodo` in its checkout and retain its
store path. Compare the packaged executables using the Nix-provided hyperfine:

```sh
nix run .#bench-compare -- /nix/store/.../bin/komodo /nix/store/.../bin/komodo
```

While new files are untracked, use `path:.#` instead of `.#` in these Nix
commands so the source includes them without changing the Git index.

The script uses three warmups and fifteen measured runs per binary and workload.
Set `RUNS` to change the measured run count. Timings include process startup,
parsing, typechecking, imports, and execution. Run on an otherwise idle machine.

Set `RESULTS_DIR` to export one hyperfine JSON file per workload (relative paths
are resolved from the invocation directory). CI uses `nix run .#bench -- PREVIOUS_COMMIT`
to build both revisions with the current pinned toolchain, run this same
`compare.sh` against the current workloads and standard library, and upload the
baseline and candidate measurements to Bencher with the existing 25% regression
threshold. Both scripts are checked by the Nix `benchmark-script` check.

## Runtime and memory results

Measured on x86_64 Linux with Nix release packages and pinned Rust 1.99.1,
three warmups and fifteen measured runs (2026-10-25). Baseline is `e046791`;
candidate is this runtime/memory implementation. Exact package paths, the
candidate source digest, and all timing samples are in the
[raw timing results](results/runtime-memory-2026-10-25.json).

| Workload | Baseline mean | Candidate mean | Speedup |
| --- | ---: | ---: | ---: |
| Recursive calls | 96.01 ms | 84.47 ms | 1.14× |
| Closures | 42.25 ms | 40.26 ms | 1.05× |
| Arithmetic loop | 45.71 ms | 36.84 ms | 1.24× |
| Polyominoes | 11.49 ms | 8.53 ms | 1.35× |
| Indexing | 2467.15 ms | 8.15 ms | 302.76× |
| Snapshots | 9.98 ms | 5.39 ms | 1.85× |
| Repeated literals | 21.67 ms | 15.38 ms | 1.41× |
| Nested blocks | 100.57 ms | 66.36 ms | 1.52× |
| Scope mutation | 12.52 ms | 9.35 ms | 1.34× |
| Tail calls | 64.79 ms | 37.33 ms | 1.74× |
| Closure capture retention | 125.54 ms | 87.19 ms | 1.44× |
| Recursive function retention | 43.62 ms | 21.02 ms | 2.08× |

Indexing deliberately repeats large-container reads to expose the original
copying cost; its speedup does not predict general application performance.
The closure timing improvement is small relative to observed variation.
Snapshot measurements approach the shell/startup noise floor. All timings
include compilation and imports; outliers and individual samples are retained
in the raw results.

The Rust memory harness sampled process high-water RSS every millisecond,
five runs per executable. Median sampled peaks were:

| Workload | Baseline | Candidate |
| --- | ---: | ---: |
| Closure capture retention | 70,768 KiB | 5,880 KiB |
| Recursive function retention | 41,196 KiB | 4,324 KiB |
| Snapshots | 4,612 KiB | 4,452 KiB |

See the [raw memory samples](results/runtime-memory-2026-10-25.csv).
These sampled peaks are lower bounds and include compiler memory; they do not
establish allocation counts or a general peak-memory guarantee.

On this platform, shared block data reduces `Block` from 56 to 8 bytes and
`Instruction` from 232 to 136 bytes. The regression suite also executes
100,000 tail calls and verifies that escaped recursive captures are released.
See [implementation coverage](../TODO.md#runtime-and-memory) for the fast paths
and remaining dynamic fallbacks.

## Capture and code sharing results

Measured locally on x86_64 Linux with Rust 1.99.1, release builds, three warmups
and fifteen measured runs (2026-10-25). Baseline is revision `8266355`, before the
copy-on-write capture and shared instruction changes; candidate includes them.

| Workload | Baseline mean | Candidate mean | Change |
| --- | ---: | ---: | ---: |
| Recursive calls | 801.5 ms | 109.3 ms | 7.33× faster |
| Closures | 235.4 ms | 41.9 ms | 5.62× faster |
| Arithmetic loop | 46.4 ms | 47.4 ms | 2% slower |
| Polyominoes | 94.7 ms | 11.2 ms | 8.44× faster |

These are workload-specific measurements, not a general interpreter speedup.
The changes remove deep copies of instruction trees, function clauses, caches,
and captured bindings on reads/calls. Bindings, clauses, and caches detach on
mutation to preserve snapshot semantics. Arithmetic and operand-stack execution
are largely unchanged; the small loop regression remains visible above.

## Pattern matching and memoization

See [the memoization evaluation](memoization.md) for the TODO 21–31 changes,
reproducible allocation/retention measurements, baseline comparisons, and remaining
key-comparison and unbounded-retention costs.
