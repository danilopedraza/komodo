# Interpreter benchmarks

Each workload checks its result with assertions:

- `calls.komodo`: recursive pattern dispatch and captured environments.
- `closures.komodo`: closure creation, capture, and anonymous calls.
- `loops.komodo`: arithmetic and mutable bindings without function calls.
- `polyominoes.komodo`: collection-heavy application using the standard library.

Build each revision with `cargo build --release --manifest-path core/Cargo.toml`
and save its `core/target/release/komodo` binary outside the build directory.
Compare them using hyperfine:

```sh
bash bench/compare.sh /absolute/path/to/baseline /absolute/path/to/candidate
```

The script uses three warmups and fifteen measured runs per binary and workload.
Set `RUNS` to change the measured run count. Timings include process startup,
parsing, typechecking, imports, and execution. Run on an otherwise idle machine.

Set `RESULTS_DIR` to export one hyperfine JSON file per workload (relative paths
are resolved from the repository root). CI uses `nix run .#bench -- PREVIOUS_COMMIT`
to build both revisions with the current pinned toolchain, run this same
`compare.sh` against the current workloads and standard library, and upload the
baseline and candidate measurements to Bencher with the existing 25% regression
threshold. Both scripts are checked by the Nix `benchmark-script` check.

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
