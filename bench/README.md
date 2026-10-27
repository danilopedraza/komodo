# Interpreter benchmarks

Each workload repeats its original work fifty times per process, checking its result
with assertions on every repetition. Collection sizes and recursion depths stay
the same; the extra work reduces the relative cost of process startup and compilation.

The workloads are:

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
Set `BASELINE_STD` and `CANDIDATE_STD` to the respective revisions' `std/`
directories when comparing interpreters with different standard libraries.
Each defaults to the current bundled standard library; relative paths are resolved
from the invocation directory.

Set `RESULTS_DIR` to export one hyperfine JSON file per workload (relative paths
are resolved from the invocation directory). CI uses `nix run .#bench -- PREVIOUS_COMMIT`
to build both revisions with the current pinned toolchain, run this same
`compare.sh` against the current workloads with each revision's own standard
library, and upload the
baseline and candidate measurements to Bencher with the existing 25% regression
threshold. Both scripts are checked by the Nix `benchmark-script` check.

## Additional measurements

Run the memoization allocation and retention harness with:

```sh
nix develop -c cargo run --release --manifest-path bench/Cargo.toml --bin memoization
```

Pass an optional maximum entry count after `--`. The harness compares integer
and list keys, with either the latest function snapshot or all snapshots retained.
Measurements include compilation and execution but exclude source construction and
standard-environment setup. Allocation counts and live bytes exclude allocator
metadata, fragmentation, stack usage, and RSS.

Compare ordered trees with sorted vectors for collection storage with:

```sh
nix develop -c cargo run --release --manifest-path core/Cargo.toml --example collection_storage
```

The harness checks iteration order and measures construction, lookup, and insertion
for integer and string keys. These exploratory timings do not establish general
interpreter speedups.

Keep generated measurements outside version control; rerun the relevant harness
for the revisions and workloads being evaluated.
