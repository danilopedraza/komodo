# Pattern dispatch and memoization evaluation

Measured on 2026-10-25, x86_64 Linux, rustc 1.99.1, Cargo release defaults.
Baseline: `c60d0e1`; candidate: the TODO 21–31 implementation. Raw single-run
observations are in [baseline CSV](memoization-baseline.csv) and
[candidate CSV](memoization.csv). Timings are illustrative, not statistical
speedup claims; allocation and live-byte measurements describe these workloads.

## Reproduce

```sh
cargo run --release --manifest-path bench/Cargo.toml --bin memoization > /tmp/memoization.csv
# Optional maximum entry count (baseline capture used 512):
cargo run --release --manifest-path bench/Cargo.toml --bin memoization -- 512
```

To compare a revision, copy this benchmark source and its Cargo manifest/lockfile
into a checkout with that revision's `core` dependency and run the same command.
The executable only needs the `memoization` bin declaration from the manifest.
No standard-library imports or network access are used during measurement.

The benchmark runs the full parse/typecheck/lower/execute pipeline in an already
constructed standard environment. Each iteration calls an identity function twice
with the same key, exercising a miss followed by a hit for memoized functions.
Scalar keys are integers; list keys have 63 equal leading integers and a varying
last integer, deliberately exercising long comparisons. Results equal keys, so
collection results share storage with their keys. Separate runs retain either the
latest function snapshot or every preceding snapshot plus the latest one. The
latest snapshot is explicitly assigned outside the loop's scope: call write-back
inside a loop would otherwise disappear when that scope exits.

The global allocator counts successful allocations and requested bytes, including
reallocations via the default allocate/copy/deallocate implementation. Peak and
retained bytes subtract the pre-run live allocation count, while after-drop bytes
subtract the live count before creating the environment. They exclude allocator
metadata, fragmentation, stack usage, and RSS. Source construction and standard
environment setup are outside the measured interval; compilation, argument
construction, saved-snapshot storage, and instrumentation overhead are inside.
The first use of new names leaves small thread-local interning allocations
(3,033 and 5,856 bytes here); subsequent rows return to the previous live baseline.
This does not test arbitrary cycles through memoized closure results.

## Observations

For 512 entries, retained snapshots, and memoization:

| Key | Baseline time | Candidate time | Baseline allocated | Candidate allocated | Baseline retained | Candidate retained |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Integer | 24.00 ms | 0.91 ms | 48.33 MB | 3.14 MB | 46.47 MB | 1.16 MB |
| 64-element list | 115.36 ms | 5.74 ms | 54.81 MB | 9.62 MB | 49.64 MB | 4.33 MB |

MB denotes decimal megabytes. The persistent AVL cache copies only the search
path on insertion, rather than copying every entry when a snapshot shares the
cache. Different cache sizes also reject function equality immediately, avoiding
structural comparison of an old cache with its newly extended version. Both
changes contribute to these results; this is not an isolated AVL comparison.

At 2,048 entries the candidate retains about 0.69 MB for scalar keys with only
the latest snapshot, versus 5.61 MB with all snapshots. Long list keys raise those
figures to 13.36 MB and 18.27 MB. Long-key memoized calls take about 27–28 ms,
versus 8–9 ms for the equivalent nonmemoized identity function: caching cheap
work can still lose. Sharing avoids deep key/result copies on insertion, but
comparison work and retained backing allocations remain significant.

## Decisions and limits

Keep deterministic ordered cache semantics and unbounded retention. Eviction
could change whether a function body executes again (including side effects),
and cache contents participate in function comparisons. A hash index or bounded
cache therefore needs separate workload and semantic review. This measurement
supports persistent snapshots, not a claim that every function should be memoized.

List and UTF-8 string cons tails now share suffix views; a small surviving suffix
can retain its complete source allocation. Writes detach the visible list suffix.
The 20,000-element recursive test in `examples/pattern-tails.komodo` exercises
shared tails together with the existing tail-call trampoline. Pattern matching
uses one binding accumulator for deterministic paths, with local accumulators at
backtracking choices to preserve existing first-match semantics. Constant keys
use direct dictionary lookup. Function clauses are indexed by tuple arity only;
non-tuple or alternative patterns retain the ordered fallback path. Constant
patterns are compiled before repeated matching. Clause replacement keeps its
original position without appending a duplicate. Shared block, clause, and cache
identities accelerate comparisons; hashing remains structural.
