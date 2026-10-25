# Ordered collection storage evaluation

2026-10-25, native Linux x86_64, optimized Cargo build. Reproduce with:

```sh
nix develop -c cargo run --release --manifest-path core/Cargo.toml --example collection_storage
```

The harness compares `BTreeSet<Object>` with a sorted `Vec<Object>` for integer
and string keys at 16, 256 and 4096 elements. Both preserve the same iteration
order, checked by assertion. It measures bulk construction, successful lookup,
and incremental insertion in descending order. Each cell aggregates about 100,000
keys; times are microseconds. Raw results: [CSV](collection-storage.csv).

At 4096 integer keys, sorted-vector bulk construction took 3812 µs versus 9644 µs
for the tree, and lookup took 5899 µs versus 7609 µs. However, incremental insertion
took 184677 µs versus 10833 µs (about 17× slower). String keys showed the same
insertion tradeoff (198254 µs versus 12410 µs), with smaller lookup differences.
Small collections were mixed: vector lookup was slower for 16 string keys.

Keep ordered trees as the general representation. A sorted vector could benefit
immutable, bulk-built collections, but inserting at the beginning repeatedly moves
large `Object` values. A hash table would additionally need sorting to preserve
iteration/display order and would change structural hashing costs. This evaluation
does not justify either as a universal replacement, or establish a specialization
threshold. Dictionaries, mixed/nested keys, deletion, allocation counts and full
application workloads remain unmeasured. These are single-run exploratory timings,
not a statistically controlled claim of interpreter speedup.

The implemented inline integer representation already removes BigInt allocations
for common integer keys/comparisons while retaining tree ordering. Owned set
prepend also reuses the existing tree. These are the selected changes for now.
