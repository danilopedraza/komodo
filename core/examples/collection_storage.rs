//! Run with cargo run --release --example collection_storage.
//! Compare ordered storage for runtime values; this is not an interpreter benchmark.
#![allow(clippy::mutable_key_type)]
use komodo::object::Object;
use std::{collections::BTreeSet, hint::black_box, time::Instant};
fn measure(mut work: impl FnMut(), rounds: usize) -> u128 {
    let start = Instant::now();
    for _ in 0..rounds {
        work();
    }
    start.elapsed().as_micros()
}
fn main() {
    println!("kind,n,rounds,tree_build_us,vec_build_us,tree_lookup_us,vec_lookup_us,tree_insert_us,vec_insert_us");
    for strings in [false, true] {
        for n in [16, 256, 4096] {
            let values: Vec<Object> = (0..n)
                .rev()
                .map(|i| {
                    if strings {
                        Object::String(format!("key-{i:08}").into())
                    } else {
                        Object::Integer((i as i64).into())
                    }
                })
                .collect();
            let rounds = 100_000 / n;
            let tree: BTreeSet<_> = values.iter().cloned().collect();
            let mut sorted = values.clone();
            sorted.sort();
            sorted.dedup();
            assert_eq!(tree.iter().cloned().collect::<Vec<_>>(), sorted);
            let tree_build = measure(
                || {
                    black_box(values.iter().cloned().collect::<BTreeSet<_>>());
                },
                rounds,
            );
            let vec_build = measure(
                || {
                    let mut v = values.clone();
                    v.sort();
                    v.dedup();
                    black_box(v);
                },
                rounds,
            );
            let tree_lookup = measure(
                || {
                    for v in &values {
                        black_box(tree.contains(black_box(v)));
                    }
                },
                rounds,
            );
            let vec_lookup = measure(
                || {
                    for v in &values {
                        black_box(sorted.binary_search(black_box(v)).is_ok());
                    }
                },
                rounds,
            );
            let tree_insert = measure(
                || {
                    let mut t = BTreeSet::new();
                    for v in &values {
                        t.insert(v.clone());
                    }
                    black_box(t);
                },
                rounds,
            );
            let vec_insert = measure(
                || {
                    let mut t = Vec::new();
                    for v in &values {
                        if let Err(i) = t.binary_search(v) {
                            t.insert(i, v.clone());
                        }
                    }
                    black_box(t);
                },
                rounds,
            );
            println!("{},{n},{rounds},{tree_build},{vec_build},{tree_lookup},{vec_lookup},{tree_insert},{vec_insert}", if strings { "string" } else { "integer" });
        }
    }
}
