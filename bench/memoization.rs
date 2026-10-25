//! Whole-pipeline memoization scaling, including allocation and retention costs.
use komodo::{builtin::standard_env, env::ExecContext, run::run};
use std::{
    alloc::{GlobalAlloc, Layout, System},
    sync::atomic::{AtomicUsize, Ordering::Relaxed},
    time::Instant,
};
struct Counting;
static LIVE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);
static BYTES: AtomicUsize = AtomicUsize::new(0);
static ALLOCS: AtomicUsize = AtomicUsize::new(0);
unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = System.alloc(layout);
        if !ptr.is_null() {
            ALLOCS.fetch_add(1, Relaxed);
            BYTES.fetch_add(layout.size(), Relaxed);
            let live = LIVE.fetch_add(layout.size(), Relaxed) + layout.size();
            PEAK.fetch_max(live, Relaxed);
        }
        ptr
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        LIVE.fetch_sub(layout.size(), Relaxed);
        System.dealloc(ptr, layout);
    }
}
#[global_allocator]
static ALLOCATOR: Counting = Counting;
fn main() {
    println!("entries,width,snapshots,memoized,elapsed_us,allocations,allocated_bytes,peak_bytes,retained_bytes,after_drop_bytes");
    let max_entries = std::env::args()
        .nth(1)
        .map(|n| n.parse::<usize>().unwrap())
        .unwrap_or(2048);
    for entries in [128, 512, 2048].into_iter().filter(|n| *n <= max_entries) {
        for width in [1, 64] {
            for snapshots in [false, true] {
                for memoized in [false, true] {
                    // Put the changing discriminator last to exercise long key comparisons.
                    let key = if width == 1 {
                        "n".into()
                    } else {
                        format!("[{}n]", "0, ".repeat(width - 1))
                    };
                    let save = if snapshots { "    saved[n] := f\n" } else { "" };
                    let source = format!("let {}f(x) := x\nvar saved := [0 for n in 0..{entries}]\nvar retained := f\nfor n in 0..{entries} do\n{save}    f({key})\n    f({key})\n    retained := f\n", if memoized { "memoize " } else { "" });
                    let before_env = LIVE.load(Relaxed);
                    let mut env = standard_env(ExecContext::default());
                    let live = LIVE.load(Relaxed);
                    PEAK.store(live, Relaxed);
                    let allocations = ALLOCS.load(Relaxed);
                    let bytes = BYTES.load(Relaxed);
                    let start = Instant::now();
                    run(&source, &mut env).unwrap();
                    let elapsed = start.elapsed().as_micros();
                    let allocations = ALLOCS.load(Relaxed) - allocations;
                    let bytes = BYTES.load(Relaxed) - bytes;
                    let peak = PEAK.load(Relaxed).saturating_sub(live);
                    let retained = LIVE.load(Relaxed).saturating_sub(live);
                    drop(env);
                    let after_drop = LIVE.load(Relaxed).saturating_sub(before_env);
                    println!("{entries},{width},{snapshots},{memoized},{elapsed},{allocations},{bytes},{peak},{retained},{after_drop}");
                }
            }
        }
    }
}
