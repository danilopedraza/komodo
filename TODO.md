# Performance improvement opportunities

Inventory from code inspection on 2026-10-25, updated during implementation.
Items 1–31 now have implementations or measured evaluations; coverage and remaining generalizations
are described below. **High** marks investigation priority, not a measured speedup.

The recent shared instruction and copy-on-write environment improvements are
already implemented.

## Runtime and memory

Implementation coverage (2026-10-25):

| Items | Implemented behavior |
| --- | --- |
| 1–2 | Lists, tuples, sets, dictionaries, and strings share storage; mutation detaches. Indexing a loaded collection no longer copies its contents. |
| 3–4 | Dense binding slots, compiler-resolved parameter/capture loads, and interned IR/environment names. Shadowable names and complex patterns retain dynamic lookup; parsing and match maps still use owned names. |
| 5–6 | Lookup detaches only reachable mutable bindings. Anonymous closures retain referenced names, including nested-body dependencies, with conservative scope metadata. Their equality now ignores unreferenced bindings. Named functions retain definition snapshots so later clauses can reference earlier bindings. |
| 7 | Recursive self bindings use weak environment references; symbol loads and escaping captures promote them. This removes direct named-function cycles while preserving recursion, escapes, and clause updates. This is not a general collector for arbitrary indirect cycles through memoized closure results. |
| 8–9 | Unchanged mutable function bindings skip insertion/detachment. Binding-kind changes remain observable. Calls batch bindings into dense frames. |
| 10–12 | Bounded operand-stack reuse, computed stack capacities, direct draining into dictionary/set storage, and reuse of owned method argument tuples. |
| 13–15 | Literals are built during lowering; kind strings and error paths are allocated on failure; environments share execution contexts. |
| 16–18 | Discarded bindings/assignments skip result copies; simple symbol effects move values. Inferred integer operations use guarded specialization; total integer constants fold, and discarded constants disappear. Error-producing operations retain runtime diagnostics. |
| 19 | Blocks share code and metadata together. On x86_64, instructions shrink from 232 to 136 bytes and blocks from 56 to 8 bytes. |
| 20 | A trampoline handles direct tail calls through nonmemoized named/anonymous bodies and selected `if`/`case` branches. Memoized bodies, method calls, and non-tail recursion retain ordinary execution. A regression test executes 100,000 tail calls. |

Original investigation notes follow; checked items indicate the scoped
implementation above, rather than elimination of every fallback path.

- [x] **1. High: share collection values with copy-on-write semantics.** Lists,
  tuples, dictionaries, sets, and strings still own their storage. Variable loads
  clone `Object`, potentially copying entire nested collections. Shared environment
  maps do not eliminate these copies. See [symbol loads](core/src/exec.rs#L583)
  and [Object representation](core/src/object/mod.rs#L97).
- [x] **2. High: avoid copying containers just to index them.** Index execution
  consumes an owned container, commonly obtained by cloning a variable, then clones
  the selected element. Consider borrowed access or specialized load-and-index
  instructions. See [index execution](core/src/exec.rs#L133).
- [x] **3. High: resolve variables to slots.** Reads search scopes and perform
  string-keyed tree lookups. Compiler-resolved local and capture slots could remove
  repeated lookup work. See [environment lookup](core/src/env.rs#L112).
- [x] **4. Intern identifiers.** Names are allocated and copied across syntax
  trees, instructions, match bindings, and environment insertion. Shared symbols or
  numeric identifiers could reduce allocations and string comparisons. See
  [binding insertion](core/src/env.rs#L59) and [IR operations](core/src/ir.rs#L111).
- [x] **5. High: detach captured scopes only when mutation will occur.**
  `Scope::get` calls `Rc::make_mut` before checking whether the name exists or is
  mutable. Mutable lookup can copy unrelated shared scopes. See
  [scope access](core/src/env.rs#L51).
- [x] **6. Capture only referenced bindings.** Closures capture the complete
  environment, including scope metadata and unrelated bindings. Free-variable
  analysis could reduce retained memory and later detachment costs. See
  [closure construction](core/src/exec.rs#L87).
- [x] **7. High: address recursive function reference cycles.** Named functions
  strongly reference captured environments containing clones of those functions,
  forming `Rc` cycles that can retain environments after execution. Preserve
  escaped closures and recursion. See [function definition](core/src/exec.rs#L637)
  and [PatternFunction](core/src/object/mod.rs#L1010).
- [x] **8. Avoid unconditional function write-back.** Named calls write functions
  back into the environment, including external and nonmemoized functions. This
  adds insertion work and can trigger scope detachment. Preserve observable
  binding behavior. See [call execution](core/src/exec.rs#L718).
- [x] **9. Reduce call-frame setup.** Pattern function calls clone environment
  metadata, allocate a function scope, and insert bindings individually. Investigate
  compact frames. See [function frame setup](core/src/object/mod.rs#L1083).
- [x] **10. High: reuse operand stacks.** Every `exec` invocation creates a fresh
  `Vec`, including repeated loop bodies, branches, and function bodies. Investigate
  a shared stack with frame boundaries. See [execution](core/src/exec.rs#L94).
- [x] **11. Precompute stack capacity.** Record maximum stack depth from known
  instruction stack effects to avoid vector growth. See [IR stack effects](core/src/ir.rs#L1).
- [x] **12. Reduce temporary argument and collection buffers.** `take` uses
  `split_off`, and method argument assembly creates another vector. Investigate
  cheaper construction for common small argument counts. See
  [stack extraction](core/src/exec.rs#L268) and [method arguments](core/src/exec.rs#L234).
- [x] **13. High: construct literals once.** Integer literals are parsed into
  `BigInt` during execution; decimals format and parse strings. Loops and functions
  repeat this work. See [constant evaluation](core/src/exec.rs#L272) and
  [decimal construction](core/src/object/float.rs#L23).
- [x] **14. High: remove successful-path diagnostic allocations.** Every infix
  operation eagerly obtains two allocated kind strings. Construct these only on
  failure, or represent kinds without allocation. See [infix execution](core/src/exec.rs#L797)
  and [kind names](core/src/object/mod.rs#L286).
- [x] **15. Avoid successful-path path copies.** Mutable-value lookup clones the
  file path before an error is known. Context copies also duplicate paths. See
  [mutable lookup](core/src/exec.rs#L406) and [context access](core/src/env.rs#L103).
- [x] **16. Avoid constructing discarded results.** Bindings and assignments
  return owned values that a following `Drop` often discards. Consider effect-only
  instruction variants. See [assignment](core/src/exec.rs#L311) and
  [binding](core/src/exec.rs#L542).
- [x] **17. Use inferred types to specialize execution.** Instructions retain
  types, but arithmetic uses generic `Object` dispatch. Preserve fallback for
  `Unknown`. See [instruction metadata](core/src/ir.rs#L86) and
  [arithmetic dispatch](core/src/exec.rs#L797).
- [x] **18. Add constant folding and instruction simplification.** Simplify pure
  constant arithmetic and redundant instruction sequences before execution. See
  [IR lowering](core/src/ir.rs#L186).
- [x] **19. Reduce instruction size.** Separate executable data from positions
  and type metadata where useful for cache locality. Measure layout before
  redesigning it. See [Instruction](core/src/ir.rs#L86).
- [x] **20. Support tail calls or explicit interpreter frames.** Calls recurse
  through Rust execution. Reduce stack usage and overhead for recursive programs.
  See [pattern function calls](core/src/object/mod.rs#L1102).

## Pattern matching and function dispatch


Implementation coverage (2026-10-25): items 21–26 use shared list/string suffixes,
a binding accumulator with isolated choice points, early failure, delayed set
removal, direct constant-key lookup, and compiled runtime constants. Case, loop,
binding, and assignment patterns compile during lowering; function patterns
compile when their clauses/closures are constructed. Item 27 indexes tuple arity
and merges fallback clauses in source order; type/literal indexes remain future
work. Item 28 replaces clauses in place. Item 29 uses a persistent AVL cache.
Item 30 has a [measurement harness](bench/README.md#additional-measurements)
rather than a cache-policy change. Item 31 adds shared-identity
comparison paths and rejects unequal cache sizes before structural equality;
hashing remains structural. Suffix views can retain their source allocation,
and memo caches remain unbounded with existing snapshot and equality semantics.

- [x] **21. High: avoid copying list and string tails.** Cons patterns materialize
  tails; recursive destructuring can produce quadratic copying. Investigate shared
  slices or views. See [cons matching](core/src/matcher.rs#L155).
- [x] **22. High: reduce match-binding allocations.** Symbol patterns clone values
  into fresh `BTreeMap`s, then merge and insert them into environments. Consider a
  single accumulator, borrowed matches, and simple-pattern fast paths. See
  [match merging](core/src/matcher.rs#L32) and [symbol matching](core/src/matcher.rs#L100).
- [x] **23. Stop matching after failure.** Sequence matching uses `fold`, so later
  subpatterns execute after failure. Set and dictionary matching also perform
  avoidable work after earlier failure. See [sequence matching](core/src/matcher.rs#L49),
  [set matching](core/src/matcher.rs#L136), and [dictionary matching](core/src/matcher.rs#L187).
- [x] **24. High: avoid cloning sets for rejected candidates.** Set-cons matching
  clones and removes from a set before knowing whether the candidate matches.
  Nested patterns amplify this cost. See [set-cons matching](core/src/matcher.rs#L231).
- [x] **25. Look up constant dictionary pattern keys directly.** Literal keys
  could use tree lookup while general key patterns retain scanning. See
  [dictionary matching](core/src/matcher.rs#L187).
- [x] **26. Precompile constant patterns.** Matching reconstructs runtime
  constants through `eval_constant` on every attempt. See
  [constant matching](core/src/matcher.rs#L262).
- [x] **27. Index function clauses.** Skip impossible clauses using arity, type,
  or literal discriminator indexes while preserving clause order. See
  [clause dispatch](core/src/object/mod.rs#L1102).
- [x] **28. Avoid duplicate replacement clauses.** `add_pattern` replaces matching
  clauses and then unconditionally appends another. Redefinitions grow the clause
  list and increase scanning work. See [clause insertion](core/src/object/mod.rs#L1072).
- [x] **29. Reduce memoization cache detachment.** `Rc::make_mut` can clone the
  entire cache when snapshots share it. Investigate persistent storage while
  preserving snapshot semantics. See [cache insertion](core/src/object/mod.rs#L1114).
- [x] **30. Measure memoization key and retention costs.** Keys and results are
  cloned, tree comparisons can traverse complex objects, and caches are unbounded.
  Alternative indexes or policies require workload evidence and semantic review.
  See [cache representation](core/src/object/mod.rs#L1013).
- [x] **31. Reduce structural function comparisons.** Ordering and hashing traverse
  clauses, code, and caches. Investigate safe shared-identity fast paths or cached
  hashes without changing equality. See [function ordering](core/src/object/mod.rs#L1037)
  and [block equality](core/src/ir.rs#L53).

## Collections, numbers, and standard library

Implementation coverage (2026-10-25): items 32–42 move owned prepend elements,
reuse set storage, reserve collection/string outputs, repeat directly into output,
and store canonical inline `i64` integers with arbitrary-precision promotion.
Ranges benefit from inline increments and expose bounded size estimates; loops
use an iterator enum. Fraction rounding consumes the rounded rational without
cloning its numerator. Native collection helpers make reductions and filtering
iterative, and prefix checks borrow UTF-8 strings. `fold` still extracts its first
item once before iterative reduction. `some`/`every` now stop traversal and callback
execution as soon as the result is known (an intentional semantic change).
`indexOf` iterates directly and retains the last matching index.

Item 43 is evaluated rather than a storage replacement: ordered trees remain.
The [collection storage harness](bench/README.md#additional-measurements)
compares ordered trees with sorted vectors. Checked entries below
refer to this scope; BigInt-backed bit operations and other generic fallbacks remain.

- [x] **32. Move owned elements instead of cloning them.** List prepend and
  list-to-set prepend clone elements despite owning the source. Set prepend also
  rebuilds owned storage. See [set prepend](core/src/exec.rs#L448) and
  [list prepend](core/src/exec.rs#L497).
- [x] **33. Reserve known output sizes.** List repetition, prepend, string
  concatenation, and comprehensions have exact or useful estimated sizes. See
  [list repetition](core/src/object/mod.rs#L1214) and [comprehensions](core/src/exec.rs#L598).
- [x] **34. Avoid temporary vectors during repetition.** Clone elements directly
  into reserved output instead of cloning a source vector for each repetition.
  See [list repetition](core/src/object/mod.rs#L1214).
- [x] **35. High: use a small-integer representation.** All integers use `BigInt`,
  including indexes and counters. Investigate inline machine integers with overflow
  promotion. This is a larger representation change. See
  [Integer](core/src/object/integer.rs#L13).
- [x] **36. Reduce range iteration overhead.** Each step clones an arbitrary-
  precision integer and constructs an integer for incrementing. Investigate a
  machine-integer fast path. See [Range iteration](core/src/object/mod.rs#L1359).
- [x] **37. Avoid boxed iterator dispatch.** An enum for supported iterator kinds
  could remove allocation and per-element virtual dispatch from loops and
  comprehensions. See [iterable construction](core/src/exec.rs#L702).
- [x] **38. Reduce rational-number copies.** Rounding clones rationals before
  extracting integer results. Investigate borrowed arithmetic and owned-operation
  paths where appropriate. See [fraction rounding](core/src/object/fraction.rs#L47).
- [x] **39. High: make collection reductions iterative.** List/set `reduce`, and
  consequently `sum`, `prod`, and `fold`, repeatedly copy tails and recurse. Avoid
  quadratic copying and stack growth. See [reductions](std/utils.komodo#L4).
- [x] **40. Avoid repeated tails in filtering and prefix checking.** Recursive
  filters copy tails and reconstruct results; `isPrefix` repeatedly copies string
  suffixes. Investigate iteration or shared views. See [filters](std/utils.komodo#L15)
  and [prefix checking](std/utils.komodo#L44).
- [x] **41. Permit early exit for `some` and `every`.** Both use `reduce` and
  continue traversal and predicate evaluation after the result is established.
  Changing this affects callback side effects and requires a semantic decision.
  See [quantifiers](std/utils.komodo#L49).
- [x] **42. Optimize `indexOf` traversal.** Indexed accesses can clone `values`.
  Use direct iteration while preserving the current last-matching-index behavior;
  stopping at the first match would change behavior. See [indexOf](std/utils.komodo#L54).
- [x] **43. Evaluate collection data structures selectively.** Tree sets and maps
  allocate nodes and perform structural comparisons. Specialized representations
  may help particular key types, but preserve ordering and display behavior. See
  [Set representation](core/src/object/mod.rs#L542).

## Compilation, imports, and REPL

- [ ] **44. High: share import compilation work.** Inference reads and parses
  modules; runtime import reads, parses, and typechecks them again. Repeated imports
  repeat the work. Cache source, checked modules, or IR with appropriate
  invalidation. See [import inference](core/src/typecheck.rs#L659) and
  [runtime module loading](core/src/run.rs#L132).
- [ ] **45. Reduce simultaneous syntax-tree storage.** CST, AST, typed AST, and IR
  use owned strings and patterns copied between stages. Consider consuming lowering
  or shared arenas. Preserve whole-program validation before execution. See
  [pipeline](core/src/run.rs#L34) and [typed lowering](core/src/typed_ast.rs#L205).
- [ ] **46. Avoid cloning tokens for lookahead.** Owned token lookahead can copy
  identifier and literal strings repeatedly. Consider borrowed lookahead or token
  discriminants. See [parser lookahead](core/src/parser.rs#L418).
- [ ] **47. High for complex types: avoid eager union Cartesian products.**
  `tuple_alternatives` materializes combinations that can grow exponentially.
  Investigate lazy compatibility checking. See [tuple alternatives](core/src/typecheck.rs#L445).
- [ ] **48. Normalize and share union types.** Comparisons repeatedly flatten and
  clone alternatives, then perform linear membership checks. Consider canonical
  representations. See [union handling](core/src/typecheck.rs#L155).
- [ ] **49. Borrow types during compatibility checks.** Reduce cloning of nested
  function inputs, outputs, and tuple members through reference-based checks or
  shared immutable types. See [type compatibility](core/src/typecheck.rs#L367).
- [ ] **50. Reduce REPL environment copies.** Each expression clones the complete
  type environment for rollback. Investigate transactional updates or persistent
  maps while preserving rollback. See [REPL execution](core/src/repl.rs#L89).

## Formatting, I/O, browser, and builds

- [ ] **51. Stream collection formatting.** Write directly to formatters instead
  of allocating element strings, collecting vectors, and joining them. See
  [collection formatting](core/src/object/mod.rs#L699).
- [ ] **52. Serialize JSON into one output buffer.** Avoid recursively building
  intermediate strings and joined vectors. See [JSON serialization](core/src/std/json.rs#L74).
- [ ] **53. Avoid JSON input copies and intermediate trees.** Parsing clones the
  input, builds a JSON tree, then converts to `Object`. Investigate borrowed input
  and direct conversion during parsing. See [value conversion](core/src/std/json.rs#L20)
  and [parse entry point](core/src/std/json.rs#L63).
- [ ] **54. Reduce standard I/O formatting and locking overhead.** Printing first
  allocates a full string, and repeated calls access output separately. Investigate
  persistent buffered I/O while preserving interactive flushing behavior. See
  [printing](core/src/builtin.rs#L10).
- [ ] **55. Move input strings into runtime values.** `getln` copies an owned line
  through `&str` into another string. See [input](core/src/builtin.rs#L22).
- [ ] **56. Defer assertion-message formatting.** Two-argument assertions format
  messages even when successful. See [assertions](core/src/builtin.rs#L31).
- [ ] **57. High for responsiveness: run the playground in a worker.** Synchronous
  Wasm execution blocks the browser main thread. A worker would keep editing and
  status rendering responsive; it would not inherently accelerate interpretation.
  See [browser execution](core-browser/index.js#L44).
- [ ] **58. Reduce browser output copies.** Execution clones the complete output
  buffer, and each print builds an intermediate string. See
  [output return](core-browser/src/lib.rs#L27) and [browser printing](core-browser/src/builtin.rs#L17).
- [ ] **59. Benchmark Wasm speed versus size settings.** Browser release builds
  use `opt-level = "z"`. Compare execution, download size, and startup latency with
  other levels. See [browser release profile](core-browser/Cargo.toml#L22).
- [ ] **60. Evaluate native release tuning.** Benchmark LTO, codegen-unit settings,
  and profile-guided optimization for portable release artifacts and local builds.
  See [core manifest](core/Cargo.toml#L1) and [release packaging](flake.nix#L45).
- [ ] **61. Compare Wasm build pipelines.** Nix uses Cargo and `wasm-bindgen`;
  the shell path uses `wasm-pack`. Check optimization parity and evaluate post-link
  Wasm optimization. See [Nix build](flake.nix#L80) and [shell build](core-browser/build.sh#L5).

## Measurement and validation

Current benchmarks cover calls, closures, arithmetic loops, and one
collection-heavy application. They measure the full process, including compilation
and imports. See [benchmark documentation](bench/README.md).

- [ ] Separate lexing, parsing, typechecking, lowering, imports, and execution timings.
- [ ] Measure allocation counts, bytes copied, peak memory, and retained memory.
- [ ] Add scaling workloads for indexing, recursive collection operations, failed
  pattern matches, memoization, repeated imports, and union-heavy types.
- [ ] Measure Wasm execution and browser responsiveness separately.
- [ ] Track regressions smaller than the current CI regression threshold of 25%.

## Suggested first investigations

Start with eager diagnostic strings (14), collection cloning during reads and
indexing (1–2), repeated literal parsing (13), match and tail copying (21–24),
scope detachment during lookup (5), operand-stack allocation (10), recursive
environment cycles (7), and duplicated import compilation (44). These have
concrete code evidence and affect common execution paths.
