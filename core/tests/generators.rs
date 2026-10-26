use komodo::{
    builtin::standard_env, env::ExecContext, error::Error, exec::ExecError, object::Object,
    run::run,
};

fn execute(source: &str) {
    run(source, &mut standard_env(ExecContext::default())).unwrap();
}

#[test]
fn recursive_generator_example() {
    // Keep the user's example itself in the regression suite. Its unused utils
    // import is covered by the executable-example runner with KOMODO_STD set.
    execute(
        include_str!("../../examples/generators.komodo")
            .trim_start_matches("from utils import sum\n"),
    );
}

#[test]
fn generators_are_lazy_repeatable_and_short_circuit() {
    execute(
        r#"
from collections import (some, every)
let numbers(n) :=
    yield n
    numbers(n + 1)
let values := numbers(0)
assert(some(values, n -> n = 10000))
assert(some(values, n -> n = 0))
assert(!every(values, n -> n < 3))
let fails() :=
    yield 1
    assert(false)
let unused := fails()
assert(some(unused, n -> n = 1))
"#,
    );
}

#[test]
fn loops_branches_cases_and_mutation_resume_in_order() {
    execute(
        r#"
let sequence() :=
    var total := 0
    for n in [1, 2, 3] do
        total := total + n
        if n = 2 then
            yield total * 10
        else
            yield total
        case n do
            2 => yield 20
            _ => ()
    yield total + 1
    999
assert([n for n in sequence()] = [1, 30, 20, 6, 7])
assert({n for n in sequence()} = {1, 30, 20, 6, 7})
var result := 0
for n in sequence() do
    result := result + n
assert(result = 64)
"#,
    );
}

#[test]
fn closures_and_nested_generators_keep_function_boundaries() {
    execute(
        r#"
let make(start) :=
    let inner() :=
        yield start
        yield start + 1
    inner()
let outer() :=
    let inner() := yield 99
    yield 1
    let ignored := inner()
    yield 2
assert([n for n in make(3)] = [3, 4])
assert([n for n in outer()] = [1, 2])
let anon := x -> yield x * 2 + 1
assert([n for n in anon(4)] = [9])
let empty() :=
    if false then yield 1 else ()
assert([n for n in empty()] = [])
let takesGenerator(value: Generator) := [n for n in value]
assert(takesGenerator(make(5)) = [5, 6])
"#,
    );
}

#[test]
fn yielded_containers_are_single_elements_and_return_values_are_ignored() {
    execute(
        r#"
let values() :=
    yield [1, 2]
    yield ()
    [3, 4]
assert([x for x in values()] = [[1, 2], ()])
let finish() := yield 3
let delegate() :=
    yield 1
    yield 2
    finish()
assert([x for x in delegate()] = [1, 2, 3])
"#,
    );
}

#[test]
fn iterator_failures_keep_the_yielding_source_position() {
    let definition = "let broken() :=\n    yield 1\n    1 / 0\n";
    for consumer in [
        "[n for n in broken()]",
        "{n for n in broken()}",
        "List(broken())",
        "Set(broken())",
        "for n in broken() do ()",
        "from collections import reduce\nreduce(broken(), (a, b) -> a + b, 0)",
    ] {
        let source = format!("{definition}{consumer}");
        let err = run(&source, &mut standard_env(ExecContext::default())).unwrap_err();
        assert!(
            matches!(
                err,
                Error::WithPosition(
                    komodo::error::ErrorKind::Exec(ExecError::DenominatorZero),
                    _,
                    _
                )
            ),
            "{err:?}"
        );
        if let Error::WithPosition(_, position, _) = err {
            assert!(position.start < definition.len());
        }
    }
}

#[test]
fn yield_outside_a_function_is_rejected_even_in_unchecked_children() {
    for source in [
        "yield 1",
        "[yield 1]",
        "for x in [] do yield x",
        "if false then yield 1 else ()",
    ] {
        assert!(
            run(source, &mut standard_env(ExecContext::default())).is_err(),
            "{source}"
        );
    }
}

#[test]
fn generator_keys_have_stable_identity() {
    let mut env = standard_env(ExecContext::default());
    run("let f() := yield 1\nlet g := f()\nlet d := {(g, g) => 42}\nassert([x for x in g] = [1])\nassert(d[(g, g)] = 42)", &mut env).unwrap();
    assert!(matches!(env.lookup("g"), Some(Object::Generator(_))));
}

#[test]
fn all_pattern_clauses_return_generators_including_empty_base_cases() {
    execute(
        r#"
let down(0) := ()
let down(n) :=
    yield n
    down(n - 1)
assert([n for n in down(0)] = [])
assert([n for n in down(3)] = [3, 2, 1])
assert(List(down(3)) = [3, 2, 1])
assert(Set(down(3)) = {1, 2, 3})
"#,
    );
}

#[test]
fn nested_iteration_and_comprehension_suspensions() {
    execute(
        r#"
let inner(n) := yield n
let nested() :=
    for n in [1, 2] do
        for m in inner(n) do
            yield m
            yield m * 10
    let ignored := [yield n + 1 for n in [4, 5]]
    yield 7
assert(List(nested()) = [1, 10, 2, 20, 5, 6, 7])
"#,
    );
}

#[test]
fn suspended_operands_and_method_calls_keep_their_stack() {
    execute(
        r#"
let values() :=
    let pair := (yield 1, yield 2)
    yield pair
    let object := {f => x -> x}
    let ignored := object.f(yield 3)
    case (yield 4) do
        () => yield 5
assert(List(values()) = [1, 2, ((), ()), 3, 4, 5])
"#,
    );
}

#[test]
fn failed_iteration_restores_the_callers_scope() {
    for consumer in [
        "[n for n in broken()]",
        "{n for n in broken()}",
        "for n in broken() do ()",
    ] {
        let mut env = standard_env(ExecContext::default());
        let source =
            format!("let n := 42\nlet broken() :=\n    yield 1\n    assert(false)\n{consumer}");
        assert!(run(&source, &mut env).is_err());
        assert_eq!(env.lookup("n"), Some(&Object::Integer(42.into())));
    }
}

#[test]
fn generator_type_is_preserved_across_clauses() {
    execute(
        r#"
let g(1) := yield 1
let g(0) := ()
let empty: Generator := g(0)
assert(List(empty) = [])
let wrap(1) := g(1)
let wrap(0) := 42
assert(wrap(0) = 42)
"#,
    );
}
