use komodo::{
    builtin::standard_env, env::ExecContext, error::Error, exec::ExecError, object::Object,
    run::run,
};

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
