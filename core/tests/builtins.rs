use std::path::PathBuf;

use komodo::{
    builtin::standard_env,
    env::ExecContext,
    error::{Error, Position},
    object::ObjectError,
    run::run,
};

#[test]
fn sorted_mixed_numbers() {
    let mut env = standard_env(ExecContext::default());
    run(include_str!("../../examples/sort-types.komodo"), &mut env).unwrap();
    // Converting all values to f64 would lose these distinctions.
    run(
        r#"
assert([9007199254740993, 9007199254740992.0].sorted() = [9007199254740992.0, 9007199254740993])
assert([0.1, 1//10].sorted() = [1//10, 0.1])
assert(["z", 2, 0.5, "a"].sorted() = [0.5, 2, "a", "z"])
"#,
        &mut env,
    )
    .unwrap();
}

#[test]
fn number_string_comparison_is_rejected() {
    let mut env = standard_env(ExecContext::default());
    assert!(run("0.5 < \"cesar\"", &mut env).is_err());
}

#[test]
fn sorted_non_finite_numbers() {
    use komodo::object::{List, Object};

    let float = |value: f64| Object::Float(value.into());
    let zero = Object::Integer(0.into());
    let values = List::from(vec![
        float(f64::NAN),
        float(f64::INFINITY),
        zero.clone(),
        float(f64::NEG_INFINITY),
        float(-f64::NAN),
    ]);
    assert_eq!(
        values.sorted(),
        List::from(vec![
            float(-f64::NAN),
            float(f64::NEG_INFINITY),
            zero,
            float(f64::INFINITY),
            float(f64::NAN),
        ])
    );
}

#[test]
fn failed_assertion() {
    let code = "assert(1 = 2, \"1 is different from 2\")";
    let mut env = standard_env(ExecContext::default());

    assert_eq!(
        run(code, &mut env),
        Err(Error::with_position(
            ObjectError::FailedAssertion(Some("1 is different from 2".into())).into(),
            Position::new(0, code.len()),
            PathBuf::default(),
        )),
    );
}
