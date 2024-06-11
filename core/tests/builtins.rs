use symstatic::{
    error::{Error, Position},
    exec::EvalError,
    run::run,
};

#[test]
fn failed_assertion() {
    let code = "assert(1 = 2, \"1 is different from 2\")";

    assert_eq!(
        run(code),
        Err(Error::new(
            EvalError::FailedAssertion(Some("1 is different from 2".into())).into(),
            Position::new(0, code.len())
        )),
    );
}
