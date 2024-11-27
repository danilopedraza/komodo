use komodo::{
    builtin::standard_env,
    env::ExecContext,
    error::{Error, Position},
    exec::ExecError,
    run::run,
};

#[test]
fn failed_assertion() {
    let code = "assert(1 = 2, \"1 is different from 2\")";
    let mut env = standard_env(ExecContext::default());

    assert_eq!(
        run(code, &mut env),
        Err(Error::with_position(
            ExecError::FailedAssertion(Some("1 is different from 2".into())).into(),
            Position::new(0, code.len())
        )),
    );
}
