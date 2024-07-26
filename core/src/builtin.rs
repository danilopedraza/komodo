use crate::{
    env::{Environment, ExecContext},
    exec::truthy,
    object::{Effect, FailedAssertion, Function, MyString, Object, Tuple},
};

use std::io::{stdin, BufRead};

fn smtc_println(args: &[Object]) -> Object {
    let str = args[0].to_string();
    println!("{str}");
    Object::Tuple(Tuple::from(vec![]))
}

fn smtc_getln(_args: &[Object]) -> Object {
    let mut line = String::new();
    stdin().lock().read_line(&mut line).unwrap();

    line.pop();

    Object::String(MyString::from(line.as_str()))
}

pub fn smtc_assert(args: &[Object]) -> Object {
    match (truthy(&args[0]), args.len()) {
        (false, len) if len > 1 => Object::Error(FailedAssertion(Some(args[1].to_string()))),
        (false, _) => Object::Error(FailedAssertion(None)),
        _ => Object::empty_tuple(),
    }
}

fn env_with(assets: Vec<(&str, Object)>, ctx: ExecContext) -> Environment {
    let mut env = Environment::new(ctx);

    for (name, value) in assets {
        env.set(name, value);
    }

    env
}

pub fn standard_env(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            (
                "println",
                Object::Function(Function::Effect(Effect::new(smtc_println, 1))),
            ),
            (
                "getln",
                Object::Function(Function::Effect(Effect::new(smtc_getln, 0))),
            ),
            (
                "assert",
                Object::Function(Function::Effect(Effect::new(smtc_assert, 1))),
            ),
        ],
        ctx,
    )
}
