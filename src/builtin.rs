use crate::{
    env::Environment,
    object::{Effect, Function, MyString, Object, Tuple},
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

pub fn standard_env() -> Environment {
    let mut env = Environment::default();

    env.set(
        "println",
        Object::Function(Function::Effect(Effect::new(smtc_println))),
    );

    env.set(
        "getln",
        Object::Function(Function::Effect(Effect::new(smtc_getln))),
    );

    env
}
