use crate::{
    env::Environment,
    object::{self, Effect, Object},
};

use std::io::{stdin, BufRead};

fn smtc_println(args: &[Object]) -> Object {
    let str = args[0].to_string();
    println!("{str}");
    Object::Tuple(object::Tuple::from(vec![]))
}

fn smtc_getln(_args: &[Object]) -> Object {
    let mut line = String::new();
    stdin().lock().read_line(&mut line).unwrap();

    line.pop();

    Object::String(object::MyString::from(line.as_str()))
}

pub fn standard_env() -> Environment {
    let mut env = Environment::default();

    env.set(
        "println",
        crate::object::Object::Function(crate::object::Function::Effect(Effect {
            func: smtc_println,
        })),
    );

    env.set(
        "getln",
        crate::object::Object::Function(crate::object::Function::Effect(Effect {
            func: smtc_getln,
        })),
    );

    env
}
