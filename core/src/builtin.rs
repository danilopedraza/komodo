use crate::{
    env::{env_with, Environment, ExecContext},
    exec::truthy,
    object::{MyString, Object, ObjectError},
};

use std::io::{stdin, BufRead};

fn komodo_println(args: &[Object]) -> Object {
    let str = args[0].to_string();
    println!("{str}");
    Object::empty_tuple()
}

fn komodo_print(args: &[Object]) -> Object {
    let str = args[0].to_string();
    print!("{str}");
    Object::empty_tuple()
}

fn komodo_getln(_args: &[Object]) -> Object {
    let mut line = String::new();
    stdin().lock().read_line(&mut line).unwrap();

    line.pop();

    Object::String(MyString::from(line.as_str()))
}

pub fn komodo_assert(args: &[Object]) -> Object {
    match args.first() {
        Some(obj) if !truthy(obj) => Object::Error(ObjectError::FailedAssertion(
            args.get(1).map(|obj| obj.to_string()),
        )),
        Some(_) | None => Object::empty_tuple(),
    }
}

macro_rules! cast_fn {
    ($name:ident, $kind:ident) => {
        fn $name(args: &[Object]) -> Object {
            match args[0].$name() {
                Ok(int) => Object::$kind(int),
                Err(err) => err.into(),
            }
        }
    };
}

cast_fn!(to_int, Integer);
cast_fn!(to_float, Float);
cast_fn!(to_list, List);
cast_fn!(to_set, Set);

pub fn standard_env(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            ("println", Object::from_fn(komodo_println, 1)),
            ("print", Object::from_fn(komodo_print, 1)),
            ("getln", Object::from_fn(komodo_getln, 0)),
            ("assert", Object::from_fn(komodo_assert, 1)),
            ("Integer", Object::from_fn(to_int, 1)),
            ("Float", Object::from_fn(to_float, 1)),
            ("List", Object::from_fn(to_list, 1)),
            ("Set", Object::from_fn(to_set, 1)),
        ],
        ctx,
    )
}
