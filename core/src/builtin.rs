use crate::{
    env::{env_with, Environment, ExecContext},
    exec::truthy,
    object::{Kind, MyString, Object, ObjectError},
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

fn to_string(args: &[Object]) -> Object {
    Object::String(args[0].to_string().into())
}

fn len(args: &[Object]) -> Object {
    match &args[0] {
        Object::List(list) => Object::Integer(list.len().into()),
        Object::Set(set) => Object::Integer(set.len().into()),
        obj => Object::Error(ObjectError::UnexpectedType(
            vec![String::from("List"), String::from("Set")],
            obj.kind(),
        )),
    }
}

fn sorted(args: &[Object]) -> Object {
    match &args[0] {
        Object::List(list) => Object::List(list.sorted()),
        obj => Object::Error(ObjectError::UnexpectedType(
            vec![String::from("List")],
            obj.kind(),
        )),
    }
}

pub fn standard_env(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            (
                "println",
                Object::from_fn(komodo_println, "println".into(), 1),
            ),
            ("print", Object::from_fn(komodo_print, "print".into(), 1)),
            ("getln", Object::from_fn(komodo_getln, "getln".into(), 0)),
            ("assert", Object::from_fn(komodo_assert, "assert".into(), 1)),
            ("Integer", Object::from_fn(to_int, "Integer".into(), 1)),
            ("Float", Object::from_fn(to_float, "Float".into(), 1)),
            ("List", Object::from_fn(to_list, "List".into(), 1)),
            ("Set", Object::from_fn(to_set, "Set".into(), 1)),
            ("String", Object::from_fn(to_string, "String".into(), 1)),
            ("len", Object::from_fn(len, "len".into(), 1)),
            ("sorted", Object::from_fn(sorted, "sorted".into(), 1)),
        ],
        ctx,
    )
}
