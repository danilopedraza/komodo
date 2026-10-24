use crate::{
    env::{env_with, Environment, ExecContext},
    exec::truthy,
    object::{Kind, MyString, Object, ObjectError},
};

use std::io::{stdin, BufRead};

fn komodo_println(arg: &Object) -> Object {
    let str = arg.to_string();
    println!("{str}");
    Object::empty_tuple()
}

fn komodo_print(arg: &Object) -> Object {
    let str = arg.to_string();
    print!("{str}");
    Object::empty_tuple()
}

fn komodo_getln(_arg: &Object) -> Object {
    let mut line = String::new();
    stdin().lock().read_line(&mut line).unwrap();

    line.pop();

    Object::String(MyString::from(line.as_str()))
}

pub fn komodo_assert(arg: &Object) -> Object {
    let (condition, message) = match arg {
        Object::Tuple(tuple) if tuple.list.len() == 2 => {
            (&tuple.list[0].0, Some(tuple.list[1].0.to_string()))
        }
        obj => (obj, None),
    };
    if !truthy(condition) {
        Object::Error(ObjectError::FailedAssertion(message))
    } else {
        Object::empty_tuple()
    }
}

macro_rules! cast_fn {
    ($name:ident, $kind:ident) => {
        fn $name(arg: &Object) -> Object {
            match arg.$name() {
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

fn to_string(arg: &Object) -> Object {
    Object::String(arg.to_string().into())
}

fn len(arg: &Object) -> Object {
    match arg {
        Object::List(list) => Object::Integer(list.len().into()),
        Object::Set(set) => Object::Integer(set.len().into()),
        obj => Object::Error(ObjectError::UnexpectedType(
            vec![String::from("List"), String::from("Set")],
            obj.kind(),
        )),
    }
}

fn sorted(arg: &Object) -> Object {
    match arg {
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
            ("println", Object::from_fn(komodo_println, "println".into())),
            ("print", Object::from_fn(komodo_print, "print".into())),
            ("getln", Object::from_fn(komodo_getln, "getln".into())),
            ("assert", Object::from_fn(komodo_assert, "assert".into())),
            ("Integer", Object::from_fn(to_int, "Integer".into())),
            ("Float", Object::from_fn(to_float, "Float".into())),
            ("List", Object::from_fn(to_list, "List".into())),
            ("Set", Object::from_fn(to_set, "Set".into())),
            ("String", Object::from_fn(to_string, "String".into())),
            ("len", Object::from_fn(len, "len".into())),
            ("sorted", Object::from_fn(sorted, "sorted".into())),
        ],
        ctx,
    )
}
