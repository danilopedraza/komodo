use std::{
    thread::{self},
    time::Duration,
};

use crate::{
    env::{env_with, Environment, ExecContext},
    object::{float::Float, Kind, Object, ObjectError},
};

fn sleep(args: &[Object]) -> Object {
    match &args[0] {
        Object::Float(f) => {
            let duration = Duration::from(f);
            thread::sleep(duration);

            Object::empty_tuple()
        }
        Object::Integer(int) => {
            let duration = Duration::from(&Float::from(int));
            thread::sleep(duration);

            Object::empty_tuple()
        }
        Object::Fraction(f) => {
            let duration = Duration::from(&Float::from(f));
            thread::sleep(duration);

            Object::empty_tuple()
        }
        obj => Object::Error(ObjectError::UnexpectedType(
            vec![String::from("Float")],
            obj.kind(),
        )),
    }
}

pub fn komodo_time(ctx: ExecContext) -> Environment {
    env_with(vec![("sleep", Object::from_fn(sleep, 1))], ctx)
}
