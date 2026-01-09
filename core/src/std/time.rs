use std::{
    thread::{self},
    time::{Duration, SystemTime, UNIX_EPOCH},
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

fn time(_args: &[Object]) -> Object {
    let start = SystemTime::now();
    let since_epoch = start.duration_since(UNIX_EPOCH).unwrap();

    Object::Float(since_epoch.as_secs_f64().into())
}

pub fn komodo_time(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            ("sleep", Object::from_fn(sleep, "sleep".into(), 1)),
            ("time", Object::from_fn(time, "time".into(), 0)),
        ],
        ctx,
    )
}
