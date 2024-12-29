use crate::{
    env::{env_with, Environment, ExecContext},
    object::Object,
};

fn sin(args: &[Object]) -> Object {
    match args.first() {
        Some(Object::Float(f)) => Object::Float(f.sin()),
        _ => todo!(),
    }
}

pub fn komodo_math(ctx: ExecContext) -> Environment {
    env_with(vec![("sin", Object::from_fn(sin, 1))], ctx)
}
