use crate::{
    env::{env_with, Environment, ExecContext},
    object::{decimal::Float, Object},
};

macro_rules! float_fn {
    ($name:ident) => {
        fn $name(args: &[Object]) -> Object {
            match args.first() {
                Some(Object::Float(num)) => Object::Float(num.$name()),
                Some(Object::Integer(num)) => Object::Float(Float::from(num).$name()),
                Some(Object::Fraction(num)) => Object::Float(Float::from(num).$name()),
                _ => todo!(),
            }
        }
    };
}

float_fn!(sin);

pub fn komodo_math(ctx: ExecContext) -> Environment {
    env_with(vec![("sin", Object::from_fn(sin, 1))], ctx)
}
