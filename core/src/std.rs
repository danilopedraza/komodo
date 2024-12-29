use crate::{
    env::{env_with, Environment, ExecContext},
    object::{decimal::Float, Kind, Object, ObjectError},
};

macro_rules! float_fn {
    ($name:ident) => {
        fn $name(args: &[Object]) -> Object {
            match &args[0] {
                Object::Float(num) => Object::Float(num.$name()),
                Object::Integer(num) => Object::Float(Float::from(num).$name()),
                Object::Fraction(num) => Object::Float(Float::from(num).$name()),
                val => ObjectError::UnexpectedType(
                    vec![
                        String::from("Float"),
                        String::from("Integer"),
                        String::from("Fraction"),
                    ],
                    val.kind(),
                )
                .into(),
            }
        }
    };
}

float_fn!(sin);

pub fn komodo_math(ctx: ExecContext) -> Environment {
    env_with(vec![("sin", Object::from_fn(sin, 1))], ctx)
}
