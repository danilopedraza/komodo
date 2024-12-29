use crate::{
    env::{env_with, Environment, ExecContext},
    object::{float::Float, Kind, Object, ObjectError},
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
float_fn!(cos);
float_fn!(tan);
float_fn!(asin);
float_fn!(acos);
float_fn!(atan);

pub fn komodo_math(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            ("sin", Object::from_fn(sin, 1)),
            ("cos", Object::from_fn(cos, 1)),
            ("tan", Object::from_fn(tan, 1)),
            ("asin", Object::from_fn(asin, 1)),
            ("acos", Object::from_fn(acos, 1)),
            ("atan", Object::from_fn(atan, 1)),
        ],
        ctx,
    )
}
