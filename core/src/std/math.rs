use crate::{
    env::{env_with, Environment, ExecContext},
    object::{Kind, Object, ObjectError},
};

fn abs(args: &[Object]) -> Object {
    match &args[0] {
        Object::Float(num) => Object::Float(num.abs()),
        Object::Integer(num) => Object::Integer(num.abs()),
        Object::Fraction(num) => Object::Fraction(num.abs()),
        obj => Object::Error(ObjectError::UnexpectedType(
            vec![
                String::from("Float"),
                String::from("Integer"),
                String::from("Fraction"),
            ],
            obj.kind(),
        )),
    }
}

macro_rules! float_fn {
    ($name:ident) => {
        fn $name(args: &[Object]) -> Object {
            match args[0].as_float() {
                Ok(f) => Object::Float(f.$name()),
                Err(err) => err.into(),
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
float_fn!(exp);
float_fn!(ln);
float_fn!(sqrt);
float_fn!(cbrt);

pub fn komodo_math(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            ("sin", Object::from_fn(sin, 1)),
            ("cos", Object::from_fn(cos, 1)),
            ("tan", Object::from_fn(tan, 1)),
            ("asin", Object::from_fn(asin, 1)),
            ("acos", Object::from_fn(acos, 1)),
            ("atan", Object::from_fn(atan, 1)),
            ("exp", Object::from_fn(exp, 1)),
            ("ln", Object::from_fn(ln, 1)),
            ("cbrt", Object::from_fn(cbrt, 1)),
            ("sqrt", Object::from_fn(sqrt, 1)),
            ("abs", Object::from_fn(abs, 1)),
        ],
        ctx,
    )
}
