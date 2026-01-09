use crate::{
    env::{env_with, Environment, ExecContext},
    object::{float::Float, Kind, Object, ObjectError},
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

fn hypot(args: &[Object]) -> Object {
    match (args[0].to_float(), args[1].to_float()) {
        (Ok(x), Ok(y)) => Object::Float(x.hypot(y)),
        (Err(err), _) => err.into(),
        (_, Err(err)) => err.into(),
    }
}

fn log(args: &[Object]) -> Object {
    match args.get(1) {
        None => ln(args),
        Some(Object::Integer(base)) => match args[0].to_float() {
            Ok(f) => Object::Float(f.ln() / Float::from(base).ln()),
            Err(err) => err.into(),
        },
        Some(obj) => Object::Error(ObjectError::UnexpectedType(
            vec![String::from("Integer")],
            obj.kind(),
        )),
    }
}

macro_rules! float_fn {
    ($name:ident) => {
        fn $name(args: &[Object]) -> Object {
            match args[0].to_float() {
                Ok(f) => Object::Float(f.$name()),
                Err(err) => err.into(),
            }
        }
    };
}

macro_rules! round_fn {
    ($name:ident) => {
        fn $name(args: &[Object]) -> Object {
            match &args[0] {
                Object::Integer(int) => Object::Integer(int.to_owned()),
                Object::Float(f) => match f.$name() {
                    Ok(int) => Object::Integer(int),
                    Err(err) => err.into(),
                },
                Object::Fraction(f) => Object::Integer(f.$name()),
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

round_fn!(round);
round_fn!(floor);
round_fn!(ceil);

pub fn komodo_math(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            ("sin", Object::from_fn(sin, "sin".into(), 1)),
            ("cos", Object::from_fn(cos, "cos".into(), 1)),
            ("tan", Object::from_fn(tan, "tan".into(), 1)),
            ("asin", Object::from_fn(asin, "asin".into(), 1)),
            ("acos", Object::from_fn(acos, "acos".into(), 1)),
            ("atan", Object::from_fn(atan, "atan".into(), 1)),
            ("exp", Object::from_fn(exp, "exp".into(), 1)),
            ("ln", Object::from_fn(ln, "ln".into(), 1)),
            ("log", Object::from_fn(log, "log".into(), 1)),
            ("cbrt", Object::from_fn(cbrt, "cbrt".into(), 1)),
            ("sqrt", Object::from_fn(sqrt, "sqrt".into(), 1)),
            ("abs", Object::from_fn(abs, "abs".into(), 1)),
            ("hypot", Object::from_fn(hypot, "hypot".into(), 2)),
            ("round", Object::from_fn(round, "round".into(), 1)),
            ("floor", Object::from_fn(floor, "floor".into(), 1)),
            ("ceil", Object::from_fn(ceil, "ceil".into(), 1)),
        ],
        ctx,
    )
}
