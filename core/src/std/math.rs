use crate::{
    env::{env_with, Environment, ExecContext},
    object::{float::Float, Kind, Object, ObjectError},
};

fn abs(arg: &Object) -> Object {
    match arg {
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

fn hypot(arg: &Object) -> Object {
    let Object::Tuple(tuple) = arg else {
        return Object::Error(ObjectError::UnexpectedType(
            vec!["Tuple".into()],
            arg.kind(),
        ));
    };
    let [(x, _), (y, _)] = tuple.list.as_slice() else {
        return Object::Error(ObjectError::UnexpectedType(
            vec!["(Number, Number)".into()],
            arg.kind(),
        ));
    };
    match (x.to_float(), y.to_float()) {
        (Ok(x), Ok(y)) => Object::Float(x.hypot(y)),
        (Err(err), _) | (_, Err(err)) => err.into(),
    }
}

fn log(arg: &Object) -> Object {
    let Object::Tuple(tuple) = arg else {
        return ln(arg);
    };
    let [(value, _), (base, _)] = tuple.list.as_slice() else {
        return Object::Error(ObjectError::UnexpectedType(
            vec!["(Number, Integer)".into()],
            arg.kind(),
        ));
    };
    match base {
        Object::Integer(base) => match value.to_float() {
            Ok(f) => Float::try_from(base)
                .map(|num| Object::Float(f.ln() / num.ln()))
                .map_err(Object::from)
                .unwrap_or_else(|e| e),
            Err(err) => err.into(),
        },
        obj => Object::Error(ObjectError::UnexpectedType(
            vec!["Integer".into()],
            obj.kind(),
        )),
    }
}

macro_rules! float_fn {
    ($name:ident) => {
        fn $name(arg: &Object) -> Object {
            match arg.to_float() {
                Ok(f) => Object::Float(f.$name()),
                Err(err) => err.into(),
            }
        }
    };
}

macro_rules! round_fn {
    ($name:ident) => {
        fn $name(arg: &Object) -> Object {
            match arg {
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
            ("sin", Object::from_fn(sin, "sin".into())),
            ("cos", Object::from_fn(cos, "cos".into())),
            ("tan", Object::from_fn(tan, "tan".into())),
            ("asin", Object::from_fn(asin, "asin".into())),
            ("acos", Object::from_fn(acos, "acos".into())),
            ("atan", Object::from_fn(atan, "atan".into())),
            ("exp", Object::from_fn(exp, "exp".into())),
            ("ln", Object::from_fn(ln, "ln".into())),
            ("log", Object::from_fn(log, "log".into())),
            ("cbrt", Object::from_fn(cbrt, "cbrt".into())),
            ("sqrt", Object::from_fn(sqrt, "sqrt".into())),
            ("abs", Object::from_fn(abs, "abs".into())),
            ("hypot", Object::from_fn(hypot, "hypot".into())),
            ("round", Object::from_fn(round, "round".into())),
            ("floor", Object::from_fn(floor, "floor".into())),
            ("ceil", Object::from_fn(ceil, "ceil".into())),
        ],
        ctx,
    )
}
