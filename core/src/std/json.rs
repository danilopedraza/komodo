use hifijson::{
    num::Parts, token::Lex, value::{parse_unbounded, Value}, IterLexer
};

use crate::{
    env::{env_with, Environment, ExecContext},
    object::{Kind, Object, ObjectError},
};

fn parse_value(value: Value<String, String>) -> Result<Object, ObjectError> {
    match value {
        Value::Null => Ok(Object::empty_tuple()),
        Value::Bool(bool) => Ok(bool.into()),
        Value::Number((num_str, Parts { dot, exp })) => match (dot, exp) {
            (Some(dot), Some(exp)) => todo!(),
            (Some(dot), None) => todo!(),
            (None, Some(exp)) => todo!(),
            (None, None) => todo!(),
        },
        Value::String(str) => Ok(Object::String(str.into())),
        Value::Array(vec) => todo!(),
        Value::Object(vec) => todo!(),
    }
}

fn parse_json(input: String) -> Object {
    let mut lexer = IterLexer::new(input.into_bytes().into_iter().map(|b| Ok::<u8, ()>(b)));
    let token = if let Some(token) = lexer.ws_token() {
        token
    } else {
        todo!()
    };

    match parse_unbounded(token, &mut lexer) {
        Ok(value) => match parse_value(value) {
            Ok(obj) => obj,
            Err(err) => Object::Error(err),
        },
        Err(err) => Object::Error(ObjectError::BadJSONParse(err.to_string())),
    }
}

fn parse(args: &[Object]) -> Object {
    match &args[0] {
        Object::String(str) => parse_json(str.to_owned().into()),
        Object::Char(chr) => parse_json(String::from(chr)),
        obj => Object::Error(ObjectError::UnexpectedType(
            vec![String::from("String"), String::from("Char")],
            obj.kind(),
        )),
    }
}

pub fn komodo_json(ctx: ExecContext) -> Environment {
    env_with(vec![("parse", Object::from_fn(parse, 1))], ctx)
}
