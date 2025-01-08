use std::collections::BTreeMap;

use hifijson::{
    num::Parts,
    token::Lex,
    value::{parse_unbounded, Value},
    IterLexer,
};

use crate::{
    env::{env_with, Environment, ExecContext},
    lexer::Radix,
    object::{float::Float, integer::Integer, Kind, Object, ObjectError},
};

fn parse_value(value: Value<String, String>) -> Result<Object, ObjectError> {
    match value {
        Value::Null => Ok(Object::empty_tuple()),
        Value::Bool(bool) => Ok(bool.into()),
        Value::Number((num_str, Parts { dot, exp })) => match (dot, exp) {
            (None, None) => Ok(Object::Integer(Integer::new(&num_str, Radix::Decimal))),
            _ => Ok(Object::Float(Float::from_num_str(&num_str))),
        },
        Value::String(str) => Ok(Object::String(str.into())),
        Value::Array(vec) => {
            let list: Result<Vec<Object>, ObjectError> = vec.into_iter().map(parse_value).collect();
            Ok(Object::List(list?.into()))
        }
        Value::Object(pairs) => {
            let mut map = BTreeMap::new();
            for (key, value) in pairs {
                map.insert(Object::String(key.into()), parse_value(value)?);
            }

            Ok(Object::Dictionary(map.into()))
        }
    }
}

fn parse_json(input: String) -> Object {
    let mut lexer = IterLexer::new(input.into_bytes().into_iter().map(Ok::<u8, ()>));
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

fn to_json(obj: &Object) -> String {
    match obj {
        Object::Boolean(bool) => bool.to_string(),
        Object::Char(char) => format!("\"{char}\""),
        Object::Integer(integer) => integer.to_string(),
        Object::Float(float) => float.to_string(),
        Object::Fraction(_fraction) => "null".into(),
        Object::Symbol(_symbol) => "null".into(),
        Object::String(my_string) => format!("\"{my_string}\""),
        Object::Tuple(tuple) => {
            if tuple.is_empty() {
                "null".into()
            } else {
                let values = tuple
                    .list
                    .iter()
                    .map(|(obj, _)| to_json(obj))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("[{values}]")
            }
        }
        Object::List(list) => {
            let values = list
                .list
                .iter()
                .map(|(obj, _)| to_json(obj))
                .collect::<Vec<_>>()
                .join(",");
            format!("[{values}]")
        }
        Object::Set(_set) => "{}".into(),
        Object::Dictionary(dictionary) => {
            let values = dictionary
                .dict
                .iter()
                .map(|(key, val)| format!("{}: {}", to_json(key), to_json(val)))
                .collect::<Vec<_>>()
                .join(",");
            format!("{{{values}}}")
        }
        Object::Function(_function) => "null".into(),
        Object::Range(_range) => "null".into(),
        Object::Error(_object_error) => "null".into(),
    }
}

fn stringify(args: &[Object]) -> Object {
    Object::String(to_json(&args[0]).into())
}

pub fn komodo_json(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            ("parse", Object::from_fn(parse, 1)),
            ("stringify", Object::from_fn(stringify, 1)),
        ],
        ctx,
    )
}

#[cfg(test)]
mod tests {
    use crate::{
        object::{float::Float, Object},
        std::json::parse_json,
    };

    #[test]
    fn parse_number_with_point() {
        let input = "1.0";

        assert_eq!(
            parse_json(input.into()),
            Object::Float(Float::new("1", "0"))
        );
    }
}
