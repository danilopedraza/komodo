//! Iterative collection primitives used by std/utils.komodo.
use crate::{
    env::{env_with, Environment, ExecContext},
    error::{Error, Position},
    exec::{truthy, ExecError, ObjectIterator},
    object::{ExternFunction, Function, Object},
};
pub const NAMES: &[&str] = &[
    "reduce",
    "filter",
    "filterFirst",
    "some",
    "every",
    "isPrefix",
];
fn unmatched(env: &Environment, pos: Position) -> Error {
    Error::with_position(ExecError::UnmatchedCall.into(), pos, env.file_path())
}
fn apply(
    arg: &Object,
    env: &mut Environment,
    pos: Position,
    operation: &str,
) -> Result<Object, Error> {
    let Object::Tuple(args) = arg else {
        return Err(unmatched(env, pos));
    };
    if operation == "isPrefix" {
        return Ok(match args.list.as_slice() {
            [prefix, Object::String(value)] => value.has_prefix(prefix),
            _ => false,
        }
        .into());
    }
    // Empty reductions historically return the initial value without inspecting the callback.
    if operation == "reduce" {
        if let [values, _, initial] = args.list.as_slice() {
            if matches!(values, Object::List(v) if v.is_empty())
                || matches!(values, Object::Set(v) if v.is_empty())
            {
                return Ok(initial.clone());
            }
        }
    }
    let (values, callback, initial) = match args.list.as_slice() {
        [values, Object::Function(f), initial] if operation == "reduce" => {
            (values, f, initial.clone())
        }
        [values, Object::Function(f)] if operation != "reduce" => {
            (values, f, Object::empty_tuple())
        }
        _ => return Err(unmatched(env, pos)),
    };
    if matches!(operation, "filter" | "filterFirst") && matches!(values, Object::Range(_)) {
        return Err(unmatched(env, pos));
    }
    // The existing filterFirst API accepts lists and the empty set only.
    if operation == "filterFirst" && matches!(values, Object::Set(v) if !v.is_empty()) {
        return Err(unmatched(env, pos));
    }
    let iter = ObjectIterator::new(values.clone()).ok_or_else(|| unmatched(env, pos))?;
    let mut result = initial;
    let mut output = Vec::with_capacity(if matches!(operation, "filter" | "filterFirst") {
        iter.size_hint().0
    } else {
        0
    });
    let mut removed = false;
    let mut f = callback.clone();
    for value in iter {
        let value = value?;
        if operation == "reduce" {
            result = f.call(&Object::Tuple(vec![result, value].into()), env, pos)?;
            continue;
        }
        if operation == "filterFirst" && removed {
            output.push(value);
            continue;
        }
        let predicate = f.call(&value, env, pos)?;
        let keep = if matches!(operation, "some" | "every") {
            let every = operation == "every";
            let op = if every {
                crate::ast::InfixOperator::LogicAnd
            } else {
                crate::ast::InfixOperator::Or
            };
            truthy(&crate::exec::infix(
                op,
                &every.into(),
                &predicate,
                pos,
                env,
            )?)
        } else {
            truthy(&predicate)
        };
        match operation {
            "some" if keep => return Ok(true.into()),
            "every" if !keep => return Ok(false.into()),
            "filter" | "filterFirst" => {
                if keep {
                    output.push(value);
                } else {
                    removed = true;
                }
            }
            _ => {}
        }
    }
    Ok(match operation {
        "reduce" => result,
        "some" => false.into(),
        "every" => true.into(),
        _ if matches!(values, Object::Set(_)) => Object::Set(
            output
                .into_iter()
                .collect::<std::collections::BTreeSet<_>>()
                .into(),
        ),
        _ => Object::List(output.into()),
    })
}
macro_rules! primitive {
    ($name:ident, $operation:literal) => {
        fn $name(arg: &Object, env: &mut Environment, pos: Position) -> Result<Object, Error> {
            apply(arg, env, pos, $operation)
        }
    };
}
primitive!(reduce, "reduce");
primitive!(filter, "filter");
primitive!(filter_first, "filterFirst");
primitive!(some, "some");
primitive!(every, "every");
primitive!(is_prefix, "isPrefix");
pub fn environment(ctx: ExecContext) -> Environment {
    env_with(
        vec![
            (
                "reduce",
                reduce as fn(&Object, &mut Environment, Position) -> Result<Object, Error>,
            ),
            ("filter", filter),
            ("filterFirst", filter_first),
            ("some", some),
            ("every", every),
            ("isPrefix", is_prefix),
        ]
        .into_iter()
        .map(|(name, f)| {
            (
                name,
                Object::Function(Function::Extern(ExternFunction::contextual(
                    f,
                    format!("collections.{name}"),
                ))),
            )
        })
        .collect(),
        ctx,
    )
}

#[cfg(test)]
mod tests {
    use crate::{builtin::standard_env, env::ExecContext, run::run};
    #[test]
    fn quantifiers_preserve_predicate_errors_and_short_circuit() {
        for name in ["some", "every"] {
            let mut env = standard_env(ExecContext::default());
            assert!(run(
                &format!("from collections import ({name})\n{name}([1], x -> 1)"),
                &mut env
            )
            .is_err());
        }
        let mut env = standard_env(ExecContext::default());
        run("from collections import (some, every, reduce, filterFirst)\nassert(some([1, 0], x -> 1 / x = 1))\nassert(!every([1, 0], x -> 1 / x /= 1))\nassert(reduce([], (), 42) = 42)\nassert(filterFirst([1, 0], x -> 1 / x /= 1) = [0])", &mut env).unwrap();
    }
}
