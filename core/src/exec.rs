use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::error::{Error, Position};
use crate::lexer::Radix;
use crate::matcher::{match_, Match};
use crate::object::fraction::Fraction;
use crate::object::{
    self, float::Float, AnonFunction, Dictionary, Function, FunctionPatternKind, Kind, List,
    PatternFunction, Range,
};

type ExecResult<T> = Result<T, Error>;

use crate::ast::{Constant, InfixOperator, Pattern};
use crate::cst::{ComprehensionKind, PrefixOperator};
use crate::env::{EnvResponse, Environment, ScopeKind, ValueKind};
use crate::ir::{AssignableSymbol, Block, Op};
use crate::object::{integer::Integer, Bool, Char, MyString, Object, Set, Symbol};
use crate::run::{self, ModuleAddress};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExecError {
    BadFraction {
        numer_kind: String,
        denom_kind: String,
    },
    BadMatch,
    DenominatorZero,
    IndexingNonContainer {
        kind: String,
    },
    InmutableAssign(String),
    InvalidIndex {
        kind: String,
    },
    ListIndexOutOfBounds,
    MutationOutOfScope {
        name: String,
    },
    NonCallableObject(String),
    NonExistentKey {
        key: String,
    },
    NonExistentPrefixOperation {
        op: String,
        rhs: String,
    },
    NonExistentInfixOperation {
        op: String,
        lhs: String,
        rhs: String,
    },
    NonIterableObject(String),
    NonPrependableObject(String),
    UnknownValue(String),
    UnmatchedCall,
    UnmatchedExpression,
}

pub fn truthy(val: &Object) -> bool {
    match val {
        Object::Boolean(boolean) => boolean.value(),
        _ => false,
    }
}

pub fn constant_list(list: &[Constant]) -> Object {
    let list: Vec<Object> = list.iter().map(eval_constant).collect();

    Object::List(list.into())
}

pub fn constant_tuple(list: &[Constant]) -> Object {
    let list: Vec<Object> = list.iter().map(eval_constant).collect();

    Object::Tuple(list.into())
}

pub fn constant_set(list: &[Constant]) -> Object {
    let list: Vec<Object> = list.iter().map(eval_constant).collect();

    Object::Set(list.into())
}

fn function(param: &Pattern, result: &Block, env: &Environment) -> ExecResult<Object> {
    Ok(Object::Function(object::Function::Anonymous(Box::new(
        AnonFunction::new(param.to_owned(), result.to_owned(), env.clone()),
    ))))
}

/// Execute a compiler-produced block with a fresh operand stack.
pub fn exec(node: &Block, env: &mut Environment) -> ExecResult<Object> {
    let mut stack = Vec::new();
    for instruction in &node.code {
        let pos = instruction.position;
        let res = match &instruction.op {
            Op::Drop => {
                stack.pop().expect("IR stack underflow");
                continue;
            }
            Op::Constant(c) => Ok(eval_constant(c)),
            Op::Load(name) => symbol(name, env, pos),
            Op::Infix(op) => {
                let rhs = pop(&mut stack);
                let lhs = pop(&mut stack);
                infix(*op, &lhs, &rhs, pos, env)
            }
            Op::Prefix(op) => prefix(*op, pop(&mut stack), pos, env),
            Op::List(n) => Ok(Object::List(take(&mut stack, *n).into())),
            Op::Tuple(n) => Ok(Object::Tuple(take(&mut stack, *n).into())),
            Op::Set(n) => Ok(Object::Set(take(&mut stack, *n).into())),
            Op::Dictionary(n) => {
                let values = take(&mut stack, n * 2);
                let mut dict = Dictionary::default();
                for pair in values.as_chunks::<2>().0 {
                    dict.dict.insert(pair[0].clone(), pair[1].clone());
                }
                Ok(Object::Dictionary(dict))
            }
            Op::Fraction(denom_pos) => {
                let denom = pop(&mut stack);
                let numer = pop(&mut stack);
                fraction(numer, denom, *denom_pos, pos, env)
            }
            Op::Index { container, index } => {
                let key = pop(&mut stack);
                let obj = pop(&mut stack);
                match obj {
                    Object::List(list) => list
                        .get(&key)
                        .map_err(|e| Error::with_position(e.into(), *index, env.file_path())),
                    Object::Dictionary(dict) => dict
                        .get(&key)
                        .map_err(|e| Error::with_position(e.into(), *index, env.file_path())),
                    obj => Err(Error::with_position(
                        ExecError::IndexingNonContainer { kind: obj.kind() }.into(),
                        *container,
                        env.file_path(),
                    )),
                }
            }
            Op::Cons(tail_pos) => {
                let tail = pop(&mut stack);
                let first = pop(&mut stack);
                cons(first, tail, *tail_pos, env)
            }
            Op::SetCons(tail_pos) => {
                let tail = pop(&mut stack);
                let first = pop(&mut stack);
                set_cons(first, tail, *tail_pos, env)
            }
            Op::Function { param, body } => function(param, body, env),
            Op::If { positive, negative } => if_(pop(&mut stack), positive, negative, env),
            Op::Case { expr, pairs } => case(expr, pairs, env),
            Op::For {
                pattern,
                iterator,
                body,
            } => for_(pattern, iterator, body, env),
            Op::Comprehension {
                element,
                variable,
                iterator,
                kind,
            } => comprehension(element, variable, iterator, *kind, env),
            Op::Bind {
                pattern,
                mutable,
                value_position,
            } => let_pattern(
                pattern,
                pop(&mut stack),
                *value_position,
                if *mutable {
                    ValueKind::Mutable
                } else {
                    ValueKind::Inmutable
                },
                env,
            ),
            Op::Symbolic { name, constraint } => let_without_value(name, constraint, env),
            Op::DefineFunction {
                name,
                param,
                body,
                memoized,
            } => let_function(
                name,
                param,
                body,
                if *memoized {
                    FunctionPatternKind::Memoized
                } else {
                    FunctionPatternKind::NotMemoized
                },
                env,
            ),
            Op::Assign {
                left,
                value_position,
            } => {
                let index = if matches!(left, AssignableSymbol::IndexableValue { .. }) {
                    Some(pop(&mut stack))
                } else {
                    None
                };
                assignment(left, pop(&mut stack), index, *value_position, env)
            }
            Op::Import { source, values } => import_from(source, values, env),
            Op::Call { name, callee } => {
                let arg = pop(&mut stack);
                let func = pop(&mut stack);
                call(func, arg, name.as_deref(), *callee, pos, env)
            }
            Op::Attribute { name, position } => object_attr(pop(&mut stack), name, *position, env),
            Op::MethodCall {
                receiver,
                method,
                argument,
                attribute,
            } => {
                let receiver_obj = exec(receiver, env)?;
                let (func, arg) = if matches!(receiver_obj, Object::Dictionary(_)) {
                    // Preserve dictionary method lookup behavior, including receiver reevaluation.
                    let obj = exec(receiver, env)?;
                    (
                        object_attr(
                            obj,
                            attribute.as_deref().expect("attribute must be a symbol"),
                            method.position,
                            env,
                        )?,
                        exec(argument, env)?,
                    )
                } else {
                    let func = exec(method, env)?;
                    let arg = exec(argument, env)?;
                    let mut values = vec![receiver_obj];
                    match arg {
                        Object::Tuple(tuple) => values.extend(tuple.list),
                        obj => values.push(obj),
                    }
                    let arg = if values.len() == 1 {
                        values.pop().unwrap()
                    } else {
                        Object::Tuple(values.into())
                    };
                    (func, arg)
                };
                call(func, arg, None, receiver.position, pos, env)
            }
        }?;
        if let Object::Error(err) = res {
            return Err(Error::with_position(err.into(), pos, env.file_path()));
        }
        stack.push(res);
    }
    debug_assert_eq!(stack.len(), 1, "invalid IR block stack effect");
    Ok(pop(&mut stack))
}
fn pop(stack: &mut Vec<Object>) -> Object {
    stack.pop().expect("IR stack underflow")
}
fn take(stack: &mut Vec<Object>, count: usize) -> Vec<Object> {
    stack.split_off(stack.len().checked_sub(count).expect("IR stack underflow"))
}

pub fn eval_constant(constant: &Constant) -> Object {
    match constant {
        Constant::Boolean(val) => boolean(*val),
        Constant::Char(chr) => char(*chr),
        Constant::Decimal { int, dec } => decimal(int, dec),
        Constant::Dictionary { pairs, complete: _ } => constant_dictionary(pairs),
        Constant::Integer { literal, radix } => integer(literal, *radix),
        Constant::List { list } => constant_list(list),
        Constant::Set { list } => constant_set(list),
        Constant::String { str } => string(str),
        Constant::Tuple { list } => constant_tuple(list),
    }
}

fn case(expr: &Block, pairs: &[(Pattern, Block)], env: &mut Environment) -> ExecResult<Object> {
    let expr_obj = exec(expr, env)?;

    for (pattern, res) in pairs {
        if let Some(Match(map)) = match_(pattern, &expr_obj) {
            env.push_scope(ScopeKind::Block);
            for (key, val) in map {
                env.set_inmutable(&key, val);
            }

            let res = exec(res, env);

            env.pop_scope();

            return res;
        }
    }

    Err(Error::with_position(
        ExecError::UnmatchedExpression.into(),
        expr.position,
        env.file_path(),
    ))
}

fn assignment(
    left: &AssignableSymbol,
    evaluated_value: Object,
    index: Option<Object>,
    value_position: Position,
    env: &mut Environment,
) -> ExecResult<Object> {
    match left {
        AssignableSymbol::Pattern(pattern) => {
            make_assignment(
                destructure(pattern, value_position, &evaluated_value, env)?,
                env,
                value_position,
            )?;
            Ok(evaluated_value)
        }
        AssignableSymbol::ObjectValue { object, value } => {
            let object = get_mutable_value(object, env, value_position)?;

            let element_ref = match object {
                Object::Dictionary(dict) => {
                    match dict.get_mut(&Object::String(value.as_str().into())) {
                        Ok(obj) => Ok(obj),
                        Err(eval_err) => Err(Error::with_position(
                            eval_err.into(),
                            value_position,
                            env.file_path(),
                        )),
                    }
                }
                obj => Err(Error::with_position(
                    ExecError::IndexingNonContainer { kind: obj.kind() }.into(),
                    value_position,
                    env.file_path(),
                )),
            }?;

            *element_ref = evaluated_value;
            Ok(element_ref.to_owned())
        }
        AssignableSymbol::IndexableValue { container } => {
            let index = index.expect("indexed assignment requires an index operand");
            let object = get_mutable_value(container, env, value_position)?;

            let element_ref = match object {
                Object::List(list) => match list.get_mut(&index) {
                    Ok(obj) => Ok(obj),
                    Err(eval_err) => Err(Error::with_position(
                        eval_err.into(),
                        value_position,
                        env.file_path(),
                    )),
                },
                obj => Err(Error::with_position(
                    ExecError::IndexingNonContainer { kind: obj.kind() }.into(),
                    value_position,
                    env.file_path(),
                )),
            }?;

            *element_ref = evaluated_value;
            Ok(element_ref.to_owned())
        }
    }
}

fn destructure(
    pattern: &Pattern,
    pos: Position,
    val: &Object,
    env: &mut Environment,
) -> ExecResult<BTreeMap<String, Object>> {
    match match_(pattern, val) {
        Some(Match(map)) => Ok(map),
        None => Err(Error::with_position(
            ExecError::BadMatch.into(),
            pos,
            env.file_path(),
        )),
    }
}

fn make_assignment(
    map: BTreeMap<String, Object>,
    env: &mut Environment,
    position: Position,
) -> Result<(), Error> {
    for (key, val) in map {
        let obj_ref = get_mutable_value(&key, env, position)?;
        *obj_ref = val;
    }

    Ok(())
}

fn get_mutable_value<'a>(
    name: &str,
    env: &'a mut Environment,
    position: Position,
) -> Result<&'a mut Object, Error> {
    let path = env.file_path();
    match env.get(name) {
        EnvResponse::Mutable(obj_ref) => Ok(obj_ref),
        EnvResponse::MutableOriginally(_) => Err(Error::with_position(
            ExecError::MutationOutOfScope {
                name: name.to_string(),
            }
            .into(),
            position,
            path,
        )),
        EnvResponse::Inmutable(_) => Err(Error::with_position(
            ExecError::InmutableAssign(name.into()).into(),
            position,
            path,
        )),
        EnvResponse::NotFound => Err(Error::with_position(
            ExecError::UnknownValue(name.into()).into(),
            position,
            path,
        )),
    }
}

fn import_from(
    module: &ModuleAddress,
    values: &[(String, Position)],
    env: &mut Environment,
) -> ExecResult<Object> {
    run::import_from(module, values, env)?;
    Ok(Object::empty_tuple())
}

#[allow(
    clippy::mutable_key_type,
    reason = "Object ordering uses stable captured-environment identity; see PatternFunction::cmp"
)]
fn set_cons(
    some: Object,
    most: Object,
    most_position: Position,
    env: &mut Environment,
) -> ExecResult<Object> {
    match most {
        Object::List(lst) => {
            let mut res = BTreeSet::new();
            res.insert(some);

            for obj in lst.list {
                res.insert(obj.to_owned());
            }

            Ok(Object::Set(res.into()))
        }
        Object::Set(set) => {
            let mut res = BTreeSet::new();
            res.insert(some);

            for obj in set.iter() {
                res.insert(obj.to_owned());
            }

            Ok(Object::Set(res.into()))
        }
        obj => Err(Error::WithPosition(
            ExecError::NonPrependableObject(obj.kind()).into(),
            most_position,
            env.file_path(),
        )),
    }
}

fn constant_dictionary(pairs: &Vec<(Constant, Constant)>) -> Object {
    let mut dict = Dictionary::default();

    for (key, value) in pairs {
        dict.dict.insert(eval_constant(key), eval_constant(value));
    }

    Object::Dictionary(dict)
}

fn decimal(int: &str, dec: &str) -> Object {
    Object::Float(Float::new(int, dec))
}

fn cons(
    first: Object,
    most: Object,
    most_position: Position,
    env: &mut Environment,
) -> ExecResult<Object> {
    match most {
        Object::List(lst) => {
            let mut res = vec![first];

            for obj in lst.list {
                res.push(obj.to_owned());
            }

            Ok(Object::List(res.into()))
        }
        Object::Set(set) => {
            let mut res = vec![first];

            for obj in set.iter() {
                res.push(obj.to_owned());
            }

            Ok(Object::List(res.into()))
        }
        obj => Err(Error::WithPosition(
            ExecError::NonPrependableObject(obj.kind()).into(),
            most_position,
            env.file_path(),
        )),
    }
}

fn string(str: &str) -> Object {
    Object::String(MyString::from(str))
}

fn char(chr: char) -> Object {
    Object::Char(Char::from(chr))
}

fn boolean(val: bool) -> Object {
    Object::Boolean(Bool::from(val))
}

fn let_pattern(
    left: &Pattern,
    value: Object,
    value_position: Position,
    kind: ValueKind,
    env: &mut Environment,
) -> ExecResult<Object> {
    match match_(left, &value) {
        Some(Match(map)) => {
            for (name, val) in map {
                match kind {
                    ValueKind::Mutable => env.set_mutable(&name, val),
                    ValueKind::Inmutable => env.set_inmutable(&name, val),
                }
            }

            Ok(value)
        }
        None => Err(Error::with_position(
            ExecError::BadMatch.into(),
            value_position,
            env.file_path(),
        )),
    }
}

fn let_without_value(name: &str, property: &str, env: &mut Environment) -> ExecResult<Object> {
    let symbol = Object::Symbol(Symbol {
        name: name.to_owned(),
        property: property.to_owned(),
    });

    env.set_inmutable(name, symbol.clone());

    Ok(symbol)
}

fn integer(str: &str, radix: Radix) -> Object {
    Object::Integer(Integer::new(str, radix))
}

fn symbol(str: &str, env: &mut Environment, position: Position) -> ExecResult<Object> {
    match env.get(str) {
        EnvResponse::Inmutable(obj) | EnvResponse::MutableOriginally(obj) => Ok(obj.to_owned()),
        EnvResponse::Mutable(obj) => Ok(obj.to_owned()),
        EnvResponse::NotFound => Err(Error::with_position(
            ExecError::UnknownValue(str.to_owned()).into(),
            position,
            env.file_path(),
        )),
    }
}

#[allow(
    clippy::mutable_key_type,
    reason = "Object ordering uses stable captured-environment identity; see PatternFunction::cmp"
)]
fn comprehension(
    element: &Block,
    variable: &str,
    iterator: &Block,
    kind: ComprehensionKind,
    env: &mut Environment,
) -> ExecResult<Object> {
    let iterator = get_iterable(iterator, env)?;

    match kind {
        ComprehensionKind::List => {
            let mut new_list = vec![];
            env.push_scope(ScopeKind::Block);

            for val in iterator {
                env.set_inmutable(variable, val);
                new_list.push(exec(element, env)?);
            }

            env.pop_scope();

            Ok(Object::List(List::from(new_list)))
        }
        ComprehensionKind::Set => {
            let mut new_set = BTreeSet::new();
            env.push_scope(ScopeKind::Block);

            for val in iterator {
                env.set_inmutable(variable, val);
                new_set.insert(exec(element, env)?);
            }

            env.pop_scope();

            Ok(Object::Set(Set::from(new_set)))
        }
    }
}

fn let_function(
    name: &str,
    param: &Pattern,
    value: &Block,
    kind: FunctionPatternKind,
    env: &mut Environment,
) -> ExecResult<Object> {
    let function: &mut PatternFunction = match env.get(name) {
        EnvResponse::NotFound => {
            env.set_mutable(
                name,
                Object::Function(Function::Pattern(PatternFunction::new(Rc::new(
                    RefCell::new(env.clone()),
                )))),
            );

            match env.get(name) {
                EnvResponse::Mutable(Object::Function(Function::Pattern(f))) => f,
                _ => unimplemented!(),
            }
        }
        EnvResponse::Mutable(Object::Function(Function::Pattern(f))) => f,
        _ => unimplemented!(),
    };

    function.add_pattern(param, value, kind);
    function
        .env
        .borrow_mut()
        .set_mutable(name, Object::Function(Function::Pattern(function.clone())));

    Ok(Object::Function(Function::Pattern(function.clone())))
}

fn if_(cond: Object, first: &Block, second: &Block, env: &mut Environment) -> ExecResult<Object> {
    if truthy(&cond) {
        exec(first, env)
    } else {
        exec(second, env)
    }
}

fn for_(
    pattern: &Pattern,
    iterable: &Block,
    proc: &Block,
    env: &mut Environment,
) -> ExecResult<Object> {
    let iter = get_iterable(iterable, env)?;

    env.push_scope(ScopeKind::Loop);

    for val in iter {
        for (name, val) in destructure(pattern, iterable.position, &val, env)? {
            env.set_inmutable(&name, val.clone());
        }

        exec(proc, env)?;
    }

    env.pop_scope();

    Ok(Object::empty_tuple())
}

fn get_iterable(
    node: &Block,
    env: &mut Environment,
) -> Result<Box<dyn Iterator<Item = Object>>, Error> {
    match exec(node, env)? {
        Object::Set(set) => Ok(Box::new(set.into_iter())),
        Object::List(list) => Ok(Box::new(list.list.into_iter())),
        Object::Range(range) => Ok(Box::new(range.into_iter())),
        obj => Err(Error::WithPosition(
            ExecError::NonIterableObject(obj.kind()).into(),
            node.position,
            env.file_path(),
        )),
    }
}

fn call(
    func: Object,
    argument: Object,
    func_name: Option<&str>,
    callee_pos: Position,
    call_pos: Position,
    env: &mut Environment,
) -> ExecResult<Object> {
    match func {
        Object::Function(mut f) => {
            let res = f.call(&argument, env, call_pos);
            if let Some(name) = func_name {
                env.set_mutable(name, Object::Function(f.clone()));
            }
            res
        }
        obj => Err(Error::WithPosition(
            ExecError::NonCallableObject(obj.kind()).into(),
            callee_pos,
            env.file_path(),
        )),
    }
}

fn range(start: &Object, end: &Object) -> Option<Object> {
    match (start, end) {
        (Object::Integer(start), Object::Integer(end)) => {
            Some(Object::Range(Range::new(start, end)))
        }
        _ => None,
    }
}

fn fraction(
    numer: Object,
    denom: Object,
    denom_position: Position,
    position: Position,
    env: &mut Environment,
) -> ExecResult<Object> {
    match (numer, denom) {
        (Object::Integer(_), Object::Integer(int)) if int.is_zero() => Err(Error::with_position(
            ExecError::DenominatorZero.into(),
            denom_position,
            env.file_path(),
        )),
        (Object::Integer(numer), Object::Integer(denom)) => {
            Ok(Object::Fraction(Fraction::new(numer, denom)))
        }
        (numer, denom) => Err(Error::with_position(
            ExecError::BadFraction {
                numer_kind: numer.kind(),
                denom_kind: denom.kind(),
            }
            .into(),
            position,
            env.file_path(),
        )),
    }
}

fn object_attr(
    obj: Object,
    name: &str,
    position: Position,
    env: &Environment,
) -> ExecResult<Object> {
    match obj {
        Object::Dictionary(dict) => dict
            .get(&Object::String(name.into()))
            .map_err(|err| Error::with_position(err.into(), position, env.file_path())),
        obj => Err(Error::with_position(
            ExecError::IndexingNonContainer { kind: obj.kind() }.into(),
            position,
            env.file_path(),
        )),
    }
}

fn infix(
    op: InfixOperator,
    lhs: &Object,
    rhs: &Object,
    infix_pos: Position,
    env: &mut Environment,
) -> ExecResult<Object> {
    let lhs_kind = lhs.kind();
    let rhs_kind = rhs.kind();

    let res = match op {
        InfixOperator::BitwiseAnd => lhs.bitwise_and(rhs),
        InfixOperator::BitwiseXor => lhs.bitwise_xor(rhs),
        InfixOperator::Division => {
            if rhs.is_zero() {
                return Err(Error::with_position(
                    ExecError::DenominatorZero.into(),
                    infix_pos,
                    env.file_path(),
                ));
            } else {
                lhs.over(rhs)
            }
        }
        InfixOperator::Equality => lhs.equality(rhs),
        InfixOperator::Exponentiation => lhs.pow(rhs),
        InfixOperator::Greater => lhs.greater(rhs),
        InfixOperator::GreaterEqual => lhs.greater_equal(rhs),
        InfixOperator::In => rhs.contains(lhs),
        InfixOperator::LeftShift => lhs.left_shift(rhs),
        InfixOperator::Less => lhs.less(rhs),
        InfixOperator::LessEqual => lhs.less_equal(rhs),
        InfixOperator::LogicAnd => lhs.logic_and(rhs),
        InfixOperator::Or => lhs.or(rhs),
        InfixOperator::Rem => lhs.rem(rhs),
        InfixOperator::NotEquality => lhs.neq(rhs),
        InfixOperator::Product => lhs.product(rhs),
        InfixOperator::RightShift => lhs.right_shift(rhs),
        InfixOperator::Substraction => lhs.substraction(rhs),
        InfixOperator::Sum => lhs.sum(rhs),
        InfixOperator::Range => range(lhs, rhs),
    };

    match res {
        None => Err(Error::WithPosition(
            ExecError::NonExistentInfixOperation {
                op: op.ident(),
                lhs: lhs_kind,
                rhs: rhs_kind,
            }
            .into(),
            infix_pos,
            env.file_path(),
        )),
        Some(obj) => Ok(obj),
    }
}

fn prefix(
    op: PrefixOperator,
    obj: Object,
    prefix_pos: Position,
    env: &Environment,
) -> ExecResult<Object> {
    let res = match op {
        PrefixOperator::BitwiseNot => obj.bitwise_not(),
        PrefixOperator::LogicNot => obj.logic_not(),
        PrefixOperator::Minus => obj.inverse(),
    };

    match res {
        None => Err(Error::WithPosition(
            ExecError::NonExistentPrefixOperation {
                op: op.ident(),
                rhs: obj.kind(),
            }
            .into(),
            prefix_pos,
            env.file_path(),
        )),
        Some(obj) => Ok(obj),
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::Mutex;
    use std::vec;

    use super::*;
    // Runtime error tests deliberately include programs rejected by inference.
    // Only this test adapter bypasses checking; production execution requires a typed tree.
    fn exec(node: &crate::ast::ASTNode, env: &mut Environment) -> ExecResult<Object> {
        super::exec(
            &crate::ir::lower(&crate::typed_ast::lower(node, &Default::default())),
            env,
        )
    }

    use crate::ast::tests::{
        _for, _if, assignable_container_element, assignable_object_value, assignable_pattern,
        assignment, block, boolean, call, case, comprehension, cons, container_element,
        dec_integer, dec_integer_pattern, decimal, dictionary, dot_notation, extension_list,
        extension_set, fraction, function, function_declaration, infix, let_,
        memoized_function_declaration, pos, prefix, range, set_cons, string, symbol,
        symbol_pattern, symbolic_let, tuple, var,
    };
    use crate::cst::tests::dummy_pos;
    use crate::env::EnvResponse;
    use crate::error::ErrorKind;
    use crate::{ast, object::*};

    #[test]
    fn unknown_value() {
        let node = symbol("a", dummy_pos());
        assert_eq!(
            exec(&node, &mut Default::default()),
            Err(Error::with_position(
                ExecError::UnknownValue(String::from("a")).into(),
                dummy_pos(),
                PathBuf::default()
            ))
        );
    }

    #[test]
    fn set_by_extension() {
        let node = extension_set(
            vec![dec_integer("1", dummy_pos()), dec_integer("1", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Default::default()).unwrap(),
            Object::Set(Set::from(vec![Object::Integer(1.into()),])),
        );
    }

    #[test]
    fn integer_sum() {
        let node = infix(
            InfixOperator::Sum,
            dec_integer("0", dummy_pos()),
            dec_integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Default::default()).unwrap(),
            Object::Integer(Integer::from("1")),
        );
    }

    #[test]
    fn integer_substraction() {
        let node = &infix(
            InfixOperator::Substraction,
            dec_integer("0", dummy_pos()),
            dec_integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Integer(Integer::from(-1))
        );
    }

    #[test]
    fn integer_product() {
        let node = &infix(
            InfixOperator::Product,
            dec_integer("0", dummy_pos()),
            dec_integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Integer(Integer::from("0"))
        );
    }

    #[test]
    fn symbol_comparison() {
        let node = &infix(
            InfixOperator::Equality,
            symbol("a", dummy_pos()),
            symbol("b", dummy_pos()),
            dummy_pos(),
        );

        let mut env = Environment::default();
        env.set_inmutable("a", Object::Symbol(Symbol::new("a".into(), "Foo".into())));
        env.set_inmutable("b", Object::Symbol(Symbol::new("b".into(), "Foo".into())));

        assert_eq!(exec(node, &mut env).unwrap(), Object::Boolean(false.into()));
    }

    #[test]
    fn let_expression() {
        let node = &let_(
            symbol_pattern("x"),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Integer(Integer::from(0))
        );
    }

    #[test]
    fn logic_operators() {
        let node = &infix(
            InfixOperator::LogicAnd,
            infix(
                InfixOperator::Or,
                boolean(true, dummy_pos()),
                boolean(false, dummy_pos()),
                dummy_pos(),
            ),
            boolean(false, dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Boolean(Bool::from(false))
        );
    }

    #[test]
    fn less_leq() {
        let node = &infix(
            InfixOperator::LogicAnd,
            infix(
                InfixOperator::Less,
                dec_integer("0", dummy_pos()),
                dec_integer("1", dummy_pos()),
                dummy_pos(),
            ),
            infix(
                InfixOperator::LessEqual,
                dec_integer("1", dummy_pos()),
                dec_integer("1", dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Boolean(Bool::from(true))
        );
    }

    #[test]
    fn greater_geq() {
        let node = &infix(
            InfixOperator::LogicAnd,
            infix(
                InfixOperator::Greater,
                dec_integer("1", dummy_pos()),
                dec_integer("0", dummy_pos()),
                dummy_pos(),
            ),
            infix(
                InfixOperator::GreaterEqual,
                dec_integer("0", dummy_pos()),
                dec_integer("0", dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Boolean(Bool::from(true))
        );
    }

    #[test]
    fn neq() {
        let node = &infix(
            InfixOperator::LogicAnd,
            infix(
                InfixOperator::NotEquality,
                dec_integer("1", dummy_pos()),
                dec_integer("2", dummy_pos()),
                dummy_pos(),
            ),
            infix(
                InfixOperator::NotEquality,
                dec_integer("1", dummy_pos()),
                boolean(true, dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Boolean(Bool::from(true))
        );
    }

    #[test]
    fn bitwise_and_xor_or() {
        let node = &infix(
            InfixOperator::Or,
            infix(
                InfixOperator::BitwiseXor,
                infix(
                    InfixOperator::BitwiseAnd,
                    dec_integer("7", dummy_pos()),
                    dec_integer("6", dummy_pos()),
                    dummy_pos(),
                ),
                dec_integer("1", dummy_pos()),
                dummy_pos(),
            ),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Integer(Integer::from(7))
        );
    }

    #[test]
    fn shifts() {
        let node = &infix(
            InfixOperator::LeftShift,
            infix(
                InfixOperator::RightShift,
                dec_integer("256", dummy_pos()),
                dec_integer("4", dummy_pos()),
                dummy_pos(),
            ),
            dec_integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Integer(Integer::from(32))
        );
    }

    #[test]
    fn power_and_division() {
        let node = &infix(
            InfixOperator::Division,
            infix(
                InfixOperator::Exponentiation,
                dec_integer("3", dummy_pos()),
                dec_integer("2", dummy_pos()),
                dummy_pos(),
            ),
            dec_integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Integer(Integer::from(4))
        );
    }

    #[test]
    fn remainder() {
        let node = &infix(
            InfixOperator::Rem,
            dec_integer("3", dummy_pos()),
            dec_integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Integer(Integer::from(1))
        );
    }

    #[test]
    fn prefix_() {
        let node = &prefix(
            PrefixOperator::LogicNot,
            infix(
                InfixOperator::NotEquality,
                prefix(
                    PrefixOperator::BitwiseNot,
                    dec_integer("1", dummy_pos()),
                    dummy_pos(),
                ),
                prefix(
                    PrefixOperator::Minus,
                    dec_integer("1", dummy_pos()),
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap(),
            Object::Boolean(Bool::from(false))
        );
    }

    #[test]
    fn if_expr() {
        let mut env = Environment::default();
        env.set_inmutable("a", Object::Integer(Integer::from(-5)));

        let node = &_if(
            infix(
                InfixOperator::Less,
                symbol("a", dummy_pos()),
                dec_integer("0", dummy_pos()),
                dummy_pos(),
            ),
            prefix(PrefixOperator::Minus, symbol("a", dummy_pos()), dummy_pos()),
            symbol("a", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut env).unwrap(),
            Object::Integer(Integer::from(5))
        );
    }

    #[test]
    fn scope_hierarchy() {
        let mut env = Environment::default();
        env.set_inmutable("x", Object::Boolean(Bool::from(true)));
        env.push_scope(ScopeKind::Block);

        let node = &symbol("x", dummy_pos());

        assert_eq!(
            exec(node, &mut env).unwrap(),
            Object::Boolean(Bool::from(true))
        );
    }

    #[test]
    fn save_value() {
        let mut env = Environment::default();

        let node = &let_(
            symbol_pattern("x"),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert!(exec(node, &mut env).is_ok());

        assert_eq!(
            env.get("x"),
            EnvResponse::Inmutable(&Object::Integer(Integer::from(0)),)
        );
    }

    #[test]
    fn tuple_() {
        let node = &tuple(
            vec![dec_integer("1", dummy_pos()), dec_integer("2", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap(),
            Object::Tuple(Tuple::from(vec![
                Object::Integer(Integer::from(1)),
                Object::Integer(Integer::from(2)),
            ])),
        );
    }

    #[test]
    fn function_() {
        let node = &function(
            vec!["x"],
            infix(
                InfixOperator::Product,
                dec_integer("2", dummy_pos()),
                symbol("x", dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap(),
            Object::Function(object::Function::Anonymous(Box::new(AnonFunction::new(
                symbol_pattern("x"),
                crate::ir::lower(&crate::typed_ast::lower(
                    &infix(
                        InfixOperator::Product,
                        dec_integer("2", dummy_pos()),
                        symbol("x", dummy_pos()),
                        dummy_pos()
                    ),
                    &Default::default()
                )),
                Environment::default(),
            )))),
        );
    }

    #[test]
    fn call_() {
        let node = &call(
            function(
                vec!["x"],
                infix(
                    InfixOperator::Product,
                    dec_integer("2", dummy_pos()),
                    symbol("x", dummy_pos()),
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            vec![dec_integer("1", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap(),
            Object::Integer(Integer::from(2))
        );
    }

    #[test]
    fn tuple_parameter_call() {
        let node = &call(
            function(
                vec!["x", "y"],
                infix(
                    InfixOperator::Sum,
                    symbol("x", dummy_pos()),
                    symbol("y", dummy_pos()),
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            vec![dec_integer("1", dummy_pos()), dec_integer("2", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap(),
            Object::Integer(Integer::from(3)),
        );
    }

    #[test]
    fn tuple_parameter_requires_matching_shape() {
        let node = &call(
            function(
                vec!["x", "y"],
                infix(
                    InfixOperator::Sum,
                    symbol("x", dummy_pos()),
                    symbol("y", dummy_pos()),
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            vec![dec_integer("1", dummy_pos())],
            pos(0, 5),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Err(Error::WithPosition(
                ExecError::UnmatchedCall.into(),
                pos(0, 5),
                PathBuf::default()
            ))
        );
    }

    #[test]
    fn for_loop() {
        // I do this to test the change of state without assignments
        static ARGS: Mutex<Vec<String>> = Mutex::new(vec![]);

        fn test(arg: &Object) -> Object {
            ARGS.lock().unwrap().push(arg.to_string());
            Object::empty_tuple()
        }

        let mut env = Environment::default();
        env.set_inmutable(
            "f",
            Object::Function(Function::Extern(ExternFunction::new(test, "test".into()))),
        );

        let node = &_for(
            symbol_pattern("val"),
            extension_list(
                vec![
                    dec_integer("1", dummy_pos()),
                    dec_integer("2", dummy_pos()),
                    dec_integer("3", dummy_pos()),
                ],
                dummy_pos(),
            ),
            vec![call(
                symbol("f", dummy_pos()),
                vec![symbol("val", dummy_pos())],
                dummy_pos(),
            )],
            dummy_pos(),
        );

        assert_eq!(exec(node, &mut env).unwrap(), Object::empty_tuple(),);

        assert_eq!(
            *ARGS.lock().unwrap(),
            vec![String::from("1"), String::from("2"), String::from("3"),]
        );
    }

    #[test]
    fn extension_list_() {
        let node = &extension_list(vec![dec_integer("1", dummy_pos())], dummy_pos());

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap(),
            Object::List(crate::object::List::from(vec![Object::Integer(
                crate::object::integer::Integer::from(1)
            ),])),
        )
    }

    #[test]
    fn function_with_code_block() {
        let node = &call(
            function(
                vec!["x"],
                block(
                    vec![
                        let_(
                            symbol_pattern("y"),
                            infix(
                                InfixOperator::Product,
                                dec_integer("2", dummy_pos()),
                                symbol("x", dummy_pos()),
                                dummy_pos(),
                            ),
                            dummy_pos(),
                        ),
                        infix(
                            InfixOperator::Sum,
                            symbol("y", dummy_pos()),
                            dec_integer("1", dummy_pos()),
                            dummy_pos(),
                        ),
                    ],
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            vec![dec_integer("2", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap(),
            Object::Integer(Integer::from(5)),
        );
    }

    #[test]
    fn comprehension_list_() {
        let node = &comprehension(
            infix(
                InfixOperator::Sum,
                symbol("k", dummy_pos()),
                dec_integer("1", dummy_pos()),
                dummy_pos(),
            ),
            "k".into(),
            extension_list(
                vec![dec_integer("0", dummy_pos()), dec_integer("1", dummy_pos())],
                dummy_pos(),
            ),
            ComprehensionKind::List,
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap(),
            Object::List(List::from(vec![
                Object::Integer(Integer::from(1)),
                Object::Integer(Integer::from(2)),
            ]))
        );
    }

    #[test]
    fn prepend() {
        let node = cons(
            dec_integer("1", dummy_pos()),
            extension_list(vec![dec_integer("2", dummy_pos())], dummy_pos()),
            dummy_pos(),
        );

        let obj = Object::List(vec![Object::Integer(1.into()), Object::Integer(2.into())].into());

        assert_eq!(exec(&node, &mut Environment::default()).unwrap(), obj);
    }

    #[test]
    fn symbol_parameter_accepts_unit() {
        let mut env = Environment::default();
        let func = function_declaration(
            "f",
            vec![symbol_pattern("x")],
            symbol("x", dummy_pos()),
            dummy_pos(),
        );

        let _ = exec(&func, &mut env);

        let call = call(symbol("f", dummy_pos()), vec![], dummy_pos());

        assert_eq!(exec(&call, &mut env).unwrap(), Object::empty_tuple());
    }

    #[test]
    fn decimal_number() {
        let node = decimal("1", "5", dummy_pos());
        let expected = Float::new("1", "5");

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap(),
            Object::Float(expected),
        );
    }

    #[test]
    fn range_() {
        let node = range(
            dec_integer("1", dummy_pos()),
            dec_integer("3", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap(),
            Object::Range(Range::_new(1, 3)),
        );
    }

    #[test]
    fn fraction_() {
        let node = fraction(
            dec_integer("1", dummy_pos()),
            dec_integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap(),
            Object::Fraction(Fraction::_new(1, 2)),
        );
    }

    #[test]
    fn denominator_zero() {
        let node = fraction(
            dec_integer("1", dummy_pos()),
            dec_integer("0", pos(5, 1)),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Err(Error::with_position(
                ExecError::DenominatorZero.into(),
                pos(5, 1),
                PathBuf::default()
            )),
        );
    }

    #[test]
    fn list_from_range() {
        let node = comprehension(
            infix(
                InfixOperator::Sum,
                symbol("k", dummy_pos()),
                dec_integer("1", dummy_pos()),
                dummy_pos(),
            ),
            "k".into(),
            range(
                dec_integer("0", dummy_pos()),
                dec_integer("3", dummy_pos()),
                dummy_pos(),
            ),
            ComprehensionKind::List,
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap(),
            Object::List(List::from(vec![
                Object::Integer(1.into()),
                Object::Integer(2.into()),
                Object::Integer(3.into()),
            ]))
        );
    }

    #[test]
    fn zero_division() {
        let node = infix(
            InfixOperator::Division,
            dec_integer("1", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Err(Error::with_position(
                ExecError::DenominatorZero.into(),
                dummy_pos(),
                PathBuf::default()
            )),
        );
    }

    #[test]
    fn container_element_() {
        let node = container_element(
            ast::tests::extension_list(vec![dec_integer("23", dummy_pos())], dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap(),
            Object::Integer(23.into())
        );
    }

    #[test]
    fn not_a_container() {
        let node = container_element(
            dec_integer("0", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Err(Error::with_position(
                ExecError::IndexingNonContainer {
                    kind: String::from("Integer")
                }
                .into(),
                dummy_pos(),
                PathBuf::default()
            ))
        );
    }

    #[test]
    fn out_of_bounds() {
        let node = container_element(
            ast::tests::extension_list(vec![], dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Err(Error::with_position(
                ExecError::ListIndexOutOfBounds.into(),
                dummy_pos(),
                PathBuf::default()
            ))
        );
    }

    #[test]
    fn not_an_index() {
        let node = container_element(
            ast::tests::extension_list(vec![], dummy_pos()),
            ast::tests::decimal("1", "5", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Err(Error::with_position(
                ExecError::InvalidIndex {
                    kind: String::from("Float")
                }
                .into(),
                dummy_pos(),
                PathBuf::default()
            ))
        );
    }

    #[test]
    fn set_cons_() {
        let node = set_cons(
            ast::tests::dec_integer("1", dummy_pos()),
            ast::tests::extension_set(vec![dec_integer("2", dummy_pos())], dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap(),
            Object::Set(Set::from(vec![
                Object::Integer(1.into()),
                Object::Integer(2.into())
            ])),
        );
    }

    #[test]
    fn set_comprehension() {
        let node = comprehension(
            symbol("a", dummy_pos()),
            "a".into(),
            extension_list(vec![], dummy_pos()),
            ComprehensionKind::Set,
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap(),
            Set::empty_set(),
        );
    }

    #[test]
    fn symbol_with_property() {
        let node = symbolic_let("x", "Real", dummy_pos());
        let mut env = Environment::default();

        let symbol = Object::Symbol(Symbol {
            name: String::from("x"),
            property: String::from("Real"),
        });

        assert_eq!(exec(&node, &mut env).unwrap(), symbol.clone(),);

        assert_eq!(env.get("x"), EnvResponse::Inmutable(&symbol,),);
    }

    #[test]
    fn mutable_value() {
        let declaration = var(
            symbol_pattern("x"),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        let mut env = Environment::default();
        exec(&declaration, &mut env).unwrap();

        let assignment = assignment(
            assignable_pattern(symbol_pattern("x")),
            dec_integer("1", dummy_pos()),
            dummy_pos(),
        );

        exec(&assignment, &mut env).unwrap();

        assert_eq!(
            env.get("x"),
            EnvResponse::Mutable(&mut Object::Integer(1.into()),)
        );
    }

    #[test]
    fn assign_inmutable() {
        let declaration = let_(
            symbol_pattern("x"),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        let mut env = Environment::default();
        exec(&declaration, &mut env).unwrap();

        let assignment = assignment(
            assignable_pattern(symbol_pattern("x")),
            dec_integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&assignment, &mut env),
            Err(Error::with_position(
                ExecError::InmutableAssign(String::from("x")).into(),
                dummy_pos(),
                PathBuf::default()
            )),
        );
    }

    #[test]
    fn mutate_list_element() {
        let list = Object::List(vec![Object::Integer(1.into()), Object::Integer(2.into())].into());
        let mut env = Environment::default();

        env.set_mutable("list", list);

        let assignable_element =
            assignable_container_element("list", dec_integer("0", dummy_pos()));

        let element = container_element(
            symbol("list", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        let assignment = assignment(assignable_element, string("foo", dummy_pos()), dummy_pos());

        assert!(exec(&assignment, &mut env).is_ok());

        assert_eq!(
            exec(&element, &mut env).unwrap(),
            Object::String("foo".into()),
        );
    }

    #[test]
    fn memoization() {
        static mut CALL_COUNTER: usize = 0;

        fn foo(_: &Object) -> Object {
            unsafe {
                CALL_COUNTER += 1;
            }
            Object::empty_tuple()
        }

        let foo_obj = Object::Function(Function::Extern(ExternFunction::new(foo, "foo".into())));

        let mut env = Environment::default();

        env.set_inmutable("foo", foo_obj);

        let func_decl = memoized_function_declaration(
            "bar",
            vec![symbol_pattern("x")],
            call(
                symbol("foo", dummy_pos()),
                vec![symbol("x", dummy_pos())],
                dummy_pos(),
            ),
            dummy_pos(),
        );

        exec(&func_decl, &mut env).unwrap();

        let call = call(
            symbol("bar", dummy_pos()),
            vec![dec_integer("1", dummy_pos())],
            dummy_pos(),
        );

        let first_res = exec(&call, &mut env);
        let second_res = exec(&call, &mut env);

        assert_eq!(first_res.as_ref().unwrap(), &Object::empty_tuple());

        assert_eq!(first_res, second_res);

        assert_eq!(unsafe { CALL_COUNTER }, 1);
    }

    #[test]
    fn _case() {
        let node = case(
            dec_integer("5", dummy_pos()),
            vec![
                (dec_integer_pattern("1"), dec_integer("1", dummy_pos())),
                (dec_integer_pattern("5"), dec_integer("10", dummy_pos())),
            ],
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap(),
            Object::Integer(10.into()),
        );
    }

    #[test]
    fn out_of_scope_mutation() {
        let func = function(
            vec![],
            block(
                vec![assignment(
                    assignable_pattern(symbol_pattern("a")),
                    dec_integer("1", dummy_pos()),
                    dummy_pos(),
                )],
                dummy_pos(),
            ),
            dummy_pos(),
        );

        let mut env = Environment::default();
        env.set_mutable("a", Object::Integer(0.into()));

        assert_eq!(
            exec(&call(func, vec![], dummy_pos()), &mut env),
            Err(Error::with_position(
                ErrorKind::Exec(ExecError::MutationOutOfScope {
                    name: String::from("a")
                }),
                dummy_pos(),
                PathBuf::default()
            )),
        );
    }

    #[test]
    fn object_attribute() {
        let dict = dictionary(
            vec![(symbol("foo", dummy_pos()), dec_integer("5", dummy_pos()))],
            true,
            dummy_pos(),
        );

        assert_eq!(
            exec(&dict, &mut Environment::default()),
            Ok(Object::Dictionary(Dictionary::from(vec![(
                Object::String("foo".into()),
                Object::Integer(5.into())
            ),])))
        );
    }

    #[test]
    fn field_notation() {
        let dict = dictionary(
            vec![(symbol("foo", dummy_pos()), dec_integer("5", dummy_pos()))],
            true,
            dummy_pos(),
        );

        let mut env = Environment::default();
        let dict = exec(&dict, &mut env).unwrap();
        env.set_inmutable("obj", dict);

        let attr = dot_notation(
            symbol("obj", dummy_pos()),
            symbol("foo", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(exec(&attr, &mut env), Ok(Object::Integer(5.into())));
    }

    #[test]
    fn false_oop_call() {
        let function = function(vec!["x"], symbol("x", dummy_pos()), dummy_pos());

        let call = call(
            dot_notation(
                dec_integer("5", dummy_pos()),
                symbol("f", dummy_pos()),
                dummy_pos(),
            ),
            vec![],
            dummy_pos(),
        );

        let mut env = Environment::default();

        let function = exec(&function, &mut env).unwrap();
        env.set_inmutable("f", function);

        assert_eq!(exec(&call, &mut env), Ok(Object::Integer(5.into())));
    }

    #[test]
    fn oop_call() {
        let dict = dictionary(
            vec![(
                symbol("foo", dummy_pos()),
                function(vec![], dec_integer("5", dummy_pos()), dummy_pos()),
            )],
            true,
            dummy_pos(),
        );

        let mut env = Environment::default();
        let dict = exec(&dict, &mut env).unwrap();
        env.set_inmutable("obj", dict);

        let attr = dot_notation(
            symbol("obj", dummy_pos()),
            symbol("foo", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&attr, &mut env),
            Ok(Object::Function(Function::Anonymous(Box::new(
                AnonFunction::new(
                    Pattern::Tuple { list: vec![] },
                    crate::ir::lower(&crate::typed_ast::lower(
                        &dec_integer("5", dummy_pos()),
                        &Default::default()
                    )),
                    Environment::default()
                )
            ))))
        );
    }

    #[test]
    fn loop_mutation() {
        let loop_node = _for(
            symbol_pattern("i"),
            extension_list(
                vec![dec_integer("1", dummy_pos()), dec_integer("2", dummy_pos())],
                dummy_pos(),
            ),
            vec![assignment(
                assignable_pattern(symbol_pattern("res")),
                cons(
                    symbol("i", dummy_pos()),
                    symbol("res", dummy_pos()),
                    dummy_pos(),
                ),
                dummy_pos(),
            )],
            dummy_pos(),
        );

        let mut env = Environment::default();

        env.set_mutable("res", Object::empty_list());

        assert_eq!(exec(&loop_node, &mut env), Ok(Object::empty_tuple()));

        assert_eq!(
            env.get("res"),
            EnvResponse::Mutable(&mut Object::List(
                vec![Object::Integer(2.into()), Object::Integer(1.into())].into()
            ),),
        );
    }

    #[test]
    fn object_attr_mutation() {
        let dict = dictionary(
            vec![(string("val", dummy_pos()), dec_integer("10", dummy_pos()))],
            true,
            dummy_pos(),
        );

        let mut env = Environment::default();
        let dict = exec(&dict, &mut env).unwrap();
        env.set_mutable("dict", dict);

        let assignable_value = assignable_object_value("dict", "val");

        let value = dot_notation(
            symbol("dict", dummy_pos()),
            symbol("val", dummy_pos()),
            dummy_pos(),
        );

        let assignment = assignment(assignable_value, dec_integer("5", dummy_pos()), dummy_pos());

        assert!(exec(&assignment, &mut env).is_ok());

        assert_eq!(exec(&value, &mut env), Ok(Object::Integer(5.into())));
    }

    #[test]
    fn list_cons_from_set() {
        assert_eq!(
            exec(
                &cons(
                    dec_integer("1", dummy_pos()),
                    extension_set(vec![], dummy_pos()),
                    dummy_pos()
                ),
                &mut Environment::default()
            ),
            Ok(Object::List(vec![Object::Integer(1.into())].into()))
        );
    }
}
