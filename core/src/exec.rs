use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::error::{Error, Position};
use crate::lexer::Radix;
use crate::matcher::{match_, Match};
use crate::object::{
    self, AnonFunction, Decimal, Dictionary, FailedAssertion, Fraction, Function,
    FunctionPatternKind, Kind, List, PatternFunction, Range,
};

type ExecResult<T> = Result<T, Error>;

use crate::ast::{ASTNode, ASTNodeKind, Declaration, InfixOperator};
use crate::cst::{ComprehensionKind, PrefixOperator};
use crate::env::{Address, EnvResponse, Environment, ScopeKind, ValueKind};
use crate::object::{Bool, Char, Integer, MyString, Object, Set, Symbol, Tuple};
use crate::run::{self, ModuleAddress};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExecError {
    BadFraction {
        numer_kind: String,
        denom_kind: String,
    },
    BadMatch,
    DenominatorZero,
    FailedAssertion(Option<String>),
    IndexingNonContainer {
        kind: String,
    },
    InmutableAssign(String),
    InvalidIndex {
        kind: String,
    },
    ListIndexOutOfBounds,
    MissingFunctionArguments {
        expected: usize,
        actual: usize,
    },
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

pub fn list(l: &[ASTNode], env: &mut Environment) -> ExecResult<Vec<(Object, Address)>> {
    l.iter().map(|node| exec(node, env)).collect()
}

fn function(
    params: &[String],
    result: &ASTNode,
    env: &Environment,
) -> ExecResult<(Object, Address)> {
    Ok((
        Object::Function(object::Function::Anonymous(AnonFunction::new(
            params.to_owned(),
            result.to_owned(),
            env.clone(),
        ))),
        Address::default(),
    ))
}

pub fn exec(node: &ASTNode, env: &mut Environment) -> ExecResult<(Object, Address)> {
    let res = match &node.kind {
        ASTNodeKind::Symbol { name } => symbol(name, env, node.position),
        ASTNodeKind::Set { list } => extension_set(list, env),
        ASTNodeKind::Integer { literal, radix } => integer(literal, *radix),
        ASTNodeKind::Function { params, result } => function(params, result, env),
        ASTNodeKind::Infix { op, lhs, rhs } => infix(op.clone(), lhs, rhs, node.position, env),
        ASTNodeKind::Boolean(val) => boolean(*val),
        ASTNodeKind::Call { called, args } => call(called, args, env, node.position),
        ASTNodeKind::Char(chr) => char(*chr),
        ASTNodeKind::If {
            cond,
            positive,
            negative,
        } => if_(exec(cond, env)?, positive, negative, env),
        ASTNodeKind::Prefix { op, val } => prefix(*op, exec(val, env)?, node.position, env),
        ASTNodeKind::String { str } => string(str),
        ASTNodeKind::Tuple { list: values } => tuple(values, env),
        ASTNodeKind::For { val, iter, proc } => for_(val, iter, proc, env),
        ASTNodeKind::List { list } => extension_list(list, env),
        ASTNodeKind::Wildcard => unimplemented!(),
        ASTNodeKind::AdInfinitum => unimplemented!(),
        ASTNodeKind::Cons { first, tail } => cons(exec(first, env)?, tail, env),
        ASTNodeKind::Decimal { int, dec } => decimal(int, dec),
        ASTNodeKind::Fraction { numer, denom } => fraction(numer, denom, node.position, env),
        ASTNodeKind::Dictionary { pairs, complete: _ } => dictionary(pairs, env),
        ASTNodeKind::IndexNotation { container, index } => {
            let container_obj = exec(container, env)?.0;
            let element_obj = exec(index, env)?.0;

            match container_obj {
                Object::List(list) => match list.get(&element_obj) {
                    Ok(obj) => Ok(obj),
                    Err(eval_err) => Err(Error::with_position(
                        eval_err.into(),
                        index.position,
                        env.file_path(),
                    )),
                },
                Object::Dictionary(dict) => match dict.get(&element_obj) {
                    Ok(obj) => Ok((obj, Address::default())),
                    Err(eval_err) => Err(Error::with_position(
                        eval_err.into(),
                        index.position,
                        env.file_path(),
                    )),
                },
                obj => Err(Error::with_position(
                    ExecError::IndexingNonContainer { kind: obj.kind() }.into(),
                    container.position,
                    env.file_path(),
                )),
            }
        }
        ASTNodeKind::SetCons { some, most } => set_cons(exec(some, env)?, most, env),
        ASTNodeKind::ImportFrom { source, values } => import_from(source, values, env),
        ASTNodeKind::Comprehension {
            element,
            variable,
            iterator,
            kind,
        } => comprehension(element, variable, iterator, *kind, env),
        ASTNodeKind::Pattern {
            exp: _,
            constraint: _,
        } => unimplemented!(),
        ASTNodeKind::Block(exprs) => block(exprs, env),
        ASTNodeKind::Assignment { left, right } => assignment(left, right, env),
        ASTNodeKind::Declaration(decl) => declaration(decl, env),
        ASTNodeKind::Case { expr, pairs } => case(expr, pairs, env),
        ASTNodeKind::DotNotation { lhs, rhs } => object_attr(lhs, rhs, env),
    };

    if let Ok((Object::Error(FailedAssertion(msg)), _)) = res {
        Err(Error::with_position(
            ExecError::FailedAssertion(msg).into(),
            node.position,
            env.file_path(),
        ))
    } else {
        res
    }
}

fn case(
    expr: &ASTNode,
    pairs: &[(ASTNode, ASTNode)],
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let expr_obj = exec(expr, env)?.0;

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

fn declaration(decl: &Declaration, env: &mut Environment) -> ExecResult<(Object, Address)> {
    match decl {
        Declaration::Symbolic { name, constraint } => let_without_value(name, constraint, env),
        Declaration::Inmutable { left, right } => {
            let_pattern(left, right, ValueKind::Inmutable, env)
        }
        Declaration::Mutable { left, right } => let_pattern(left, right, ValueKind::Mutable, env),
        Declaration::Function {
            name,
            params,
            result,
        } => let_function(name, params, result, FunctionPatternKind::NotMemoized, env),
        Declaration::MemoizedFunction {
            name,
            params,
            result,
        } => let_function(name, params, result, FunctionPatternKind::Memoized, env),
    }
}

fn get_named_container_element(node: &ASTNode) -> Option<(&str, &ASTNode, Position)> {
    match &node.kind {
        ASTNodeKind::IndexNotation { container, index } => match &container.kind {
            ASTNodeKind::Symbol { name } => Some((name, index, container.position)),
            _ => None,
        },
        _ => None,
    }
}

fn assignment(
    left: &ASTNode,
    right: &ASTNode,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let value = exec(right, env)?.0;

    if let Some((name, index, name_position)) = get_named_container_element(left) {
        let index_obj = exec(index, env)?.0;
        let container = get_mutable_value(name, env, name_position)?;

        let element_ref = match container {
            Object::List(list) => match list.get_mut(&index_obj) {
                Ok(obj) => Ok(&mut obj.0),
                Err(eval_err) => Err(Error::with_position(
                    eval_err.into(),
                    index.position,
                    env.file_path(),
                )),
            },
            Object::Dictionary(dict) => match dict.get_mut(&index_obj) {
                Ok(obj) => Ok(obj),
                Err(eval_err) => Err(Error::with_position(
                    eval_err.into(),
                    index.position,
                    env.file_path(),
                )),
            },
            obj => Err(Error::with_position(
                ExecError::IndexingNonContainer { kind: obj.kind() }.into(),
                name_position,
                env.file_path(),
            )),
        }?;

        *element_ref = value;
        return Ok((element_ref.to_owned(), Address::default()));
    }

    match match_(left, &value) {
        Some(Match(map)) => {
            make_assignment(map, env, left.position)?;
            Ok((value, Address::default()))
        }
        None => Err(Error::with_position(
            ExecError::BadMatch.into(),
            left.position.join(right.position),
            env.file_path(),
        )),
    }
}

fn make_assignment(
    map: BTreeMap<String, (Object, Address)>,
    env: &mut Environment,
    position: Position,
) -> Result<(), Error> {
    for (key, val) in map {
        let obj_ref = get_mutable_value(&key, env, position)?;
        *obj_ref = val.0;
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
        EnvResponse::Mutable(obj_ref) => Ok(obj_ref.0),
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

fn block(exprs: &[ASTNode], env: &mut Environment) -> ExecResult<(Object, Address)> {
    let mut res = (Object::empty_tuple(), Address::default());

    for exp in exprs {
        res = exec(exp, env)?;
    }

    Ok(res)
}

fn import_from(
    module: &ModuleAddress,
    values: &[(String, Position)],
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    run::import_from(module, values, env)?;
    Ok((Object::empty_tuple(), Address::default()))
}

fn set_cons(
    some: (Object, Address),
    most: &ASTNode,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let some = some.0;
    match exec(most, env)?.0 {
        Object::List(lst) => {
            let mut res = BTreeSet::new();
            res.insert(some);

            for obj in lst.list {
                res.insert(obj.0.to_owned());
            }

            Ok((Object::Set(res.into()), Address::default()))
        }
        Object::Set(set) => {
            let mut res = BTreeSet::new();
            res.insert(some);

            for obj in set.iter() {
                res.insert(obj.to_owned());
            }

            Ok((Object::Set(res.into()), Address::default()))
        }
        obj => Err(Error::WithPosition(
            ExecError::NonPrependableObject(obj.kind()).into(),
            most.position,
            env.file_path(),
        )),
    }
}

fn dictionary(
    pairs: &Vec<(ASTNode, ASTNode)>,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let mut dict = Dictionary::default();

    for (key, value) in pairs {
        let key_obj = match key {
            ASTNode {
                kind: ASTNodeKind::Symbol { name },
                ..
            } => Object::String(name.as_str().into()),
            key => exec(key, env)?.0,
        };

        dict.dict.insert(key_obj, exec(value, env)?.0);
    }

    Ok((Object::Dictionary(dict), Address::default()))
}

fn decimal(int: &str, dec: &str) -> ExecResult<(Object, Address)> {
    Ok((Object::Decimal(Decimal::new(int, dec)), Address::default()))
}

fn cons(
    first: (Object, Address),
    most: &ASTNode,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    match exec(most, env)?.0 {
        Object::List(lst) => {
            let mut res = vec![first];

            for obj in lst.list {
                res.push(obj.to_owned());
            }

            Ok((Object::List(res.into()), Address::default()))
        }
        Object::Set(set) => {
            let mut res = vec![first.0];

            for obj in set.iter() {
                res.push(obj.to_owned());
            }

            Ok((Object::List(res.into()), Address::default()))
        }
        obj => Err(Error::WithPosition(
            ExecError::NonPrependableObject(obj.kind()).into(),
            most.position,
            env.file_path(),
        )),
    }
}

fn extension_list(l: &[ASTNode], env: &mut Environment) -> ExecResult<(Object, Address)> {
    list(l, env).map(|lst| (Object::List(List::from(lst)), Address::default()))
}

fn tuple(l: &[ASTNode], env: &mut Environment) -> ExecResult<(Object, Address)> {
    list(l, env).map(|lst| (Object::Tuple(Tuple::from(lst)), Address::default()))
}

fn string(str: &str) -> ExecResult<(Object, Address)> {
    Ok((Object::String(MyString::from(str)), Address::default()))
}

fn char(chr: char) -> ExecResult<(Object, Address)> {
    Ok((Object::Char(Char::from(chr)), Address::default()))
}

fn boolean(val: bool) -> ExecResult<(Object, Address)> {
    Ok((Object::Boolean(Bool::from(val)), Address::default()))
}

fn let_pattern(
    left: &ASTNode,
    right: &ASTNode,
    kind: ValueKind,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let value = exec(right, env)?.0;
    match match_(left, &value) {
        Some(Match(map)) => {
            for (name, val) in map {
                match kind {
                    ValueKind::Mutable => env.set_mutable(&name, val),
                    ValueKind::Inmutable => env.set_inmutable(&name, val),
                }
            }

            Ok((value, Address::default()))
        }
        None => Err(Error::with_position(
            ExecError::BadMatch.into(),
            left.position.join(right.position),
            env.file_path(),
        )),
    }
}

fn let_without_value(
    name: &str,
    property: &str,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let symbol = Object::Symbol(Symbol {
        name: name.to_owned(),
        property: property.to_owned(),
    });

    env.set_inmutable(name, (symbol.clone(), Address::default()));

    Ok((symbol, Address::default()))
}

fn integer(str: &str, radix: Radix) -> ExecResult<(Object, Address)> {
    Ok((
        Object::Integer(Integer::new(str, radix)),
        Address::default(),
    ))
}

fn extension_set(l: &[ASTNode], env: &mut Environment) -> ExecResult<(Object, Address)> {
    list(l, env).map(|lst| (Object::Set(Set::from(lst)), Address::default()))
}

fn symbol(str: &str, env: &mut Environment, position: Position) -> ExecResult<(Object, Address)> {
    match env.get(str) {
        EnvResponse::Inmutable((obj, addr)) | EnvResponse::MutableOriginally((obj, addr)) => {
            Ok((obj.to_owned(), addr))
        }
        EnvResponse::Mutable((obj, addr)) => Ok((obj.to_owned(), addr)),
        EnvResponse::NotFound => Err(Error::with_position(
            ExecError::UnknownValue(str.to_owned()).into(),
            position,
            env.file_path(),
        )),
    }
}

fn comprehension(
    element: &ASTNode,
    variable: &str,
    iterator: &ASTNode,
    kind: ComprehensionKind,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let iterator = get_iterable(iterator, env)?;

    match kind {
        ComprehensionKind::List => {
            let mut new_list = vec![];
            env.push_scope(ScopeKind::Block);

            for val in iterator {
                env.set_inmutable(variable, val);
                new_list.push(exec(element, env)?);
            }

            Ok((Object::List(List::from(new_list)), Address::default()))
        }
        ComprehensionKind::Set => {
            let mut new_set = BTreeSet::new();
            env.push_scope(ScopeKind::Block);

            for val in iterator {
                env.set_inmutable(variable, val);
                new_set.insert(exec(element, env)?.0);
            }

            Ok((Object::Set(Set::from(new_set)), Address::default()))
        }
    }
}

fn let_function(
    name: &str,
    args: &[ASTNode],
    value: &ASTNode,
    kind: FunctionPatternKind,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let function: &mut PatternFunction = match env.get(name) {
        EnvResponse::NotFound => {
            env.set_mutable(
                name,
                (
                    Object::Function(Function::Pattern(PatternFunction::new(Rc::new(
                        RefCell::new(env.clone()),
                    )))),
                    Address::default(),
                ),
            );

            match env.get(name) {
                EnvResponse::Mutable((Object::Function(Function::Pattern(f)), _)) => f,
                _ => unimplemented!(),
            }
        }
        EnvResponse::Mutable((Object::Function(Function::Pattern(f)), _)) => f,
        _ => unimplemented!(),
    };

    function.add_pattern(args, value, kind);
    function.env.borrow_mut().set_mutable(
        name,
        (
            Object::Function(Function::Pattern(function.clone())),
            Address::default(),
        ),
    );

    Ok((
        Object::Function(Function::Pattern(function.clone())),
        Address::default(),
    ))
}

fn if_(
    cond: (Object, Address),
    first: &ASTNode,
    second: &ASTNode,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let cond = cond.0;
    if truthy(&cond) {
        exec(first, env)
    } else {
        exec(second, env)
    }
}

fn for_(
    symbol: &str,
    iterable: &ASTNode,
    proc: &[ASTNode],
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let iter = get_iterable(iterable, env)?;

    env.push_scope(ScopeKind::Loop);

    for val in iter {
        env.set_inmutable(symbol, val.clone());

        for step in proc {
            exec(step, env)?;
        }
    }

    env.pop_scope();

    Ok((Object::empty_tuple(), Address::default()))
}

fn get_iterable(
    node: &ASTNode,
    env: &mut Environment,
) -> Result<Box<dyn Iterator<Item = (Object, Address)>>, Error> {
    match exec(node, env)?.0 {
        Object::Set(set) => Ok(Box::new(
            set.into_iter().map(|obj| (obj, Address::default())),
        )),
        Object::List(list) => Ok(Box::new(list.list.into_iter())),
        Object::Range(range) => Ok(Box::new(
            range.into_iter().map(|obj| (obj, Address::default())),
        )),
        obj => Err(Error::WithPosition(
            ExecError::NonIterableObject(obj.kind()).into(),
            node.position,
            env.file_path(),
        )),
    }
}

fn call(
    func_node: &ASTNode,
    args: &[ASTNode],
    env: &mut Environment,
    call_pos: Position,
) -> ExecResult<(Object, Address)> {
    let (func, args): (Object, Vec<&ASTNode>) = match (func_node, args) {
        (
            ASTNode {
                kind: ASTNodeKind::DotNotation { lhs, rhs },
                ..
            },
            args,
        ) => match exec(lhs, env) {
            Ok((Object::Dictionary(_), _)) => {
                (object_attr(lhs, rhs, env)?.0, args.iter().collect())
            }
            _ => {
                let mut new_args = vec![lhs.as_ref()];
                for arg in args {
                    new_args.push(arg);
                }

                (exec(rhs, env)?.0, new_args)
            }
        },
        (func_node, args) => (exec(func_node, env)?.0, args.iter().collect()),
    };

    let func_name = match &func_node.kind {
        ASTNodeKind::Symbol { name } => Some(name),
        _ => None,
    };

    if let Object::Function(ref f) = func {
        if args.len() < f.param_number() {
            return Err(Error::WithPosition(
                ExecError::MissingFunctionArguments {
                    expected: f.param_number(),
                    actual: args.len(),
                }
                .into(),
                call_pos,
                env.file_path(),
            ));
        }
    }

    let mut func_args = vec![];
    for arg in args {
        let func_arg = exec(arg, env)?.0;
        func_args.push(func_arg);
    }

    match func {
        Object::Function(mut f) => {
            let res = f.call(&func_args, env, call_pos);

            if let Some(name) = func_name {
                env.set_mutable(name, (Object::Function(f.clone()), Address::default()));
            }

            res.map(|obj| (obj, Address::default()))
        }
        obj => Err(Error::WithPosition(
            ExecError::NonCallableObject(obj.kind()).into(),
            func_node.position,
            env.file_path(),
        ))?,
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
    numer: &ASTNode,
    denom: &ASTNode,
    position: Position,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    match (exec(numer, env)?.0, exec(denom, env)?.0) {
        (Object::Integer(_), Object::Integer(int)) if int.is_zero() => Err(Error::with_position(
            ExecError::DenominatorZero.into(),
            denom.position,
            env.file_path(),
        )),
        (Object::Integer(numer), Object::Integer(denom)) => Ok((
            Object::Fraction(Fraction::new(numer, denom)),
            Address::default(),
        )),
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
    obj: &ASTNode,
    attr: &ASTNode,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    match (exec(obj, env)?.0, &attr.kind) {
        (Object::Dictionary(dict), ASTNodeKind::Symbol { name }) => {
            let element_obj = Object::String(name.as_str().into());

            match dict.get(&element_obj) {
                Ok(obj) => Ok((obj, Address::default())),
                Err(eval_err) => Err(Error::with_position(
                    eval_err.into(),
                    attr.position,
                    env.file_path(),
                )),
            }
        }
        _ => todo!(),
    }
}

fn infix(
    op: InfixOperator,
    lhs: &ASTNode,
    rhs: &ASTNode,
    infix_pos: Position,
    env: &mut Environment,
) -> ExecResult<(Object, Address)> {
    let lhs = &exec(lhs, env)?.0;
    let rhs = &exec(rhs, env)?.0;
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
        Some(obj) => Ok((obj, Address::default())),
    }
}

fn prefix(
    op: PrefixOperator,
    obj: (Object, Address),
    prefix_pos: Position,
    env: &Environment,
) -> ExecResult<(Object, Address)> {
    let obj = obj.0;

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
        Some(obj) => Ok((obj, Address::default())),
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::Mutex;
    use std::vec;

    use bigdecimal::BigDecimal;

    use super::*;
    use crate::ast::tests::{
        _for, _if, assignment, block, boolean, call, case, comprehension, cons, container_element,
        dec_integer, decimal, dictionary, dot_notation, extension_list, extension_set, fraction,
        function, function_declaration, infix, let_, memoized_function_declaration, pos, prefix,
        range, set_cons, string, symbol, symbolic_let, tuple, var,
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
            exec(&node, &mut Default::default()).unwrap().0,
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
            exec(&node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
        env.set_inmutable(
            "a",
            (
                Object::Symbol(Symbol::new("a".into(), "Foo".into())),
                Address::default(),
            ),
        );
        env.set_inmutable(
            "b",
            (
                Object::Symbol(Symbol::new("b".into(), "Foo".into())),
                Address::default(),
            ),
        );

        assert_eq!(
            exec(node, &mut env).unwrap().0,
            Object::Boolean(false.into())
        );
    }

    #[test]
    fn let_expression() {
        let node = &let_(
            symbol("x", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
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
            exec(node, &mut Default::default()).unwrap().0,
            Object::Boolean(Bool::from(false))
        );
    }

    #[test]
    fn if_expr() {
        let mut env = Environment::default();
        env.set_inmutable(
            "a",
            (Object::Integer(Integer::from(-5)), Address::default()),
        );

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
            exec(node, &mut env).unwrap().0,
            Object::Integer(Integer::from(5))
        );
    }

    #[test]
    fn scope_hierarchy() {
        let mut env = Environment::default();
        env.set_inmutable("x", (Object::Boolean(Bool::from(true)), Address::default()));
        env.push_scope(ScopeKind::Block);

        let node = &symbol("x", dummy_pos());

        assert_eq!(
            exec(node, &mut env).unwrap().0,
            Object::Boolean(Bool::from(true))
        );
    }

    #[test]
    fn save_value() {
        let mut env = Environment::default();

        let node = &let_(
            symbol("x", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert!(exec(node, &mut env).is_ok());

        assert_eq!(
            env.get("x"),
            EnvResponse::Inmutable((&Object::Integer(Integer::from(0)), Address::default()),)
        );
    }

    #[test]
    fn tuple_() {
        let node = &tuple(
            vec![dec_integer("1", dummy_pos()), dec_integer("2", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap().0,
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
            exec(node, &mut Environment::default()).unwrap().0,
            Object::Function(object::Function::Anonymous(AnonFunction::new(
                vec![String::from("x"),],
                infix(
                    InfixOperator::Product,
                    dec_integer("2", dummy_pos()),
                    symbol("x", dummy_pos()),
                    dummy_pos()
                ),
                Environment::default(),
            ))),
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
            exec(node, &mut Environment::default()).unwrap().0,
            Object::Integer(Integer::from(2))
        );
    }

    #[test]
    fn several_params_call() {
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
            exec(node, &mut Environment::default()).unwrap().0,
            Object::Integer(Integer::from(3)),
        );
    }

    #[test]
    fn missing_args() {
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
                ExecError::MissingFunctionArguments {
                    expected: 2,
                    actual: 1,
                }
                .into(),
                pos(0, 5),
                PathBuf::default()
            ))
        );
    }

    #[test]
    fn for_loop() {
        // I do this to test the change of state without assignments
        static ARGS: Mutex<Vec<String>> = Mutex::new(vec![]);

        fn test(args: &[Object]) -> Object {
            ARGS.lock().unwrap().push(args[0].to_string());
            Object::empty_tuple()
        }

        let mut env = Environment::default();
        env.set_inmutable(
            "f",
            (
                Object::Function(Function::Extern(ExternFunction::new(test, 1))),
                Address::default(),
            ),
        );

        let node = &_for(
            "val",
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

        assert_eq!(exec(node, &mut env).unwrap().0, Object::empty_tuple(),);

        assert_eq!(
            *ARGS.lock().unwrap(),
            vec![String::from("1"), String::from("2"), String::from("3"),]
        );
    }

    #[test]
    fn extension_list_() {
        let node = &extension_list(vec![dec_integer("1", dummy_pos())], dummy_pos());

        assert_eq!(
            exec(node, &mut Environment::default()).unwrap().0,
            Object::List(crate::object::List::from(vec![Object::Integer(
                crate::object::Integer::from(1)
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
                            symbol("y", dummy_pos()),
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
            exec(node, &mut Environment::default()).unwrap().0,
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
            exec(node, &mut Environment::default()).unwrap().0,
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

        assert_eq!(exec(&node, &mut Environment::default()).unwrap().0, obj);
    }

    #[test]
    fn missing_args_2() {
        let mut env = Environment::default();
        let func = function_declaration(
            "f",
            vec![symbol("x", dummy_pos())],
            symbol("x", dummy_pos()),
            dummy_pos(),
        );

        let _ = exec(&func, &mut env);

        let call = call(symbol("f", dummy_pos()), vec![], dummy_pos());

        assert_eq!(
            exec(&call, &mut env),
            Err(Error::WithPosition(
                ErrorKind::Exec(ExecError::MissingFunctionArguments {
                    expected: 1,
                    actual: 0
                }),
                dummy_pos(),
                PathBuf::default()
            ))
        );
    }

    #[test]
    fn decimal_number() {
        let node = decimal("1", "5", dummy_pos());
        let expected = Decimal::from(BigDecimal::from(3) / BigDecimal::from(2));

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap().0,
            Object::Decimal(expected),
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
            exec(&node, &mut Environment::default()).unwrap().0,
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
            exec(&node, &mut Environment::default()).unwrap().0,
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
            exec(&node, &mut Environment::default()).unwrap().0,
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
            exec(&node, &mut Environment::default()).unwrap().0,
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
                    kind: String::from("Decimal")
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
            exec(&node, &mut Environment::default()).unwrap().0,
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
            exec(&node, &mut Environment::default()).unwrap().0,
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

        assert_eq!(exec(&node, &mut env).unwrap().0, symbol.clone(),);

        assert_eq!(
            env.get("x"),
            EnvResponse::Inmutable((&symbol, Address::default()),),
        );
    }

    #[test]
    fn mutable_value() {
        let declaration = var(
            symbol("x", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        let mut env = Environment::default();
        exec(&declaration, &mut env).unwrap();

        let assignment = assignment(
            symbol("x", dummy_pos()),
            dec_integer("1", dummy_pos()),
            dummy_pos(),
        );

        exec(&assignment, &mut env).unwrap();

        assert_eq!(
            env.get("x"),
            EnvResponse::Mutable((&mut Object::Integer(1.into()), Address::default()),)
        );
    }

    #[test]
    fn assign_inmutable() {
        let declaration = let_(
            symbol("x", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        let mut env = Environment::default();
        exec(&declaration, &mut env).unwrap();

        let assignment = assignment(
            symbol("x", dummy_pos()),
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

        env.set_mutable("list", (list, Address::default()));

        let element = container_element(
            symbol("list", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        let assignment = assignment(element.clone(), string("foo", dummy_pos()), dummy_pos());

        assert!(exec(&assignment, &mut env).is_ok());

        assert_eq!(
            exec(&element, &mut env).unwrap().0,
            Object::String("foo".into()),
        );
    }

    #[test]
    fn memoization() {
        static mut CALL_COUNTER: usize = 0;

        fn foo(_: &[Object]) -> Object {
            unsafe {
                CALL_COUNTER += 1;
            }
            Object::empty_tuple()
        }

        let foo_obj = Object::Function(Function::Extern(ExternFunction::new(foo, 1)));

        let mut env = Environment::default();

        env.set_inmutable("foo", (foo_obj, Address::default()));

        let func_decl = memoized_function_declaration(
            "bar",
            vec![symbol("x", dummy_pos())],
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

        assert_eq!(first_res.as_ref().unwrap().0, Object::empty_tuple());

        assert_eq!(first_res, second_res);

        assert_eq!(unsafe { CALL_COUNTER }, 1);
    }

    #[test]
    fn _case() {
        let node = case(
            dec_integer("5", dummy_pos()),
            vec![
                (dec_integer("1", dummy_pos()), dec_integer("1", dummy_pos())),
                (
                    dec_integer("5", dummy_pos()),
                    dec_integer("10", dummy_pos()),
                ),
            ],
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()).unwrap().0,
            Object::Integer(10.into()),
        );
    }

    #[test]
    fn out_of_scope_mutation() {
        let func = function(
            vec![],
            block(
                vec![assignment(
                    symbol("a", dummy_pos()),
                    dec_integer("1", dummy_pos()),
                    dummy_pos(),
                )],
                dummy_pos(),
            ),
            dummy_pos(),
        );

        let mut env = Environment::default();
        env.set_mutable("a", (Object::Integer(0.into()), Address::default()));

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
            Ok((
                Object::Dictionary(Dictionary::from(vec![(
                    Object::String("foo".into()),
                    Object::Integer(5.into())
                ),])),
                Address::default()
            ))
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

        assert_eq!(
            exec(&attr, &mut env),
            Ok((Object::Integer(5.into()), Address::default()))
        );
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

        assert_eq!(
            exec(&call, &mut env),
            Ok((Object::Integer(5.into()), Address::default()))
        );
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
            Ok((
                Object::Function(Function::Anonymous(AnonFunction::new(
                    vec![],
                    dec_integer("5", dummy_pos()),
                    Environment::default()
                ))),
                Address::default()
            ))
        );
    }

    #[test]
    fn loop_mutation() {
        let loop_node = _for(
            "i",
            extension_list(
                vec![dec_integer("1", dummy_pos()), dec_integer("2", dummy_pos())],
                dummy_pos(),
            ),
            vec![assignment(
                symbol("res", dummy_pos()),
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

        env.set_mutable("res", (Object::empty_list(), Address::default()));

        assert_eq!(
            exec(&loop_node, &mut env),
            Ok((Object::empty_tuple(), Address::default()))
        );

        assert_eq!(
            env.get("res"),
            EnvResponse::Mutable((
                &mut Object::List(
                    vec![Object::Integer(2.into()), Object::Integer(1.into())].into()
                ),
                Address::default()
            ),),
        );
    }
}
