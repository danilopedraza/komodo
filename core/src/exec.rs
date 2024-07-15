use crate::error::{Error, Position};
use crate::object::{
    self, Callable, ComprehensionSet, Decimal, ExtensionList, FailedAssertion, Fraction, Function,
    Kind, Range,
};

use crate::env::Environment;
use crate::object::{
    Bool, Char, DefinedFunction, ExtensionSet, Integer, MyString, Object, Symbol, Tuple,
};
use crate::parse_node::{InfixOperator, ParseNode, ParseNodeType, PrefixOperator};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalError {
    BadFraction {
        numer_kind: String,
        denom_kind: String,
    },
    DenominatorZero,
    FailedAssertion(Option<String>),
    MissingFunctionArguments {
        expected: usize,
        actual: usize,
    },
    NonCallableObject(String),
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
}

pub fn truthy(val: &Object) -> bool {
    match val {
        Object::Boolean(boolean) => boolean.value(),
        _ => false,
    }
}

pub fn list(l: &[ParseNode], env: &mut Environment) -> Result<Vec<Object>, Error> {
    l.iter().map(|node| exec(node, env)).collect()
}

fn function(params: &[String], proc: &[ParseNode]) -> Result<Object, Error> {
    Ok(Object::Function(object::Function::DefinedFunction(
        DefinedFunction::new(params.to_owned(), proc.to_owned()),
    )))
}

pub fn exec(node: &ParseNode, env: &mut Environment) -> Result<Object, Error> {
    let res = match &node._type {
        ParseNodeType::Symbol(str) => symbol(str, env),
        ParseNodeType::ExtensionSet(l) => extension_set(l, env),
        ParseNodeType::Integer(str) => integer(str),
        ParseNodeType::Function(params, proc) => function(params, proc),
        ParseNodeType::Infix(op, lhs, rhs) => {
            infix(*op, &exec(lhs, env)?, &exec(rhs, env)?, node.position)
        }
        ParseNodeType::Let(ident, params, value) if params.is_empty() => let_(ident, value, env),
        ParseNodeType::Let(ident, args, value) => let_function(ident, args, value, env),
        ParseNodeType::Boolean(val) => boolean(*val),
        ParseNodeType::Call(func_node, args) => call(func_node, args, env, node.position),
        ParseNodeType::Char(chr) => char(*chr),
        ParseNodeType::ComprehensionSet(value, prop) => comprehension_set(value, prop),
        ParseNodeType::If(cond, first, second) => if_(exec(cond, env)?, first, second, env),
        ParseNodeType::Prefix(op, node) => prefix(*op, exec(node, env)?, node.position),
        ParseNodeType::Signature(_, _) => unimplemented!(),
        ParseNodeType::String(str) => string(str),
        ParseNodeType::Tuple(l) => tuple(l, env),
        ParseNodeType::For(symbol, iterable, proc) => for_(symbol, iterable, proc, env),
        ParseNodeType::ExtensionList(l) => extension_list(l, env),
        ParseNodeType::ComprehensionList(transform, prop) => {
            comprehension_list(transform, prop, env)
        }
        ParseNodeType::Wildcard => unimplemented!(),
        ParseNodeType::Cons(first, most) => prepend(exec(first, env)?, most, env),
        ParseNodeType::Decimal(int, dec) => decimal(int, dec),
        ParseNodeType::Fraction(numer, denom) => fraction(numer, denom, node.position, env),
    };

    if let Ok(Object::Error(FailedAssertion(msg))) = res {
        Err(Error::new(
            EvalError::FailedAssertion(msg).into(),
            node.position,
        ))
    } else {
        res
    }
}

fn decimal(int: &str, dec: &str) -> Result<Object, Error> {
    Ok(Object::Decimal(Decimal::new(int, dec)))
}

fn prepend(first: Object, most: &ParseNode, env: &mut Environment) -> Result<Object, Error> {
    match exec(most, env)? {
        Object::ExtensionList(lst) => {
            let mut res = vec![first];

            for obj in lst.list {
                res.push(obj.clone());
            }

            Ok(Object::ExtensionList(res.into()))
        }
        obj => Err(Error(
            EvalError::NonPrependableObject(obj.kind()).into(),
            most.position,
        )),
    }
}

fn extension_list(l: &[ParseNode], env: &mut Environment) -> Result<Object, Error> {
    list(l, env).map(|lst| Object::ExtensionList(ExtensionList::from(lst)))
}

fn tuple(l: &[ParseNode], env: &mut Environment) -> Result<Object, Error> {
    list(l, env).map(|lst| Object::Tuple(Tuple::from(lst)))
}

fn string(str: &str) -> Result<Object, Error> {
    Ok(Object::String(MyString::from(str)))
}

fn comprehension_set(value: &ParseNode, prop: &ParseNode) -> Result<Object, Error> {
    Ok(Object::ComprehensionSet(ComprehensionSet::from((
        value.clone(),
        prop.clone(),
    ))))
}

fn char(chr: char) -> Result<Object, Error> {
    Ok(Object::Char(Char::from(chr)))
}

fn boolean(val: bool) -> Result<Object, Error> {
    Ok(Object::Boolean(Bool::from(val)))
}

fn let_(ident: &ParseNode, value: &ParseNode, env: &mut Environment) -> Result<Object, Error> {
    match &ident._type {
        ParseNodeType::Symbol(name) => exec_and_set(value, name, env),
        ParseNodeType::Signature(ident, None) => match &ident._type {
            ParseNodeType::Symbol(name) => exec_and_set(value, name, env),
            _ => todo!(),
        },
        _ => unimplemented!(),
    }
}

fn integer(str: &str) -> Result<Object, Error> {
    Ok(Object::Integer(Integer::from(str)))
}

fn extension_set(l: &[ParseNode], env: &mut Environment) -> Result<Object, Error> {
    list(l, env).map(|lst| Object::ExtensionSet(ExtensionSet::from(lst)))
}

fn symbol(str: &str, env: &mut Environment) -> Result<Object, Error> {
    match env.get(str) {
        Some(obj) => Ok(obj.clone()),
        None => Ok(Object::Symbol(Symbol::from(str))),
    }
}

fn comprehension_list(
    transform: &ParseNode,
    prop: &ParseNode,
    env: &mut Environment,
) -> Result<Object, Error> {
    let symbol = match &prop._type {
        ParseNodeType::Infix(InfixOperator::In, lhs, _) => match &lhs._type {
            ParseNodeType::Symbol(ident) => ident,
            _ => todo!(),
        },
        _ => todo!(),
    };

    let iterator: Vec<Object> = match &prop._type {
        ParseNodeType::Infix(InfixOperator::In, _, rhs) => get_iterable(rhs, env)?,
        _ => todo!(),
    };

    let mut new_list = vec![];
    env.push_scope();

    for val in iterator {
        env.set(symbol, val);
        new_list.push(exec(transform, env)?);
    }

    Ok(Object::ExtensionList(ExtensionList::from(new_list)))
}

fn let_function(
    ident: &ParseNode,
    args: &[ParseNode],
    value: &ParseNode,
    env: &mut Environment,
) -> Result<Object, Error> {
    let name = match &ident._type {
        ParseNodeType::Symbol(name) => name,
        _ => unimplemented!(),
    };

    let function: &mut DefinedFunction = match env.get(name) {
        None => {
            env.set(
                name,
                Object::Function(Function::DefinedFunction(DefinedFunction::default())),
            );

            match env.get(name) {
                Some(Object::Function(Function::DefinedFunction(f))) => f,
                _ => unimplemented!(),
            }
        }
        Some(Object::Function(Function::DefinedFunction(f))) => f,
        _ => unimplemented!(),
    };

    function.add_pattern(args, value);

    Ok(Object::Function(Function::DefinedFunction(
        function.clone(),
    )))
}

fn exec_and_set(node: &ParseNode, name: &str, env: &mut Environment) -> Result<Object, Error> {
    let val = exec(node, env)?;
    env.set(name, val.clone());
    Ok(val)
}

fn if_(
    cond: Object,
    first: &ParseNode,
    second: &ParseNode,
    env: &mut Environment,
) -> Result<Object, Error> {
    if truthy(&cond) {
        exec(first, env)
    } else {
        exec(second, env)
    }
}

fn for_(
    symbol: &str,
    iterable: &ParseNode,
    proc: &[ParseNode],
    env: &mut Environment,
) -> Result<Object, Error> {
    let iter: Vec<Object> = get_iterable(iterable, env)?;

    env.push_scope();

    for val in iter {
        env.set(symbol, val.clone());

        for step in proc {
            exec(step, env)?;
        }
    }

    env.pop_scope();

    Ok(Object::empty_tuple())
}

fn get_iterable(node: &ParseNode, env: &mut Environment) -> Result<Vec<Object>, Error> {
    match exec(node, env)? {
        Object::ExtensionSet(set) => Ok(set.set.iter().map(|obj| obj.to_owned()).collect()),
        Object::ExtensionList(list) => Ok(list.list.iter().map(|obj| obj.to_owned()).collect()),
        Object::Range(range) => Ok(range.collect()),
        obj => Err(Error(
            EvalError::NonIterableObject(obj.kind()).into(),
            node.position,
        )),
    }
}

fn call(
    func_node: &ParseNode,
    args: &[ParseNode],
    env: &mut Environment,
    call_pos: Position,
) -> Result<Object, Error> {
    let func = exec(func_node, env)?;
    if let Object::Function(ref f) = func {
        if args.len() < f.param_number() {
            return Err(Error(
                EvalError::MissingFunctionArguments {
                    expected: f.param_number(),
                    actual: args.len(),
                }
                .into(),
                call_pos,
            ));
        }
    }

    let mut func_args = vec![];
    for arg in args {
        let func_arg = exec(arg, env)?;
        func_args.push(func_arg);
    }

    match func {
        Object::Function(f) => f.call(&func_args, env),
        obj => Err(Error(
            EvalError::NonCallableObject(obj.kind()).into(),
            func_node.position,
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
    numer: &ParseNode,
    denom: &ParseNode,
    position: Position,
    env: &mut Environment,
) -> Result<Object, Error> {
    match (exec(numer, env)?, exec(denom, env)?) {
        (Object::Integer(_), Object::Integer(int)) if int.is_zero() => Err(Error::new(
            EvalError::DenominatorZero.into(),
            denom.position,
        )),
        (Object::Integer(numer), Object::Integer(denom)) => {
            Ok(Object::Fraction(Fraction::new(numer, denom)))
        }
        (numer, denom) => Err(Error::new(
            EvalError::BadFraction {
                numer_kind: numer.kind(),
                denom_kind: denom.kind(),
            }
            .into(),
            position,
        )),
    }
}

fn infix(
    op: InfixOperator,
    lhs: &Object,
    rhs: &Object,
    infix_pos: Position,
) -> Result<Object, Error> {
    let lhs_kind = lhs.kind();
    let rhs_kind = rhs.kind();

    let res = match op {
        InfixOperator::BitwiseAnd => lhs.bitwise_and(rhs),
        InfixOperator::BitwiseXor => lhs.bitwise_xor(rhs),
        InfixOperator::Call => unimplemented!(),
        InfixOperator::Correspondence => unimplemented!(),
        InfixOperator::Division => {
            if rhs.is_zero() {
                return Err(Error::new(EvalError::DenominatorZero.into(), infix_pos));
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
        InfixOperator::Dot => unimplemented!(),
        InfixOperator::Range => range(lhs, rhs),
        InfixOperator::Fraction => unimplemented!(),
    };

    match res {
        None => Err(Error(
            EvalError::NonExistentInfixOperation {
                op: op.ident(),
                lhs: lhs_kind,
                rhs: rhs_kind,
            }
            .into(),
            infix_pos,
        )),
        Some(obj) => Ok(obj),
    }
}

fn prefix(op: PrefixOperator, obj: Object, prefix_pos: Position) -> Result<Object, Error> {
    let res = match op {
        PrefixOperator::BitwiseNot => obj.bitwise_not(),
        PrefixOperator::LogicNot => obj.logic_not(),
        PrefixOperator::Minus => obj.inverse(),
    };

    match res {
        None => Err(Error(
            EvalError::NonExistentPrefixOperation {
                op: op.ident(),
                rhs: obj.kind(),
            }
            .into(),
            prefix_pos,
        )),
        Some(obj) => Ok(obj),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;
    use std::vec;

    use bigdecimal::BigDecimal;

    use super::*;
    use crate::error::ErrorType;
    use crate::object::*;
    use crate::parse_node::tests::{_pos, boolean, decimal, dummy_pos, function, integer, range};
    use crate::parse_node::{
        _for, call, comprehension_list, comprehension_set, cons, extension_list, extension_set,
        fraction, infix, let_, prefix, signature, symbol, tuple,
    };

    #[test]
    fn symbol_() {
        let node = symbol("a", dummy_pos());
        assert_eq!(
            exec(&node, &mut Default::default()),
            Ok(Object::Symbol(Symbol::from("a")))
        );
    }

    #[test]
    fn set_by_extension() {
        let node = extension_set(
            vec![symbol("a", dummy_pos()), symbol("a", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Default::default()),
            Ok(Object::ExtensionSet(ExtensionSet::from(vec![
                Object::Symbol(Symbol::from("a")),
                Object::Symbol(Symbol::from("a")),
            ]))),
        );
    }

    #[test]
    fn integer_sum() {
        let node = infix(
            InfixOperator::Sum,
            integer("0", dummy_pos()),
            integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Default::default()),
            Ok(Object::Integer(Integer::from("1")))
        );
    }

    #[test]
    fn integer_substraction() {
        let node = &infix(
            InfixOperator::Substraction,
            integer("0", dummy_pos()),
            integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(-1)))
        );
    }

    #[test]
    fn integer_product() {
        let node = &infix(
            InfixOperator::Product,
            integer("0", dummy_pos()),
            integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from("0")))
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

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(false)))
        );
    }

    #[test]
    fn let_expression() {
        let node = &let_(
            signature(symbol("x", dummy_pos()), None, dummy_pos()),
            vec![],
            integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(0)))
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
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(false)))
        );
    }

    #[test]
    fn less_leq() {
        let node = &infix(
            InfixOperator::LogicAnd,
            infix(
                InfixOperator::Less,
                integer("0", dummy_pos()),
                integer("1", dummy_pos()),
                dummy_pos(),
            ),
            infix(
                InfixOperator::LessEqual,
                integer("1", dummy_pos()),
                integer("1", dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(true)))
        );
    }

    #[test]
    fn greater_geq() {
        let node = &infix(
            InfixOperator::LogicAnd,
            infix(
                InfixOperator::Greater,
                integer("1", dummy_pos()),
                integer("0", dummy_pos()),
                dummy_pos(),
            ),
            infix(
                InfixOperator::GreaterEqual,
                integer("0", dummy_pos()),
                integer("0", dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(true)))
        );
    }

    #[test]
    fn neq() {
        let node = &infix(
            InfixOperator::LogicAnd,
            infix(
                InfixOperator::NotEquality,
                integer("1", dummy_pos()),
                integer("2", dummy_pos()),
                dummy_pos(),
            ),
            infix(
                InfixOperator::NotEquality,
                integer("1", dummy_pos()),
                boolean(true, dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(true)))
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
                    integer("7", dummy_pos()),
                    integer("6", dummy_pos()),
                    dummy_pos(),
                ),
                integer("1", dummy_pos()),
                dummy_pos(),
            ),
            integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(7)))
        );
    }

    #[test]
    fn shifts() {
        let node = &infix(
            InfixOperator::LeftShift,
            infix(
                InfixOperator::RightShift,
                integer("256", dummy_pos()),
                integer("4", dummy_pos()),
                dummy_pos(),
            ),
            integer("1", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(32)))
        );
    }

    #[test]
    fn power_and_division() {
        let node = &infix(
            InfixOperator::Division,
            infix(
                InfixOperator::Exponentiation,
                integer("3", dummy_pos()),
                integer("2", dummy_pos()),
                dummy_pos(),
            ),
            integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(4)))
        );
    }

    #[test]
    fn remainder() {
        let node = &infix(
            InfixOperator::Rem,
            integer("3", dummy_pos()),
            integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(1)))
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
                    integer("1", dummy_pos()),
                    dummy_pos(),
                ),
                prefix(
                    PrefixOperator::Minus,
                    integer("1", dummy_pos()),
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(false)))
        );
    }

    #[test]
    fn if_expr() {
        let mut env = Environment::default();
        env.set("a", Object::Integer(Integer::from(-5)));

        let node = &ParseNode::new(
            ParseNodeType::If(
                Box::new(infix(
                    InfixOperator::Less,
                    symbol("a", dummy_pos()),
                    integer("0", dummy_pos()),
                    dummy_pos(),
                )),
                Box::new(prefix(
                    PrefixOperator::Minus,
                    symbol("a", dummy_pos()),
                    dummy_pos(),
                )),
                Box::new(symbol("a", dummy_pos())),
            ),
            dummy_pos(),
        );

        assert_eq!(exec(node, &mut env), Ok(Object::Integer(Integer::from(5))));
    }

    #[test]
    fn scope_hierarchy() {
        let mut env = Environment::default();
        env.set("x", Object::Boolean(Bool::from(true)));
        env.push_scope();

        let node = &symbol("x", dummy_pos());

        assert_eq!(exec(node, &mut env), Ok(Object::Boolean(Bool::from(true))));
    }

    #[test]
    fn save_value() {
        let mut env = Environment::default();

        let node = &let_(
            symbol("x", dummy_pos()),
            vec![],
            integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert!(exec(node, &mut env).is_ok());

        assert_eq!(env.get("x"), Some(&mut Object::Integer(Integer::from(0))));
    }

    #[test]
    fn tuple_() {
        let node = &tuple(
            vec![integer("1", dummy_pos()), integer("2", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Tuple(Tuple::from(vec![
                Object::Integer(Integer::from(1)),
                Object::Integer(Integer::from(2)),
            ]))),
        );
    }

    #[test]
    fn function_() {
        let node = &function(
            vec!["x"],
            vec![infix(
                InfixOperator::Product,
                integer("2", dummy_pos()),
                symbol("x", dummy_pos()),
                dummy_pos(),
            )],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Function(object::Function::DefinedFunction(
                DefinedFunction::new(
                    vec![String::from("x"),],
                    vec![infix(
                        InfixOperator::Product,
                        integer("2", dummy_pos()),
                        symbol("x", dummy_pos()),
                        dummy_pos()
                    )],
                )
            ))),
        );
    }

    #[test]
    fn call_() {
        let node = &call(
            function(
                vec!["x"],
                vec![infix(
                    InfixOperator::Product,
                    integer("2", dummy_pos()),
                    symbol("x", dummy_pos()),
                    dummy_pos(),
                )],
                dummy_pos(),
            ),
            vec![integer("1", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Integer(Integer::from(2)))
        );
    }

    #[test]
    fn several_params_call() {
        let node = &call(
            function(
                vec!["x", "y"],
                vec![infix(
                    InfixOperator::Sum,
                    symbol("x", dummy_pos()),
                    symbol("y", dummy_pos()),
                    dummy_pos(),
                )],
                dummy_pos(),
            ),
            vec![integer("1", dummy_pos()), integer("2", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Integer(Integer::from(3))),
        );
    }

    #[test]
    fn missing_args() {
        let node = &call(
            function(
                vec!["x", "y"],
                vec![infix(
                    InfixOperator::Sum,
                    symbol("x", dummy_pos()),
                    symbol("y", dummy_pos()),
                    dummy_pos(),
                )],
                dummy_pos(),
            ),
            vec![integer("1", dummy_pos())],
            _pos(0, 5),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Err(Error(
                EvalError::MissingFunctionArguments {
                    expected: 2,
                    actual: 1,
                }
                .into(),
                _pos(0, 5),
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
        env.set(
            "f",
            Object::Function(Function::Effect(Effect::new(test, 1))),
        );

        let node = &_for(
            "val",
            extension_list(
                vec![
                    integer("1", dummy_pos()),
                    integer("2", dummy_pos()),
                    integer("3", dummy_pos()),
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

        assert_eq!(exec(node, &mut env), Ok(Object::empty_tuple()),);

        assert_eq!(
            *ARGS.lock().unwrap(),
            vec![String::from("1"), String::from("2"), String::from("3"),]
        );
    }

    #[test]
    fn comprehension_set_() {
        let node = &comprehension_set(
            symbol("k", dummy_pos()),
            infix(
                InfixOperator::Greater,
                symbol("k", dummy_pos()),
                integer("1", dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::ComprehensionSet(ComprehensionSet::from((
                symbol("k", dummy_pos()),
                infix(
                    InfixOperator::Greater,
                    symbol("k", dummy_pos()),
                    integer("1", dummy_pos()),
                    dummy_pos()
                )
            )))),
        );
    }

    #[test]
    fn comprehension_set_question() {
        let node = &infix(
            InfixOperator::In,
            integer("1", dummy_pos()),
            comprehension_set(
                symbol("k", dummy_pos()),
                infix(
                    InfixOperator::GreaterEqual,
                    symbol("k", dummy_pos()),
                    integer("1", dummy_pos()),
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Boolean(crate::object::Bool::from(true))),
        );
    }

    #[test]
    fn extension_list_() {
        let node = &extension_list(vec![integer("1", dummy_pos())], dummy_pos());

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::ExtensionList(crate::object::ExtensionList::from(
                vec![Object::Integer(crate::object::Integer::from(1)),]
            ))),
        )
    }

    #[test]
    fn function_with_code_block() {
        let node = &call(
            function(
                vec!["x"],
                vec![
                    let_(
                        symbol("y", dummy_pos()),
                        vec![],
                        infix(
                            InfixOperator::Product,
                            integer("2", dummy_pos()),
                            symbol("x", dummy_pos()),
                            dummy_pos(),
                        ),
                        dummy_pos(),
                    ),
                    infix(
                        InfixOperator::Sum,
                        symbol("y", dummy_pos()),
                        integer("1", dummy_pos()),
                        dummy_pos(),
                    ),
                ],
                dummy_pos(),
            ),
            vec![integer("2", dummy_pos())],
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Integer(Integer::from(5))),
        );
    }

    #[test]
    fn comprehension_list_() {
        let node = &comprehension_list(
            infix(
                InfixOperator::Sum,
                symbol("k", dummy_pos()),
                integer("1", dummy_pos()),
                dummy_pos(),
            ),
            infix(
                InfixOperator::In,
                symbol("k", dummy_pos()),
                extension_list(
                    vec![integer("0", dummy_pos()), integer("1", dummy_pos())],
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::ExtensionList(ExtensionList::from(vec![
                Object::Integer(Integer::from(1)),
                Object::Integer(Integer::from(2)),
            ])))
        );
    }

    #[test]
    fn prepend() {
        let node = cons(
            integer("1", dummy_pos()),
            extension_list(vec![symbol("s", dummy_pos())], dummy_pos()),
            dummy_pos(),
        );

        let obj = Object::ExtensionList(
            vec![
                Object::Integer(Integer::from(1)),
                Object::Symbol(Symbol::from("s")),
            ]
            .into(),
        );

        assert_eq!(exec(&node, &mut Environment::default()), Ok(obj));
    }

    #[test]
    #[ignore = "not yet implemented"]
    fn missing_args_2() {
        let mut env = Environment::default();
        let func = let_(
            symbol("f", dummy_pos()),
            vec![symbol("x", dummy_pos())],
            symbol("x", dummy_pos()),
            dummy_pos(),
        );

        let _ = exec(&func, &mut env);

        let call = call(symbol("f", dummy_pos()), vec![], dummy_pos());

        assert_eq!(
            exec(&call, &mut env),
            Err(Error(
                ErrorType::Exec(EvalError::MissingFunctionArguments {
                    expected: 1,
                    actual: 0
                }),
                dummy_pos()
            ))
        );
    }

    #[test]
    fn decimal_number() {
        let node = decimal("1", "5", dummy_pos());
        let expected = Decimal::from(BigDecimal::from(3) / BigDecimal::from(2));

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Ok(Object::Decimal(expected)),
        );
    }

    #[test]
    fn range_() {
        let node = range(
            integer("1", dummy_pos()),
            integer("3", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Ok(Object::Range(Range::_new(1, 3))),
        );
    }

    #[test]
    fn fraction_() {
        let node = fraction(
            integer("1", dummy_pos()),
            integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Ok(Object::Fraction(Fraction::_new(1, 2))),
        );
    }

    #[test]
    fn denominator_zero() {
        let node = fraction(
            integer("1", dummy_pos()),
            integer("0", _pos(5, 1)),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Err(Error::new(EvalError::DenominatorZero.into(), _pos(5, 1))),
        );
    }

    #[test]
    fn list_from_range() {
        let node = comprehension_list(
            infix(
                InfixOperator::Sum,
                symbol("k", dummy_pos()),
                integer("1", dummy_pos()),
                dummy_pos(),
            ),
            infix(
                InfixOperator::In,
                symbol("k", dummy_pos()),
                range(
                    integer("0", dummy_pos()),
                    integer("3", dummy_pos()),
                    dummy_pos(),
                ),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Ok(Object::ExtensionList(ExtensionList::from(vec![
                Object::Integer(1.into()),
                Object::Integer(2.into()),
                Object::Integer(3.into()),
            ])))
        );
    }

    #[test]
    fn zero_division() {
        let node = infix(
            InfixOperator::Division,
            integer("1", dummy_pos()),
            integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            exec(&node, &mut Environment::default()),
            Err(Error::new(EvalError::DenominatorZero.into(), dummy_pos())),
        );
    }
}
