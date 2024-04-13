use crate::object::{self, Callable, ComprehensionSet, ExtensionList, Function};

use crate::ast::{ASTNode, InfixOperator, PrefixOperator};
use crate::env::Environment;
use crate::object::{
    Bool, Char, DefinedFunction, ExtensionSet, Integer, MyString, Object, Symbol, Tuple,
};

#[derive(Debug, PartialEq, Eq)]
pub enum EvalError {
    MissingFunctionArguments,
    NonCallableObject,
    NonExistentOperation,
    NonIterableObject,
}

fn truthy(val: Object) -> bool {
    match val {
        Object::Boolean(boolean) => boolean.value(),
        _ => false,
    }
}

pub fn list(l: &[ASTNode], env: &mut Environment) -> Result<Vec<Object>, EvalError> {
    l.iter().map(|node| exec(node, env)).collect()
}

fn function(params: &[String], proc: &[ASTNode]) -> Result<Object, EvalError> {
    Ok(Object::Function(object::Function::DefinedFunction(
        DefinedFunction::new(params.to_owned(), proc.to_owned()),
    )))
}

pub fn exec(node: &ASTNode, env: &mut Environment) -> Result<Object, EvalError> {
    match node {
        ASTNode::Symbol(str) => symbol(str, env),
        ASTNode::ExtensionSet(l) => extension_set(l, env),
        ASTNode::Integer(str) => integer(str),
        ASTNode::Function(params, proc) => function(params, proc),
        ASTNode::Infix(op, lhs, rhs) => infix(*op, exec(lhs, env)?, exec(rhs, env)?),
        ASTNode::Let(ident, params, value) if params.is_empty() => let_(ident, value, env),
        ASTNode::Let(ident, args, value) => let_function(ident, args, value, env),
        ASTNode::Boolean(val) => boolean(*val),
        ASTNode::Call(func_node, args) => call(func_node, args, env),
        ASTNode::Char(chr) => char(*chr),
        ASTNode::ComprehensionSet(value, prop) => comprehension_set(value, prop),
        ASTNode::If(cond, first, second) => if_(exec(cond, env)?, first, second, env),
        ASTNode::Prefix(op, node) => prefix(*op, exec(node, env)?),
        ASTNode::Signature(_, _) => todo!(),
        ASTNode::String(str) => string(str),
        ASTNode::Tuple(l) => tuple(l, env),
        ASTNode::For(symbol, iterable, proc) => for_(symbol, exec(iterable, env)?, proc, env),
        ASTNode::ExtensionList(l) => extension_list(l, env),
        ASTNode::ComprehensionList(transform, prop) => comprehension_list(transform, prop, env),
        ASTNode::Wildcard => todo!(),
    }
}

fn extension_list(l: &[ASTNode], env: &mut Environment) -> Result<Object, EvalError> {
    list(l, env).map(|lst| Object::ExtensionList(ExtensionList::from(lst)))
}

fn tuple(l: &[ASTNode], env: &mut Environment) -> Result<Object, EvalError> {
    list(l, env).map(|lst| Object::Tuple(Tuple::from(lst)))
}

fn string(str: &str) -> Result<Object, EvalError> {
    Ok(Object::String(MyString::from(str)))
}

fn comprehension_set(value: &ASTNode, prop: &ASTNode) -> Result<Object, EvalError> {
    Ok(Object::ComprehensionSet(ComprehensionSet::from((
        value.clone(),
        prop.clone(),
    ))))
}

fn char(chr: char) -> Result<Object, EvalError> {
    Ok(Object::Char(Char::from(chr)))
}

fn boolean(val: bool) -> Result<Object, EvalError> {
    Ok(Object::Boolean(Bool::from(val)))
}

fn let_(ident: &ASTNode, value: &ASTNode, env: &mut Environment) -> Result<Object, EvalError> {
    match ident.clone() {
        ASTNode::Symbol(name) => exec_and_set(value, &name, env),
        ASTNode::Signature(ident, None) => match *ident {
            ASTNode::Symbol(name) => exec_and_set(value, &name, env),
            _ => todo!(),
        },
        _ => todo!(),
    }
}

fn integer(str: &str) -> Result<Object, EvalError> {
    Ok(Object::Integer(Integer::from(str)))
}

fn extension_set(l: &[ASTNode], env: &mut Environment) -> Result<Object, EvalError> {
    list(l, env).map(|lst| Object::ExtensionSet(ExtensionSet::from(lst)))
}

fn symbol(str: &str, env: &mut Environment) -> Result<Object, EvalError> {
    match env.get(str) {
        Some(obj) => Ok(obj.clone()),
        None => Ok(Object::Symbol(Symbol::from(str))),
    }
}

fn comprehension_list(
    transform: &ASTNode,
    prop: &ASTNode,
    env: &mut Environment,
) -> Result<Object, EvalError> {
    let (symbol, iterator) = match prop {
        ASTNode::Infix(InfixOperator::In, lhs, rhs) => match (&**lhs, &**rhs) {
            (ASTNode::Symbol(ident), ASTNode::ExtensionList(l)) => (ident, list(l, env)?),
            _ => todo!(),
        },
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
    ident: &ASTNode,
    args: &[ASTNode],
    value: &ASTNode,
    env: &mut Environment,
) -> Result<Object, EvalError> {
    let name = match ident {
        ASTNode::Symbol(name) => name,
        _ => todo!(),
    };

    let function: &mut DefinedFunction = match env.get(name) {
        None => {
            env.set(
                name,
                Object::Function(Function::DefinedFunction(DefinedFunction::default())),
            );

            match env.get(name) {
                Some(Object::Function(Function::DefinedFunction(f))) => f,
                _ => todo!(),
            }
        }
        Some(Object::Function(Function::DefinedFunction(f))) => f,
        _ => todo!(),
    };

    function.add_pattern(args, value);

    Ok(Object::Function(Function::DefinedFunction(
        function.clone(),
    )))
}

fn exec_and_set(node: &ASTNode, name: &str, env: &mut Environment) -> Result<Object, EvalError> {
    let val = exec(node, env)?;
    env.set(name, val.clone());
    Ok(val)
}

fn if_(
    cond: Object,
    first: &ASTNode,
    second: &ASTNode,
    env: &mut Environment,
) -> Result<Object, EvalError> {
    if truthy(cond) {
        exec(first, env)
    } else {
        exec(second, env)
    }
}

fn for_(
    symbol: &str,
    iterable_obj: Object,
    proc: &[ASTNode],
    env: &mut Environment,
) -> Result<Object, EvalError> {
    let iter = match &iterable_obj {
        Object::ExtensionSet(set) => Ok(set.list()),
        _ => Err(EvalError::NonIterableObject),
    }?;

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

fn call(func_node: &ASTNode, args: &[ASTNode], env: &mut Environment) -> Result<Object, EvalError> {
    let func = exec(func_node, env)?;

    let mut func_args = vec![];
    for arg in args {
        let func_arg = exec(arg, env)?;
        func_args.push(func_arg);
    }

    match func {
        Object::Function(f) => f.call(&func_args, env),
        _ => Err(EvalError::NonCallableObject),
    }
}

fn infix(op: InfixOperator, lhs: Object, rhs: Object) -> Result<Object, EvalError> {
    let res = match op {
        InfixOperator::BitwiseAnd => lhs.bitwise_and(rhs),
        InfixOperator::BitwiseOr => lhs.bitwise_or(rhs),
        InfixOperator::BitwiseXor => lhs.bitwise_xor(rhs),
        InfixOperator::Call => todo!(),
        InfixOperator::Correspondence => todo!(),
        InfixOperator::Division => lhs.over(rhs),
        InfixOperator::Equality => lhs.equality(rhs),
        InfixOperator::Exponentiation => lhs.pow(rhs),
        InfixOperator::Greater => lhs.greater(rhs),
        InfixOperator::GreaterEqual => lhs.greater_equal(rhs),
        InfixOperator::In => rhs.contains(lhs),
        InfixOperator::LeftShift => lhs.left_shift(rhs),
        InfixOperator::Less => lhs.less(rhs),
        InfixOperator::LessEqual => lhs.less_equal(rhs),
        InfixOperator::LogicAnd => lhs.logic_and(rhs),
        InfixOperator::LogicOr => lhs.logic_or(rhs),
        InfixOperator::Mod => lhs.modulo(rhs),
        InfixOperator::NotEquality => lhs.neq(rhs),
        InfixOperator::Product => lhs.product(rhs),
        InfixOperator::RightShift => lhs.right_shift(rhs),
        InfixOperator::Substraction => lhs.substraction(rhs),
        InfixOperator::Sum => lhs.sum(rhs),
    };

    match res {
        Err(()) => Err(EvalError::NonExistentOperation),
        Ok(obj) => Ok(obj),
    }
}

fn prefix(op: PrefixOperator, obj: Object) -> Result<Object, EvalError> {
    let res = match op {
        PrefixOperator::BitwiseNot => obj.bitwise_not(),
        PrefixOperator::LogicNot => obj.logic_not(),
        PrefixOperator::Minus => obj.inverse(),
    };

    match res {
        Err(()) => Err(EvalError::NonExistentOperation),
        Ok(obj) => Ok(obj),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;
    use std::vec;

    use super::*;
    use crate::ast::ASTNode;
    use crate::object::*;

    #[test]
    fn symbol() {
        let node = &ASTNode::Symbol(String::from("a"));
        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Symbol(Symbol::from("a")))
        );
    }

    #[test]
    fn set_by_extension() {
        let node = &ASTNode::ExtensionSet(vec![
            ASTNode::Symbol(String::from("a")),
            ASTNode::Symbol(String::from("a")),
        ]);
        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::ExtensionSet(ExtensionSet::from(vec![
                Object::Symbol(Symbol::from("a")),
                Object::Symbol(Symbol::from("a")),
            ]))),
        );
    }

    #[test]
    fn integer_sum() {
        let node = &ASTNode::Infix(
            InfixOperator::Sum,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from("1")))
        );
    }

    #[test]
    fn integer_substraction() {
        let node = &ASTNode::Infix(
            InfixOperator::Substraction,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(-1)))
        );
    }

    #[test]
    fn integer_product() {
        let node = &ASTNode::Infix(
            InfixOperator::Product,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from("0")))
        );
    }

    #[test]
    fn symbol_comparison() {
        let node = &ASTNode::Infix(
            InfixOperator::Equality,
            Box::new(ASTNode::Symbol(String::from("a"))),
            Box::new(ASTNode::Symbol(String::from("b"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(false)))
        );
    }

    #[test]
    fn let_expression() {
        let node = &ASTNode::Let(
            Box::new(ASTNode::Signature(
                Box::new(ASTNode::Symbol(String::from("x"))),
                None,
            )),
            vec![],
            Box::new(ASTNode::Integer(String::from("0"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(0)))
        );
    }

    #[test]
    fn logic_operators() {
        let node = &ASTNode::Infix(
            InfixOperator::LogicAnd,
            Box::new(ASTNode::Infix(
                InfixOperator::LogicOr,
                Box::new(ASTNode::Boolean(true)),
                Box::new(ASTNode::Boolean(false)),
            )),
            Box::new(ASTNode::Boolean(false)),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(false)))
        );
    }

    #[test]
    fn less_leq() {
        let node = &ASTNode::Infix(
            InfixOperator::LogicAnd,
            Box::new(ASTNode::Infix(
                InfixOperator::Less,
                Box::new(ASTNode::Integer(String::from('0'))),
                Box::new(ASTNode::Integer(String::from('1'))),
            )),
            Box::new(ASTNode::Infix(
                InfixOperator::LessEqual,
                Box::new(ASTNode::Integer(String::from('1'))),
                Box::new(ASTNode::Integer(String::from('1'))),
            )),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(true)))
        );
    }

    #[test]
    fn greater_geq() {
        let node = &ASTNode::Infix(
            InfixOperator::LogicAnd,
            Box::new(ASTNode::Infix(
                InfixOperator::Greater,
                Box::new(ASTNode::Integer(String::from('1'))),
                Box::new(ASTNode::Integer(String::from('0'))),
            )),
            Box::new(ASTNode::Infix(
                InfixOperator::GreaterEqual,
                Box::new(ASTNode::Integer(String::from('0'))),
                Box::new(ASTNode::Integer(String::from('0'))),
            )),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(true)))
        );
    }

    #[test]
    fn neq() {
        let node = &ASTNode::Infix(
            InfixOperator::LogicAnd,
            Box::new(ASTNode::Infix(
                InfixOperator::NotEquality,
                Box::new(ASTNode::Integer(String::from('1'))),
                Box::new(ASTNode::Integer(String::from('2'))),
            )),
            Box::new(ASTNode::Infix(
                InfixOperator::NotEquality,
                Box::new(ASTNode::Integer(String::from('1'))),
                Box::new(ASTNode::Boolean(true)),
            )),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Boolean(Bool::from(true)))
        );
    }

    #[test]
    fn bitwise_and_xor_or() {
        let node = &ASTNode::Infix(
            InfixOperator::BitwiseOr,
            Box::new(ASTNode::Infix(
                InfixOperator::BitwiseXor,
                Box::new(ASTNode::Infix(
                    InfixOperator::BitwiseAnd,
                    Box::new(ASTNode::Integer(String::from("7"))),
                    Box::new(ASTNode::Integer(String::from("6"))),
                )),
                Box::new(ASTNode::Integer(String::from("1"))),
            )),
            Box::new(ASTNode::Integer(String::from("0"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(7)))
        );
    }

    #[test]
    fn shifts() {
        let node = &ASTNode::Infix(
            InfixOperator::LeftShift,
            Box::new(ASTNode::Infix(
                InfixOperator::RightShift,
                Box::new(ASTNode::Integer(String::from("256"))),
                Box::new(ASTNode::Integer(String::from("4"))),
            )),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(32)))
        );
    }

    #[test]
    fn power_and_division() {
        let node = &ASTNode::Infix(
            InfixOperator::Division,
            Box::new(ASTNode::Infix(
                InfixOperator::Exponentiation,
                Box::new(ASTNode::Integer(String::from("3"))),
                Box::new(ASTNode::Integer(String::from("2"))),
            )),
            Box::new(ASTNode::Integer(String::from("2"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(4)))
        );
    }

    #[test]
    fn remainder() {
        let node = &ASTNode::Infix(
            InfixOperator::Mod,
            Box::new(ASTNode::Integer(String::from("3"))),
            Box::new(ASTNode::Integer(String::from("2"))),
        );

        assert_eq!(
            exec(node, &mut Default::default()),
            Ok(Object::Integer(Integer::from(1)))
        );
    }

    #[test]
    fn prefix() {
        let node = &ASTNode::Prefix(
            PrefixOperator::LogicNot,
            Box::new(ASTNode::Infix(
                InfixOperator::NotEquality,
                Box::new(ASTNode::Prefix(
                    PrefixOperator::BitwiseNot,
                    Box::new(ASTNode::Integer(String::from("1"))),
                )),
                Box::new(ASTNode::Prefix(
                    PrefixOperator::Minus,
                    Box::new(ASTNode::Integer(String::from("1"))),
                )),
            )),
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

        let node = &ASTNode::If(
            Box::new(ASTNode::Infix(
                InfixOperator::Less,
                Box::new(ASTNode::Symbol(String::from("a"))),
                Box::new(ASTNode::Integer(String::from("0"))),
            )),
            Box::new(ASTNode::Prefix(
                PrefixOperator::Minus,
                Box::new(ASTNode::Symbol(String::from("a"))),
            )),
            Box::new(ASTNode::Symbol(String::from("a"))),
        );

        assert_eq!(exec(node, &mut env), Ok(Object::Integer(Integer::from(5))));
    }

    #[test]
    fn scope_hierarchy() {
        let mut env = Environment::default();
        env.set("x", Object::Boolean(Bool::from(true)));
        env.push_scope();

        let node = &ASTNode::Symbol(String::from("x"));

        assert_eq!(exec(node, &mut env), Ok(Object::Boolean(Bool::from(true))));
    }

    #[test]
    fn save_value() {
        let mut env = Environment::default();

        let node = &ASTNode::Let(
            Box::new(ASTNode::Symbol(String::from("x"))),
            vec![],
            Box::new(ASTNode::Integer(String::from("0"))),
        );

        assert!(exec(node, &mut env).is_ok());

        assert_eq!(env.get("x"), Some(&mut Object::Integer(Integer::from(0))));
    }

    #[test]
    fn tuple() {
        let node = &ASTNode::Tuple(vec![
            ASTNode::Integer(String::from("1")),
            ASTNode::Integer(String::from("2")),
        ]);

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Tuple(Tuple::from(vec![
                Object::Integer(Integer::from(1)),
                Object::Integer(Integer::from(2)),
            ]))),
        );
    }

    #[test]
    fn function() {
        let node = &ASTNode::Function(
            vec![String::from("x")],
            vec![ASTNode::Infix(
                InfixOperator::Product,
                Box::new(ASTNode::Integer(String::from("2"))),
                Box::new(ASTNode::Symbol(String::from("x"))),
            )],
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Function(object::Function::DefinedFunction(
                DefinedFunction::new(
                    vec![String::from("x"),],
                    vec![ASTNode::Infix(
                        InfixOperator::Product,
                        Box::new(ASTNode::Integer(String::from("2"))),
                        Box::new(ASTNode::Symbol(String::from("x"))),
                    )],
                )
            ))),
        );
    }

    #[test]
    fn call() {
        let node = &ASTNode::Call(
            Box::new(ASTNode::Function(
                vec![String::from("x")],
                vec![ASTNode::Infix(
                    InfixOperator::Product,
                    Box::new(ASTNode::Integer(String::from("2"))),
                    Box::new(ASTNode::Symbol(String::from("x"))),
                )],
            )),
            vec![ASTNode::Integer(String::from("1"))],
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Integer(Integer::from(2)))
        );
    }

    #[test]
    fn several_params_call() {
        let node = &ASTNode::Call(
            Box::new(ASTNode::Function(
                vec![String::from("x"), String::from("y")],
                vec![ASTNode::Infix(
                    InfixOperator::Sum,
                    Box::new(ASTNode::Symbol(String::from("x"))),
                    Box::new(ASTNode::Symbol(String::from("y"))),
                )],
            )),
            vec![
                ASTNode::Integer(String::from("1")),
                ASTNode::Integer(String::from("2")),
            ],
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Integer(Integer::from(3))),
        );
    }

    #[test]
    fn missing_args() {
        let node = &ASTNode::Call(
            Box::new(ASTNode::Function(
                vec![String::from("x"), String::from("y")],
                vec![ASTNode::Infix(
                    InfixOperator::Sum,
                    Box::new(ASTNode::Symbol(String::from("x"))),
                    Box::new(ASTNode::Symbol(String::from("y"))),
                )],
            )),
            vec![ASTNode::Integer(String::from("1"))],
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Err(EvalError::MissingFunctionArguments)
        );
    }

    #[test]
    fn for_loop() {
        // I preferred this to test the change of state without assignments
        static ARGS: Mutex<Vec<String>> = Mutex::new(vec![]);

        fn test(args: &[Object]) -> Object {
            ARGS.lock().unwrap().push(args[0].to_string());
            Object::empty_tuple()
        }

        let mut env = Environment::default();
        env.set("f", Object::Function(Function::Effect(Effect::new(test))));

        let node = &ASTNode::For(
            "val".into(),
            Box::new(ASTNode::ExtensionSet(vec![
                ASTNode::Integer(String::from("1")),
                ASTNode::Integer(String::from("2")),
                ASTNode::Integer(String::from("3")),
            ])),
            vec![ASTNode::Call(
                Box::new(ASTNode::Symbol(String::from("f"))),
                vec![ASTNode::Symbol(String::from("val"))],
            )],
        );

        assert_eq!(exec(node, &mut env), Ok(Object::empty_tuple()),);

        assert_eq!(
            *ARGS.lock().unwrap(),
            vec![String::from("1"), String::from("2"), String::from("3"),]
        );
    }

    #[test]
    fn comprehension_set() {
        let node = &ASTNode::ComprehensionSet(
            Box::new(ASTNode::Symbol(String::from("k"))),
            Box::new(ASTNode::Infix(
                InfixOperator::Greater,
                Box::new(ASTNode::Symbol(String::from("k"))),
                Box::new(ASTNode::Integer(String::from("1"))),
            )),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::ComprehensionSet(ComprehensionSet::from((
                ASTNode::Symbol(String::from("k")),
                ASTNode::Infix(
                    InfixOperator::Greater,
                    Box::new(ASTNode::Symbol(String::from("k"))),
                    Box::new(ASTNode::Integer(String::from("1"))),
                )
            )))),
        );
    }

    #[test]
    fn comprehension_set_question() {
        let node = &ASTNode::Infix(
            InfixOperator::In,
            Box::new(ASTNode::Integer(String::from("1"))),
            Box::new(ASTNode::ComprehensionSet(
                Box::new(ASTNode::Symbol(String::from("k"))),
                Box::new(ASTNode::Infix(
                    InfixOperator::GreaterEqual,
                    Box::new(ASTNode::Symbol(String::from("k"))),
                    Box::new(ASTNode::Integer(String::from("1"))),
                )),
            )),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Boolean(crate::object::Bool::from(true))),
        );
    }

    #[test]
    fn extension_list() {
        let node = &ASTNode::ExtensionList(vec![ASTNode::Integer(String::from("1"))]);

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::ExtensionList(crate::object::ExtensionList::from(
                vec![Object::Integer(crate::object::Integer::from(1)),]
            ))),
        )
    }

    #[test]
    fn function_with_code_block() {
        let node = &ASTNode::Call(
            Box::new(ASTNode::Function(
                vec![String::from("x")],
                vec![
                    ASTNode::Let(
                        Box::new(ASTNode::Symbol(String::from("y"))),
                        vec![],
                        Box::new(ASTNode::Infix(
                            InfixOperator::Product,
                            Box::new(ASTNode::Integer(String::from("2"))),
                            Box::new(ASTNode::Symbol(String::from("x"))),
                        )),
                    ),
                    ASTNode::Infix(
                        InfixOperator::Sum,
                        Box::new(ASTNode::Symbol(String::from("y"))),
                        Box::new(ASTNode::Integer(String::from("1"))),
                    ),
                ],
            )),
            vec![ASTNode::Integer(String::from("2"))],
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::Integer(Integer::from(5))),
        );
    }

    #[test]
    fn comprehension_list() {
        let node = &ASTNode::ComprehensionList(
            Box::new(ASTNode::Infix(
                InfixOperator::Sum,
                Box::new(ASTNode::Symbol(String::from("k"))),
                Box::new(ASTNode::Integer(String::from("1"))),
            )),
            Box::new(ASTNode::Infix(
                InfixOperator::In,
                Box::new(ASTNode::Symbol(String::from("k"))),
                Box::new(ASTNode::ExtensionList(vec![
                    ASTNode::Integer(String::from("0")),
                    ASTNode::Integer(String::from("1")),
                ])),
            )),
        );

        assert_eq!(
            exec(node, &mut Environment::default()),
            Ok(Object::ExtensionList(ExtensionList::from(vec![
                Object::Integer(Integer::from(1)),
                Object::Integer(Integer::from(2)),
            ])))
        );
    }
}
