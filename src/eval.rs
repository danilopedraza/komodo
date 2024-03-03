use crate::ast::{ASTNode, InfixOperator, PrefixOperator};
use crate::env::Environment;
use crate::object::{ExtensionSet, Integer, Symbol, _Object};

#[derive(Debug, PartialEq, Eq)]
pub enum EvalError {
    NonExistentOperationError,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
    Char(char),
    ExtensionSet(Vec<Object>),
    Number(i64),
    String(String),
    Symbol(String),
}

pub fn to_string(t: &Object) -> String {
    match t {
        Object::Boolean(true) => String::from("true"),
        Object::Boolean(false) => String::from("false"),
        Object::ExtensionSet(vec) => vec
            .iter()
            .map(to_string)
            .collect::<Vec<String>>()
            .join(", "),
        Object::Number(num) => num.to_string(),
        Object::Symbol(s) => s.to_string(),
        Object::Char(chr) => String::from(*chr),
        Object::String(str) => str.clone(),
    }
}

fn remove_repeated(vec: &Vec<ASTNode>) -> Vec<&ASTNode> {
    let mut res: Vec<&ASTNode> = vec![];
    for val in vec {
        if !res.contains(&val) {
            res.push(val);
        }
    }

    res
}

fn infix(op: InfixOperator, lhs: Object, rhs: Object) -> Result<Object, EvalError> {
    type O = InfixOperator;
    type T = Object;
    Ok(match (op, lhs, rhs) {
        (O::Equality, T::Symbol(l), T::Symbol(r)) => T::Boolean(l == r),
        (O::NotEquality, T::Number(l), T::Number(r)) => T::Boolean(l != r),
        (O::NotEquality, _, _) => T::Boolean(true),
        (O::Product, T::Number(l), T::Number(r)) => T::Number(l * r),
        (O::Sum, T::Number(l), T::Number(r)) => T::Number(l + r),
        (O::Substraction, T::Number(l), T::Number(r)) => T::Number(l - r),
        (O::Exponentiation, T::Number(l), T::Number(r)) => T::Number(l.pow(r.try_into().unwrap())),
        (O::Division, T::Number(l), T::Number(r)) => T::Number(l / r),
        (O::Mod, T::Number(l), T::Number(r)) => T::Number(l % r),
        (O::GreaterEqual, T::Number(l), T::Number(r)) => T::Boolean(l >= r),
        (O::Greater, T::Number(l), T::Number(r)) => T::Boolean(l > r),
        (O::LessEqual, T::Number(l), T::Number(r)) => T::Boolean(l <= r),
        (O::Less, T::Number(l), T::Number(r)) => T::Boolean(l < r),
        (O::LogicAnd, T::Boolean(l), T::Boolean(r)) => T::Boolean(l && r),
        (O::LogicOr, T::Boolean(l), T::Boolean(r)) => T::Boolean(l || r),
        (O::BitwiseAnd, T::Number(l), T::Number(r)) => T::Number(l & r),
        (O::BitwiseOr, T::Number(l), T::Number(r)) => T::Number(l | r),
        (O::BitwiseXor, T::Number(l), T::Number(r)) => T::Number(l ^ r),
        (O::LeftShift, T::Number(l), T::Number(r)) => T::Number(l << r),
        (O::RightShift, T::Number(l), T::Number(r)) => T::Number(l >> r),
        _ => todo!(),
    })
}

fn prefix(op: PrefixOperator, expr: Object) -> Result<Object, EvalError> {
    type P = PrefixOperator;
    Ok(match (op, expr) {
        (P::BitwiseNot, Object::Number(num)) => Object::Number(!num),
        (P::LogicNot, Object::Boolean(val)) => Object::Boolean(!val),
        (P::Minus, Object::Number(num)) => Object::Number(-num),
        _ => todo!(),
    })
}

fn truthy(val: Object) -> bool {
    match val {
        Object::Boolean(res) => res,
        _ => todo!(),
    }
}

pub fn eval(node: &ASTNode, _env: &Environment) -> Result<Object, EvalError> {
    Ok(match node {
        ASTNode::Boolean(val) => Object::Boolean(*val),
        ASTNode::Integer(str) => Object::Number(str.parse().unwrap()),
        ASTNode::Symbol(str) => match _env.get(str) {
            Some(val) => eval(val, _env)?,
            None => Object::Symbol(str.clone()),
        },
        ASTNode::ExtensionSet(lst) => Object::ExtensionSet(
            remove_repeated(lst)
                .iter()
                .map(|val| eval(val, _env).unwrap())
                .collect(),
        ),
        ASTNode::ComprehensionSet(_, _) => todo!(),
        ASTNode::Let(_, _, val) => eval(val, _env)?,
        ASTNode::Tuple(_) => todo!(),
        ASTNode::Signature(_, _) => todo!(),
        ASTNode::Infix(op, lhs, rhs) => infix(*op, eval(lhs, _env)?, eval(rhs, _env)?)?,
        ASTNode::Prefix(op, expr) => prefix(*op, eval(expr, _env)?)?,
        ASTNode::If(cond, true_res, false_res) => {
            if truthy(eval(cond, _env)?) {
                eval(true_res, _env)?
            } else {
                eval(false_res, _env)?
            }
        }
        ASTNode::Call(_, _) => todo!(),
        ASTNode::Char(chr) => Object::Char(*chr),
        ASTNode::String(str) => Object::String(str.clone()),
    })
}

pub fn _eval(node: &ASTNode, _env: &Environment) -> Result<_Object, EvalError> {
    match node {
        ASTNode::Symbol(str) => Ok(Symbol::new(&str)),
        ASTNode::ExtensionSet(list) => Ok(ExtensionSet::new(
            list.iter().map(|node| _eval(node, _env).unwrap()).collect(),
        )),
        ASTNode::Integer(str) => Ok(Integer::new(&str)),
        ASTNode::Infix(op, lhs, rhs) => _infix(*op, _eval(lhs, _env)?, _eval(rhs, _env)?),
        _ => todo!(),
    }
}

fn _infix(op: InfixOperator, lhs: _Object, rhs: _Object) -> Result<_Object, EvalError> {
    let res = match op {
        InfixOperator::Sum => lhs.sum(rhs),
        _ => todo!(),
    };

    match res {
        Err(()) => Err(EvalError::NonExistentOperationError),
        Ok(obj) => Ok(obj),
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::ast::ASTNode;
    use crate::object::*;

    #[test]
    fn symbol() {
        let node = &ASTNode::Symbol(String::from("a"));
        assert_eq!(_eval(node, &Default::default()), Ok(Symbol::new("a")));
    }

    #[test]
    fn set_by_extension() {
        let node = &ASTNode::ExtensionSet(vec![
            ASTNode::Symbol(String::from("a")),
            ASTNode::Symbol(String::from("a")),
        ]);
        assert_eq!(
            _eval(node, &Default::default()),
            Ok(ExtensionSet::new(vec![Symbol::new("a"), Symbol::new("a"),])),
        );
    }

    #[test]
    fn integer_sum() {
        let node = &ASTNode::Infix(
            InfixOperator::Sum,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(_eval(node, &Default::default()), Ok(Integer::new("1")));
    }

    #[test]
    fn integer_substraction() {
        let node = &ASTNode::Infix(
            InfixOperator::Substraction,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(eval(node, &Default::default()), Ok(Object::Number(-1)));
    }

    #[test]
    fn integer_product() {
        let node = &ASTNode::Infix(
            InfixOperator::Product,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(eval(node, &Default::default()), Ok(Object::Number(0)));
    }

    #[test]
    fn symbol_comparison() {
        let node = &ASTNode::Infix(
            InfixOperator::Equality,
            Box::new(ASTNode::Symbol(String::from("a"))),
            Box::new(ASTNode::Symbol(String::from("b"))),
        );

        assert_eq!(eval(node, &Default::default()), Ok(Object::Boolean(false)));
    }

    #[test]
    fn let_expression() {
        let node = &ASTNode::Let(
            Box::new(ASTNode::Symbol(String::from("x"))),
            vec![],
            Box::new(ASTNode::Integer(String::from("0"))),
        );

        assert_eq!(eval(node, &Default::default()), Ok(Object::Number(0)));
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

        assert_eq!(eval(node, &Default::default()), Ok(Object::Boolean(false)));
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

        assert_eq!(eval(node, &Default::default()), Ok(Object::Boolean(true)));
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

        assert_eq!(eval(node, &Default::default()), Ok(Object::Boolean(true)));
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

        assert_eq!(eval(node, &Default::default()), Ok(Object::Boolean(true)));
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

        assert_eq!(eval(node, &Default::default()), Ok(Object::Number(7)));
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

        assert_eq!(eval(node, &Default::default()), Ok(Object::Number(32)));
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

        assert_eq!(eval(node, &Default::default()), Ok(Object::Number(4)));
    }

    #[test]
    fn remainder() {
        let node = &ASTNode::Infix(
            InfixOperator::Mod,
            Box::new(ASTNode::Integer(String::from("3"))),
            Box::new(ASTNode::Integer(String::from("2"))),
        );

        assert_eq!(eval(node, &Default::default()), Ok(Object::Number(1)));
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

        assert_eq!(eval(node, &Default::default()), Ok(Object::Boolean(false)));
    }

    #[test]
    fn if_expr() {
        let mut env = Environment::default();
        env.set(
            "a",
            ASTNode::Prefix(
                PrefixOperator::Minus,
                Box::new(ASTNode::Integer(String::from("5"))),
            ),
        );

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

        assert_eq!(eval(node, &env), Ok(Object::Number(5)));
    }

    #[test]
    fn scope_hierarchy() {
        let mut env = Environment::default();
        env.set("x", ASTNode::Boolean(true));
        env = env.new_scope();

        let node = &ASTNode::Symbol(String::from("x"));

        assert_eq!(eval(node, &env), Ok(Object::Boolean(true)));
    }
}
