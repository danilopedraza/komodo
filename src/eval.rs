use crate::ast::{ASTNode, InfixOperator, PrefixOperator};
use crate::env::Environment;
use crate::object::{Bool, Char, ExtensionSet, Integer, MyString, Object, Symbol};

#[derive(Debug, PartialEq, Eq)]
pub enum EvalError {
    NonExistentOperationError,
}

fn truthy(val: Object) -> bool {
    match val {
        Object::Boolean(Bool { val }) => val,
        _ => false,
    }
}

fn list(l: &Vec<ASTNode>, env: &mut Environment) -> Result<Vec<Object>, EvalError> {
    let mut objs = vec![];

    for node in l {
        let obj = exec(node, env)?;

        objs.push(obj);
    }

    Ok(objs)
}

pub fn exec(node: &ASTNode, _env: &mut Environment) -> Result<Object, EvalError> {
    match node {
        ASTNode::Symbol(str) => match _env.get(str) {
            Some(obj) => Ok(obj.clone()),
            None => Ok(Object::Symbol(Symbol::from(str.as_str()))),
        },
        ASTNode::ExtensionSet(l) => {
            list(l, _env).map(|lst| Object::ExtensionSet(ExtensionSet::from(lst)))
        }
        ASTNode::Integer(str) => Ok(Object::Integer(Integer::from(str.as_str()))),
        ASTNode::Infix(op, lhs, rhs) => infix(*op, exec(lhs, _env)?, exec(rhs, _env)?),
        ASTNode::Let(ident, _, node) => match *ident.clone() {
            ASTNode::Symbol(name) => {
                let val = exec(node, _env)?;
                _env.set(&name.clone(), val.clone());

                Ok(val)
            }
            _ => todo!(),
        },
        ASTNode::Boolean(val) => Ok(Object::Boolean(Bool::from(*val))),
        ASTNode::Call(_, _) => todo!(),
        ASTNode::Char(chr) => Ok(Object::Char(Char::from(*chr))),
        ASTNode::ComprehensionSet(_, _) => todo!(),
        ASTNode::If(cond, first, second) => {
            if truthy(exec(cond, _env)?) {
                exec(first, _env)
            } else {
                exec(second, _env)
            }
        }
        ASTNode::Prefix(op, node) => prefix(*op, exec(node, _env)?),
        ASTNode::Signature(_, _) => todo!(),
        ASTNode::String(str) => Ok(Object::String(MyString::from(str.as_str()))),
        ASTNode::Tuple(_) => todo!(),
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
        Err(()) => Err(EvalError::NonExistentOperationError),
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
            Box::new(ASTNode::Symbol(String::from("x"))),
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
        env = env.new_scope();

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

        assert_eq!(env.get("x"), Some(&Object::Integer(Integer::from(0))));
    }
}
