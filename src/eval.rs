use crate::ast::{ASTNode, InfixOperator, PrefixOperator};
use crate::env::Environment;

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Boolean(bool),
    ExtensionSet(Vec<Type>),
    Number(i64),
    Symbol(String),
}

pub fn to_string(t: &Type) -> String {
    match t {
        Type::Boolean(true) => String::from("true"),
        Type::Boolean(false) => String::from("false"),
        Type::ExtensionSet(vec) => vec
            .iter()
            .map(to_string)
            .collect::<Vec<String>>()
            .join(", "),
        Type::Number(num) => num.to_string(),
        Type::Symbol(s) => s.to_string(),
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

fn infix(op: InfixOperator, lhs: Type, rhs: Type) -> Type {
    type O = InfixOperator;
    type T = Type;
    match (op, lhs, rhs) {
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
    }
}

fn prefix(op: PrefixOperator, expr: Type) -> Type {
    type P = PrefixOperator;
    match (op, expr) {
        (P::BitwiseNot, Type::Number(num)) => Type::Number(!num),
        (P::LogicNot, Type::Boolean(val)) => Type::Boolean(!val),
        (P::Minus, Type::Number(num)) => Type::Number(-num),
        _ => todo!(),
    }
}

fn truthy(val: Type) -> bool {
    match val {
        Type::Boolean(res) => res,
        _ => todo!(),
    }
}

pub fn eval(node: &ASTNode, _env: &Environment) -> Type {
    match node {
        ASTNode::Boolean(val) => Type::Boolean(*val),
        ASTNode::Integer(str) => Type::Number(str.parse().unwrap()),
        ASTNode::Symbol(str) => match _env.get(str) {
            Some(val) => eval(val, _env),
            None => Type::Symbol(str.clone()),
        },
        ASTNode::ExtensionSet(lst) => Type::ExtensionSet(
            remove_repeated(lst)
                .iter()
                .map(|val| eval(val, _env))
                .collect(),
        ),
        ASTNode::ComprehensionSet(_, _) => todo!(),
        ASTNode::Let(_, _, val) => eval(val, _env),
        ASTNode::Tuple(_) => todo!(),
        ASTNode::Signature(_, _) => todo!(),
        ASTNode::Infix(op, lhs, rhs) => infix(*op, eval(lhs, _env), eval(rhs, _env)),
        ASTNode::Prefix(op, expr) => prefix(*op, eval(expr, _env)),
        ASTNode::If(cond, true_res, false_res) => {
            if truthy(eval(cond, _env)) {
                eval(true_res, _env)
            } else {
                eval(false_res, _env)
            }
        }
        ASTNode::Call(_, _) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::ast::ASTNode;

    #[test]
    fn symbol() {
        let node = &ASTNode::Symbol(String::from("a"));
        assert_eq!(
            eval(node, &Default::default()),
            Type::Symbol(String::from("a"))
        );
    }

    #[test]
    fn set_with_repeated_elements() {
        let node = &ASTNode::ExtensionSet(vec![
            ASTNode::Symbol(String::from("a")),
            ASTNode::Symbol(String::from("a")),
        ]);
        assert_eq!(
            eval(node, &Default::default()),
            Type::ExtensionSet(vec![Type::Symbol(String::from("a")),])
        );
    }

    #[test]
    fn integer_sum() {
        let node = &ASTNode::Infix(
            InfixOperator::Sum,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(eval(node, &Default::default()), Type::Number(1));
    }

    #[test]
    fn integer_substraction() {
        let node = &ASTNode::Infix(
            InfixOperator::Substraction,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(eval(node, &Default::default()), Type::Number(-1));
    }

    #[test]
    fn integer_product() {
        let node = &ASTNode::Infix(
            InfixOperator::Product,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(eval(node, &Default::default()), Type::Number(0));
    }

    #[test]
    fn symbol_comparison() {
        let node = &ASTNode::Infix(
            InfixOperator::Equality,
            Box::new(ASTNode::Symbol(String::from("a"))),
            Box::new(ASTNode::Symbol(String::from("b"))),
        );

        assert_eq!(eval(node, &Default::default()), Type::Boolean(false));
    }

    #[test]
    fn let_expression() {
        let node = &ASTNode::Let(
            Box::new(ASTNode::Symbol(String::from("x"))),
            vec![],
            Box::new(ASTNode::Integer(String::from("0"))),
        );

        assert_eq!(eval(node, &Default::default()), Type::Number(0));
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

        assert_eq!(eval(node, &Default::default()), Type::Boolean(false));
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

        assert_eq!(eval(node, &Default::default()), Type::Boolean(true));
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

        assert_eq!(eval(node, &Default::default()), Type::Boolean(true));
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

        assert_eq!(eval(node, &Default::default()), Type::Boolean(true));
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

        assert_eq!(eval(node, &Default::default()), Type::Number(7));
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

        assert_eq!(eval(node, &Default::default()), Type::Number(32));
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

        assert_eq!(eval(node, &Default::default()), Type::Number(4));
    }

    #[test]
    fn remainder() {
        let node = &ASTNode::Infix(
            InfixOperator::Mod,
            Box::new(ASTNode::Integer(String::from("3"))),
            Box::new(ASTNode::Integer(String::from("2"))),
        );

        assert_eq!(eval(node, &Default::default()), Type::Number(1));
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

        assert_eq!(eval(node, &Default::default()), Type::Boolean(false));
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

        assert_eq!(eval(node, &env), Type::Number(5));
    }

    #[test]
    fn scope_hierarchy() {
        let mut env = Environment::default();
        env.set("x", ASTNode::Boolean(true));
        env = env.new_scope();

        let node = &ASTNode::Symbol(String::from("x"));

        assert_eq!(eval(node, &env), Type::Boolean(true));
    }
}
