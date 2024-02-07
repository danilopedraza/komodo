use crate::parser::{ASTNode, InfixOperator};

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
        (O::Exponentiation, T::Number(l), T::Number(r)) => T::Number(l.pow(r.try_into().unwrap())),
        (O::Division, T::Number(l), T::Number(r)) => T::Number(l / r),
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

pub fn eval(node: &ASTNode) -> Type {
    match node {
        ASTNode::Boolean(val) => Type::Boolean(*val),
        ASTNode::Integer(str) => Type::Number(str.parse().unwrap()),
        ASTNode::Symbol(str) => Type::Symbol(str.clone()),
        ASTNode::ExtensionSet(lst) => {
            Type::ExtensionSet(remove_repeated(lst).iter().map(|val| eval(val)).collect())
        }
        ASTNode::ComprehensionSet(_, _) => todo!(),
        ASTNode::Let(_, _, val) => eval(val),
        ASTNode::Tuple(_) => todo!(),
        ASTNode::Signature(_, _) => todo!(),
        ASTNode::Infix(op, lhs, rhs) => infix(*op, eval(lhs), eval(rhs)),
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::parser::ASTNode;

    #[test]
    fn symbol() {
        let node = &ASTNode::Symbol(String::from("a"));
        assert_eq!(eval(node), Type::Symbol(String::from("a")));
    }

    #[test]
    fn set_with_repeated_elements() {
        let node = &ASTNode::ExtensionSet(vec![
            ASTNode::Symbol(String::from("a")),
            ASTNode::Symbol(String::from("a")),
        ]);
        assert_eq!(
            eval(node),
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

        assert_eq!(eval(node), Type::Number(1));
    }

    #[test]
    fn integer_product() {
        let node = &ASTNode::Infix(
            InfixOperator::Product,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1"))),
        );

        assert_eq!(eval(node), Type::Number(0));
    }

    #[test]
    fn symbol_comparison() {
        let node = &ASTNode::Infix(
            InfixOperator::Equality,
            Box::new(ASTNode::Symbol(String::from("a"))),
            Box::new(ASTNode::Symbol(String::from("b"))),
        );

        assert_eq!(eval(node), Type::Boolean(false));
    }

    #[test]
    fn let_expression() {
        let node = &ASTNode::Let(
            Box::new(ASTNode::Symbol(String::from("x"))),
            vec![],
            Box::new(ASTNode::Integer(String::from("0"))),
        );

        assert_eq!(eval(node), Type::Number(0));
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

        assert_eq!(eval(node), Type::Boolean(false));
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

        assert_eq!(eval(node), Type::Boolean(true));
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

        assert_eq!(eval(node), Type::Boolean(true));
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

        assert_eq!(eval(node), Type::Boolean(true));
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

        assert_eq!(eval(node), Type::Number(7));
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

        assert_eq!(eval(node), Type::Number(32));
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

        assert_eq!(eval(node), Type::Number(4));
    }
}
