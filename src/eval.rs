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
        Type::Boolean(true ) => String::from("true"),
        Type::Boolean(false) => String::from("false"),
        Type::ExtensionSet(vec) => vec.iter()
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
    match (op, lhs, rhs) {
        (InfixOperator::Equality, Type::Symbol(l), Type::Symbol(r)) => Type::Boolean(l == r),
        (InfixOperator::Product, Type::Number(l), Type::Number(r)) => Type::Number(l * r),
        (InfixOperator::Sum, Type::Number(l), Type::Number(r)) => Type::Number(l + r),
        _ => todo!(),
    }
}

pub fn eval(node: &ASTNode) -> Type {
    match node {
        ASTNode::Boolean(val) => Type::Boolean(*val),
        ASTNode::Integer(str) => Type::Number(str.parse().unwrap()),
        ASTNode::Symbol(str) => Type::Symbol(str.clone()),
        ASTNode::ExtensionSet(lst) => Type::ExtensionSet(
            remove_repeated(lst)
            .iter()
            .map(|val| eval(val))
            .collect()
        ),
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
            Type::ExtensionSet(vec![
                Type::Symbol(String::from("a")),
            ])
        );
    }

    #[test]
    fn integer_sum() {
        let node = &ASTNode::Infix(
            InfixOperator::Sum,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1")))
        );

        assert_eq!(
            eval(node),
            Type::Number(1)
        );
    }

    #[test]
    fn integer_product() {
        let node = &ASTNode::Infix(
            InfixOperator::Product,
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("1")))
        );

        assert_eq!(
            eval(node),
            Type::Number(0)
        );
    }

    #[test]
    fn symbol_comparison() {
        let node = &ASTNode::Infix(
            InfixOperator::Equality,
            Box::new(ASTNode::Symbol(String::from("a"))),
            Box::new(ASTNode::Symbol(String::from("b")))
        );

        assert_eq!(
            eval(node),
            Type::Boolean(false)
        );
    }

    #[test]
    fn let_expression() {
        let node = &ASTNode::Let(
            Box::new(ASTNode::Symbol(
                String::from("x"))),
            vec![],
            Box::new(ASTNode::Integer(String::from("0")))
        );

        assert_eq!(
            eval(node),
            Type::Number(0)
        );
    }
}
