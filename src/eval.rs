use crate::parser::ASTNode;

#[derive(Debug, PartialEq, Eq)]
enum Type {
    Number(i64),
    Symbol(String),
    ExtensionSet(Vec<Type>),
}

fn remove_repeated(vec: &Vec<ASTNode>) -> Vec<&ASTNode> {
    let mut res: Vec<&ASTNode> = vec![];
    for val in vec {
        if !res.contains(&val) {
            res.push(val);
        }
    }

    return res;
}

fn sum(lhs: Type, rhs: Type) -> Type {
    match (lhs, rhs) {
        (Type::Number(l), Type::Number(r)) => Type::Number(l + r),
        _ => todo!(),
    }
}

fn eval(node: &ASTNode) -> Type {
    match node {
        ASTNode::Integer(str) => Type::Number(str.parse().unwrap()),
        ASTNode::Symbol(str) => Type::Symbol(str.clone()),
        ASTNode::ExtensionSet(lst) => Type::ExtensionSet(
            remove_repeated(lst)
            .iter()
            .map(|val| eval(*val))
            .collect()
        ),
        ASTNode::Sum(lhs, rhs) => sum(eval(lhs), eval(rhs)),
        _ => todo!(),
    }
}


#[cfg(test)]
mod tests {
    use super::{eval, Type};
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
        let node = &ASTNode::Sum(
            Box::new(ASTNode::Integer(String::from("0"))),
            Box::new(ASTNode::Integer(String::from("0")))
        );

        assert_eq!(
            eval(node),
            Type::Number(0)
        );
    }
}
