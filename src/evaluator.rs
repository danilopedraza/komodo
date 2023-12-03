use crate::parser::ASTNode;

fn evaluate(node: ASTNode) -> Result<ASTNode, ()> {
    match node {
        ASTNode::Sum(lhs, rhs) => sum(lhs, rhs),
        node => Ok(node),
    }
}

fn sum(lhs: Box<ASTNode>, rhs: Box<ASTNode>) -> Result<ASTNode, ()> {
    match (*lhs, *rhs) {
        (ASTNode::Integer(l), ASTNode::Integer(r)) => Ok(ASTNode::Integer(l + r)),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ASTNode;
    use super::evaluate;

    #[test]
    fn integer() {
        assert_eq!(evaluate(ASTNode::Integer(1)), Ok(ASTNode::Integer(1)));
    }

    #[test]
    fn sum() {
        assert_eq!(
            evaluate(
                ASTNode::Sum(
                    Box::new(ASTNode::Integer(1)),
                    Box::new(ASTNode::Integer(1))
                )
            ),
            Ok(ASTNode::Integer(2))
        );
    }
}
