use std::{ops::Index, collections::HashMap};

use crate::parser::ASTNode;

fn eval(node: ASTNode, env: HashMap<String, ASTNode>) -> Result<ASTNode, ()> {
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
    use std::collections::HashMap;

    use crate::parser::ASTNode;
    use super::eval;

    #[test]
    fn integer() {
        assert_eq!(eval(ASTNode::Integer(1), HashMap::new()), Ok(ASTNode::Integer(1)));
    }

    #[test]
    fn sum() {
        assert_eq!(
            eval(
                ASTNode::Sum(
                    Box::new(ASTNode::Integer(1)),
                    Box::new(ASTNode::Integer(1))
                ),
                HashMap::new()
            ),
            Ok(ASTNode::Integer(2))
        );
    }
}
