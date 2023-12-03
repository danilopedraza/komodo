use crate::parser::ASTNode;

fn evaluate(node: ASTNode) -> Result<ASTNode, ()> {
    Ok(node)
}

#[cfg(test)]
mod tests {
    use crate::parser::ASTNode;
    use super::evaluate;

    #[test]
    fn integer() {
        assert_eq!(evaluate(ASTNode::Integer(1)), Ok(ASTNode::Integer(1)));
    }
}
