use crate::ast::{ASTNode, InfixOperator};

// pub enum AnalyzerError {}

pub fn postprocess(node: ASTNode) -> ASTNode {
    match node {
        ASTNode::Infix(InfixOperator::Correspondence, params, proc) => function(*params, *proc),
        ASTNode::Infix(InfixOperator::Call, called, args) => call(*called, *args),
        node => node,
    }
}

fn call(called_node: ASTNode, args_node: ASTNode) -> ASTNode {
    let called = postprocess(called_node);
    let args = match postprocess(args_node) {
        ASTNode::Tuple(v) => v,
        _ => todo!(),
    };

    ASTNode::Call(Box::new(called), args)
}

fn function(params_node: ASTNode, proc_node: ASTNode) -> ASTNode {
    let params = match params_node {
        ASTNode::Symbol(s) => vec![s.to_string()],
        _ => todo!(),
    };

    let proc = match proc_node {
        ASTNode::Tuple(v) => v.clone(),
        node => vec![node.clone()],
    };

    ASTNode::Function(params, proc)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::ast::{ASTNode, InfixOperator};

    use super::*;

    #[test]
    fn inlined_function() {
        let node = ASTNode::Infix(
            InfixOperator::Call,
            Box::new(ASTNode::Infix(
                InfixOperator::Correspondence,
                Box::new(ASTNode::Symbol(String::from("x"))),
                Box::new(ASTNode::Symbol(String::from("x"))),
            )),
            Box::new(ASTNode::Tuple(vec![ASTNode::Integer(String::from("1"))])),
        );

        assert_eq!(
            postprocess(node),
            ASTNode::Call(
                Box::new(ASTNode::Function(
                    vec![String::from("x")],
                    vec![ASTNode::Symbol(String::from("x"))]
                )),
                vec![ASTNode::Integer(String::from("1"))],
            )
        );
    }
}
