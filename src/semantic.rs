use crate::ast::{ASTNode, InfixOperator};

// pub enum AnalyzerError {}

pub fn postprocess(node: ASTNode) -> ASTNode {
    match node {
        ASTNode::Infix(InfixOperator::Correspondence, params, proc) => function(*params, *proc),
        node => node,
    }
}

fn function(params_node: ASTNode, proc_node: ASTNode) -> ASTNode {
    let params = match params_node {
        ASTNode::Symbol(s) => vec![ASTNode::Symbol(s.to_string())],
        _ => todo!(),
    };

    let proc = match proc_node {
        ASTNode::Tuple(v) => v.clone(),
        node => vec![node.clone()],
    };

    ASTNode::Function(params, proc)
}
