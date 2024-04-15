use crate::ast::{ASTNode, InfixOperator};

// pub enum AnalyzerError {}

#[allow(clippy::boxed_local)]
fn postprocessed_box(node: Box<ASTNode>) -> Box<ASTNode> {
    Box::new(postprocess(*node))
}

fn postprocessed_vec(vec: Vec<ASTNode>) -> Vec<ASTNode> {
    vec.iter().map(|node| postprocess(node.clone())).collect()
}

pub fn postprocess(node: ASTNode) -> ASTNode {
    match node {
        ASTNode::Infix(InfixOperator::Correspondence, params, proc) => function(*params, *proc),
        ASTNode::Infix(InfixOperator::Call, called, args) => call(*called, *args),
        ASTNode::Infix(op, lhs, rhs) => infix(op, lhs, rhs),
        ASTNode::For(ident, iter, proc) => {
            ASTNode::For(ident, postprocessed_box(iter), postprocessed_vec(proc))
        }
        ASTNode::ComprehensionList(value, prop) => {
            ASTNode::ComprehensionList(postprocessed_box(value), postprocessed_box(prop))
        }
        ASTNode::ComprehensionSet(value, prop) => {
            ASTNode::ComprehensionSet(postprocessed_box(value), postprocessed_box(prop))
        }
        ASTNode::ExtensionList(vals) => ASTNode::ExtensionList(postprocessed_vec(vals)),
        ASTNode::ExtensionSet(vals) => ASTNode::ExtensionSet(postprocessed_vec(vals)),
        ASTNode::Function(args, proc) => ASTNode::Function(args, postprocessed_vec(proc)),
        ASTNode::If(cond, first, second) => ASTNode::If(
            postprocessed_box(cond),
            postprocessed_box(first),
            postprocessed_box(second),
        ),
        ASTNode::Let(ident, params, val) => ASTNode::Let(ident, params, postprocessed_box(val)),
        ASTNode::Prefix(op, node) => ASTNode::Prefix(op, postprocessed_box(node)),
        ASTNode::Tuple(vals) => ASTNode::Tuple(postprocessed_vec(vals)),
        node => node,
    }
}

fn infix(op: InfixOperator, lhs: Box<ASTNode>, rhs: Box<ASTNode>) -> ASTNode {
    ASTNode::Infix(op, postprocessed_box(lhs), postprocessed_box(rhs))
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
        ASTNode::Tuple(tuple_params) => {
            let mut res = vec![];

            for param in tuple_params {
                match param {
                    ASTNode::Symbol(s) => res.push(s),
                    _ => todo!(),
                }
            }

            res
        }
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

    use crate::{
        ast::{ASTNode, InfixOperator},
        lexer::build_lexer,
        parser::parser_from,
    };

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

    #[test]
    fn several_params_function() {
        let node = ASTNode::Infix(
            InfixOperator::Correspondence,
            Box::new(ASTNode::Tuple(vec![
                ASTNode::Symbol(String::from("x")),
                ASTNode::Symbol(String::from("y")),
            ])),
            Box::new(ASTNode::Symbol(String::from("x"))),
        );

        assert_eq!(
            postprocess(node),
            ASTNode::Function(
                vec![String::from("x"), String::from("y"),],
                vec![ASTNode::Symbol(String::from("x"))]
            ),
        );
    }

    #[test]
    fn signature() {
        let node = ASTNode::Signature(
            Box::new(ASTNode::Symbol(String::from("f"))),
            Some(Box::new(ASTNode::Infix(
                InfixOperator::Correspondence,
                Box::new(ASTNode::Symbol(String::from("Real"))),
                Box::new(ASTNode::Symbol(String::from("Real"))),
            ))),
        );

        assert_eq!(postprocess(node.clone()), node);
    }

    #[test]
    #[ignore = "not yet implemented"]
    fn prepend() {
        let code = "[1|[2,3]]";
        let lexer = build_lexer(code).map(|res| res.unwrap());
        let node = parser_from(lexer).next().unwrap().unwrap();

        assert_eq!(
            postprocess(node),
            ASTNode::Prepend(
                Box::new(ASTNode::Integer(String::from("1"))),
                Box::new(ASTNode::ExtensionList(vec![
                    ASTNode::Integer(String::from("2")),
                    ASTNode::Integer(String::from("3")),
                ])),
            ),
        );
    }
}
