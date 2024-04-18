use crate::ast::{ASTNodeType, InfixOperator};

// pub enum AnalyzerError {}

#[allow(clippy::boxed_local)]
fn postprocessed_box(node: Box<ASTNodeType>) -> Box<ASTNodeType> {
    Box::new(postprocess(*node))
}

fn postprocessed_vec(vec: Vec<ASTNodeType>) -> Vec<ASTNodeType> {
    vec.iter().map(|node| postprocess(node.clone())).collect()
}

pub fn postprocess(node: ASTNodeType) -> ASTNodeType {
    match node {
        ASTNodeType::Infix(InfixOperator::Correspondence, params, proc) => function(*params, *proc),
        ASTNodeType::Infix(InfixOperator::Call, called, args) => call(*called, *args),
        ASTNodeType::Infix(op, lhs, rhs) => infix(op, lhs, rhs),
        ASTNodeType::For(ident, iter, proc) => {
            ASTNodeType::For(ident, postprocessed_box(iter), postprocessed_vec(proc))
        }
        ASTNodeType::ComprehensionList(value, prop) => {
            ASTNodeType::ComprehensionList(postprocessed_box(value), postprocessed_box(prop))
        }
        ASTNodeType::ComprehensionSet(value, prop) => {
            ASTNodeType::ComprehensionSet(postprocessed_box(value), postprocessed_box(prop))
        }
        ASTNodeType::ExtensionList(vals) => ASTNodeType::ExtensionList(postprocessed_vec(vals)),
        ASTNodeType::ExtensionSet(vals) => ASTNodeType::ExtensionSet(postprocessed_vec(vals)),
        ASTNodeType::Function(args, proc) => ASTNodeType::Function(args, postprocessed_vec(proc)),
        ASTNodeType::If(cond, first, second) => ASTNodeType::If(
            postprocessed_box(cond),
            postprocessed_box(first),
            postprocessed_box(second),
        ),
        ASTNodeType::Let(ident, params, val) => {
            ASTNodeType::Let(ident, params, postprocessed_box(val))
        }
        ASTNodeType::Prefix(op, node) => ASTNodeType::Prefix(op, postprocessed_box(node)),
        ASTNodeType::Tuple(vals) => ASTNodeType::Tuple(postprocessed_vec(vals)),
        node => node,
    }
}

fn infix(op: InfixOperator, lhs: Box<ASTNodeType>, rhs: Box<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::Infix(op, postprocessed_box(lhs), postprocessed_box(rhs))
}

fn call(called_node: ASTNodeType, args_node: ASTNodeType) -> ASTNodeType {
    let called = postprocess(called_node);
    let args = match postprocess(args_node) {
        ASTNodeType::Tuple(v) => v,
        _ => todo!(),
    };

    ASTNodeType::Call(Box::new(called), args)
}

fn function(params_node: ASTNodeType, proc_node: ASTNodeType) -> ASTNodeType {
    let params = match params_node {
        ASTNodeType::Symbol(s) => vec![s.to_string()],
        ASTNodeType::Tuple(tuple_params) => {
            let mut res = vec![];

            for param in tuple_params {
                match param {
                    ASTNodeType::Symbol(s) => res.push(s),
                    _ => todo!(),
                }
            }

            res
        }
        _ => todo!(),
    };

    let proc = match proc_node {
        ASTNodeType::Tuple(v) => v.clone(),
        node => vec![node.clone()],
    };

    ASTNodeType::Function(params, proc)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn inlined_function() {
        let node = ASTNodeType::Infix(
            InfixOperator::Call,
            Box::new(ASTNodeType::Infix(
                InfixOperator::Correspondence,
                Box::new(ASTNodeType::Symbol(String::from("x"))),
                Box::new(ASTNodeType::Symbol(String::from("x"))),
            )),
            Box::new(ASTNodeType::Tuple(vec![ASTNodeType::Integer(
                String::from("1"),
            )])),
        );

        assert_eq!(
            postprocess(node),
            ASTNodeType::Call(
                Box::new(ASTNodeType::Function(
                    vec![String::from("x")],
                    vec![ASTNodeType::Symbol(String::from("x"))]
                )),
                vec![ASTNodeType::Integer(String::from("1"))],
            )
        );
    }

    #[test]
    fn several_params_function() {
        let node = ASTNodeType::Infix(
            InfixOperator::Correspondence,
            Box::new(ASTNodeType::Tuple(vec![
                ASTNodeType::Symbol(String::from("x")),
                ASTNodeType::Symbol(String::from("y")),
            ])),
            Box::new(ASTNodeType::Symbol(String::from("x"))),
        );

        assert_eq!(
            postprocess(node),
            ASTNodeType::Function(
                vec![String::from("x"), String::from("y"),],
                vec![ASTNodeType::Symbol(String::from("x"))]
            ),
        );
    }

    #[test]
    fn signature() {
        let node = ASTNodeType::Signature(
            Box::new(ASTNodeType::Symbol(String::from("f"))),
            Some(Box::new(ASTNodeType::Infix(
                InfixOperator::Correspondence,
                Box::new(ASTNodeType::Symbol(String::from("Real"))),
                Box::new(ASTNodeType::Symbol(String::from("Real"))),
            ))),
        );

        assert_eq!(postprocess(node.clone()), node);
    }
}
