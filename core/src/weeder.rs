use crate::{
    ast,
    ast::{
        comprehension_list, comprehension_set, extension_list, extension_set, ASTNode, ASTNodeType,
        InfixOperator, _for, _if, let_, prefix, tuple,
    },
    error::Position,
};

// pub enum AnalyzerError {}

fn postprocessed_vec(vec: Vec<ASTNode>) -> Vec<ASTNode> {
    vec.iter().map(|node| postprocess(node.clone())).collect()
}

pub fn postprocess(node: ASTNode) -> ASTNode {
    let position = node.position;

    match node._type {
        ASTNodeType::Infix(InfixOperator::Correspondence, params, proc) => {
            function(*params, *proc, position)
        }
        ASTNodeType::Infix(InfixOperator::Call, called, args) => call(*called, *args, position),
        ASTNodeType::Infix(InfixOperator::Dot, obj, old_call) => {
            let processed_call = postprocess(*old_call);

            match processed_call._type {
                ASTNodeType::Call(called, args) => {
                    let mut new_args = vec![postprocess(*obj)];
                    for arg in args {
                        new_args.push(arg);
                    }

                    ASTNode::new(ASTNodeType::Call(called, new_args), position)
                }
                _ => todo!(),
            }
        }
        ASTNodeType::Infix(InfixOperator::Fraction, numer, denom) => {
            fraction(*numer, *denom, node.position)
        }
        ASTNodeType::Infix(op, lhs, rhs) => infix(op, *lhs, *rhs, position),
        ASTNodeType::For(ident, iter, proc) => _for(
            &ident,
            postprocess(*iter),
            postprocessed_vec(proc),
            position,
        ),
        ASTNodeType::ComprehensionList(value, prop) => {
            comprehension_list(postprocess(*value), postprocess(*prop), position)
        }
        ASTNodeType::ComprehensionSet(value, prop) => {
            comprehension_set(postprocess(*value), postprocess(*prop), position)
        }
        ASTNodeType::ExtensionList(vals) => extension_list(postprocessed_vec(vals), position),
        ASTNodeType::ExtensionSet(vals) => extension_set(postprocessed_vec(vals), position),
        ASTNodeType::Function(args, proc) => ASTNode::new(
            ASTNodeType::Function(args, postprocessed_vec(proc)),
            position,
        ),
        ASTNodeType::If(cond, first, second) => _if(
            postprocess(*cond),
            postprocess(*first),
            postprocess(*second),
            position,
        ),
        ASTNodeType::Let(ident, params, val) => let_(*ident, params, postprocess(*val), position),
        ASTNodeType::Prefix(op, node) => prefix(op, postprocess(*node), position),
        ASTNodeType::Tuple(vals) => tuple(postprocessed_vec(vals), position),
        _ => node,
    }
}

fn fraction(numer: ASTNode, denom: ASTNode, position: Position) -> ASTNode {
    ast::fraction(numer, denom, position)
}

fn infix(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
    ast::infix(op, postprocess(lhs), postprocess(rhs), position)
}

fn call(called_node: ASTNode, args_node: ASTNode, position: Position) -> ASTNode {
    let called = postprocess(called_node);
    let args = match postprocess(args_node)._type {
        ASTNodeType::Tuple(v) => v,
        _ => todo!(),
    };

    ast::call(called, args, position)
}

fn function(params_node: ASTNode, proc_node: ASTNode, position: Position) -> ASTNode {
    let params = match params_node._type {
        ASTNodeType::Symbol(s) => vec![s.to_owned()],
        ASTNodeType::Tuple(tuple_params) => {
            let mut res = vec![];

            for param in tuple_params {
                match param._type {
                    ASTNodeType::Symbol(s) => res.push(s),
                    _ => todo!(),
                }
            }

            res
        }
        _ => todo!(),
    };

    let proc = match proc_node._type {
        ASTNodeType::Tuple(v) => v.clone(),
        _ => vec![proc_node.clone()],
    };

    ASTNode::new(ASTNodeType::Function(params, proc), position)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::ast::{
        signature, symbol,
        tests::{dummy_pos, function, integer},
    };

    use super::*;

    #[test]
    fn inlined_function() {
        let node = infix(
            InfixOperator::Call,
            infix(
                InfixOperator::Correspondence,
                symbol("x", dummy_pos()),
                symbol("x", dummy_pos()),
                dummy_pos(),
            ),
            tuple(vec![integer("1", dummy_pos())], dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            ast::call(
                function(vec!["x"], vec![symbol("x", dummy_pos())], dummy_pos()),
                vec![integer("1", dummy_pos())],
                dummy_pos()
            )
        );
    }

    #[test]
    fn several_params_function() {
        let node = infix(
            InfixOperator::Correspondence,
            tuple(
                vec![symbol("x", dummy_pos()), symbol("y", dummy_pos())],
                dummy_pos(),
            ),
            symbol("x", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            function(vec!["x", "y",], vec![symbol("x", dummy_pos())], dummy_pos()),
        );
    }

    #[test]
    fn signature_() {
        let node = signature(
            symbol("f", dummy_pos()),
            Some(infix(
                InfixOperator::Correspondence,
                symbol("Real", dummy_pos()),
                symbol("Real", dummy_pos()),
                dummy_pos(),
            )),
            dummy_pos(),
        );

        assert_eq!(postprocess(node.clone()), node);
    }

    #[test]
    fn oop_function_call() {
        let node = infix(
            InfixOperator::Dot,
            symbol("set", dummy_pos()),
            infix(
                InfixOperator::Call,
                symbol("map", dummy_pos()),
                tuple(vec![symbol("func", dummy_pos())], dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            ast::call(
                symbol("map", dummy_pos()),
                vec![symbol("set", dummy_pos()), symbol("func", dummy_pos())],
                dummy_pos()
            ),
        );
    }

    #[test]
    fn fraction_() {
        let node = infix(
            InfixOperator::Fraction,
            integer("1", dummy_pos()),
            integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            fraction(
                integer("1", dummy_pos()),
                integer("2", dummy_pos()),
                dummy_pos()
            )
        );
    }
}
