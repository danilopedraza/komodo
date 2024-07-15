use crate::{
    cst,
    cst::{
        comprehension_list, comprehension_set, extension_list, extension_set, CSTNode, CSTNodeType,
        InfixOperator, _for, _if, let_, prefix, tuple,
    },
    error::Position,
};

// pub enum AnalyzerError {}

fn postprocessed_vec(vec: Vec<CSTNode>) -> Vec<CSTNode> {
    vec.into_iter().map(postprocess).collect()
}

pub fn postprocess(node: CSTNode) -> CSTNode {
    let position = node.position;

    match node._type {
        CSTNodeType::Infix(InfixOperator::Correspondence, params, proc) => {
            function(*params, *proc, position)
        }
        CSTNodeType::Infix(InfixOperator::Call, called, args) => call(*called, *args, position),
        CSTNodeType::Infix(InfixOperator::Dot, obj, old_call) => {
            let processed_call = postprocess(*old_call);

            match processed_call._type {
                CSTNodeType::Call(called, args) => {
                    let mut new_args = vec![postprocess(*obj)];
                    for arg in args {
                        new_args.push(arg);
                    }

                    CSTNode::new(CSTNodeType::Call(called, new_args), position)
                }
                _ => todo!(),
            }
        }
        CSTNodeType::Infix(InfixOperator::Fraction, numer, denom) => {
            fraction(*numer, *denom, node.position)
        }
        CSTNodeType::Infix(op, lhs, rhs) => infix(op, *lhs, *rhs, position),
        CSTNodeType::For(ident, iter, proc) => _for(
            &ident,
            postprocess(*iter),
            postprocessed_vec(proc),
            position,
        ),
        CSTNodeType::ComprehensionList(value, prop) => {
            comprehension_list(postprocess(*value), postprocess(*prop), position)
        }
        CSTNodeType::ComprehensionSet(value, prop) => {
            comprehension_set(postprocess(*value), postprocess(*prop), position)
        }
        CSTNodeType::ExtensionList(vals) => extension_list(postprocessed_vec(vals), position),
        CSTNodeType::ExtensionSet(vals) => extension_set(postprocessed_vec(vals), position),
        CSTNodeType::Function(args, proc) => CSTNode::new(
            CSTNodeType::Function(args, postprocessed_vec(proc)),
            position,
        ),
        CSTNodeType::If(cond, first, second) => _if(
            postprocess(*cond),
            postprocess(*first),
            postprocess(*second),
            position,
        ),
        CSTNodeType::Let(ident, params, val) => let_(*ident, params, postprocess(*val), position),
        CSTNodeType::Prefix(op, node) => prefix(op, postprocess(*node), position),
        CSTNodeType::Tuple(vals) => tuple(postprocessed_vec(vals), position),
        _ => node,
    }
}

fn fraction(numer: CSTNode, denom: CSTNode, position: Position) -> CSTNode {
    cst::fraction(numer, denom, position)
}

fn infix(op: InfixOperator, lhs: CSTNode, rhs: CSTNode, position: Position) -> CSTNode {
    cst::infix(op, postprocess(lhs), postprocess(rhs), position)
}

fn call(called_node: CSTNode, args_node: CSTNode, position: Position) -> CSTNode {
    let called = postprocess(called_node);
    let args = match postprocess(args_node)._type {
        CSTNodeType::Tuple(v) => v,
        _ => todo!(),
    };

    cst::call(called, args, position)
}

fn function(params_node: CSTNode, proc_node: CSTNode, position: Position) -> CSTNode {
    let params = match params_node._type {
        CSTNodeType::Symbol(s) => vec![s.to_owned()],
        CSTNodeType::Tuple(tuple_params) => {
            let mut res = vec![];

            for param in tuple_params {
                match param._type {
                    CSTNodeType::Symbol(s) => res.push(s),
                    _ => todo!(),
                }
            }

            res
        }
        _ => todo!(),
    };

    let proc = match proc_node._type {
        CSTNodeType::Tuple(v) => postprocessed_vec(v),
        _ => vec![postprocess(proc_node)],
    };

    CSTNode::new(CSTNodeType::Function(params, proc), position)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use cst::call;

    use crate::cst::{
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
            cst::call(
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
            cst::call(
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

    #[test]
    fn function_inside_procedure() {
        let node = infix(
            InfixOperator::Correspondence,
            symbol("n", dummy_pos()),
            tuple(
                vec![
                    let_(
                        symbol("f", dummy_pos()),
                        vec![symbol("k", dummy_pos())],
                        symbol("k", dummy_pos()),
                        dummy_pos(),
                    ),
                    infix(
                        InfixOperator::Call,
                        symbol("f", dummy_pos()),
                        tuple(vec![symbol("n", dummy_pos())], dummy_pos()),
                        dummy_pos(),
                    ),
                ],
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            function(
                vec!["n"],
                vec![
                    let_(
                        symbol("f", dummy_pos()),
                        vec![symbol("k", dummy_pos())],
                        symbol("k", dummy_pos()),
                        dummy_pos(),
                    ),
                    call(
                        symbol("f", dummy_pos()),
                        vec![symbol("n", dummy_pos())],
                        dummy_pos()
                    )
                ],
                dummy_pos()
            ),
        );
    }
}
