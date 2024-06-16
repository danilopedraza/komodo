use crate::{
    ast::{
        ASTNode, ASTNodeType, InfixOperator, _call, _comprehension_list, _comprehension_set,
        _extension_list, _extension_set, _for, _fraction, _if_, _infix, _let_, _prefix, _tuple,
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
            _comprehension_list(postprocess(*value), postprocess(*prop), position)
        }
        ASTNodeType::ComprehensionSet(value, prop) => {
            _comprehension_set(postprocess(*value), postprocess(*prop), position)
        }
        ASTNodeType::ExtensionList(vals) => _extension_list(postprocessed_vec(vals), position),
        ASTNodeType::ExtensionSet(vals) => _extension_set(postprocessed_vec(vals), position),
        ASTNodeType::Function(args, proc) => ASTNode::new(
            ASTNodeType::Function(args, postprocessed_vec(proc)),
            position,
        ),
        ASTNodeType::If(cond, first, second) => _if_(
            postprocess(*cond),
            postprocess(*first),
            postprocess(*second),
            position,
        ),
        ASTNodeType::Let(ident, params, val) => _let_(*ident, params, postprocess(*val), position),
        ASTNodeType::Prefix(op, node) => _prefix(op, postprocess(*node), position),
        ASTNodeType::Tuple(vals) => _tuple(postprocessed_vec(vals), position),
        _ => node,
    }
}

fn fraction(numer: ASTNode, denom: ASTNode, position: Position) -> ASTNode {
    _fraction(numer, denom, position)
}

fn infix(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
    _infix(op, postprocess(lhs), postprocess(rhs), position)
}

fn call(called_node: ASTNode, args_node: ASTNode, position: Position) -> ASTNode {
    let called = postprocess(called_node);
    let args = match postprocess(args_node)._type {
        ASTNodeType::Tuple(v) => v,
        _ => todo!(),
    };

    _call(called, args, position)
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
        _signature, _symbol,
        tests::{_dummy_pos, _function, _integer},
    };

    use super::*;

    #[test]
    fn inlined_function() {
        let node = _infix(
            InfixOperator::Call,
            _infix(
                InfixOperator::Correspondence,
                _symbol("x", _dummy_pos()),
                _symbol("x", _dummy_pos()),
                _dummy_pos(),
            ),
            _tuple(vec![_integer("1", _dummy_pos())], _dummy_pos()),
            _dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            _call(
                _function(vec!["x"], vec![_symbol("x", _dummy_pos())], _dummy_pos()),
                vec![_integer("1", _dummy_pos())],
                _dummy_pos()
            )
        );
    }

    #[test]
    fn several_params_function() {
        let node = _infix(
            InfixOperator::Correspondence,
            _tuple(
                vec![_symbol("x", _dummy_pos()), _symbol("y", _dummy_pos())],
                _dummy_pos(),
            ),
            _symbol("x", _dummy_pos()),
            _dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            _function(
                vec!["x", "y",],
                vec![_symbol("x", _dummy_pos())],
                _dummy_pos()
            ),
        );
    }

    #[test]
    fn signature() {
        let node = _signature(
            _symbol("f", _dummy_pos()),
            Some(_infix(
                InfixOperator::Correspondence,
                _symbol("Real", _dummy_pos()),
                _symbol("Real", _dummy_pos()),
                _dummy_pos(),
            )),
            _dummy_pos(),
        );

        assert_eq!(postprocess(node.clone()), node);
    }

    #[test]
    fn oop_function_call() {
        let node = _infix(
            InfixOperator::Dot,
            _symbol("set", _dummy_pos()),
            _infix(
                InfixOperator::Call,
                _symbol("map", _dummy_pos()),
                _tuple(vec![_symbol("func", _dummy_pos())], _dummy_pos()),
                _dummy_pos(),
            ),
            _dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            _call(
                _symbol("map", _dummy_pos()),
                vec![_symbol("set", _dummy_pos()), _symbol("func", _dummy_pos())],
                _dummy_pos()
            ),
        );
    }

    #[test]
    fn fraction() {
        let node = _infix(
            InfixOperator::Fraction,
            _integer("1", _dummy_pos()),
            _integer("2", _dummy_pos()),
            _dummy_pos(),
        );

        assert_eq!(
            postprocess(node),
            _fraction(
                _integer("1", _dummy_pos()),
                _integer("2", _dummy_pos()),
                _dummy_pos()
            )
        );
    }
}
