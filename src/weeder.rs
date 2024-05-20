use crate::{
    ast::{
        ASTNode, ASTNodeType_, InfixOperator, _call, _comprehension_list, _comprehension_set,
        _extension_list, _extension_set, _for, _if_, _infix, _let_, _prefix, _tuple,
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
        ASTNodeType_::Infix(InfixOperator::Correspondence, params, proc) => {
            function(*params, *proc, position)
        }
        ASTNodeType_::Infix(InfixOperator::Call, called, args) => call(*called, *args, position),
        ASTNodeType_::Infix(op, lhs, rhs) => infix(op, *lhs, *rhs, position),
        ASTNodeType_::For(ident, iter, proc) => _for(
            &ident,
            postprocess(*iter),
            postprocessed_vec(proc),
            position,
        ),
        ASTNodeType_::ComprehensionList(value, prop) => {
            _comprehension_list(postprocess(*value), postprocess(*prop), position)
        }
        ASTNodeType_::ComprehensionSet(value, prop) => {
            _comprehension_set(postprocess(*value), postprocess(*prop), position)
        }
        ASTNodeType_::ExtensionList(vals) => _extension_list(postprocessed_vec(vals), position),
        ASTNodeType_::ExtensionSet(vals) => _extension_set(postprocessed_vec(vals), position),
        ASTNodeType_::Function(args, proc) => ASTNode::new(
            ASTNodeType_::Function(args, postprocessed_vec(proc)),
            position,
        ),
        ASTNodeType_::If(cond, first, second) => _if_(
            postprocess(*cond),
            postprocess(*first),
            postprocess(*second),
            position,
        ),
        ASTNodeType_::Let(ident, params, val) => _let_(*ident, params, postprocess(*val), position),
        ASTNodeType_::Prefix(op, node) => _prefix(op, postprocess(*node), position),
        ASTNodeType_::Tuple(vals) => _tuple(postprocessed_vec(vals), position),
        _ => node,
    }
}

fn infix(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
    _infix(op, postprocess(lhs), postprocess(rhs), position)
}

fn call(called_node: ASTNode, args_node: ASTNode, position: Position) -> ASTNode {
    let called = postprocess(called_node);
    let args = match postprocess(args_node)._type {
        ASTNodeType_::Tuple(v) => v,
        _ => todo!(),
    };

    _call(called, args, position)
}

fn function(params_node: ASTNode, proc_node: ASTNode, position: Position) -> ASTNode {
    let params = match params_node._type {
        ASTNodeType_::Symbol(s) => vec![s.to_owned()],
        ASTNodeType_::Tuple(tuple_params) => {
            let mut res = vec![];

            for param in tuple_params {
                match param._type {
                    ASTNodeType_::Symbol(s) => res.push(s),
                    _ => todo!(),
                }
            }

            res
        }
        _ => todo!(),
    };

    let proc = match proc_node._type {
        ASTNodeType_::Tuple(v) => v.clone(),
        _ => vec![proc_node.clone()],
    };

    ASTNode::new(ASTNodeType_::Function(params, proc), position)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::ast::{_dummy_pos, _function, _integer, _signature, _symbol};

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
}
