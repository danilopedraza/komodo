use crate::{
    ast::{self, ASTNode, ASTNodeKind},
    cst::{
        CSTNode, CSTNodeKind, ComprehensionKind, DeclarationKind, InfixOperator, PrefixOperator,
    },
    error::Error,
    lexer::Radix,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WeederError {}

type WeederResult<T> = Result<T, Error>;

pub fn rewrite(node: CSTNode) -> WeederResult<ASTNode> {
    let tp: ASTNodeKind = match node.kind {
        CSTNodeKind::Boolean(bool) => boolean(bool),
        CSTNodeKind::Char(chr) => char(chr),
        CSTNodeKind::ExtensionList(list) => extension_list(list),
        CSTNodeKind::ExtensionSet(list) => extension_set(list),
        CSTNodeKind::For(val, iter, proc) => _for(val, *iter, proc),
        CSTNodeKind::If(cond, positive, negative) => _if(*cond, *positive, *negative),
        CSTNodeKind::Infix(op, lhs, rhs) => infix(op, *lhs, *rhs),
        CSTNodeKind::Integer(dec, radix) => integer(dec, radix),
        CSTNodeKind::Prefix(op, val) => prefix(op, *val),
        CSTNodeKind::Cons(first, tail) => cons(*first, *tail),
        CSTNodeKind::String(str) => string(str),
        CSTNodeKind::Symbol(name) => symbol(name),
        CSTNodeKind::Tuple(values) => tuple(values),
        CSTNodeKind::Wildcard => wildcard(),
        CSTNodeKind::Dictionary { pairs, complete } => dictionary(pairs, complete),
        CSTNodeKind::AdInfinitum => ad_infinitum(),
        CSTNodeKind::SetCons { some, most } => set_cons(*some, *most),
        CSTNodeKind::Import { name: _, alias: _ } => todo!(),
        CSTNodeKind::ImportFrom { source, values } => import_from(source, values),
        CSTNodeKind::Comprehension {
            element,
            variable,
            iterator,
            kind,
        } => comprehension(*element, variable, *iterator, kind),
        CSTNodeKind::Block(exprs) => block(exprs),
        CSTNodeKind::Declaration(node, kind) => declaration(*node, kind),
    }?;

    Ok(ASTNode::new(tp, node.position))
}

fn rewrite_vec(vec: Vec<CSTNode>) -> WeederResult<Vec<ASTNode>> {
    vec.into_iter().map(rewrite).collect()
}

fn rewrite_pair((left, right): (CSTNode, CSTNode)) -> WeederResult<(ASTNode, ASTNode)> {
    let left = rewrite(left)?;
    let right = rewrite(right)?;

    Ok((left, right))
}

fn boolean(val: bool) -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::Boolean(val))
}

fn char(chr: char) -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::Char(chr))
}

fn comprehension(
    element: CSTNode,
    variable: String,
    iterator: CSTNode,
    kind: ComprehensionKind,
) -> WeederResult<ASTNodeKind> {
    let element = Box::new(rewrite(element)?);
    let iterator = Box::new(rewrite(iterator)?);

    Ok(ASTNodeKind::Comprehension {
        element,
        variable,
        iterator,
        kind,
    })
}

fn decimal(int: String, dec: String) -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::Decimal { int, dec })
}

fn extension_list(list: Vec<CSTNode>) -> WeederResult<ASTNodeKind> {
    let list = rewrite_vec(list)?;
    Ok(ASTNodeKind::List { list })
}

fn extension_set(list: Vec<CSTNode>) -> WeederResult<ASTNodeKind> {
    let list = rewrite_vec(list)?;
    Ok(ASTNodeKind::Set { list })
}

fn _for(val: String, iter: CSTNode, proc: Vec<CSTNode>) -> WeederResult<ASTNodeKind> {
    let iter = Box::new(rewrite(iter)?);
    let proc = rewrite_vec(proc)?;
    Ok(ASTNodeKind::For { val, iter, proc })
}

fn function(params: Vec<String>, result: CSTNode) -> WeederResult<ASTNodeKind> {
    let result = Box::new(rewrite(result)?);
    Ok(ASTNodeKind::Function { params, result })
}

fn fraction(numer: CSTNode, denom: CSTNode) -> WeederResult<ASTNodeKind> {
    let numer = Box::new(rewrite(numer)?);
    let denom = Box::new(rewrite(denom)?);
    Ok(ASTNodeKind::Fraction { numer, denom })
}

fn _if(cond: CSTNode, positive: CSTNode, negative: CSTNode) -> WeederResult<ASTNodeKind> {
    let cond = Box::new(rewrite(cond)?);
    let positive = Box::new(rewrite(positive)?);
    let negative = Box::new(rewrite(negative)?);
    Ok(ASTNodeKind::If {
        cond,
        positive,
        negative,
    })
}

fn infix(cst_op: InfixOperator, lhs: CSTNode, rhs: CSTNode) -> WeederResult<ASTNodeKind> {
    match cst_op {
        InfixOperator::BitwiseAnd => infix_node(ast::InfixOperator::BitwiseAnd, lhs, rhs),
        InfixOperator::BitwiseXor => infix_node(ast::InfixOperator::BitwiseXor, lhs, rhs),
        InfixOperator::Call => {
            let args = match rhs.kind {
                CSTNodeKind::Tuple(v) => v,
                _ => todo!(),
            };

            call(lhs, args)
        }
        InfixOperator::Correspondence => {
            let params = match lhs.kind {
                CSTNodeKind::Symbol(s) => vec![s.to_string()],
                CSTNodeKind::Tuple(tuple_params) => {
                    let mut res = vec![];

                    for param in tuple_params {
                        match param.kind {
                            CSTNodeKind::Symbol(s) => res.push(s),
                            _ => todo!(),
                        }
                    }

                    res
                }
                _ => todo!(),
            };

            function(params, rhs)
        }
        InfixOperator::Division => infix_node(ast::InfixOperator::Division, lhs, rhs),
        InfixOperator::Dot => match (rewrite(lhs)?, rewrite(rhs)?.kind) {
            (
                ASTNode {
                    kind:
                        ASTNodeKind::Integer {
                            literal: int,
                            radix: _,
                        },
                    position: _,
                },
                ASTNodeKind::Integer {
                    literal: dec,
                    radix: _,
                },
            ) => decimal(int, dec),
            (first_arg, ASTNodeKind::Call { called, args }) => Ok(ASTNodeKind::Call {
                called,
                args: vec![first_arg].into_iter().chain(args).collect(),
            }),
            _ => todo!(),
        },
        InfixOperator::Equality => infix_node(ast::InfixOperator::Equality, lhs, rhs),
        InfixOperator::Exponentiation => infix_node(ast::InfixOperator::Exponentiation, lhs, rhs),
        InfixOperator::Fraction => fraction(lhs, rhs),
        InfixOperator::Greater => infix_node(ast::InfixOperator::Greater, lhs, rhs),
        InfixOperator::GreaterEqual => infix_node(ast::InfixOperator::GreaterEqual, lhs, rhs),
        InfixOperator::In => infix_node(ast::InfixOperator::In, lhs, rhs),
        InfixOperator::LeftShift => infix_node(ast::InfixOperator::LeftShift, lhs, rhs),
        InfixOperator::Less => infix_node(ast::InfixOperator::Less, lhs, rhs),
        InfixOperator::LessEqual => infix_node(ast::InfixOperator::LessEqual, lhs, rhs),
        InfixOperator::LogicAnd => infix_node(ast::InfixOperator::LogicAnd, lhs, rhs),
        InfixOperator::Or => infix_node(ast::InfixOperator::Or, lhs, rhs),
        InfixOperator::Rem => infix_node(ast::InfixOperator::Rem, lhs, rhs),
        InfixOperator::NotEquality => infix_node(ast::InfixOperator::NotEquality, lhs, rhs),
        InfixOperator::Product => infix_node(ast::InfixOperator::Product, lhs, rhs),
        InfixOperator::Range => infix_node(ast::InfixOperator::Range, lhs, rhs),
        InfixOperator::RightShift => infix_node(ast::InfixOperator::RightShift, lhs, rhs),
        InfixOperator::Substraction => infix_node(ast::InfixOperator::Substraction, lhs, rhs),
        InfixOperator::Sum => infix_node(ast::InfixOperator::Sum, lhs, rhs),
        InfixOperator::Element => container_element(lhs, rhs),
        InfixOperator::Assignment => todo!(),
        InfixOperator::Constraint => pattern(lhs, rhs),
    }
}

fn container_element(container: CSTNode, element: CSTNode) -> WeederResult<ASTNodeKind> {
    let container = Box::new(rewrite(container)?);
    let element = Box::new(rewrite(element)?);
    Ok(ASTNodeKind::IndexNotation {
        container,
        index: element,
    })
}

fn infix_node(op: ast::InfixOperator, lhs: CSTNode, rhs: CSTNode) -> WeederResult<ASTNodeKind> {
    let lhs = Box::new(rewrite(lhs)?);
    let rhs = Box::new(rewrite(rhs)?);
    Ok(ASTNodeKind::Infix { op, lhs, rhs })
}

fn integer(dec: String, radix: Radix) -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::Integer {
        literal: dec,
        radix,
    })
}

fn declaration(node: CSTNode, kind: DeclarationKind) -> WeederResult<ASTNodeKind> {
    let (left, right) = match node {
        CSTNode {
            kind: CSTNodeKind::Infix(InfixOperator::Assignment, left, right),
            ..
        } => (Box::new(rewrite(*left)?), Some(Box::new(rewrite(*right)?))),
        kind => (Box::new(rewrite(kind)?), None),
    };

    Ok(ASTNodeKind::Declaration { left, right, kind })
}

fn prefix(op: PrefixOperator, val: CSTNode) -> WeederResult<ASTNodeKind> {
    let val = Box::new(rewrite(val)?);
    Ok(ASTNodeKind::Prefix { op, val })
}

fn cons(first: CSTNode, tail: CSTNode) -> WeederResult<ASTNodeKind> {
    let first = Box::new(rewrite(first)?);
    let tail = Box::new(rewrite(tail)?);
    Ok(ASTNodeKind::Cons { first, tail })
}

fn pattern(exp: CSTNode, constraint: CSTNode) -> WeederResult<ASTNodeKind> {
    let exp = Box::new(rewrite(exp)?);
    let constraint = match constraint.kind {
        CSTNodeKind::Symbol(name) => Some(name),
        _ => None,
    };
    // let constraint = constraint.map(rewrite).transpose()?.map(Box::new);
    Ok(ASTNodeKind::Pattern { exp, constraint })
}

fn block(exprs: Vec<CSTNode>) -> WeederResult<ASTNodeKind> {
    let exprs: WeederResult<Vec<ASTNode>> = exprs.into_iter().map(rewrite).collect();
    Ok(ASTNodeKind::Block(exprs?))
}

fn string(str: String) -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::String { str })
}

fn symbol(name: String) -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::Symbol { name })
}

fn tuple(values: Vec<CSTNode>) -> WeederResult<ASTNodeKind> {
    let values = rewrite_vec(values)?;
    Ok(ASTNodeKind::Tuple { list: values })
}

fn wildcard() -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::Wildcard)
}

fn call(called: CSTNode, args: Vec<CSTNode>) -> WeederResult<ASTNodeKind> {
    let called = Box::new(rewrite(called)?);
    let args = rewrite_vec(args)?;
    Ok(ASTNodeKind::Call { called, args })
}

fn dictionary(pairs: Vec<(CSTNode, CSTNode)>, complete: bool) -> WeederResult<ASTNodeKind> {
    let pairs: WeederResult<Vec<(ASTNode, ASTNode)>> =
        pairs.into_iter().map(rewrite_pair).collect();

    Ok(ASTNodeKind::Dictionary {
        pairs: pairs?,
        complete,
    })
}

fn ad_infinitum() -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::AdInfinitum)
}

fn set_cons(some: CSTNode, most: CSTNode) -> WeederResult<ASTNodeKind> {
    let some = Box::new(rewrite(some)?);
    let most = Box::new(rewrite(most)?);

    Ok(ASTNodeKind::SetCons { some, most })
}

fn import_from(source: String, values: Vec<String>) -> WeederResult<ASTNodeKind> {
    Ok(ASTNodeKind::ImportFrom { source, values })
}

#[cfg(test)]
mod tests {
    use crate::{
        ast,
        cst::{
            self,
            tests::{dec_integer, dummy_pos, symbol},
            InfixOperator,
        },
    };

    use super::rewrite;

    #[test]
    fn inlined_function() {
        let node = cst::infix(
            InfixOperator::Call,
            cst::infix(
                InfixOperator::Correspondence,
                symbol("x", dummy_pos()),
                symbol("x", dummy_pos()),
                dummy_pos(),
            ),
            cst::tuple(vec![cst::tests::dec_integer("1", dummy_pos())], dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            rewrite(node),
            Ok(ast::tests::call(
                ast::tests::function(vec!["x"], ast::tests::symbol("x", dummy_pos()), dummy_pos()),
                vec![ast::tests::dec_integer("1", dummy_pos())],
                dummy_pos()
            ))
        );
    }

    #[test]
    fn fraction_() {
        let node = cst::infix(
            InfixOperator::Fraction,
            cst::tests::dec_integer("1", dummy_pos()),
            cst::tests::dec_integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            rewrite(node),
            Ok(ast::tests::fraction(
                ast::tests::dec_integer("1", dummy_pos()),
                ast::tests::dec_integer("2", dummy_pos()),
                dummy_pos()
            ))
        );
    }

    #[test]
    fn oop_function_call() {
        let node = cst::infix(
            InfixOperator::Dot,
            symbol("set", dummy_pos()),
            cst::infix(
                InfixOperator::Call,
                symbol("map", dummy_pos()),
                cst::tuple(vec![symbol("func", dummy_pos())], dummy_pos()),
                dummy_pos(),
            ),
            dummy_pos(),
        );

        assert_eq!(
            rewrite(node),
            Ok(ast::tests::call(
                ast::tests::symbol("map", dummy_pos()),
                vec![
                    ast::tests::symbol("set", dummy_pos()),
                    ast::tests::symbol("func", dummy_pos())
                ],
                dummy_pos()
            )),
        );
    }

    #[test]
    fn decimal() {
        let node = cst::infix(
            InfixOperator::Dot,
            dec_integer("1", dummy_pos()),
            dec_integer("5", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            rewrite(node),
            Ok(ast::tests::decimal("1", "5", dummy_pos()))
        );
    }

    #[test]
    fn container_element() {
        let node = cst::infix(
            InfixOperator::Element,
            symbol("list", dummy_pos()),
            dec_integer("0", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            rewrite(node),
            Ok(ast::tests::container_element(
                ast::tests::symbol("list", dummy_pos()),
                ast::tests::dec_integer("0", dummy_pos()),
                dummy_pos()
            ))
        );
    }
}
