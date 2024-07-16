use crate::{
    ast::{self, ASTNode, ASTNodeType},
    cst::{CSTNode, CSTNodeType, InfixOperator, PrefixOperator},
    error::Error,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WeederError {}

type WeederResult<T> = Result<T, Error>;

#[allow(unused)]
pub fn rewrite(node: CSTNode) -> WeederResult<ASTNode> {
    let tp: ASTNodeType = match node._type {
        CSTNodeType::Boolean(bool) => boolean(bool),
        CSTNodeType::Char(chr) => char(chr),
        CSTNodeType::ComprehensionSet(val, prop) => comprehension_set(*val, *prop),
        CSTNodeType::ComprehensionList(val, prop) => comprehension_list(*val, *prop),
        CSTNodeType::ExtensionList(list) => extension_list(list),
        CSTNodeType::ExtensionSet(list) => extension_set(list),
        CSTNodeType::For(val, iter, proc) => _for(val, *iter, proc),
        CSTNodeType::If(cond, positive, negative) => _if(*cond, *positive, *negative),
        CSTNodeType::Infix(op, lhs, rhs) => infix(op, *lhs, *rhs),
        CSTNodeType::Integer(dec) => integer(dec),
        CSTNodeType::Let(ident, params, val) => _let(*ident, params, *val),
        CSTNodeType::Prefix(op, val) => prefix(op, *val),
        CSTNodeType::Cons(first, tail) => cons(*first, *tail),
        CSTNodeType::Signature(val, constraint) => signature(*val, constraint),
        CSTNodeType::String(str) => string(str),
        CSTNodeType::Symbol(name) => symbol(name),
        CSTNodeType::Tuple(values) => tuple(values),
        CSTNodeType::Wildcard => wildcard(),
    }?;

    Ok(ASTNode::new(tp, node.position))
}

fn rewrite_vec(vec: Vec<CSTNode>) -> WeederResult<Vec<ASTNode>> {
    vec.into_iter().map(rewrite).collect()
}

fn boolean(val: bool) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Boolean(val))
}

fn char(chr: char) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Char(chr))
}

fn comprehension_set(val: CSTNode, prop: CSTNode) -> WeederResult<ASTNodeType> {
    let val = Box::new(rewrite(val)?);
    let prop = Box::new(rewrite(prop)?);
    Ok(ASTNodeType::ComprehensionSet { val, prop })
}

fn comprehension_list(val: CSTNode, prop: CSTNode) -> WeederResult<ASTNodeType> {
    let val = Box::new(rewrite(val)?);
    let prop = Box::new(rewrite(prop)?);
    Ok(ASTNodeType::ComprehensionList { val, prop })
}

fn decimal(int: String, dec: String) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Decimal { int, dec })
}

fn extension_list(list: Vec<CSTNode>) -> WeederResult<ASTNodeType> {
    let list = rewrite_vec(list)?;
    Ok(ASTNodeType::ExtensionList { list })
}

fn extension_set(list: Vec<CSTNode>) -> WeederResult<ASTNodeType> {
    let list = rewrite_vec(list)?;
    Ok(ASTNodeType::ExtensionSet { list })
}

fn _for(val: String, iter: CSTNode, proc: Vec<CSTNode>) -> WeederResult<ASTNodeType> {
    let iter = Box::new(rewrite(iter)?);
    let proc = rewrite_vec(proc)?;
    Ok(ASTNodeType::For { val, iter, proc })
}

fn function(params: Vec<String>, proc: Vec<CSTNode>) -> WeederResult<ASTNodeType> {
    let proc = rewrite_vec(proc)?;
    Ok(ASTNodeType::Function { params, proc })
}

fn fraction(numer: CSTNode, denom: CSTNode) -> WeederResult<ASTNodeType> {
    let numer = Box::new(rewrite(numer)?);
    let denom = Box::new(rewrite(denom)?);
    Ok(ASTNodeType::Fraction { numer, denom })
}

fn _if(cond: CSTNode, positive: CSTNode, negative: CSTNode) -> WeederResult<ASTNodeType> {
    let cond = Box::new(rewrite(cond)?);
    let positive = Box::new(rewrite(positive)?);
    let negative = Box::new(rewrite(negative)?);
    Ok(ASTNodeType::If {
        cond,
        positive,
        negative,
    })
}

fn infix(cst_op: InfixOperator, lhs: CSTNode, rhs: CSTNode) -> WeederResult<ASTNodeType> {
    match cst_op {
        InfixOperator::BitwiseAnd => infix_node(ast::InfixOperator::BitwiseAnd, lhs, rhs),
        InfixOperator::BitwiseXor => infix_node(ast::InfixOperator::BitwiseXor, lhs, rhs),
        InfixOperator::Call => {
            let args = match rhs._type {
                CSTNodeType::Tuple(v) => v,
                _ => todo!(),
            };

            call(lhs, args)
        }
        InfixOperator::Correspondence => {
            let params = match lhs._type {
                CSTNodeType::Symbol(s) => vec![s.to_string()],
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

            let proc = match rhs._type {
                CSTNodeType::Tuple(v) => v,
                _ => vec![rhs],
            };

            function(params, proc)
        }
        InfixOperator::Division => infix_node(ast::InfixOperator::Division, lhs, rhs),
        InfixOperator::Dot => {
            let called = rewrite(rhs)?;

            match called._type {
                ASTNodeType::Call { called, args } => {
                    let mut new_args = vec![rewrite(lhs)?];
                    for arg in args {
                        new_args.push(arg);
                    }

                    Ok(ASTNodeType::Call {
                        called,
                        args: new_args,
                    })
                }
                ASTNodeType::Integer { dec } => {
                    if let Ok(ASTNode {
                        _type: ASTNodeType::Integer { dec: int },
                        position: _,
                    }) = rewrite(lhs)
                    {
                        decimal(int, dec)
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }
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
    }
}

fn infix_node(op: ast::InfixOperator, lhs: CSTNode, rhs: CSTNode) -> WeederResult<ASTNodeType> {
    let lhs = Box::new(rewrite(lhs)?);
    let rhs = Box::new(rewrite(rhs)?);
    Ok(ASTNodeType::Infix { op, lhs, rhs })
}

fn integer(dec: String) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Integer { dec })
}

fn _let(ident: CSTNode, params: Vec<CSTNode>, val: CSTNode) -> WeederResult<ASTNodeType> {
    let ident = Box::new(rewrite(ident)?);
    let params = rewrite_vec(params)?;
    let val = Box::new(rewrite(val)?);
    Ok(ASTNodeType::Let { ident, params, val })
}

fn prefix(op: PrefixOperator, val: CSTNode) -> WeederResult<ASTNodeType> {
    let val = Box::new(rewrite(val)?);
    Ok(ASTNodeType::Prefix { op, val })
}

fn cons(first: CSTNode, tail: CSTNode) -> WeederResult<ASTNodeType> {
    let first = Box::new(rewrite(first)?);
    let tail = Box::new(rewrite(tail)?);
    Ok(ASTNodeType::Cons { first, tail })
}

fn signature(val: CSTNode, constraint: Option<Box<CSTNode>>) -> WeederResult<ASTNodeType> {
    let val = Box::new(rewrite(val)?);
    let constraint = match constraint {
        None => None,
        Some(node) => Some(Box::new(rewrite(*node)?)),
    };
    Ok(ASTNodeType::Signature { val, constraint })
}

fn string(str: String) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::String { str })
}

fn symbol(name: String) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Symbol { name })
}

fn tuple(values: Vec<CSTNode>) -> WeederResult<ASTNodeType> {
    let values = rewrite_vec(values)?;
    Ok(ASTNodeType::Tuple { values })
}

fn wildcard() -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Wildcard)
}

fn call(called: CSTNode, args: Vec<CSTNode>) -> WeederResult<ASTNodeType> {
    let called = Box::new(rewrite(called)?);
    let args = rewrite_vec(args)?;
    Ok(ASTNodeType::Call { called, args })
}

#[cfg(test)]
mod tests {
    use crate::{
        ast,
        cst::{
            self,
            tests::{dummy_pos, integer},
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
                cst::symbol("x", dummy_pos()),
                cst::symbol("x", dummy_pos()),
                dummy_pos(),
            ),
            cst::tuple(vec![cst::tests::integer("1", dummy_pos())], dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            rewrite(node),
            Ok(ast::tests::call(
                ast::tests::function(
                    vec!["x"],
                    vec![ast::tests::symbol("x", dummy_pos())],
                    dummy_pos()
                ),
                vec![ast::tests::integer("1", dummy_pos())],
                dummy_pos()
            ))
        );
    }

    #[test]
    fn fraction_() {
        let node = cst::infix(
            InfixOperator::Fraction,
            cst::tests::integer("1", dummy_pos()),
            cst::tests::integer("2", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            rewrite(node),
            Ok(ast::tests::fraction(
                ast::tests::integer("1", dummy_pos()),
                ast::tests::integer("2", dummy_pos()),
                dummy_pos()
            ))
        );
    }

    #[test]
    fn oop_function_call() {
        let node = cst::infix(
            InfixOperator::Dot,
            cst::symbol("set", dummy_pos()),
            cst::infix(
                InfixOperator::Call,
                cst::symbol("map", dummy_pos()),
                cst::tuple(vec![cst::symbol("func", dummy_pos())], dummy_pos()),
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
            integer("1", dummy_pos()),
            integer("5", dummy_pos()),
            dummy_pos(),
        );

        assert_eq!(
            rewrite(node),
            Ok(ast::tests::decimal("1", "5", dummy_pos()))
        );
    }
}
