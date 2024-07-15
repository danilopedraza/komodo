use crate::{
    ast::{ASTNode, ASTNodeType},
    cst::{CSTNode, CSTNodeType, InfixOperator},
    error::Error,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WeederError {}

type WeederResult<T> = Result<T, Error>;

#[allow(unused)]
pub fn rewrite(node: CSTNode) -> WeederResult<ASTNode> {
    let tp: ASTNodeType = match node._type {
        CSTNodeType::Boolean(bool) => boolean(bool),
        CSTNodeType::Call(called, proc) => call(*called, proc),
        CSTNodeType::Char(chr) => char(chr),
        CSTNodeType::ComprehensionSet(val, prop) => comprehension_set(*val, *prop),
        CSTNodeType::ComprehensionList(val, prop) => comprehension_list(*val, *prop),
        CSTNodeType::Decimal(int, dec) => decimal(int, dec),
        CSTNodeType::ExtensionList(list) => extension_list(list),
        CSTNodeType::ExtensionSet(list) => extension_set(list),
        CSTNodeType::For(val, iter, proc) => _for(val, *iter, proc),
        CSTNodeType::Function(params, proc) => function(params, proc),
        CSTNodeType::Fraction(numer, denom) => fraction(*numer, *denom),
        CSTNodeType::If(cond, positive, negative) => _if(*cond, *positive, *negative),
        CSTNodeType::Infix(op, lhs, rhs) => infix(op, *lhs, *rhs),
        CSTNodeType::Integer(dec) => integer(dec),
        CSTNodeType::Let(ident, params, val) => _let(*ident, params, *val),
        CSTNodeType::Prefix(_, _) => todo!(),
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
        InfixOperator::BitwiseAnd => todo!(),
        InfixOperator::BitwiseXor => todo!(),
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
        InfixOperator::Division => todo!(),
        InfixOperator::Dot => todo!(),
        InfixOperator::Equality => todo!(),
        InfixOperator::Exponentiation => todo!(),
        InfixOperator::Fraction => todo!(),
        InfixOperator::Greater => todo!(),
        InfixOperator::GreaterEqual => todo!(),
        InfixOperator::In => todo!(),
        InfixOperator::LeftShift => todo!(),
        InfixOperator::Less => todo!(),
        InfixOperator::LessEqual => todo!(),
        InfixOperator::LogicAnd => todo!(),
        InfixOperator::Or => todo!(),
        InfixOperator::Rem => todo!(),
        InfixOperator::NotEquality => todo!(),
        InfixOperator::Product => todo!(),
        InfixOperator::Range => todo!(),
        InfixOperator::RightShift => todo!(),
        InfixOperator::Substraction => todo!(),
        InfixOperator::Sum => todo!(),
    }
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
        cst::{self, tests::dummy_pos, InfixOperator},
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
}