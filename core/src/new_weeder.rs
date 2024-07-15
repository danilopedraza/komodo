use crate::{
    ast::{ASTNode, ASTNodeType},
    error::Error,
    parse_node::{InfixOperator, ParseNode, ParseNodeType},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WeederError {}

type WeederResult<T> = Result<T, Error>;

#[allow(unused)]
pub fn rewrite(node: ParseNode) -> WeederResult<ASTNode> {
    let tp: ASTNodeType = match node._type {
        ParseNodeType::Boolean(bool) => boolean(bool),
        ParseNodeType::Call(called, proc) => call(*called, proc),
        ParseNodeType::Char(chr) => char(chr),
        ParseNodeType::ComprehensionSet(val, prop) => comprehension_set(*val, *prop),
        ParseNodeType::ComprehensionList(val, prop) => comprehension_list(*val, *prop),
        ParseNodeType::Decimal(int, dec) => decimal(int, dec),
        ParseNodeType::ExtensionList(list) => extension_list(list),
        ParseNodeType::ExtensionSet(list) => extension_set(list),
        ParseNodeType::For(val, iter, proc) => _for(val, *iter, proc),
        ParseNodeType::Function(params, proc) => function(params, proc),
        ParseNodeType::Fraction(numer, denom) => fraction(*numer, *denom),
        ParseNodeType::If(cond, positive, negative) => _if(*cond, *positive, *negative),
        ParseNodeType::Infix(op, lhs, rhs) => infix(op, *lhs, *rhs),
        ParseNodeType::Integer(dec) => integer(dec),
        ParseNodeType::Let(ident, params, val) => _let(*ident, params, *val),
        ParseNodeType::Prefix(_, _) => todo!(),
        ParseNodeType::Cons(first, tail) => cons(*first, *tail),
        ParseNodeType::Signature(val, constraint) => signature(*val, constraint),
        ParseNodeType::String(str) => string(str),
        ParseNodeType::Symbol(name) => symbol(name),
        ParseNodeType::Tuple(values) => tuple(values),
        ParseNodeType::Wildcard => wildcard(),
    }?;

    Ok(ASTNode::new(tp, node.position))
}

fn rewrite_vec(vec: Vec<ParseNode>) -> WeederResult<Vec<ASTNode>> {
    vec.into_iter().map(rewrite).collect()
}

fn boolean(val: bool) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Boolean(val))
}

fn char(chr: char) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Char(chr))
}

fn comprehension_set(val: ParseNode, prop: ParseNode) -> WeederResult<ASTNodeType> {
    let val = Box::new(rewrite(val)?);
    let prop = Box::new(rewrite(prop)?);
    Ok(ASTNodeType::ComprehensionSet { val, prop })
}

fn comprehension_list(val: ParseNode, prop: ParseNode) -> WeederResult<ASTNodeType> {
    let val = Box::new(rewrite(val)?);
    let prop = Box::new(rewrite(prop)?);
    Ok(ASTNodeType::ComprehensionList { val, prop })
}

fn decimal(int: String, dec: String) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Decimal { int, dec })
}

fn extension_list(list: Vec<ParseNode>) -> WeederResult<ASTNodeType> {
    let list = rewrite_vec(list)?;
    Ok(ASTNodeType::ExtensionList { list })
}

fn extension_set(list: Vec<ParseNode>) -> WeederResult<ASTNodeType> {
    let list = rewrite_vec(list)?;
    Ok(ASTNodeType::ExtensionSet { list })
}

fn _for(val: String, iter: ParseNode, proc: Vec<ParseNode>) -> WeederResult<ASTNodeType> {
    let iter = Box::new(rewrite(iter)?);
    let proc = rewrite_vec(proc)?;
    Ok(ASTNodeType::For { val, iter, proc })
}

fn function(params: Vec<String>, proc: Vec<ParseNode>) -> WeederResult<ASTNodeType> {
    let proc = rewrite_vec(proc)?;
    Ok(ASTNodeType::Function { params, proc })
}

fn fraction(numer: ParseNode, denom: ParseNode) -> WeederResult<ASTNodeType> {
    let numer = Box::new(rewrite(numer)?);
    let denom = Box::new(rewrite(denom)?);
    Ok(ASTNodeType::Fraction { numer, denom })
}

fn _if(cond: ParseNode, positive: ParseNode, negative: ParseNode) -> WeederResult<ASTNodeType> {
    let cond = Box::new(rewrite(cond)?);
    let positive = Box::new(rewrite(positive)?);
    let negative = Box::new(rewrite(negative)?);
    Ok(ASTNodeType::If {
        cond,
        positive,
        negative,
    })
}

fn infix(cst_op: InfixOperator, lhs: ParseNode, rhs: ParseNode) -> WeederResult<ASTNodeType> {
    match cst_op {
        InfixOperator::BitwiseAnd => todo!(),
        InfixOperator::BitwiseXor => todo!(),
        InfixOperator::Call => {
            let args = match rhs._type {
                ParseNodeType::Tuple(v) => v,
                _ => todo!(),
            };

            call(lhs, args)
        }
        InfixOperator::Correspondence => {
            let params = match lhs._type {
                ParseNodeType::Symbol(s) => vec![s.to_string()],
                ParseNodeType::Tuple(tuple_params) => {
                    let mut res = vec![];

                    for param in tuple_params {
                        match param._type {
                            ParseNodeType::Symbol(s) => res.push(s),
                            _ => todo!(),
                        }
                    }

                    res
                }
                _ => todo!(),
            };

            let proc = match rhs._type {
                ParseNodeType::Tuple(v) => v,
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

fn _let(ident: ParseNode, params: Vec<ParseNode>, val: ParseNode) -> WeederResult<ASTNodeType> {
    let ident = Box::new(rewrite(ident)?);
    let params = rewrite_vec(params)?;
    let val = Box::new(rewrite(val)?);
    Ok(ASTNodeType::Let { ident, params, val })
}

fn cons(first: ParseNode, tail: ParseNode) -> WeederResult<ASTNodeType> {
    let first = Box::new(rewrite(first)?);
    let tail = Box::new(rewrite(tail)?);
    Ok(ASTNodeType::Cons { first, tail })
}

fn signature(val: ParseNode, constraint: Option<Box<ParseNode>>) -> WeederResult<ASTNodeType> {
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

fn tuple(values: Vec<ParseNode>) -> WeederResult<ASTNodeType> {
    let values = rewrite_vec(values)?;
    Ok(ASTNodeType::Tuple { values })
}

fn wildcard() -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Wildcard)
}

fn call(called: ParseNode, args: Vec<ParseNode>) -> WeederResult<ASTNodeType> {
    let called = Box::new(rewrite(called)?);
    let args = rewrite_vec(args)?;
    Ok(ASTNodeType::Call { called, args })
}

#[cfg(test)]
mod tests {
    use crate::{
        ast,
        parse_node::{self, tests::dummy_pos, InfixOperator},
    };

    use super::rewrite;

    #[test]
    fn inlined_function() {
        let node = parse_node::infix(
            InfixOperator::Call,
            parse_node::infix(
                InfixOperator::Correspondence,
                parse_node::symbol("x", dummy_pos()),
                parse_node::symbol("x", dummy_pos()),
                dummy_pos(),
            ),
            parse_node::tuple(
                vec![parse_node::tests::integer("1", dummy_pos())],
                dummy_pos(),
            ),
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
