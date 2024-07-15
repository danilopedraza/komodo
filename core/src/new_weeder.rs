use crate::{
    ast::{ASTNode, ASTNodeType},
    error::Error,
    parse_node::{ParseNode, ParseNodeType},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WeederError {}

type WeederResult<T> = Result<T, Error>;

#[allow(unused)]
pub fn postprocess(node: ParseNode) -> WeederResult<ASTNode> {
    let tp: WeederResult<ASTNodeType> = match node._type {
        ParseNodeType::Boolean(bool) => boolean(bool),
        ParseNodeType::Call(called, proc) => call(*called, proc),
        ParseNodeType::Char(_) => todo!(),
        ParseNodeType::ComprehensionSet(_, _) => todo!(),
        ParseNodeType::ComprehensionList(_, _) => todo!(),
        ParseNodeType::Decimal(_, _) => todo!(),
        ParseNodeType::ExtensionList(_) => todo!(),
        ParseNodeType::ExtensionSet(_) => todo!(),
        ParseNodeType::For(_, _, _) => todo!(),
        ParseNodeType::Function(_, _) => todo!(),
        ParseNodeType::Fraction(_, _) => todo!(),
        ParseNodeType::If(_, _, _) => todo!(),
        ParseNodeType::Infix(_, _, _) => todo!(),
        ParseNodeType::Integer(_) => todo!(),
        ParseNodeType::Let(_, _, _) => todo!(),
        ParseNodeType::Prefix(_, _) => todo!(),
        ParseNodeType::Cons(_, _) => todo!(),
        ParseNodeType::Signature(_, _) => todo!(),
        ParseNodeType::String(_) => todo!(),
        ParseNodeType::Symbol(_) => todo!(),
        ParseNodeType::Tuple(_) => todo!(),
        ParseNodeType::Wildcard => todo!(),
    };

    Ok(ASTNode::new(tp?, node.position))
}

fn postprocess_vec(vec: Vec<ParseNode>) -> WeederResult<Vec<ASTNode>> {
    vec.into_iter().map(postprocess).collect()
}

fn boolean(val: bool) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Boolean(val))
}

fn call(called: ParseNode, proc: Vec<ParseNode>) -> WeederResult<ASTNodeType> {
    Ok(ASTNodeType::Call(
        Box::new(postprocess(called)?),
        postprocess_vec(proc)?,
    ))
}
