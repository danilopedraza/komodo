use crate::{
    ast::{ASTNode, ASTNodeType},
    parse_node::{ParseNode, ParseNodeType},
};

// #[derive(Clone, Debug, PartialEq, Eq)]
// pub enum WeederError {}

// type WeederResult<T> = Result<T, Error>;

#[allow(unused)]
pub fn postprocess(node: ParseNode) -> ASTNode {
    let tp: ASTNodeType = match node._type {
        ParseNodeType::Boolean(bool) => boolean(bool),
        ParseNodeType::Call(called, proc) => call(*called, proc),
        ParseNodeType::Char(chr) => char(chr),
        ParseNodeType::ComprehensionSet(val, prop) => comprehension_set(*val, *prop),
        ParseNodeType::ComprehensionList(val, prop) => comprehension_list(*val, *prop),
        ParseNodeType::Decimal(int, dec) => decimal(int, dec),
        ParseNodeType::ExtensionList(list) => extension_list(list),
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

    ASTNode::new(tp, node.position)
}

fn postprocess_vec(vec: Vec<ParseNode>) -> Vec<ASTNode> {
    vec.into_iter().map(postprocess).collect()
}

fn boolean(val: bool) -> ASTNodeType {
    ASTNodeType::Boolean(val)
}

fn char(chr: char) -> ASTNodeType {
    ASTNodeType::Char(chr)
}

fn comprehension_set(val: ParseNode, prop: ParseNode) -> ASTNodeType {
    let val = Box::new(postprocess(val));
    let prop = Box::new(postprocess(prop));
    ASTNodeType::ComprehensionSet { val, prop }
}

fn comprehension_list(val: ParseNode, prop: ParseNode) -> ASTNodeType {
    let val = Box::new(postprocess(val));
    let prop = Box::new(postprocess(prop));
    ASTNodeType::ComprehensionList { val, prop }
}

fn decimal(int: String, dec: String) -> ASTNodeType {
    ASTNodeType::Decimal { int, dec }
}

fn extension_list(list: Vec<ParseNode>) -> ASTNodeType {
    let list = postprocess_vec(list);
    ASTNodeType::ExtensionList { list }
}

fn call(called: ParseNode, args: Vec<ParseNode>) -> ASTNodeType {
    let called = Box::new(postprocess(called));
    let args = postprocess_vec(args);
    ASTNodeType::Call { called, args }
}
