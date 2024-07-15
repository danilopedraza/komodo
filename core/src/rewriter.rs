use crate::{
    ast::{ASTNode, ASTNodeType},
    parse_node::{ParseNode, ParseNodeType},
};

// #[derive(Clone, Debug, PartialEq, Eq)]
// pub enum WeederError {}

// type WeederResult<T> = Result<T, Error>;

#[allow(unused)]
pub fn rewrite(node: ParseNode) -> ASTNode {
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

fn rewrite_vec(vec: Vec<ParseNode>) -> Vec<ASTNode> {
    vec.into_iter().map(rewrite).collect()
}

fn boolean(val: bool) -> ASTNodeType {
    ASTNodeType::Boolean(val)
}

fn char(chr: char) -> ASTNodeType {
    ASTNodeType::Char(chr)
}

fn comprehension_set(val: ParseNode, prop: ParseNode) -> ASTNodeType {
    let val = Box::new(rewrite(val));
    let prop = Box::new(rewrite(prop));
    ASTNodeType::ComprehensionSet { val, prop }
}

fn comprehension_list(val: ParseNode, prop: ParseNode) -> ASTNodeType {
    let val = Box::new(rewrite(val));
    let prop = Box::new(rewrite(prop));
    ASTNodeType::ComprehensionList { val, prop }
}

fn decimal(int: String, dec: String) -> ASTNodeType {
    ASTNodeType::Decimal { int, dec }
}

fn extension_list(list: Vec<ParseNode>) -> ASTNodeType {
    let list = rewrite_vec(list);
    ASTNodeType::ExtensionList { list }
}

fn extension_set(list: Vec<ParseNode>) -> ASTNodeType {
    let list = rewrite_vec(list);
    ASTNodeType::ExtensionSet { list }
}

fn _for(val: String, iter: ParseNode, proc: Vec<ParseNode>) -> ASTNodeType {
    let iter = Box::new(rewrite(iter));
    let proc = rewrite_vec(proc);
    ASTNodeType::For { val, iter, proc }
}

fn function(params: Vec<String>, proc: Vec<ParseNode>) -> ASTNodeType {
    let proc = rewrite_vec(proc);
    ASTNodeType::Function { params, proc }
}

fn fraction(numer: ParseNode, denom: ParseNode) -> ASTNodeType {
    let numer = Box::new(rewrite(numer));
    let denom = Box::new(rewrite(denom));
    ASTNodeType::Fraction { numer, denom }
}

fn call(called: ParseNode, args: Vec<ParseNode>) -> ASTNodeType {
    let called = Box::new(rewrite(called));
    let args = rewrite_vec(args);
    ASTNodeType::Call { called, args }
}
