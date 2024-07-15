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
        ParseNodeType::If(cond, positive, negative) => _if(*cond, *positive, *negative),
        ParseNodeType::Infix(_, _, _) => todo!(),
        ParseNodeType::Integer(dec) => integer(dec),
        ParseNodeType::Let(ident, params, val) => _let(*ident, params, *val),
        ParseNodeType::Prefix(_, _) => todo!(),
        ParseNodeType::Cons(first, tail) => cons(*first, *tail),
        ParseNodeType::Signature(val, constraint) => signature(*val, constraint),
        ParseNodeType::String(str) => string(str),
        ParseNodeType::Symbol(name) => symbol(name),
        ParseNodeType::Tuple(values) => tuple(values),
        ParseNodeType::Wildcard => wildcard(),
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

fn _if(cond: ParseNode, positive: ParseNode, negative: ParseNode) -> ASTNodeType {
    let cond = Box::new(rewrite(cond));
    let positive = Box::new(rewrite(positive));
    let negative = Box::new(rewrite(negative));
    ASTNodeType::If {
        cond,
        positive,
        negative,
    }
}

fn integer(dec: String) -> ASTNodeType {
    ASTNodeType::Integer { dec }
}

fn _let(ident: ParseNode, params: Vec<ParseNode>, val: ParseNode) -> ASTNodeType {
    let ident = Box::new(rewrite(ident));
    let params = rewrite_vec(params);
    let val = Box::new(rewrite(val));
    ASTNodeType::Let { ident, params, val }
}

fn cons(first: ParseNode, tail: ParseNode) -> ASTNodeType {
    let first = Box::new(rewrite(first));
    let tail = Box::new(rewrite(tail));
    ASTNodeType::Cons { first, tail }
}

fn signature(val: ParseNode, constraint: Option<Box<ParseNode>>) -> ASTNodeType {
    let val = Box::new(rewrite(val));
    let constraint = constraint.map(|node| Box::new(rewrite(*node)));
    ASTNodeType::Signature { val, constraint }
}

fn string(str: String) -> ASTNodeType {
    ASTNodeType::String { str }
}

fn symbol(name: String) -> ASTNodeType {
    ASTNodeType::Symbol { name }
}

fn tuple(values: Vec<ParseNode>) -> ASTNodeType {
    let values = rewrite_vec(values);
    ASTNodeType::Tuple { values }
}

fn wildcard() -> ASTNodeType {
    ASTNodeType::Wildcard
}

fn call(called: ParseNode, args: Vec<ParseNode>) -> ASTNodeType {
    let called = Box::new(rewrite(called));
    let args = rewrite_vec(args);
    ASTNodeType::Call { called, args }
}
