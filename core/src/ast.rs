use crate::error::Position;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InfixOperator {}

#[allow(unused)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ASTNode {
    _type: ASTNodeType,
    position: Position,
}

impl ASTNode {
    pub fn new(_type: ASTNodeType, position: Position) -> Self {
        Self { _type, position }
    }
}

impl Hash for ASTNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self._type.hash(state);
    }
}

#[allow(unused)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ASTNodeType {
    Boolean(bool),
    Call {
        called: Box<ASTNode>,
        args: Vec<ASTNode>,
    },
    Char(char),
    ComprehensionList {
        val: Box<ASTNode>,
        prop: Box<ASTNode>,
    },
    ComprehensionSet {
        val: Box<ASTNode>,
        prop: Box<ASTNode>,
    },
    Decimal {
        int: String,
        dec: String,
    },
    ExtensionList {
        list: Vec<ASTNode>,
    },
    ExtensionSet {
        list: Vec<ASTNode>,
    },
    For {
        val: String,
        iter: Box<ASTNode>,
        proc: Vec<ASTNode>,
    },
    Function {
        params: Vec<String>,
        proc: Vec<ASTNode>,
    },
    Fraction {
        numer: Box<ASTNode>,
        denom: Box<ASTNode>,
    },
    If {
        cond: Box<ASTNode>,
        positive: Box<ASTNode>,
        negative: Box<ASTNode>,
    },
    Infix {
        op: InfixOperator,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    Integer {
        dec: String,
    },
    Let {
        ident: Box<ASTNode>,
        params: Vec<ASTNode>,
        val: Box<ASTNode>,
    },
    Cons {
        first: Box<ASTNode>,
        tail: Box<ASTNode>,
    },
    Signature {
        val: Box<ASTNode>,
        constraint: Option<Box<ASTNode>>,
    },
    String {
        str: String,
    },
    Symbol {
        name: String,
    },
    Tuple {
        values: Vec<ASTNode>,
    },
    Wildcard,
}

#[cfg(test)]
pub mod tests {
    use super::*;
    pub fn call(called: ASTNode, args: Vec<ASTNode>, position: Position) -> ASTNode {
        let called = Box::new(called);
        ASTNode::new(ASTNodeType::Call { called, args }, position)
    }

    pub fn function(params: Vec<&str>, proc: Vec<ASTNode>, position: Position) -> ASTNode {
        let params = params.into_iter().map(|str| str.to_string()).collect();
        ASTNode::new(ASTNodeType::Function { params, proc }, position)
    }

    pub fn fraction(numer: ASTNode, denom: ASTNode, position: Position) -> ASTNode {
        let numer = Box::new(numer);
        let denom = Box::new(denom);
        ASTNode::new(ASTNodeType::Fraction { numer, denom }, position)
    }

    pub fn symbol(name: &str, position: Position) -> ASTNode {
        let name = name.to_string();
        ASTNode::new(ASTNodeType::Symbol { name }, position)
    }

    pub fn integer(dec: &str, position: Position) -> ASTNode {
        let dec = dec.to_string();
        ASTNode::new(ASTNodeType::Integer { dec }, position)
    }
}
