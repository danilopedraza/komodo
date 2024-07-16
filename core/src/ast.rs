use crate::{cst, error::Position};
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InfixOperator {
    BitwiseAnd,
    BitwiseXor,
    Division,
    Equality,
    Exponentiation,
    Greater,
    GreaterEqual,
    In,
    LeftShift,
    Less,
    LessEqual,
    LogicAnd,
    Or,
    Rem,
    NotEquality,
    Product,
    Range,
    RightShift,
    Substraction,
    Sum,
}

impl InfixOperator {
    pub fn ident(&self) -> String {
        match self {
            InfixOperator::BitwiseAnd => "bitwise AND",
            InfixOperator::BitwiseXor => "bitwise XOR",
            InfixOperator::Division => "division",
            InfixOperator::Equality => "equality",
            InfixOperator::Exponentiation => "exponentiation",
            InfixOperator::Greater => "greater",
            InfixOperator::GreaterEqual => "greater-or-equal",
            InfixOperator::In => "membership",
            InfixOperator::LeftShift => "left shift",
            InfixOperator::Less => "less",
            InfixOperator::LessEqual => "less-or-equal",
            InfixOperator::LogicAnd => "logic AND",
            InfixOperator::Or => "OR",
            InfixOperator::Rem => "remainder",
            InfixOperator::NotEquality => "non-equality",
            InfixOperator::Product => "multiplication",
            InfixOperator::Range => "range generation",
            InfixOperator::RightShift => "right shift",
            InfixOperator::Substraction => "substraction",
            InfixOperator::Sum => "addition",
        }
        .into()
    }
}

#[allow(unused)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ASTNode {
    pub _type: ASTNodeType,
    pub position: Position,
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
    Prefix {
        op: cst::PrefixOperator,
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
    use cst::PrefixOperator;

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

    pub fn cons(first: ASTNode, tail: ASTNode, position: Position) -> ASTNode {
        let first = Box::new(first);
        let tail = Box::new(tail);
        ASTNode::new(ASTNodeType::Cons { first, tail }, position)
    }

    pub fn extension_list(list: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeType::ExtensionList { list }, position)
    }

    pub fn _for(val: &str, iter: ASTNode, proc: Vec<ASTNode>, position: Position) -> ASTNode {
        let val = val.to_string();
        let iter = Box::new(iter);
        ASTNode::new(ASTNodeType::For { val, iter, proc }, position)
    }

    pub fn infix(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        ASTNode::new(ASTNodeType::Infix { op, lhs, rhs }, position)
    }

    pub fn pos(start: usize, length: usize) -> Position {
        Position::new(start, length)
    }

    pub fn boolean(val: bool, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeType::Boolean(val), position)
    }

    pub fn comprehension_list(val: ASTNode, prop: ASTNode, position: Position) -> ASTNode {
        let val = Box::new(val);
        let prop = Box::new(prop);
        ASTNode::new(ASTNodeType::ComprehensionList { val, prop }, position)
    }

    pub fn comprehension_set(val: ASTNode, prop: ASTNode, position: Position) -> ASTNode {
        let val = Box::new(val);
        let prop = Box::new(prop);
        ASTNode::new(ASTNodeType::ComprehensionSet { val, prop }, position)
    }

    pub fn decimal(int: &str, dec: &str, position: Position) -> ASTNode {
        let int = int.to_string();
        let dec = dec.to_string();
        ASTNode::new(ASTNodeType::Decimal { int, dec }, position)
    }

    pub fn extension_set(list: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeType::ExtensionSet { list }, position)
    }

    pub fn let_(ident: ASTNode, params: Vec<ASTNode>, val: ASTNode, position: Position) -> ASTNode {
        let ident = Box::new(ident);
        let val = Box::new(val);
        ASTNode::new(ASTNodeType::Let { ident, params, val }, position)
    }

    pub fn prefix(op: PrefixOperator, val: ASTNode, position: Position) -> ASTNode {
        let val = Box::new(val);
        ASTNode::new(ASTNodeType::Prefix { op, val }, position)
    }

    pub fn signature(val: ASTNode, constraint: Option<ASTNode>, position: Position) -> ASTNode {
        let val = Box::new(val);
        let constraint = constraint.map(Box::new);
        ASTNode::new(ASTNodeType::Signature { val, constraint }, position)
    }

    pub fn tuple(values: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeType::Tuple { values }, position)
    }

    pub fn range(start: ASTNode, end: ASTNode, position: Position) -> ASTNode {
        infix(InfixOperator::Range, start, end, position)
    }

    pub fn _if(cond: ASTNode, positive: ASTNode, negative: ASTNode, position: Position) -> ASTNode {
        let cond = Box::new(cond);
        let positive = Box::new(positive);
        let negative = Box::new(negative);
        ASTNode::new(
            ASTNodeType::If {
                cond,
                positive,
                negative,
            },
            position,
        )
    }
}
