use std::hash::Hash;

use crate::{error::Position, lexer::TokenType};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    In,
    Correspondence,
    Range,
    LogicOr,
    LogicAnd,
    Comparison,
    BitwiseXor,
    BitwiseAnd,
    Shift,
    Addition,
    Multiplication,
    Exponentiation,
    Dot,
    Call,
    Highest,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum InfixOperator {
    BitwiseAnd,
    BitwiseXor,
    Call,
    Correspondence,
    Division,
    Dot,
    Equality,
    Exponentiation,
    Fraction,
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
    pub fn from(tok: TokenType) -> Option<Self> {
        match tok {
            TokenType::BitwiseAnd => Some(Self::BitwiseAnd),
            TokenType::BitwiseXor => Some(Self::BitwiseXor),
            TokenType::Dot => Some(Self::Dot),
            TokenType::DotDot => Some(Self::Range),
            TokenType::Lparen => Some(Self::Call),
            TokenType::Greater => Some(Self::Greater),
            TokenType::GreaterEqual => Some(Self::GreaterEqual),
            TokenType::LeftShift => Some(Self::LeftShift),
            TokenType::RightShift => Some(Self::RightShift),
            TokenType::Less => Some(Self::Less),
            TokenType::LessEqual => Some(Self::LessEqual),
            TokenType::LogicAnd => Some(Self::LogicAnd),
            TokenType::LogicOr => Some(Self::Or),
            TokenType::In => Some(Self::In),
            TokenType::Percent => Some(Self::Rem),
            TokenType::Over => Some(Self::Division),
            TokenType::Plus => Some(Self::Sum),
            TokenType::Minus => Some(Self::Substraction),
            TokenType::SlashSlash => Some(Self::Fraction),
            TokenType::Times => Some(Self::Product),
            TokenType::ToThe => Some(Self::Exponentiation),
            TokenType::Arrow => Some(Self::Correspondence),
            TokenType::Equals => Some(Self::Equality),
            TokenType::NotEqual => Some(Self::NotEquality),
            _ => None,
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            Self::BitwiseAnd => Precedence::BitwiseAnd,
            Self::BitwiseXor => Precedence::BitwiseXor,
            Self::Call => Precedence::Call,
            Self::Correspondence => Precedence::Correspondence,
            Self::Division => Precedence::Multiplication,
            Self::Dot => Precedence::Dot,
            Self::Equality => Precedence::Comparison,
            Self::Exponentiation => Precedence::Exponentiation,
            Self::Fraction => Precedence::Multiplication,
            Self::Greater => Precedence::Comparison,
            Self::GreaterEqual => Precedence::Comparison,
            Self::In => Precedence::In,
            Self::LeftShift => Precedence::Shift,
            Self::Less => Precedence::Comparison,
            Self::LessEqual => Precedence::Comparison,
            Self::LogicAnd => Precedence::LogicAnd,
            Self::Or => Precedence::LogicOr,
            Self::Rem => Precedence::Multiplication,
            Self::NotEquality => Precedence::Comparison,
            Self::Product => Precedence::Multiplication,
            Self::Range => Precedence::Range,
            Self::RightShift => Precedence::Shift,
            Self::Substraction => Precedence::Addition,
            Self::Sum => Precedence::Addition,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PrefixOperator {
    BitwiseNot,
    LogicNot,
    Minus,
}

impl PrefixOperator {
    pub fn from(tok: &TokenType) -> Option<Self> {
        match tok {
            TokenType::Bang => Some(Self::LogicNot),
            TokenType::Minus => Some(Self::Minus),
            TokenType::Tilde => Some(Self::BitwiseNot),
            _ => None,
        }
    }

    pub fn ident(&self) -> String {
        match self {
            PrefixOperator::BitwiseNot => "bitwise negation",
            PrefixOperator::LogicNot => "logic negation",
            PrefixOperator::Minus => "additive inversion",
        }
        .into()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CSTNode {
    pub _type: CSTNodeType,
    pub position: Position,
}

impl CSTNode {
    pub fn new(_type: CSTNodeType, position: Position) -> Self {
        Self { _type, position }
    }
}

impl Hash for CSTNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self._type.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CSTNodeType {
    Boolean(bool),
    Char(char),
    ComprehensionSet(Box<CSTNode>, Box<CSTNode>),
    ComprehensionList(Box<CSTNode>, Box<CSTNode>),
    ExtensionList(Vec<CSTNode>),
    ExtensionSet(Vec<CSTNode>),
    For(String, Box<CSTNode>, Vec<CSTNode>),
    If(Box<CSTNode>, Box<CSTNode>, Box<CSTNode>),
    Infix(InfixOperator, Box<CSTNode>, Box<CSTNode>),
    Integer(String),
    Let(Box<CSTNode>, Vec<CSTNode>, Box<CSTNode>),
    Prefix(PrefixOperator, Box<CSTNode>),
    Cons(Box<CSTNode>, Box<CSTNode>),
    Signature(Box<CSTNode>, Option<Box<CSTNode>>),
    String(String),
    Symbol(String),
    Tuple(Vec<CSTNode>),
    Wildcard,
}

pub fn let_(ident: CSTNode, params: Vec<CSTNode>, val: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeType::Let(Box::new(ident), params, Box::new(val)),
        position,
    )
}

pub fn signature(symbol: CSTNode, type_: Option<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeType::Signature(Box::new(symbol), type_.map(Box::new)),
        position,
    )
}

pub fn symbol(name: &str, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeType::Symbol(name.into()), position)
}

pub fn infix(op: InfixOperator, lhs: CSTNode, rhs: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeType::Infix(op, Box::new(lhs), Box::new(rhs)),
        position,
    )
}

pub fn tuple(list: Vec<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeType::Tuple(list), position)
}

pub fn extension_list(list: Vec<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeType::ExtensionList(list), position)
}

pub fn extension_set(list: Vec<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeType::ExtensionSet(list), position)
}

pub fn comprehension_list(val: CSTNode, prop: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeType::ComprehensionList(Box::new(val), Box::new(prop)),
        position,
    )
}

pub fn cons(first: CSTNode, most: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeType::Cons(Box::new(first), Box::new(most)), position)
}

pub fn _for(var: &str, iter: CSTNode, block: Vec<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeType::For(var.into(), Box::new(iter), block),
        position,
    )
}

pub fn _if(cond: CSTNode, first_res: CSTNode, second_res: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeType::If(Box::new(cond), Box::new(first_res), Box::new(second_res)),
        position,
    )
}

pub fn prefix(op: PrefixOperator, val: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeType::Prefix(op, Box::new(val)), position)
}

pub fn comprehension_set(val: CSTNode, prop: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeType::ComprehensionSet(Box::new(val), Box::new(prop)),
        position,
    )
}

// pub fn call(called: CSTNode, args: Vec<CSTNode>, position: Position) -> CSTNode {
//     CSTNode::new(CSTNodeType::Call(Box::new(called), args), position)
// }

// pub fn fraction(num: CSTNode, den: CSTNode, position: Position) -> CSTNode {
//     CSTNode::new(
//         CSTNodeType::Fraction(Box::new(num), Box::new(den)),
//         position,
//     )
// }

#[cfg(test)]
pub mod tests {
    use super::*;
    pub fn _pos(start: usize, length: usize) -> Position {
        Position::new(start, length)
    }

    pub fn dummy_pos() -> Position {
        Position::new(0, 0)
    }

    pub fn boolean(val: bool, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeType::Boolean(val), position)
    }

    pub fn char(val: char, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeType::Char(val), position)
    }

    pub fn string(str: &str, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeType::String(str.into()), position)
    }

    pub fn integer(int: &str, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeType::Integer(int.into()), position)
    }

    // pub fn function(params: Vec<&str>, proc: Vec<CSTNode>, position: Position) -> CSTNode {
    //     CSTNode::new(
    //         CSTNodeType::Function(params.into_iter().map(|str| str.to_owned()).collect(), proc),
    //         position,
    //     )
    // }

    // pub fn decimal(int: &str, dec: &str, position: Position) -> CSTNode {
    //     CSTNode::new(
    //         CSTNodeType::Decimal(int.to_string(), dec.to_string()),
    //         position,
    //     )
    // }

    // pub fn range(start: CSTNode, end: CSTNode, position: Position) -> CSTNode {
    //     infix(InfixOperator::Range, start, end, position)
    // }
}
