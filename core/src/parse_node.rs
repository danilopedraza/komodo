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

    pub fn ident(&self) -> String {
        match self {
            InfixOperator::BitwiseAnd => "bitwise AND",
            InfixOperator::BitwiseXor => "bitwise XOR",
            InfixOperator::Call => unimplemented!(),
            InfixOperator::Correspondence => unimplemented!(),
            InfixOperator::Division => "division",
            InfixOperator::Dot => "shorthand function call",
            InfixOperator::Equality => "equality",
            InfixOperator::Exponentiation => "exponentiation",
            InfixOperator::Fraction => "fraction generation",
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
pub struct ParseNode {
    pub _type: ParseNodeType,
    pub position: Position,
}

impl ParseNode {
    pub fn new(_type: ParseNodeType, position: Position) -> Self {
        Self { _type, position }
    }
}

impl Hash for ParseNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self._type.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ParseNodeType {
    Boolean(bool),
    Call(Box<ParseNode>, Vec<ParseNode>),
    Char(char),
    ComprehensionSet(Box<ParseNode>, Box<ParseNode>),
    ComprehensionList(Box<ParseNode>, Box<ParseNode>),
    Decimal(String, String),
    ExtensionList(Vec<ParseNode>),
    ExtensionSet(Vec<ParseNode>),
    For(String, Box<ParseNode>, Vec<ParseNode>),
    Function(Vec<String>, Vec<ParseNode>),
    Fraction(Box<ParseNode>, Box<ParseNode>),
    If(Box<ParseNode>, Box<ParseNode>, Box<ParseNode>),
    Infix(InfixOperator, Box<ParseNode>, Box<ParseNode>),
    Integer(String),
    Let(Box<ParseNode>, Vec<ParseNode>, Box<ParseNode>),
    Prefix(PrefixOperator, Box<ParseNode>),
    Cons(Box<ParseNode>, Box<ParseNode>),
    Signature(Box<ParseNode>, Option<Box<ParseNode>>),
    String(String),
    Symbol(String),
    Tuple(Vec<ParseNode>),
    Wildcard,
}

pub fn let_(
    ident: ParseNode,
    params: Vec<ParseNode>,
    val: ParseNode,
    position: Position,
) -> ParseNode {
    ParseNode::new(
        ParseNodeType::Let(Box::new(ident), params, Box::new(val)),
        position,
    )
}

pub fn signature(symbol: ParseNode, type_: Option<ParseNode>, position: Position) -> ParseNode {
    ParseNode::new(
        ParseNodeType::Signature(Box::new(symbol), type_.map(Box::new)),
        position,
    )
}

pub fn symbol(name: &str, position: Position) -> ParseNode {
    ParseNode::new(ParseNodeType::Symbol(name.into()), position)
}

pub fn infix(op: InfixOperator, lhs: ParseNode, rhs: ParseNode, position: Position) -> ParseNode {
    ParseNode::new(
        ParseNodeType::Infix(op, Box::new(lhs), Box::new(rhs)),
        position,
    )
}

pub fn tuple(list: Vec<ParseNode>, position: Position) -> ParseNode {
    ParseNode::new(ParseNodeType::Tuple(list), position)
}

pub fn extension_list(list: Vec<ParseNode>, position: Position) -> ParseNode {
    ParseNode::new(ParseNodeType::ExtensionList(list), position)
}

pub fn extension_set(list: Vec<ParseNode>, position: Position) -> ParseNode {
    ParseNode::new(ParseNodeType::ExtensionSet(list), position)
}

pub fn comprehension_list(val: ParseNode, prop: ParseNode, position: Position) -> ParseNode {
    ParseNode::new(
        ParseNodeType::ComprehensionList(Box::new(val), Box::new(prop)),
        position,
    )
}

pub fn cons(first: ParseNode, most: ParseNode, position: Position) -> ParseNode {
    ParseNode::new(
        ParseNodeType::Cons(Box::new(first), Box::new(most)),
        position,
    )
}

pub fn _for(var: &str, iter: ParseNode, block: Vec<ParseNode>, position: Position) -> ParseNode {
    ParseNode::new(
        ParseNodeType::For(var.into(), Box::new(iter), block),
        position,
    )
}

pub fn _if(
    cond: ParseNode,
    first_res: ParseNode,
    second_res: ParseNode,
    position: Position,
) -> ParseNode {
    ParseNode::new(
        ParseNodeType::If(Box::new(cond), Box::new(first_res), Box::new(second_res)),
        position,
    )
}

pub fn prefix(op: PrefixOperator, val: ParseNode, position: Position) -> ParseNode {
    ParseNode::new(ParseNodeType::Prefix(op, Box::new(val)), position)
}

pub fn comprehension_set(val: ParseNode, prop: ParseNode, position: Position) -> ParseNode {
    ParseNode::new(
        ParseNodeType::ComprehensionSet(Box::new(val), Box::new(prop)),
        position,
    )
}

pub fn call(called: ParseNode, args: Vec<ParseNode>, position: Position) -> ParseNode {
    ParseNode::new(ParseNodeType::Call(Box::new(called), args), position)
}

pub fn fraction(num: ParseNode, den: ParseNode, position: Position) -> ParseNode {
    ParseNode::new(
        ParseNodeType::Fraction(Box::new(num), Box::new(den)),
        position,
    )
}

#[cfg(test)]
pub mod tests {
    use super::*;
    pub fn _pos(start: usize, length: usize) -> Position {
        Position::new(start, length)
    }

    pub fn dummy_pos() -> Position {
        Position::new(0, 0)
    }

    pub fn boolean(val: bool, position: Position) -> ParseNode {
        ParseNode::new(ParseNodeType::Boolean(val), position)
    }

    pub fn char(val: char, position: Position) -> ParseNode {
        ParseNode::new(ParseNodeType::Char(val), position)
    }

    pub fn string(str: &str, position: Position) -> ParseNode {
        ParseNode::new(ParseNodeType::String(str.into()), position)
    }

    pub fn integer(int: &str, position: Position) -> ParseNode {
        ParseNode::new(ParseNodeType::Integer(int.into()), position)
    }

    pub fn function(params: Vec<&str>, proc: Vec<ParseNode>, position: Position) -> ParseNode {
        ParseNode::new(
            ParseNodeType::Function(params.into_iter().map(|str| str.to_owned()).collect(), proc),
            position,
        )
    }

    pub fn decimal(int: &str, dec: &str, position: Position) -> ParseNode {
        ParseNode::new(
            ParseNodeType::Decimal(int.to_string(), dec.to_string()),
            position,
        )
    }

    pub fn range(start: ParseNode, end: ParseNode, position: Position) -> ParseNode {
        infix(InfixOperator::Range, start, end, position)
    }
}
