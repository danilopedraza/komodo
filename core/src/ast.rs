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
    Greater,
    GreaterEqual,
    In,
    LeftShift,
    Less,
    LessEqual,
    LogicAnd,
    Or,
    Mod,
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
            TokenType::Mod => Some(Self::Mod),
            TokenType::Over => Some(Self::Division),
            TokenType::Plus => Some(Self::Sum),
            TokenType::Minus => Some(Self::Substraction),
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
            Self::Greater => Precedence::Comparison,
            Self::GreaterEqual => Precedence::Comparison,
            Self::In => Precedence::In,
            Self::LeftShift => Precedence::Shift,
            Self::Less => Precedence::Comparison,
            Self::LessEqual => Precedence::Comparison,
            Self::LogicAnd => Precedence::LogicAnd,
            Self::Or => Precedence::LogicOr,
            Self::Mod => Precedence::Multiplication,
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
            InfixOperator::Greater => "greater",
            InfixOperator::GreaterEqual => "greater-or-equal",
            InfixOperator::In => "membership",
            InfixOperator::LeftShift => "left shift",
            InfixOperator::Less => "less",
            InfixOperator::LessEqual => "less-or-equal",
            InfixOperator::LogicAnd => "logic AND",
            InfixOperator::Or => "OR",
            InfixOperator::Mod => "modulo",
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ASTNodeType {
    Boolean(bool),
    Call(Box<ASTNode>, Vec<ASTNode>),
    Char(char),
    ComprehensionSet(Box<ASTNode>, Box<ASTNode>),
    ComprehensionList(Box<ASTNode>, Box<ASTNode>),
    Decimal(String, String),
    ExtensionList(Vec<ASTNode>),
    ExtensionSet(Vec<ASTNode>),
    For(String, Box<ASTNode>, Vec<ASTNode>),
    Function(Vec<String>, Vec<ASTNode>),
    If(Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
    Infix(InfixOperator, Box<ASTNode>, Box<ASTNode>),
    Integer(String),
    Let(Box<ASTNode>, Vec<ASTNode>, Box<ASTNode>),
    Prefix(PrefixOperator, Box<ASTNode>),
    Prepend(Box<ASTNode>, Box<ASTNode>),
    Signature(Box<ASTNode>, Option<Box<ASTNode>>),
    String(String),
    Symbol(String),
    Tuple(Vec<ASTNode>),
    Wildcard,
}

pub fn _pos(start: usize, length: usize) -> Position {
    Position::new(start, length)
}

pub fn _dummy_pos() -> Position {
    Position::new(0, 0)
}

pub fn _boolean(val: bool, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::Boolean(val), position)
}

pub fn _char(val: char, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::Char(val), position)
}

pub fn _string(str: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::String(str.into()), position)
}

pub fn _symbol(name: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::Symbol(name.into()), position)
}

pub fn _integer(int: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::Integer(int.into()), position)
}

pub fn _infix(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::Infix(op, Box::new(lhs), Box::new(rhs)),
        position,
    )
}

pub fn _prefix(op: PrefixOperator, val: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::Prefix(op, Box::new(val)), position)
}

pub fn _let_(ident: ASTNode, params: Vec<ASTNode>, val: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::Let(Box::new(ident), params, Box::new(val)),
        position,
    )
}

pub fn _signature(symbol: ASTNode, type_: Option<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::Signature(Box::new(symbol), type_.map(Box::new)),
        position,
    )
}

pub fn _if_(cond: ASTNode, first_res: ASTNode, second_res: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::If(Box::new(cond), Box::new(first_res), Box::new(second_res)),
        position,
    )
}

pub fn _tuple(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::Tuple(list), position)
}

pub fn _comprehension_set(val: ASTNode, prop: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::ComprehensionSet(Box::new(val), Box::new(prop)),
        position,
    )
}

pub fn _comprehension_list(val: ASTNode, prop: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::ComprehensionList(Box::new(val), Box::new(prop)),
        position,
    )
}

pub fn _prepend(first: ASTNode, most: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::Prepend(Box::new(first), Box::new(most)),
        position,
    )
}

pub fn _extension_list(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::ExtensionList(list), position)
}

pub fn _extension_set(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::ExtensionSet(list), position)
}

pub fn _for(var: &str, iter: ASTNode, block: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::For(var.into(), Box::new(iter), block),
        position,
    )
}

pub fn _function(params: Vec<&str>, proc: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::Function(params.into_iter().map(|str| str.to_owned()).collect(), proc),
        position,
    )
}

pub fn _call(called: ASTNode, args: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType::Call(Box::new(called), args), position)
}

pub fn _decimal(int: &str, dec: &str, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType::Decimal(int.to_string(), dec.to_string()),
        position,
    )
}

pub fn _range(start: ASTNode, end: ASTNode, position: Position) -> ASTNode {
    _infix(InfixOperator::Range, start, end, position)
}
