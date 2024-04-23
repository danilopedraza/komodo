use crate::{error::Position, lexer::TokenType};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    In,
    Correspondence,
    LogicOr,
    LogicAnd,
    Comparison,
    BitwiseXor,
    BitwiseAnd,
    Shift,
    Addition,
    Multiplication,
    Exponentiation,
    Call,
    Highest,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InfixOperator {
    BitwiseAnd,
    BitwiseXor,
    Call,
    Correspondence,
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
    Mod,
    NotEquality,
    Product,
    RightShift,
    Substraction,
    Sum,
}

impl InfixOperator {
    pub fn from(tok: TokenType) -> Option<Self> {
        match tok {
            TokenType::BitwiseAnd => Some(Self::BitwiseAnd),
            TokenType::BitwiseXor => Some(Self::BitwiseXor),
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
            Self::RightShift => Precedence::Shift,
            Self::Substraction => Precedence::Addition,
            Self::Sum => Precedence::Addition,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrefixOperator {
    BitwiseNot,
    LogicNot,
    Minus,
}

impl PrefixOperator {
    pub fn from(tok: TokenType) -> Option<Self> {
        match tok {
            TokenType::Bang => Some(Self::LogicNot),
            TokenType::Minus => Some(Self::Minus),
            TokenType::Tilde => Some(Self::BitwiseNot),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ASTNodeType {
    Boolean(bool),
    Call(Box<ASTNodeType>, Vec<ASTNodeType>),
    Char(char),
    ComprehensionSet(Box<ASTNodeType>, Box<ASTNodeType>),
    ComprehensionList(Box<ASTNodeType>, Box<ASTNodeType>),
    ExtensionList(Vec<ASTNodeType>),
    ExtensionSet(Vec<ASTNodeType>),
    For(String, Box<ASTNodeType>, Vec<ASTNodeType>),
    Function(Vec<String>, Vec<ASTNodeType>),
    If(Box<ASTNodeType>, Box<ASTNodeType>, Box<ASTNodeType>),
    Infix(InfixOperator, Box<ASTNodeType>, Box<ASTNodeType>),
    Integer(String),
    Let(Box<ASTNodeType>, Vec<ASTNodeType>, Box<ASTNodeType>),
    Prefix(PrefixOperator, Box<ASTNodeType>),
    Prepend(Box<ASTNodeType>, Box<ASTNodeType>),
    Signature(Box<ASTNodeType>, Option<Box<ASTNodeType>>),
    String(String),
    Symbol(String),
    Tuple(Vec<ASTNodeType>),
    Wildcard,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ASTNode {
    pub _type: ASTNodeType_,
    pub position: Position,
}

#[allow(dead_code)]
impl ASTNode {
    pub fn new(_type: ASTNodeType_, position: Position) -> Self {
        Self { _type, position }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ASTNodeType_ {
    Boolean(bool),
    Call(Box<ASTNode>, Vec<ASTNode>),
    Char(char),
    ComprehensionSet(Box<ASTNode>, Box<ASTNode>),
    ComprehensionList(Box<ASTNode>, Box<ASTNode>),
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

#[allow(dead_code)]
pub fn dummy_pos(nodetype: ASTNodeType_) -> ASTNode {
    ASTNode::new(nodetype, Position::new(0, 0))
}

pub fn _pos(start: u32, length: u32) -> Position {
    Position::new(start, length)
}

pub fn _dummy_pos() -> Position {
    Position::new(0, 0)
}

pub fn _boolean(val: bool, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Boolean(val), position)
}

pub fn _char(val: char, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Char(val), position)
}

pub fn _string(str: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::String(str.into()), position)
}

pub fn symbol(name: &str) -> ASTNodeType {
    ASTNodeType::Symbol(name.to_string())
}

pub fn _symbol(name: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Symbol(name.into()), position)
}

pub fn integer(int: &str) -> ASTNodeType {
    ASTNodeType::Integer(int.to_string())
}

pub fn _integer(int: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Integer(int.into()), position)
}

pub fn infix(op: InfixOperator, lhs: ASTNodeType, rhs: ASTNodeType) -> ASTNodeType {
    ASTNodeType::Infix(op, Box::new(lhs), Box::new(rhs))
}

pub fn _infix(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::Infix(op, Box::new(lhs), Box::new(rhs)),
        position,
    )
}

pub fn prefix(op: PrefixOperator, val: ASTNodeType) -> ASTNodeType {
    ASTNodeType::Prefix(op, Box::new(val))
}

pub fn _prefix(op: PrefixOperator, val: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Prefix(op, Box::new(val)), position)
}

pub fn let_(ident: ASTNodeType, params: Vec<ASTNodeType>, val: ASTNodeType) -> ASTNodeType {
    ASTNodeType::Let(Box::new(ident), params, Box::new(val))
}

pub fn _let_(ident: ASTNode, params: Vec<ASTNode>, val: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::Let(Box::new(ident), params, Box::new(val)),
        position,
    )
}

pub fn signature(symbol: ASTNodeType, type_: Option<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::Signature(Box::new(symbol), type_.map(Box::new))
}

pub fn _signature(symbol: ASTNode, type_: Option<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::Signature(Box::new(symbol), type_.map(Box::new)),
        position,
    )
}
