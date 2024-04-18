use crate::lexer::TokenType;

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
pub enum ASTNode {
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
