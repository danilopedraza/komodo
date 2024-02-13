use crate::lexer::Token;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    LogicOr,
    LogicAnd,
    Comparison,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Shift,
    Addition,
    Multiplication,
    Exponentiation,
    Correspondence,
    Highest,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InfixOperator {
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Correspondence,
    Division,
    Equality,
    Exponentiation,
    Greater,
    GreaterEqual,
    LeftShift,
    Less,
    LessEqual,
    LogicAnd,
    LogicOr,
    Mod,
    NotEquality,
    Product,
    RightShift,
    Substraction,
    Sum,
}

impl InfixOperator {
    pub fn from(tok: Token) -> Option<Self> {
        match tok {
            Token::BitwiseAnd => Some(Self::BitwiseAnd),
            Token::BitwiseOr => Some(Self::BitwiseOr),
            Token::BitwiseXor => Some(Self::BitwiseXor),
            Token::Greater => Some(Self::Greater),
            Token::GreaterEqual => Some(Self::GreaterEqual),
            Token::LeftShift => Some(Self::LeftShift),
            Token::RightShift => Some(Self::RightShift),
            Token::Less => Some(Self::Less),
            Token::LessEqual => Some(Self::LessEqual),
            Token::LogicAnd => Some(Self::LogicAnd),
            Token::LogicOr => Some(Self::LogicOr),
            Token::Mod => Some(Self::Mod),
            Token::Over => Some(Self::Division),
            Token::Plus => Some(Self::Sum),
            Token::Minus => Some(Self::Substraction),
            Token::Times => Some(Self::Product),
            Token::ToThe => Some(Self::Exponentiation),
            Token::Arrow => Some(Self::Correspondence),
            Token::Equals => Some(Self::Equality),
            Token::NotEqual => Some(Self::NotEquality),
            _ => None,
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            Self::BitwiseAnd => Precedence::BitwiseAnd,
            Self::BitwiseOr => Precedence::BitwiseOr,
            Self::BitwiseXor => Precedence::BitwiseXor,
            Self::Correspondence => Precedence::Correspondence,
            Self::Division => Precedence::Multiplication,
            Self::Equality => Precedence::Comparison,
            Self::Exponentiation => Precedence::Exponentiation,
            Self::Greater => Precedence::Comparison,
            Self::GreaterEqual => Precedence::Comparison,
            Self::LeftShift => Precedence::Shift,
            Self::Less => Precedence::Comparison,
            Self::LessEqual => Precedence::Comparison,
            Self::LogicAnd => Precedence::LogicAnd,
            Self::LogicOr => Precedence::LogicOr,
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
    pub fn from(tok: Token) -> Option<Self> {
        match tok {
            Token::Bang => Some(Self::LogicNot),
            Token::Minus => Some(Self::Minus),
            Token::Tilde => Some(Self::BitwiseNot),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ASTNode {
    Boolean(bool),
    ComprehensionSet(Box<ASTNode>, Box<ASTNode>),
    ExtensionSet(Vec<ASTNode>),
    If(Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
    Infix(InfixOperator, Box<ASTNode>, Box<ASTNode>),
    Integer(String),
    Let(Box<ASTNode>, Vec<ASTNode>, Box<ASTNode>),
    Prefix(PrefixOperator, Box<ASTNode>),
    Signature(Box<ASTNode>, Option<Box<ASTNode>>),
    Symbol(String),
    Tuple(Vec<ASTNode>),
}
