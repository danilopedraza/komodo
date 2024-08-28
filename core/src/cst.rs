use std::hash::Hash;

use crate::{
    error::Position,
    lexer::{Radix, TokenType},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Assignment,
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
    Constraint,
    Prefix,
    Call,
    // Highest,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum InfixOperator {
    Assignment,
    BitwiseAnd,
    BitwiseXor,
    Call,
    Constraint,
    Correspondence,
    Division,
    Dot,
    Element,
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
            TokenType::Assign => Some(Self::Assignment),
            TokenType::BitwiseAnd => Some(Self::BitwiseAnd),
            TokenType::BitwiseXor => Some(Self::BitwiseXor),
            TokenType::Colon => Some(Self::Constraint),
            TokenType::Dot => Some(Self::Dot),
            TokenType::DotDot => Some(Self::Range),
            TokenType::Lparen => Some(Self::Call),
            TokenType::Greater => Some(Self::Greater),
            TokenType::GreaterEqual => Some(Self::GreaterEqual),
            TokenType::LeftShift => Some(Self::LeftShift),
            TokenType::RightShift => Some(Self::RightShift),
            TokenType::Lbrack => Some(Self::Element),
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
            Self::Assignment => Precedence::Assignment,
            Self::BitwiseAnd => Precedence::BitwiseAnd,
            Self::BitwiseXor => Precedence::BitwiseXor,
            Self::Call => Precedence::Call,
            Self::Constraint => Precedence::Constraint,
            Self::Correspondence => Precedence::Correspondence,
            Self::Division => Precedence::Multiplication,
            Self::Dot => Precedence::Dot,
            Self::Element => Precedence::Call,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    pub kind: CSTNodeKind,
    pub position: Position,
}

impl CSTNode {
    pub fn new(kind: CSTNodeKind, position: Position) -> Self {
        Self { kind, position }
    }
}

impl Hash for CSTNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ComprehensionKind {
    List,
    Set,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DeclarationKind {
    Inmutable,
    Mutable,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CSTNodeKind {
    AdInfinitum,
    Boolean(bool),
    Block(Vec<CSTNode>),
    Char(char),
    Comprehension {
        kind: ComprehensionKind,
        element: Box<CSTNode>,
        variable: String,
        iterator: Box<CSTNode>,
    },
    Dictionary {
        pairs: Vec<(CSTNode, CSTNode)>,
        complete: bool,
    },
    ExtensionList(Vec<CSTNode>),
    ExtensionSet(Vec<CSTNode>),
    For(String, Box<CSTNode>, Vec<CSTNode>),
    If(Box<CSTNode>, Box<CSTNode>, Box<CSTNode>),
    Import {
        name: String,
        alias: Option<String>,
    },
    ImportFrom {
        source: String,
        values: Vec<String>,
    },
    Infix(InfixOperator, Box<CSTNode>, Box<CSTNode>),
    Integer(String, Radix),
    Declaration(Box<CSTNode>, DeclarationKind),
    Prefix(PrefixOperator, Box<CSTNode>),
    Cons(Box<CSTNode>, Box<CSTNode>),
    SetCons {
        some: Box<CSTNode>,
        most: Box<CSTNode>,
    },
    String(String),
    Symbol(String),
    Tuple(Vec<CSTNode>),
    Wildcard,
}

pub fn infix(op: InfixOperator, lhs: CSTNode, rhs: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeKind::Infix(op, Box::new(lhs), Box::new(rhs)),
        position,
    )
}

pub fn tuple(list: Vec<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeKind::Tuple(list), position)
}

pub fn extension_list(list: Vec<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeKind::ExtensionList(list), position)
}

pub fn extension_set(list: Vec<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeKind::ExtensionSet(list), position)
}

pub fn comprehension(
    element: CSTNode,
    variable: String,
    iterator: CSTNode,
    kind: ComprehensionKind,
    position: Position,
) -> CSTNode {
    let element = Box::new(element);
    let iterator = Box::new(iterator);

    CSTNode::new(
        CSTNodeKind::Comprehension {
            element,
            variable,
            iterator,
            kind,
        },
        position,
    )
}

pub fn cons(first: CSTNode, most: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeKind::Cons(Box::new(first), Box::new(most)), position)
}

pub fn _for(var: &str, iter: CSTNode, block: Vec<CSTNode>, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeKind::For(var.into(), Box::new(iter), block),
        position,
    )
}

pub fn _if(cond: CSTNode, first_res: CSTNode, second_res: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(
        CSTNodeKind::If(Box::new(cond), Box::new(first_res), Box::new(second_res)),
        position,
    )
}

pub fn prefix(op: PrefixOperator, val: CSTNode, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeKind::Prefix(op, Box::new(val)), position)
}

pub fn dictionary(pairs: Vec<(CSTNode, CSTNode)>, complete: bool, position: Position) -> CSTNode {
    CSTNode::new(CSTNodeKind::Dictionary { pairs, complete }, position)
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
        CSTNode::new(CSTNodeKind::Boolean(val), position)
    }

    pub fn char(val: char, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeKind::Char(val), position)
    }

    pub fn string(str: &str, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeKind::String(str.into()), position)
    }

    pub fn dec_integer(int: &str, position: Position) -> CSTNode {
        integer(int, Radix::Decimal, position)
    }

    pub fn integer(int: &str, radix: Radix, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeKind::Integer(int.into(), radix), position)
    }

    pub fn ad_infinitum(position: Position) -> CSTNode {
        CSTNode::new(CSTNodeKind::AdInfinitum, position)
    }

    pub fn wildcard(position: Position) -> CSTNode {
        CSTNode::new(CSTNodeKind::Wildcard, position)
    }

    pub fn set_cons(some: CSTNode, most: CSTNode, position: Position) -> CSTNode {
        let some = Box::new(some);
        let most = Box::new(most);
        CSTNode::new(CSTNodeKind::SetCons { some, most }, position)
    }

    pub fn import(name: &str, alias: Option<&str>, position: Position) -> CSTNode {
        let name = name.to_string();
        let alias = alias.map(|s| s.to_string());
        CSTNode::new(CSTNodeKind::Import { name, alias }, position)
    }

    pub fn import_from(source: &str, values: Vec<&str>, position: Position) -> CSTNode {
        let source = source.to_string();
        let values = values.into_iter().map(|s| s.to_string()).collect();
        CSTNode::new(CSTNodeKind::ImportFrom { source, values }, position)
    }

    pub fn let_(left: CSTNode, right: Option<CSTNode>, position: Position) -> CSTNode {
        // let left = Box::new(left);
        // let right = right.map(Box::new);
        // CSTNode::new(CSTNodeKind::Let(left, right), position)

        let node = Box::new(match right {
            None => left,
            Some(right) => {
                let infix_position = Position::new(
                    left.position.start,
                    right.position.start + right.position.length - left.position.start,
                );
                infix(InfixOperator::Assignment, left, right, infix_position)
            }
        });

        CSTNode::new(
            CSTNodeKind::Declaration(node, DeclarationKind::Inmutable),
            position,
        )
    }

    pub fn pattern(
        expression: CSTNode,
        constraint: Option<CSTNode>,
        position: Position,
    ) -> CSTNode {
        match constraint {
            None => expression,
            Some(constraint) => infix(InfixOperator::Constraint, expression, constraint, position),
        }
    }

    pub fn symbol(name: &str, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeKind::Symbol(name.into()), position)
    }

    pub fn block(exprs: Vec<CSTNode>, position: Position) -> CSTNode {
        CSTNode::new(CSTNodeKind::Block(exprs), position)
    }

    pub fn var(expr: CSTNode, position: Position) -> CSTNode {
        CSTNode::new(
            CSTNodeKind::Declaration(Box::new(expr), DeclarationKind::Mutable),
            position,
        )
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
