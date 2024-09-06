use crate::{
    cst::{self, ComprehensionKind, DeclarationKind},
    error::Position,
    lexer::Radix,
};
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ASTNode {
    pub kind: ASTNodeKind,
    pub position: Position,
}

impl ASTNode {
    pub fn new(kind: ASTNodeKind, position: Position) -> Self {
        Self { kind, position }
    }
}

impl Hash for ASTNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl PartialOrd for ASTNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.kind.cmp(&other.kind))
    }
}

impl Ord for ASTNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.kind.cmp(&other.kind)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ASTNodeKind {
    AdInfinitum,
    Assignment {
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
    Boolean(bool),
    Block(Vec<ASTNode>),
    Call {
        called: Box<ASTNode>,
        args: Vec<ASTNode>,
    },
    Char(char),
    Comprehension {
        element: Box<ASTNode>,
        variable: String,
        iterator: Box<ASTNode>,
        kind: ComprehensionKind,
    },
    IndexNotation {
        container: Box<ASTNode>,
        index: Box<ASTNode>,
    },
    Decimal {
        int: String,
        dec: String,
    },
    Dictionary {
        pairs: Vec<(ASTNode, ASTNode)>,
        complete: bool,
    },
    List {
        list: Vec<ASTNode>,
    },
    Set {
        list: Vec<ASTNode>,
    },
    For {
        val: String,
        iter: Box<ASTNode>,
        proc: Vec<ASTNode>,
    },
    Function {
        params: Vec<String>,
        result: Box<ASTNode>,
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
    ImportFrom {
        source: String,
        values: Vec<(String, Position)>,
    },
    Infix {
        op: InfixOperator,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    Integer {
        literal: String,
        radix: Radix,
    },
    Declaration {
        left: Box<ASTNode>,
        right: Box<ASTNode>,
        kind: DeclarationKind,
    },
    Pattern {
        exp: Box<ASTNode>,
        constraint: Option<String>,
    },
    Prefix {
        op: cst::PrefixOperator,
        val: Box<ASTNode>,
    },
    Cons {
        first: Box<ASTNode>,
        tail: Box<ASTNode>,
    },
    SetCons {
        some: Box<ASTNode>,
        most: Box<ASTNode>,
    },
    String {
        str: String,
    },
    Symbol {
        name: String,
    },
    SymbolicDeclaration {
        name: String,
        constraint: String,
        kind: DeclarationKind,
    },
    Tuple {
        list: Vec<ASTNode>,
    },
    Wildcard,
}

#[cfg(test)]
pub mod tests {
    use cst::PrefixOperator;

    use super::*;
    pub fn call(called: ASTNode, args: Vec<ASTNode>, position: Position) -> ASTNode {
        let called = Box::new(called);
        ASTNode::new(ASTNodeKind::Call { called, args }, position)
    }

    pub fn function(params: Vec<&str>, result: ASTNode, position: Position) -> ASTNode {
        let params = params.into_iter().map(|str| str.to_string()).collect();
        let result = Box::new(result);
        ASTNode::new(ASTNodeKind::Function { params, result }, position)
    }

    pub fn fraction(numer: ASTNode, denom: ASTNode, position: Position) -> ASTNode {
        let numer = Box::new(numer);
        let denom = Box::new(denom);
        ASTNode::new(ASTNodeKind::Fraction { numer, denom }, position)
    }

    pub fn symbol(name: &str, position: Position) -> ASTNode {
        let name = name.to_string();
        ASTNode::new(ASTNodeKind::Symbol { name }, position)
    }

    pub fn dec_integer(dec: &str, position: Position) -> ASTNode {
        let dec = dec.to_string();
        ASTNode::new(
            ASTNodeKind::Integer {
                literal: dec,
                radix: Radix::Decimal,
            },
            position,
        )
    }

    pub fn cons(first: ASTNode, tail: ASTNode, position: Position) -> ASTNode {
        let first = Box::new(first);
        let tail = Box::new(tail);
        ASTNode::new(ASTNodeKind::Cons { first, tail }, position)
    }

    pub fn extension_list(list: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::List { list }, position)
    }

    pub fn _for(val: &str, iter: ASTNode, proc: Vec<ASTNode>, position: Position) -> ASTNode {
        let val = val.to_string();
        let iter = Box::new(iter);
        ASTNode::new(ASTNodeKind::For { val, iter, proc }, position)
    }

    pub fn infix(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        ASTNode::new(ASTNodeKind::Infix { op, lhs, rhs }, position)
    }

    pub fn pos(start: usize, length: usize) -> Position {
        Position::new(start, length)
    }

    pub fn boolean(val: bool, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::Boolean(val), position)
    }

    pub fn comprehension(
        element: ASTNode,
        variable: String,
        iterator: ASTNode,
        kind: ComprehensionKind,
        position: Position,
    ) -> ASTNode {
        let element = Box::new(element);
        let iterator = Box::new(iterator);
        ASTNode::new(
            ASTNodeKind::Comprehension {
                element,
                variable,
                iterator,
                kind,
            },
            position,
        )
    }

    pub fn decimal(int: &str, dec: &str, position: Position) -> ASTNode {
        let int = int.to_string();
        let dec = dec.to_string();
        ASTNode::new(ASTNodeKind::Decimal { int, dec }, position)
    }

    pub fn extension_set(list: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::Set { list }, position)
    }

    pub fn let_(left: ASTNode, right: ASTNode, position: Position) -> ASTNode {
        let left = Box::new(left);
        let right = Box::new(right);
        let kind = DeclarationKind::Inmutable;
        ASTNode::new(ASTNodeKind::Declaration { left, right, kind }, position)
    }

    pub fn var(left: ASTNode, right: ASTNode, position: Position) -> ASTNode {
        let left = Box::new(left);
        let right = Box::new(right);
        let kind = DeclarationKind::Mutable;
        ASTNode::new(ASTNodeKind::Declaration { left, right, kind }, position)
    }

    pub fn prefix(op: PrefixOperator, val: ASTNode, position: Position) -> ASTNode {
        let val = Box::new(val);
        ASTNode::new(ASTNodeKind::Prefix { op, val }, position)
    }

    pub fn pattern(val: ASTNode, constraint: Option<&str>, position: Position) -> ASTNode {
        let val = Box::new(val);
        let constraint = constraint.map(|str| str.to_string());
        ASTNode::new(
            ASTNodeKind::Pattern {
                exp: val,
                constraint,
            },
            position,
        )
    }

    pub fn tuple(values: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::Tuple { list: values }, position)
    }

    pub fn range(start: ASTNode, end: ASTNode, position: Position) -> ASTNode {
        infix(InfixOperator::Range, start, end, position)
    }

    pub fn _if(cond: ASTNode, positive: ASTNode, negative: ASTNode, position: Position) -> ASTNode {
        let cond = Box::new(cond);
        let positive = Box::new(positive);
        let negative = Box::new(negative);
        ASTNode::new(
            ASTNodeKind::If {
                cond,
                positive,
                negative,
            },
            position,
        )
    }

    pub fn container_element(container: ASTNode, element: ASTNode, position: Position) -> ASTNode {
        let container = Box::new(container);
        let element = Box::new(element);

        ASTNode::new(
            ASTNodeKind::IndexNotation {
                container,
                index: element,
            },
            position,
        )
    }

    pub fn dictionary(
        pairs: Vec<(ASTNode, ASTNode)>,
        complete: bool,
        position: Position,
    ) -> ASTNode {
        ASTNode::new(ASTNodeKind::Dictionary { pairs, complete }, position)
    }

    pub fn string(str: &str, position: Position) -> ASTNode {
        let str = str.to_string();
        ASTNode::new(ASTNodeKind::String { str }, position)
    }

    pub fn wildcard(position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::Wildcard, position)
    }

    pub fn ad_infinitum(position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::AdInfinitum, position)
    }

    pub fn set_cons(some: ASTNode, most: ASTNode, position: Position) -> ASTNode {
        let some = Box::new(some);
        let most = Box::new(most);

        ASTNode::new(ASTNodeKind::SetCons { some, most }, position)
    }

    pub fn block(exprs: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::Block(exprs), position)
    }

    pub fn assignment(left: ASTNode, right: ASTNode, position: Position) -> ASTNode {
        let left = Box::new(left);
        let right = Box::new(right);
        ASTNode::new(ASTNodeKind::Assignment { left, right }, position)
    }

    pub fn symbolic_let(name: &str, constraint: &str, position: Position) -> ASTNode {
        let name = name.to_string();
        let constraint = constraint.to_string();
        let kind = DeclarationKind::Inmutable;

        ASTNode::new(
            ASTNodeKind::SymbolicDeclaration {
                name,
                constraint,
                kind,
            },
            position,
        )
    }
}
