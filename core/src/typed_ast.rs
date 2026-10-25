//! Executable syntax emitted by the typechecker.
//!
//! Every expression carries the inferred type. `Unknown` preserves the checker's
//! gradual typing behavior, including expressions it does not yet inspect.
pub use crate::typecheck::Type;
use crate::{
    ast::{self, InfixOperator, Pattern},
    cst::{self, ComprehensionKind},
    error::Position,
    lexer::Radix,
    run::ModuleAddress,
};
use std::{
    cmp::Ordering,
    collections::HashMap,
    hash::{Hash, Hasher},
};

#[derive(Clone, Debug)]
pub struct ASTNode {
    pub(crate) kind: ASTNodeKind,
    pub(crate) position: Position,
    pub(crate) typ: Type,
}

impl ASTNode {
    pub fn typ(&self) -> &Type {
        &self.typ
    }
    pub fn position(&self) -> Position {
        self.position
    }
}

// Type annotations do not change function identity or ordering at runtime.
impl PartialEq for ASTNode {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.position == other.position
    }
}
impl Eq for ASTNode {}
impl Hash for ASTNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}
impl PartialOrd for ASTNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for ASTNode {
    fn cmp(&self, other: &Self) -> Ordering {
        self.kind.cmp(&other.kind)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum ASTNodeKind {
    Assignment {
        left: AssignableSymbol,
        right: Box<ASTNode>,
    },
    Boolean(bool),
    Block(Vec<ASTNode>),
    Call {
        called: Box<ASTNode>,
        arg: Box<ASTNode>,
    },
    Case {
        expr: Box<ASTNode>,
        pairs: Vec<(Pattern, ASTNode)>,
    },
    Char(char),
    Comprehension {
        element: Box<ASTNode>,
        variable: String,
        iterator: Box<ASTNode>,
        kind: ComprehensionKind,
    },
    DotNotation {
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
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
        val: Pattern,
        iter: Box<ASTNode>,
        proc: Box<ASTNode>,
    },
    Function {
        param: Pattern,
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
        source: ModuleAddress,
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
    Declaration(Declaration),
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
    Tuple {
        list: Vec<ASTNode>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum Declaration {
    Mutable {
        left: Pattern,
        right: Box<ASTNode>,
    },
    Inmutable {
        left: Pattern,
        right: Box<ASTNode>,
    },
    Symbolic {
        name: String,
        constraint: String,
    },
    Function {
        name: String,
        param: Pattern,
        result: Box<ASTNode>,
    },
    MemoizedFunction {
        name: String,
        param: Pattern,
        result: Box<ASTNode>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum AssignableSymbol {
    Pattern(Pattern),
    ObjectValue {
        object: String,
        value: String,
    },
    IndexableValue {
        container: String,
        index: Box<ASTNode>,
    },
}

fn lower_kind(value: &ast::ASTNodeKind, types: &HashMap<usize, Type>) -> ASTNodeKind {
    match value {
        ast::ASTNodeKind::Assignment { left, right } => ASTNodeKind::Assignment {
            left: lower_assignable_symbol(left, types),
            right: Box::new(lower(right, types)),
        },
        ast::ASTNodeKind::Boolean(value) => ASTNodeKind::Boolean(*value),
        ast::ASTNodeKind::Block(value) => {
            ASTNodeKind::Block(value.iter().map(|node| lower(node, types)).collect())
        }
        ast::ASTNodeKind::Call { called, arg } => ASTNodeKind::Call {
            called: Box::new(lower(called, types)),
            arg: Box::new(lower(arg, types)),
        },
        ast::ASTNodeKind::Case { expr, pairs } => ASTNodeKind::Case {
            expr: Box::new(lower(expr, types)),
            pairs: pairs
                .iter()
                .map(|(pattern, node)| (pattern.clone(), lower(node, types)))
                .collect(),
        },
        ast::ASTNodeKind::Char(value) => ASTNodeKind::Char(*value),
        ast::ASTNodeKind::Comprehension {
            element,
            variable,
            iterator,
            kind,
        } => ASTNodeKind::Comprehension {
            element: Box::new(lower(element, types)),
            variable: variable.clone(),
            iterator: Box::new(lower(iterator, types)),
            kind: *kind,
        },
        ast::ASTNodeKind::DotNotation { lhs, rhs } => ASTNodeKind::DotNotation {
            lhs: Box::new(lower(lhs, types)),
            rhs: Box::new(lower(rhs, types)),
        },
        ast::ASTNodeKind::IndexNotation { container, index } => ASTNodeKind::IndexNotation {
            container: Box::new(lower(container, types)),
            index: Box::new(lower(index, types)),
        },
        ast::ASTNodeKind::Decimal { int, dec } => ASTNodeKind::Decimal {
            int: int.clone(),
            dec: dec.clone(),
        },
        ast::ASTNodeKind::Dictionary { pairs, complete } => ASTNodeKind::Dictionary {
            pairs: pairs
                .iter()
                .map(|(key, value)| (lower(key, types), lower(value, types)))
                .collect(),
            complete: *complete,
        },
        ast::ASTNodeKind::List { list } => ASTNodeKind::List {
            list: list.iter().map(|node| lower(node, types)).collect(),
        },
        ast::ASTNodeKind::Set { list } => ASTNodeKind::Set {
            list: list.iter().map(|node| lower(node, types)).collect(),
        },
        ast::ASTNodeKind::For { val, iter, proc } => ASTNodeKind::For {
            val: val.clone(),
            iter: Box::new(lower(iter, types)),
            proc: Box::new(lower(proc, types)),
        },
        ast::ASTNodeKind::Function { param, result } => ASTNodeKind::Function {
            param: param.clone(),
            result: Box::new(lower(result, types)),
        },
        ast::ASTNodeKind::Fraction { numer, denom } => ASTNodeKind::Fraction {
            numer: Box::new(lower(numer, types)),
            denom: Box::new(lower(denom, types)),
        },
        ast::ASTNodeKind::If {
            cond,
            positive,
            negative,
        } => ASTNodeKind::If {
            cond: Box::new(lower(cond, types)),
            positive: Box::new(lower(positive, types)),
            negative: Box::new(lower(negative, types)),
        },
        ast::ASTNodeKind::ImportFrom { source, values } => ASTNodeKind::ImportFrom {
            source: source.clone(),
            values: values.clone(),
        },
        ast::ASTNodeKind::Infix { op, lhs, rhs } => ASTNodeKind::Infix {
            op: *op,
            lhs: Box::new(lower(lhs, types)),
            rhs: Box::new(lower(rhs, types)),
        },
        ast::ASTNodeKind::Integer { literal, radix } => ASTNodeKind::Integer {
            literal: literal.clone(),
            radix: *radix,
        },
        ast::ASTNodeKind::Declaration(value) => {
            ASTNodeKind::Declaration(lower_declaration(value, types))
        }
        ast::ASTNodeKind::Prefix { op, val } => ASTNodeKind::Prefix {
            op: *op,
            val: Box::new(lower(val, types)),
        },
        ast::ASTNodeKind::Cons { first, tail } => ASTNodeKind::Cons {
            first: Box::new(lower(first, types)),
            tail: Box::new(lower(tail, types)),
        },
        ast::ASTNodeKind::SetCons { some, most } => ASTNodeKind::SetCons {
            some: Box::new(lower(some, types)),
            most: Box::new(lower(most, types)),
        },
        ast::ASTNodeKind::String { str } => ASTNodeKind::String { str: str.clone() },
        ast::ASTNodeKind::Symbol { name } => ASTNodeKind::Symbol { name: name.clone() },
        ast::ASTNodeKind::Tuple { list } => ASTNodeKind::Tuple {
            list: list.iter().map(|node| lower(node, types)).collect(),
        },
    }
}

fn lower_declaration(value: &ast::Declaration, types: &HashMap<usize, Type>) -> Declaration {
    match value {
        ast::Declaration::Mutable { left, right } => Declaration::Mutable {
            left: left.clone(),
            right: Box::new(lower(right, types)),
        },
        ast::Declaration::Inmutable { left, right } => Declaration::Inmutable {
            left: left.clone(),
            right: Box::new(lower(right, types)),
        },
        ast::Declaration::Symbolic { name, constraint } => Declaration::Symbolic {
            name: name.clone(),
            constraint: constraint.clone(),
        },
        ast::Declaration::Function {
            name,
            param,
            result,
        } => Declaration::Function {
            name: name.clone(),
            param: param.clone(),
            result: Box::new(lower(result, types)),
        },
        ast::Declaration::MemoizedFunction {
            name,
            param,
            result,
        } => Declaration::MemoizedFunction {
            name: name.clone(),
            param: param.clone(),
            result: Box::new(lower(result, types)),
        },
    }
}

fn lower_assignable_symbol(
    value: &ast::AssignableSymbol,
    types: &HashMap<usize, Type>,
) -> AssignableSymbol {
    match value {
        ast::AssignableSymbol::Pattern(value) => AssignableSymbol::Pattern(value.clone()),
        ast::AssignableSymbol::ObjectValue { object, value } => AssignableSymbol::ObjectValue {
            object: object.clone(),
            value: value.clone(),
        },
        ast::AssignableSymbol::IndexableValue { container, index } => {
            AssignableSymbol::IndexableValue {
                container: container.clone(),
                index: Box::new(lower(index, types)),
            }
        }
    }
}

// Kept private to the compiler pipeline: callers cannot bless an unchecked tree.
pub(super) fn lower(node: &ast::ASTNode, types: &HashMap<usize, Type>) -> ASTNode {
    ASTNode {
        kind: lower_kind(&node.kind, types),
        position: node.position,
        typ: types
            .get(&(node as *const ast::ASTNode as usize))
            .cloned()
            .unwrap_or(Type::Unknown),
    }
}
