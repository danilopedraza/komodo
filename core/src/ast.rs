use crate::{
    cst::{self, ComprehensionKind},
    error::Position,
    lexer::Radix,
    run::ModuleAddress,
};
use core::fmt;
use std::hash::Hash;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident())
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

impl ASTNode {
    pub(crate) fn has_yield(&self) -> bool {
        let mut found = false;
        self.walk(0, &mut |node, depth| {
            found |= depth == 0
                && matches!(
                    node.kind,
                    ASTNodeKind::Prefix {
                        op: cst::PrefixOperator::Yield,
                        ..
                    }
                );
        });
        found
    }

    pub(crate) fn walk(&self, depth: usize, visit: &mut impl FnMut(&ASTNode, usize)) {
        use ASTNodeKind::*;
        visit(self, depth);
        let mut child = |node: &ASTNode| node.walk(depth, visit);
        match &self.kind {
            Function { result, .. }
            | Declaration(
                crate::ast::Declaration::Function { result, .. }
                | crate::ast::Declaration::MemoizedFunction { result, .. },
            ) => result.walk(depth + 1, visit),
            Assignment { left, right } => {
                if let AssignableSymbol::IndexableValue { index, .. } = left {
                    child(index);
                }
                child(right);
            }
            Declaration(
                crate::ast::Declaration::Mutable { right, .. }
                | crate::ast::Declaration::Inmutable { right, .. },
            ) => child(right),
            Block(list) | List { list } | Set { list } | Tuple { list } => {
                for node in list {
                    child(node);
                }
            }
            Call { called, arg } => {
                child(called);
                child(arg);
            }
            Case { expr, pairs } => {
                child(expr);
                for (_, body) in pairs {
                    child(body);
                }
            }
            Comprehension {
                element, iterator, ..
            } => {
                child(element);
                child(iterator);
            }
            DotNotation { lhs, rhs } | Infix { lhs, rhs, .. } => {
                child(lhs);
                child(rhs);
            }
            IndexNotation { container, index } => {
                child(container);
                child(index);
            }
            Dictionary { pairs, .. } => {
                for (key, value) in pairs {
                    child(key);
                    child(value);
                }
            }
            For { iter, proc, .. } => {
                child(iter);
                child(proc);
            }
            Fraction { numer, denom } => {
                child(numer);
                child(denom);
            }
            If {
                cond,
                positive,
                negative,
            } => {
                child(cond);
                child(positive);
                child(negative);
            }
            Prefix { val, .. } => child(val),
            Cons { first, tail } => {
                child(first);
                child(tail);
            }
            SetCons { some, most } => {
                child(some);
                child(most);
            }
            Boolean(_)
            | Char(_)
            | Decimal { .. }
            | Integer { .. }
            | ImportFrom { .. }
            | String { .. }
            | Symbol { .. }
            | Declaration(crate::ast::Declaration::Symbolic { .. }) => {}
        }
    }
}

impl Hash for ASTNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl PartialOrd for ASTNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ASTNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.kind.cmp(&other.kind)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ASTNodeKind {
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
pub enum Declaration {
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
pub enum Pattern {
    Constant(Constant),
    Dictionary {
        pairs: Vec<(Pattern, Pattern)>,
        complete: bool,
    },
    List {
        list: Vec<Pattern>,
    },
    Set {
        list: Vec<Pattern>,
    },
    ListCons {
        first: Box<Pattern>,
        tail: Box<Pattern>,
    },
    SetCons {
        some: Box<Pattern>,
        most: Box<Pattern>,
    },
    Symbol {
        name: String,
    },
    Tuple {
        list: Vec<Pattern>,
    },
    Range {
        start: Box<Pattern>,
        end: Box<Pattern>,
    },
    Fraction {
        numer: Box<Pattern>,
        denom: Box<Pattern>,
    },
    Signature {
        pattern: Box<Pattern>,
        constraint: TypeHint,
    },
    Either {
        lhs: Box<Pattern>,
        rhs: Box<Pattern>,
    },
    Wildcard,
    AdInfinitum,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeHint {
    Simple(String),
    Either {
        lhs: Box<TypeHint>,
        rhs: Box<TypeHint>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AssignableSymbol {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Constant {
    Boolean(bool),
    Char(char),
    Decimal {
        int: String,
        dec: String,
    },
    Dictionary {
        pairs: Vec<(Constant, Constant)>,
        complete: bool,
    },
    Integer {
        literal: String,
        radix: Radix,
    },
    List {
        list: Vec<Constant>,
    },
    Set {
        list: Vec<Constant>,
    },
    String {
        str: String,
    },
    Tuple {
        list: Vec<Constant>,
    },
}

// Parentheses group a single value; comma-separated values form a tuple.
pub fn single_argument(mut values: Vec<ASTNode>, position: Position) -> ASTNode {
    if values.len() == 1 {
        values.pop().unwrap()
    } else {
        ASTNode::new(ASTNodeKind::Tuple { list: values }, position)
    }
}

pub fn single_pattern(mut patterns: Vec<Pattern>) -> Pattern {
    if patterns.len() == 1 {
        patterns.pop().unwrap()
    } else {
        Pattern::Tuple { list: patterns }
    }
}

#[cfg(test)]
pub mod tests {
    use cst::{DeclarationKind, PrefixOperator};

    use super::*;
    pub fn call(called: ASTNode, args: Vec<ASTNode>, position: Position) -> ASTNode {
        let called = Box::new(called);
        ASTNode::new(
            ASTNodeKind::Call {
                called,
                arg: Box::new(single_argument(args, position)),
            },
            position,
        )
    }

    pub fn function(params: Vec<&str>, result: ASTNode, position: Position) -> ASTNode {
        let param = single_pattern(params.into_iter().map(symbol_pattern).collect());
        let result = Box::new(result);
        ASTNode::new(ASTNodeKind::Function { param, result }, position)
    }

    pub fn fraction(numer: ASTNode, denom: ASTNode, position: Position) -> ASTNode {
        let numer = Box::new(numer);
        let denom = Box::new(denom);
        ASTNode::new(ASTNodeKind::Fraction { numer, denom }, position)
    }

    pub fn fraction_pattern(numer: Pattern, denom: Pattern) -> Pattern {
        let numer = Box::new(numer);
        let denom = Box::new(denom);
        Pattern::Fraction { numer, denom }
    }

    pub fn symbol(name: &str, position: Position) -> ASTNode {
        let name = name.to_string();
        ASTNode::new(ASTNodeKind::Symbol { name }, position)
    }

    pub fn symbol_pattern(name: &str) -> Pattern {
        let name = name.to_string();
        Pattern::Symbol { name }
    }

    pub fn simple_type_hint(name: &str) -> TypeHint {
        TypeHint::Simple(name.into())
    }

    pub fn either_type_hint(lhs: TypeHint, rhs: TypeHint) -> TypeHint {
        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        TypeHint::Either { lhs, rhs }
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

    pub fn dec_integer_pattern(dec: &str) -> Pattern {
        let dec = dec.to_string();

        Pattern::Constant(Constant::Integer {
            literal: dec,
            radix: Radix::Decimal,
        })
    }

    pub fn cons(first: ASTNode, tail: ASTNode, position: Position) -> ASTNode {
        let first = Box::new(first);
        let tail = Box::new(tail);
        ASTNode::new(ASTNodeKind::Cons { first, tail }, position)
    }

    pub fn cons_pattern(first: Pattern, tail: Pattern) -> Pattern {
        let first = Box::new(first);
        let tail = Box::new(tail);
        Pattern::ListCons { first, tail }
    }

    pub fn extension_list(list: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::List { list }, position)
    }

    pub fn list_pattern(list: Vec<Pattern>) -> Pattern {
        Pattern::List { list }
    }

    pub fn _for(val: Pattern, iter: ASTNode, proc: Vec<ASTNode>, position: Position) -> ASTNode {
        let iter = Box::new(iter);
        let pos = proc
            .first()
            .unwrap()
            .position
            .join(proc.last().unwrap().position);
        let proc = Box::new(block(proc, pos));
        ASTNode::new(ASTNodeKind::For { val, iter, proc }, position)
    }

    pub fn infix(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        ASTNode::new(ASTNodeKind::Infix { op, lhs, rhs }, position)
    }

    pub fn either_pattern(lhs: Pattern, rhs: Pattern) -> Pattern {
        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        Pattern::Either { lhs, rhs }
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

    pub fn set_pattern(list: Vec<Pattern>) -> Pattern {
        Pattern::Set { list }
    }

    pub fn let_(left: Pattern, right: ASTNode, position: Position) -> ASTNode {
        declaration(left, right, DeclarationKind::Inmutable, position)
    }

    pub fn var(left: Pattern, right: ASTNode, position: Position) -> ASTNode {
        declaration(left, right, DeclarationKind::Mutable, position)
    }

    pub fn declaration(
        left: Pattern,
        right: ASTNode,
        kind: DeclarationKind,
        position: Position,
    ) -> ASTNode {
        let right = Box::new(right);

        ASTNode::new(
            ASTNodeKind::Declaration(match kind {
                DeclarationKind::Inmutable => Declaration::Inmutable { left, right },
                DeclarationKind::Mutable => Declaration::Mutable { left, right },
                _ => unimplemented!(),
            }),
            position,
        )
    }

    pub fn function_declaration(
        name: &str,
        params: Vec<Pattern>,
        result: ASTNode,
        position: Position,
    ) -> ASTNode {
        let name = name.to_string();
        let result = Box::new(result);
        ASTNode::new(
            ASTNodeKind::Declaration(Declaration::Function {
                name,
                param: single_pattern(params),
                result,
            }),
            position,
        )
    }

    pub fn memoized_function_declaration(
        name: &str,
        params: Vec<Pattern>,
        result: ASTNode,
        position: Position,
    ) -> ASTNode {
        let name = name.to_string();
        let result = Box::new(result);
        ASTNode::new(
            ASTNodeKind::Declaration(Declaration::MemoizedFunction {
                name,
                param: single_pattern(params),
                result,
            }),
            position,
        )
    }

    pub fn prefix(op: PrefixOperator, val: ASTNode, position: Position) -> ASTNode {
        let val = Box::new(val);
        ASTNode::new(ASTNodeKind::Prefix { op, val }, position)
    }

    pub fn signature_pattern(pattern: Pattern, constraint: TypeHint) -> Pattern {
        let pattern = Box::new(pattern);

        Pattern::Signature {
            pattern,
            constraint,
        }
    }

    pub fn tuple(values: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::Tuple { list: values }, position)
    }

    pub fn range(start: ASTNode, end: ASTNode, position: Position) -> ASTNode {
        infix(InfixOperator::Range, start, end, position)
    }

    pub fn range_pattern(start: Pattern, end: Pattern) -> Pattern {
        let start = Box::new(start);
        let end = Box::new(end);

        Pattern::Range { start, end }
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

    pub fn dictionary_pattern(pairs: Vec<(Pattern, Pattern)>, complete: bool) -> Pattern {
        Pattern::Dictionary { pairs, complete }
    }

    pub fn import_from(
        source: ModuleAddress,
        values: Vec<(&str, Position)>,
        position: Position,
    ) -> ASTNode {
        let values = values
            .into_iter()
            .map(|(str, pos)| (str.to_string(), pos))
            .collect();
        ASTNode::new(ASTNodeKind::ImportFrom { source, values }, position)
    }

    pub fn string(str: &str, position: Position) -> ASTNode {
        let str = str.to_string();
        ASTNode::new(ASTNodeKind::String { str }, position)
    }

    pub fn char(chr: char, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::Char(chr), position)
    }

    pub fn string_pattern(str: &str) -> Pattern {
        let str = str.to_string();
        Pattern::Constant(Constant::String { str })
    }

    pub fn wildcard() -> Pattern {
        Pattern::Wildcard
    }

    pub fn ad_infinitum() -> Pattern {
        Pattern::AdInfinitum
    }

    pub fn set_cons(some: ASTNode, most: ASTNode, position: Position) -> ASTNode {
        let some = Box::new(some);
        let most = Box::new(most);

        ASTNode::new(ASTNodeKind::SetCons { some, most }, position)
    }

    pub fn set_cons_pattern(some: Pattern, most: Pattern) -> Pattern {
        let some = Box::new(some);
        let most = Box::new(most);

        Pattern::SetCons { some, most }
    }

    pub fn block(exprs: Vec<ASTNode>, position: Position) -> ASTNode {
        ASTNode::new(ASTNodeKind::Block(exprs), position)
    }

    pub fn assignment(left: AssignableSymbol, right: ASTNode, position: Position) -> ASTNode {
        let right = Box::new(right);
        ASTNode::new(ASTNodeKind::Assignment { left, right }, position)
    }

    pub fn assignable_pattern(pattern: Pattern) -> AssignableSymbol {
        AssignableSymbol::Pattern(pattern)
    }

    pub fn assignable_object_value(object: &str, value: &str) -> AssignableSymbol {
        let object = String::from(object);
        let value = String::from(value);

        AssignableSymbol::ObjectValue { object, value }
    }

    pub fn assignable_container_element(container: &str, index: ASTNode) -> AssignableSymbol {
        let container = String::from(container);
        let index = Box::new(index);
        AssignableSymbol::IndexableValue { container, index }
    }

    pub fn symbolic_let(name: &str, constraint: &str, position: Position) -> ASTNode {
        let name = name.to_string();
        let constraint = constraint.to_string();

        ASTNode::new(
            ASTNodeKind::Declaration(Declaration::Symbolic { name, constraint }),
            position,
        )
    }

    pub fn case(expr: ASTNode, pairs: Vec<(Pattern, ASTNode)>, position: Position) -> ASTNode {
        let expr = Box::new(expr);

        ASTNode::new(ASTNodeKind::Case { expr, pairs }, position)
    }

    pub fn dot_notation(lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);

        ASTNode::new(ASTNodeKind::DotNotation { lhs, rhs }, position)
    }
}
