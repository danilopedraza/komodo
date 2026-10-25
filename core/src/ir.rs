//! Stack IR between typechecking and execution.
//!
//! Operands are pushed in source order (the right operand is on top). Value
//! instructions replace their operands with one result. Blocks have a net stack
//! effect of +1; `Drop` discards intermediate expression results.
//! Structured control instructions execute only the selected/repeated block.
//! Runtime values and collection operations are abstract, independent of a heap
//! layout or a future Wasm representation. No executable AST is retained.
//!
//! Constants, loads, closures, definitions, and imports push a value. Unary
//! operations and bindings consume one value; binary operations, indexing, and
//! calls consume two. Collection constructors consume their element count
//! (twice the pair count for dictionaries). Assignment consumes a value and,
//! for indexed targets, an index. `If` consumes a condition; the other structured
//! operations own blocks for pattern scopes and iteration. Calls preserve the
//! source language's tuple argument convention.
//!
//! Blocks and instructions retain inferred types (including gradual `Unknown`)
//! and source positions. Only lowering can construct a block; consumers can
//! inspect its instructions without bypassing typechecking. Runtime addresses
//! travel with stack values to preserve references to mutable list elements.
use crate::{
    ast::{Constant, InfixOperator, Pattern},
    cst::{ComprehensionKind, PrefixOperator},
    error::Position,
    run::ModuleAddress,
    typed_ast::{self, Type},
};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub struct Block(Rc<BlockData>);

/// Immutable executable data shared together with its metadata.
#[derive(Clone, Debug)]
pub struct BlockData {
    pub(crate) code: Box<[Instruction]>,
    pub(crate) position: Position,
    pub(crate) typ: Type,
    pub(crate) stack_capacity: usize,
    pub(crate) references: Box<[Rc<str>]>,
    parameters_resolved: bool,
    pub(crate) capture_names: Box<[Rc<str>]>,
}
impl std::ops::Deref for Block {
    type Target = BlockData;
    fn deref(&self) -> &BlockData {
        &self.0
    }
}

impl Block {
    pub fn instructions(&self) -> &[Instruction] {
        &self.code
    }
    pub fn typ(&self) -> &Type {
        &self.typ
    }
    pub fn position(&self) -> Position {
        self.position
    }
}
// Type metadata does not participate in runtime function identity.
impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.code == other.code && self.position == other.position
    }
}
impl Eq for Block {}
impl Hash for Block {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.code.hash(state);
    }
}
impl PartialOrd for Block {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Block {
    fn cmp(&self, other: &Self) -> Ordering {
        self.code.cmp(&other.code)
    }
}

#[derive(Clone, Debug)]
pub struct Instruction {
    pub op: Op,
    pub position: Position,
    pub typ: Type,
}
impl PartialEq for Instruction {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op && self.position == other.position
    }
}
impl Eq for Instruction {}
impl Hash for Instruction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.op.hash(state);
    }
}
impl PartialOrd for Instruction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Instruction {
    fn cmp(&self, other: &Self) -> Ordering {
        self.op.cmp(&other.op)
    }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Op {
    Constant(crate::object::Object),
    Load(Rc<str>),
    LoadLocal {
        name: Rc<str>,
        slot: usize,
    },
    LoadCapture {
        name: Rc<str>,
        index: usize,
    },
    Drop,
    Infix(InfixOperator),
    IntegerInfix(InfixOperator),
    Prefix(PrefixOperator),
    List(usize),
    Tuple(usize),
    Set(usize),
    Dictionary(usize),
    Fraction(Position),
    Index {
        container: Position,
        index: Position,
    },
    Cons(Position),
    SetCons(Position),
    Function {
        param: Pattern,
        body: Block,
    },
    If {
        positive: Block,
        negative: Block,
    },
    Case {
        expr: Block,
        pairs: Vec<(Pattern, Block)>,
    },
    For {
        pattern: Pattern,
        iterator: Block,
        body: Block,
    },
    Comprehension {
        element: Block,
        variable: Rc<str>,
        iterator: Block,
        kind: ComprehensionKind,
    },
    /// Consume an initializer and leave its value after binding the pattern.
    Bind {
        pattern: Pattern,
        mutable: bool,
        value_position: Position,
        discard: bool,
    },
    Symbolic {
        name: Rc<str>,
        constraint: String,
    },
    DefineFunction {
        name: Rc<str>,
        param: Pattern,
        body: Block,
        memoized: bool,
    },
    Assign {
        left: AssignableSymbol,
        discard: bool,
        value_position: Position,
    },
    Import {
        source: ModuleAddress,
        values: Vec<(String, Position)>,
    },
    Call {
        name: Option<Rc<str>>,
        callee: Position,
    },
    MethodCall {
        receiver: Block,
        method: Block,
        argument: Block,
        attribute: Option<Rc<str>>,
    },
    Attribute {
        name: Rc<str>,
        position: Position,
    },
}
impl Op {
    fn stack_effect(&self) -> (usize, usize) {
        match self {
            Self::Drop => (1, 0),
            Self::Bind { discard: true, .. } => (1, 0),
            Self::Assign {
                discard: true,
                left,
                ..
            } => (
                if matches!(left, AssignableSymbol::IndexableValue { .. }) {
                    2
                } else {
                    1
                },
                0,
            ),
            Self::IntegerInfix(_)
            | Self::Infix(_)
            | Self::Fraction(_)
            | Self::Index { .. }
            | Self::Cons(_)
            | Self::SetCons(_)
            | Self::Call { .. } => (2, 1),
            Self::Prefix(_) | Self::Bind { .. } | Self::If { .. } | Self::Attribute { .. } => {
                (1, 1)
            }
            Self::List(n) | Self::Tuple(n) | Self::Set(n) => (*n, 1),
            Self::Dictionary(n) => (n * 2, 1),
            Self::Assign {
                left: AssignableSymbol::IndexableValue { .. },
                ..
            } => (2, 1),
            Self::Assign { .. } => (1, 1),
            _ => (0, 1),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AssignableSymbol {
    Pattern(Pattern),
    ObjectValue { object: Rc<str>, value: Rc<str> },
    IndexableValue { container: Rc<str> },
}

/// Lower a checked expression to a block producing exactly one value.
fn constant_op(value: Constant) -> Op {
    Op::Constant(crate::exec::eval_constant(&value))
}

pub fn lower(node: &typed_ast::ASTNode) -> Block {
    let mut code = Vec::new();
    emit(node, &mut code);
    let code = simplify(code);
    let mut depth = 0usize;
    let mut stack_capacity = 0;
    for instruction in &code {
        let (inputs, outputs) = instruction.op.stack_effect();
        depth = depth
            .checked_sub(inputs)
            .expect("invalid compiler stack effect")
            + outputs;
        stack_capacity = stack_capacity.max(depth);
    }
    let references = referenced_names(&code).into_iter().collect();
    Block(Rc::new(BlockData {
        references,
        parameters_resolved: false,
        capture_names: Box::default(),
        stack_capacity,
        code: code.into(),
        position: node.position,
        typ: node.typ.clone(),
    }))
}
fn simplify(code: Vec<Instruction>) -> Vec<Instruction> {
    let mut optimized: Vec<Instruction> = Vec::with_capacity(code.len());
    for mut instruction in code {
        if let Op::IntegerInfix(op) | Op::Infix(op) = instruction.op {
            if let [.., Instruction {
                op: Op::Constant(lhs),
                ..
            }, Instruction {
                op: Op::Constant(rhs),
                ..
            }] = optimized.as_slice()
            {
                // Only total integer operations are folded. Division, remainder,
                // shifts, powers, and all failures retain runtime diagnostics.
                if let Some(value) = crate::exec::integer_infix(op, lhs, rhs) {
                    optimized.truncate(optimized.len() - 2);
                    instruction.op = Op::Constant(value);
                }
            }
        }
        if matches!(instruction.op, Op::Drop)
            && matches!(optimized.last().map(|i| &i.op), Some(Op::Constant(_)))
        {
            optimized.pop();
        } else {
            optimized.push(instruction);
        }
    }
    optimized
}

fn lower_function(param: &Pattern, result: &typed_ast::ASTNode) -> Block {
    resolve_function(param, lower(result))
}

pub(crate) fn resolve_function(param: &Pattern, mut body: Block) -> Block {
    if body.parameters_resolved {
        return body;
    }
    // Parameters with fixed binding sets receive local slots. Capture slots are
    // restricted to names that cannot be shadowed anywhere in this frame.
    fn names(pattern: &Pattern, result: &mut std::collections::BTreeSet<String>) -> bool {
        match pattern {
            Pattern::Symbol { name } => {
                result.insert(name.clone());
                true
            }
            Pattern::Tuple { list } | Pattern::List { list } => {
                list.iter().all(|p| names(p, result))
            }
            Pattern::Signature { pattern, .. } => names(pattern, result),
            Pattern::Wildcard | Pattern::Constant(_) => true,
            _ => false,
        }
    }
    fn resolve(block: &mut Block, slots: &std::collections::BTreeMap<String, usize>) {
        for instruction in Rc::make_mut(&mut block.0).code.iter_mut() {
            match &mut instruction.op {
                Op::Load(name) => {
                    if let Some(&slot) = slots.get(name.as_ref()) {
                        instruction.op = Op::LoadLocal {
                            name: name.clone(),
                            slot,
                        };
                    }
                }
                Op::If { positive, negative } => {
                    resolve(positive, slots);
                    resolve(negative, slots);
                }
                Op::MethodCall {
                    receiver,
                    method,
                    argument,
                    ..
                } => {
                    resolve(receiver, slots);
                    resolve(method, slots);
                    resolve(argument, slots);
                }
                Op::Case { expr, .. } => resolve(expr, slots),
                Op::For { iterator, .. } | Op::Comprehension { iterator, .. } => {
                    resolve(iterator, slots)
                }
                // These bodies introduce scopes; their names may shadow parameters.
                _ => {}
            }
        }
    }
    Rc::make_mut(&mut body.0).parameters_resolved = true;
    let mut bindings = std::collections::BTreeSet::new();
    if names(param, &mut bindings) {
        let slots = bindings
            .into_iter()
            .enumerate()
            .map(|(slot, name)| (name, slot))
            .collect();
        resolve(&mut body, &slots);
    }
    fn unstable(block: &Block, bound: &mut std::collections::BTreeSet<Rc<str>>) {
        for instruction in block.instructions() {
            match &instruction.op {
                Op::Bind { pattern, .. } => pattern_names(pattern, bound),
                Op::Symbolic { name, .. } | Op::DefineFunction { name, .. } => {
                    bound.insert(name.clone());
                }
                Op::Call {
                    name: Some(name), ..
                } => {
                    bound.insert(name.clone());
                }
                Op::Import { values, .. } => {
                    bound.extend(values.iter().map(|(name, _)| crate::names::intern(name)))
                }
                Op::If { positive, negative } => {
                    unstable(positive, bound);
                    unstable(negative, bound);
                }
                Op::For {
                    pattern,
                    iterator,
                    body,
                } => {
                    pattern_names(pattern, bound);
                    unstable(iterator, bound);
                    unstable(body, bound);
                }
                Op::Case { expr, pairs } => {
                    unstable(expr, bound);
                    for (pattern, body) in pairs {
                        pattern_names(pattern, bound);
                        unstable(body, bound);
                    }
                }
                Op::Comprehension {
                    variable,
                    iterator,
                    element,
                    ..
                } => {
                    bound.insert(variable.clone());
                    unstable(iterator, bound);
                    unstable(element, bound);
                }
                Op::MethodCall {
                    receiver,
                    method,
                    argument,
                    ..
                } => {
                    for block in [receiver, method, argument] {
                        unstable(block, bound);
                    }
                }
                _ => {}
            }
        }
    }
    fn resolve_free(block: &mut Block, slots: &std::collections::BTreeMap<Rc<str>, usize>) {
        for instruction in Rc::make_mut(&mut block.0).code.iter_mut() {
            match &mut instruction.op {
                Op::Load(name) => {
                    if let Some(&index) = slots.get(name) {
                        instruction.op = Op::LoadCapture {
                            name: name.clone(),
                            index,
                        };
                    }
                }
                Op::If { positive, negative } => {
                    resolve_free(positive, slots);
                    resolve_free(negative, slots);
                }
                Op::For { iterator, body, .. } => {
                    resolve_free(iterator, slots);
                    resolve_free(body, slots);
                }
                Op::Case { expr, pairs } => {
                    resolve_free(expr, slots);
                    for (_, body) in pairs {
                        resolve_free(body, slots);
                    }
                }
                Op::Comprehension {
                    iterator, element, ..
                } => {
                    resolve_free(iterator, slots);
                    resolve_free(element, slots);
                }
                Op::MethodCall {
                    receiver,
                    method,
                    argument,
                    ..
                } => {
                    for block in [receiver, method, argument] {
                        resolve_free(block, slots);
                    }
                }
                _ => {}
            }
        }
    }
    let mut bound = std::collections::BTreeSet::new();
    pattern_names(param, &mut bound);
    unstable(&body, &mut bound);
    let captures: Vec<_> = body
        .references
        .iter()
        .filter(|name| !bound.contains(*name))
        .cloned()
        .collect();
    let slots = captures
        .iter()
        .enumerate()
        .map(|(index, name)| (name.clone(), index))
        .collect();
    resolve_free(&mut body, &slots);
    Rc::make_mut(&mut body.0).capture_names = captures.into();
    body
}

pub(crate) fn pattern_names(pattern: &Pattern, names: &mut std::collections::BTreeSet<Rc<str>>) {
    match pattern {
        Pattern::Symbol { name } => {
            names.insert(crate::names::intern(name));
        }
        Pattern::Dictionary { pairs, .. } => {
            for (key, value) in pairs {
                pattern_names(key, names);
                pattern_names(value, names);
            }
        }
        Pattern::Tuple { list } | Pattern::List { list } | Pattern::Set { list } => {
            for p in list {
                pattern_names(p, names);
            }
        }
        Pattern::Signature { pattern, .. } => pattern_names(pattern, names),
        Pattern::ListCons { first, tail } => {
            pattern_names(first, names);
            pattern_names(tail, names);
        }
        Pattern::SetCons { some, most } => {
            pattern_names(some, names);
            pattern_names(most, names);
        }
        Pattern::Either { lhs, rhs } => {
            pattern_names(lhs, names);
            pattern_names(rhs, names);
        }
        Pattern::Fraction { numer, denom } => {
            pattern_names(numer, names);
            pattern_names(denom, names);
        }
        Pattern::Range { start, end } => {
            pattern_names(start, names);
            pattern_names(end, names);
        }
        _ => {}
    }
}

fn referenced_names(code: &[Instruction]) -> std::collections::BTreeSet<Rc<str>> {
    let mut names = std::collections::BTreeSet::new();
    for instruction in code {
        match &instruction.op {
            Op::Load(name) | Op::LoadLocal { name, .. } | Op::LoadCapture { name, .. } => {
                names.insert(name.clone());
            }
            Op::Assign { left, .. } => match left {
                AssignableSymbol::Pattern(pattern) => pattern_names(pattern, &mut names),
                AssignableSymbol::ObjectValue { object, .. } => {
                    names.insert(object.clone());
                }
                AssignableSymbol::IndexableValue { container } => {
                    names.insert(container.clone());
                }
            },
            Op::Function { body, .. } => names.extend(body.references.iter().cloned()),
            Op::DefineFunction { name, body, .. } => {
                names.insert(name.clone());
                names.extend(body.references.iter().cloned());
            }
            Op::If { positive, negative } => {
                names.extend(positive.references.iter().cloned());
                names.extend(negative.references.iter().cloned());
            }
            Op::Case { expr, pairs } => {
                names.extend(expr.references.iter().cloned());
                for (_, body) in pairs {
                    names.extend(body.references.iter().cloned());
                }
            }
            Op::For { iterator, body, .. } => {
                names.extend(iterator.references.iter().cloned());
                names.extend(body.references.iter().cloned());
            }
            Op::Comprehension {
                element, iterator, ..
            } => {
                names.extend(element.references.iter().cloned());
                names.extend(iterator.references.iter().cloned());
            }
            Op::MethodCall {
                receiver,
                method,
                argument,
                ..
            } => {
                for block in [receiver, method, argument] {
                    names.extend(block.references.iter().cloned());
                }
            }
            _ => {}
        }
    }
    names
}

fn symbol(node: &typed_ast::ASTNode) -> Option<Rc<str>> {
    if let typed_ast::ASTNodeKind::Symbol { name } = &node.kind {
        Some(crate::names::intern(name))
    } else {
        None
    }
}
fn emit(node: &typed_ast::ASTNode, code: &mut Vec<Instruction>) {
    use typed_ast::{ASTNodeKind as A, AssignableSymbol as S, Declaration as D};
    let op = match &node.kind {
        A::Boolean(v) => constant_op(Constant::Boolean(*v)),
        A::Char(v) => constant_op(Constant::Char(*v)),
        A::Integer { literal, radix } => constant_op(Constant::Integer {
            literal: literal.clone(),
            radix: *radix,
        }),
        A::Decimal { int, dec } => constant_op(Constant::Decimal {
            int: int.clone(),
            dec: dec.clone(),
        }),
        A::String { str } => constant_op(Constant::String { str: str.clone() }),
        A::Symbol { name } => Op::Load(crate::names::intern(name)),
        A::Infix { op, lhs, rhs } => {
            emit(lhs, code);
            emit(rhs, code);
            if lhs.typ == Type::Integer && rhs.typ == Type::Integer {
                Op::IntegerInfix(*op)
            } else {
                Op::Infix(*op)
            }
        }
        A::Prefix { op, val } => {
            emit(val, code);
            Op::Prefix(*op)
        }
        A::List { list } | A::Tuple { list } | A::Set { list } => {
            for item in list {
                emit(item, code);
            }
            match &node.kind {
                A::List { .. } => Op::List(list.len()),
                A::Tuple { .. } => Op::Tuple(list.len()),
                _ => Op::Set(list.len()),
            }
        }
        A::Dictionary { pairs, .. } => {
            for (key, value) in pairs {
                if let Some(name) = symbol(key) {
                    code.push(Instruction {
                        op: constant_op(Constant::String {
                            str: name.to_string(),
                        }),
                        position: key.position,
                        typ: Type::String,
                    });
                } else {
                    emit(key, code);
                }
                emit(value, code);
            }
            Op::Dictionary(pairs.len())
        }
        A::Fraction { numer, denom } => {
            emit(numer, code);
            emit(denom, code);
            Op::Fraction(denom.position)
        }
        A::IndexNotation { container, index } => {
            emit(container, code);
            emit(index, code);
            Op::Index {
                container: container.position,
                index: index.position,
            }
        }
        A::Cons { first, tail } => {
            emit(first, code);
            emit(tail, code);
            Op::Cons(tail.position)
        }
        A::SetCons { some, most } => {
            emit(some, code);
            emit(most, code);
            Op::SetCons(most.position)
        }
        A::Function { param, result } => Op::Function {
            param: param.clone(),
            body: lower_function(param, result),
        },
        A::If {
            cond,
            positive,
            negative,
        } => {
            emit(cond, code);
            Op::If {
                positive: lower(positive),
                negative: lower(negative),
            }
        }
        A::Block(exprs) => {
            for (i, expr) in exprs.iter().enumerate() {
                emit(expr, code);
                if i + 1 < exprs.len() {
                    match &mut code.last_mut().expect("expression instruction").op {
                        Op::Bind { discard, .. } | Op::Assign { discard, .. } => *discard = true,
                        _ => code.push(Instruction {
                            op: Op::Drop,
                            position: expr.position,
                            typ: Type::Unknown,
                        }),
                    }
                }
            }
            if !exprs.is_empty() {
                return;
            }
            Op::Tuple(0)
        }
        A::Case { expr, pairs } => Op::Case {
            expr: lower(expr),
            pairs: pairs.iter().map(|(p, b)| (p.clone(), lower(b))).collect(),
        },
        A::For { val, iter, proc } => Op::For {
            pattern: val.clone(),
            iterator: lower(iter),
            body: lower(proc),
        },
        A::Comprehension {
            element,
            variable,
            iterator,
            kind,
        } => Op::Comprehension {
            element: lower(element),
            variable: crate::names::intern(variable),
            iterator: lower(iterator),
            kind: *kind,
        },
        A::ImportFrom { source, values } => Op::Import {
            source: source.clone(),
            values: values.clone(),
        },
        A::Assignment { left, right } => {
            emit(right, code);
            let left = match left {
                S::Pattern(p) => AssignableSymbol::Pattern(p.clone()),
                S::ObjectValue { object, value } => AssignableSymbol::ObjectValue {
                    object: crate::names::intern(object),
                    value: crate::names::intern(value),
                },
                S::IndexableValue { container, index } => {
                    emit(index, code);
                    AssignableSymbol::IndexableValue {
                        container: crate::names::intern(container),
                    }
                }
            };
            Op::Assign {
                left,
                discard: false,
                value_position: right.position,
            }
        }
        A::Declaration(d) => match d {
            D::Mutable { left, right } | D::Inmutable { left, right } => {
                emit(right, code);
                Op::Bind {
                    discard: false,
                    pattern: left.clone(),
                    mutable: matches!(d, D::Mutable { .. }),
                    value_position: right.position,
                }
            }
            D::Symbolic { name, constraint } => Op::Symbolic {
                name: crate::names::intern(name),
                constraint: constraint.clone(),
            },
            D::Function {
                name,
                param,
                result,
            }
            | D::MemoizedFunction {
                name,
                param,
                result,
            } => Op::DefineFunction {
                name: crate::names::intern(name),
                param: param.clone(),
                body: lower_function(param, result),
                memoized: matches!(d, D::MemoizedFunction { .. }),
            },
        },
        A::DotNotation { lhs, rhs } => {
            emit(lhs, code);
            Op::Attribute {
                name: symbol(rhs).expect("attribute must be a symbol"),
                position: rhs.position,
            }
        }
        A::Call { called, arg } => {
            if let A::DotNotation { lhs, rhs } = &called.kind {
                Op::MethodCall {
                    receiver: lower(lhs),
                    method: lower(rhs),
                    argument: lower(arg),
                    attribute: symbol(rhs),
                }
            } else {
                emit(called, code);
                emit(arg, code);
                Op::Call {
                    name: symbol(called),
                    callee: called.position,
                }
            }
        }
    };
    code.push(Instruction {
        op,
        position: node.position,
        typ: node.typ.clone(),
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{env::Environment, exec::exec, object::Object};

    fn compile(source: &str) -> Block {
        let nodes = crate::run::collect_nodes(crate::parser::Parser::from(
            crate::lexer::Lexer::from((source, std::path::PathBuf::default())),
        ))
        .unwrap();
        assert_eq!(nodes.len(), 1);
        lower(&crate::typecheck::elaborate(&nodes[0], &mut Default::default()).unwrap())
    }

    #[test]
    fn deep_tail_recursion_and_escaped_recursive_closures_release_captures() {
        let mut env = Environment::default();
        crate::run::run("let make(seed) :=\n    let recurse(0) := seed\n    let recurse(n) := recurse(n - 1)\n    x -> recurse(x)\nlet escaped := make(7)\nlet result := escaped(100000)", &mut env).unwrap();
        assert_eq!(env.lookup("result"), Some(&Object::Integer(7.into())));
        assert!(matches!(
            env.lookup("escaped"),
            Some(Object::Function(crate::object::Function::Anonymous(_)))
        ));
        // The inner recursive capture retains make's environment, so its weak
        // reference also detects a leaked inner recursive cycle.
        let Object::Function(crate::object::Function::Pattern(make)) = env.lookup("make").unwrap()
        else {
            panic!("named function");
        };
        let weak = Rc::downgrade(&make.env.upgrade());
        drop(env);
        assert!(weak.upgrade().is_none());
    }

    #[test]
    fn parameter_slots_preserve_shadowing_and_rebinding() {
        let mut env = Environment::default();
        crate::run::run("let f(x) :=\n    var total := x\n    for x in [2, 3] do\n        total := total + x\n    total + x\nlet result := f(10)", &mut env).unwrap();
        assert_eq!(env.lookup("result"), Some(&Object::Integer(25.into())));
    }

    #[test]
    fn recursive_aliases_keep_clause_updates_visible() {
        let mut env = Environment::default();
        crate::run::run("let f(0) := 0\nlet f(n) := f(n - 1)\nlet old := f\nlet f(0) := 42\nlet result := old(3)", &mut env).unwrap();
        assert_eq!(env.lookup("result"), Some(&Object::Integer(42.into())));
    }

    #[test]
    fn closure_equality_ignores_unused_captures() {
        let mut env = Environment::default();
        crate::run::run(
            "let make(unused) := x -> x + 1\nlet first := make(1)\nlet second := make(2)",
            &mut env,
        )
        .unwrap();
        assert_eq!(env.lookup("first"), env.lookup("second"));
    }

    #[test]
    fn postfix_operands_keep_types_and_positions() {
        let block = compile("10 - 2 * 3");
        let mut emitted = Vec::new();
        let nodes = crate::run::collect_nodes(crate::parser::Parser::from(
            crate::lexer::Lexer::from(("10 - 2 * 3", Default::default())),
        ))
        .unwrap();
        emit(
            &crate::typecheck::elaborate(&nodes[0], &mut Default::default()).unwrap(),
            &mut emitted,
        );
        let code = emitted.as_slice();
        assert_eq!(code.len(), 5);
        assert!(matches!(
            code[3].op,
            Op::IntegerInfix(InfixOperator::Product)
        ));
        assert!(matches!(
            code[4].op,
            Op::IntegerInfix(InfixOperator::Substraction)
        ));
        assert!(code.iter().all(|i| i.typ == Type::Integer));
        assert_ne!(code[0].position, code[1].position);
        assert_eq!(block.typ(), &Type::Integer);
        assert_eq!(block.instructions().len(), 1);
        assert!(
            matches!(&block.instructions()[0].op, Op::Constant(Object::Integer(n)) if n == &4.into())
        );
        assert_eq!(
            exec(&block, &mut Environment::default()).unwrap(),
            Object::Integer(4.into())
        );
    }

    #[test]
    fn branch_runs_only_selected_code_with_its_own_stack() {
        let block = compile("10 + (if false then 1 / 0 else 3)");
        assert!(matches!(block.code[2].op, Op::If { .. }));
        assert_eq!(
            exec(&block, &mut Environment::default()).unwrap(),
            Object::Integer(13.into())
        );
    }

    #[test]
    fn block_discards_intermediate_values_and_supports_unit() {
        use crate::ast::tests::{block, dec_integer};
        let pos = crate::cst::tests::dummy_pos();
        for (exprs, expected) in [
            (vec![], Object::empty_tuple()),
            (
                vec![dec_integer("1", pos), dec_integer("2", pos)],
                Object::Integer(2.into()),
            ),
        ] {
            let ast = block(exprs, pos);
            let ir = lower(&crate::typecheck::elaborate(&ast, &mut Default::default()).unwrap());
            assert_eq!(exec(&ir, &mut Environment::default()).unwrap(), expected);
            if ir.code.len() > 1 {
                assert!(matches!(ir.code[1].op, Op::Drop));
            }
        }
    }

    #[test]
    fn collection_construction_keeps_operand_order_and_symbol_keys() {
        let block = compile("{foo => (10 - 3, 9 - 2)}");
        assert!(
            matches!(&block.code[0].op, Op::Constant(Object::String(value)) if value.to_string() == "foo")
        );
        let result = exec(&block, &mut Environment::default()).unwrap();
        let Object::Dictionary(dict) = result else {
            panic!("expected dictionary")
        };
        assert_eq!(
            dict.get(&Object::String("foo".into())).unwrap(),
            Object::Tuple(vec![Object::Integer(7.into()), Object::Integer(7.into())].into())
        );
    }

    #[test]
    fn function_bodies_are_lowered_and_keep_result_type() {
        let block = compile("let f(x: Integer) := x + 1");
        let Op::DefineFunction { body, .. } = &block.code[0].op else {
            panic!("expected function definition")
        };
        assert_eq!(body.typ(), &Type::Integer);
        assert!(matches!(
            body.code.last().unwrap().op,
            Op::IntegerInfix(InfixOperator::Sum)
        ));
    }

    #[test]
    fn runtime_errors_keep_operand_source_position() {
        let block = compile("1 // 0");
        let Op::Fraction(denom_position) = block.code.last().unwrap().op else {
            panic!("expected fraction")
        };
        assert_eq!(
            exec(&block, &mut Environment::default()),
            Err(crate::error::Error::with_position(
                crate::exec::ExecError::DenominatorZero.into(),
                denom_position,
                Default::default()
            ))
        );
    }
}
