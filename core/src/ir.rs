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
pub struct Block {
    // Executable code is immutable and shared by closure and environment copies.
    pub(crate) code: Rc<[Instruction]>,
    pub(crate) position: Position,
    pub(crate) typ: Type,
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
    Constant(Constant),
    Load(String),
    Drop,
    Infix(InfixOperator),
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
        variable: String,
        iterator: Block,
        kind: ComprehensionKind,
    },
    /// Consume an initializer and leave its value after binding the pattern.
    Bind {
        pattern: Pattern,
        mutable: bool,
        value_position: Position,
    },
    Symbolic {
        name: String,
        constraint: String,
    },
    DefineFunction {
        name: String,
        param: Pattern,
        body: Block,
        memoized: bool,
    },
    Assign {
        left: AssignableSymbol,
        value_position: Position,
    },
    Import {
        source: ModuleAddress,
        values: Vec<(String, Position)>,
    },
    Call {
        name: Option<String>,
        callee: Position,
    },
    MethodCall {
        receiver: Block,
        method: Block,
        argument: Block,
        attribute: Option<String>,
    },
    Attribute {
        name: String,
        position: Position,
    },
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AssignableSymbol {
    Pattern(Pattern),
    ObjectValue { object: String, value: String },
    IndexableValue { container: String },
}

/// Lower a checked expression to a block producing exactly one value.
pub fn lower(node: &typed_ast::ASTNode) -> Block {
    let mut code = Vec::new();
    emit(node, &mut code);
    Block {
        code: code.into(),
        position: node.position,
        typ: node.typ.clone(),
    }
}
fn symbol(node: &typed_ast::ASTNode) -> Option<String> {
    if let typed_ast::ASTNodeKind::Symbol { name } = &node.kind {
        Some(name.clone())
    } else {
        None
    }
}
fn emit(node: &typed_ast::ASTNode, code: &mut Vec<Instruction>) {
    use typed_ast::{ASTNodeKind as A, AssignableSymbol as S, Declaration as D};
    let op = match &node.kind {
        A::Boolean(v) => Op::Constant(Constant::Boolean(*v)),
        A::Char(v) => Op::Constant(Constant::Char(*v)),
        A::Integer { literal, radix } => Op::Constant(Constant::Integer {
            literal: literal.clone(),
            radix: *radix,
        }),
        A::Decimal { int, dec } => Op::Constant(Constant::Decimal {
            int: int.clone(),
            dec: dec.clone(),
        }),
        A::String { str } => Op::Constant(Constant::String { str: str.clone() }),
        A::Symbol { name } => Op::Load(name.clone()),
        A::Infix { op, lhs, rhs } => {
            emit(lhs, code);
            emit(rhs, code);
            Op::Infix(*op)
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
                        op: Op::Constant(Constant::String { str: name }),
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
            body: lower(result),
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
                    code.push(Instruction {
                        op: Op::Drop,
                        position: expr.position,
                        typ: Type::Unknown,
                    });
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
            variable: variable.clone(),
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
                    object: object.clone(),
                    value: value.clone(),
                },
                S::IndexableValue { container, index } => {
                    emit(index, code);
                    AssignableSymbol::IndexableValue {
                        container: container.clone(),
                    }
                }
            };
            Op::Assign {
                left,
                value_position: right.position,
            }
        }
        A::Declaration(d) => match d {
            D::Mutable { left, right } | D::Inmutable { left, right } => {
                emit(right, code);
                Op::Bind {
                    pattern: left.clone(),
                    mutable: matches!(d, D::Mutable { .. }),
                    value_position: right.position,
                }
            }
            D::Symbolic { name, constraint } => Op::Symbolic {
                name: name.clone(),
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
                name: name.clone(),
                param: param.clone(),
                body: lower(result),
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
    fn postfix_operands_keep_types_and_positions() {
        let block = compile("10 - 2 * 3");
        let code = block.instructions();
        assert_eq!(code.len(), 5);
        assert!(matches!(code[3].op, Op::Infix(InfixOperator::Product)));
        assert!(matches!(code[4].op, Op::Infix(InfixOperator::Substraction)));
        assert!(code.iter().all(|i| i.typ == Type::Integer));
        assert_ne!(code[0].position, code[1].position);
        assert_eq!(block.typ(), &Type::Integer);
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
            matches!(&block.code[0].op, Op::Constant(Constant::String { str }) if str == "foo")
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
            Op::Infix(InfixOperator::Sum)
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
