use std::{collections::HashMap, vec};

use crate::{
    ast::{ASTNode, ASTNodeKind},
    cst::{ComprehensionKind, PrefixOperator},
    error::Position,
};

#[allow(dead_code)]
#[derive(Debug, Default)]
struct SymbolTable {
    bottom: Level,
    stack: Vec<Level>,
}

#[allow(dead_code)]
impl SymbolTable {
    pub fn set_type(&mut self, name: &str, typ: Type) {
        self.stack
            .last_mut()
            .unwrap_or(&mut self.bottom)
            .set_type(name, typ)
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.stack.last().unwrap_or(&self.bottom).get_type(name)
    }

    pub fn within_new_level<T, F: FnOnce(&mut Self) -> T>(&mut self, procedure: F) -> T {
        self.stack.push(Level::default());
        let res = procedure(self);
        self.stack.pop();
        res
    }
}

#[derive(Debug, Default)]
struct Level {
    dict: HashMap<String, Type>,
}

impl Level {
    fn set_type(&mut self, name: &str, typ: Type) {
        self.dict.insert(name.to_string(), typ);
    }

    fn get_type(&self, name: &str) -> Option<&Type> {
        self.dict.get(name)
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum TypeError {
    TypeMismatch { expected: Type, actual: Type },
    UnknownSymbol { name: String },
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
struct Either {
    left: Box<Type>,
    right: Box<Type>,
}

impl Either {
    fn new(left: Type, right: Type) -> Self {
        let left = Box::new(left);
        let right = Box::new(right);
        Self { left, right }
    }

    fn collect(&self) -> Vec<Type> {
        let mut stack = vec![self.right.as_ref(), self.left.as_ref()];
        let mut res = vec![];

        while let Some(top) = stack.pop() {
            match top {
                Type::Either(either) => {
                    stack.push(either.right.as_ref());
                    stack.push(either.left.as_ref());
                }
                typ => {
                    res.push(typ.to_owned().to_owned());
                }
            }
        }

        res
    }
}

impl From<(Type, Type, Vec<Type>)> for Either {
    fn from((first, second, tail): (Type, Type, Vec<Type>)) -> Self {
        let mut res = Either::new(first, second);

        for typ in tail {
            res = Either::new(Type::Either(res), typ);
        }

        res
    }
}

impl PartialOrd for Either {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let left = self.collect();
        let right = other.collect();
        let left_subset_right = left.iter().all(|typ| right.contains(typ));
        let right_subset_left = right.iter().all(|typ| left.contains(typ));

        match (left_subset_right, right_subset_left) {
            (true, true) => Some(std::cmp::Ordering::Equal),
            (true, false) => Some(std::cmp::Ordering::Less),
            (false, true) => Some(std::cmp::Ordering::Greater),
            (false, false) => None,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
enum Type {
    Boolean,
    Char,
    Dictionary,
    Float,
    Fraction,
    Integer,
    List,
    Set,
    String,
    Function { input: Box<Type>, output: Box<Type> },
    Tuple(Vec<Type>),
    Either(Either),
    Unknown,
}

impl Type {
    fn any_number() -> Self {
        Self::Either((Self::Integer, Self::Float, vec![Self::Fraction]).into())
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(std::cmp::Ordering::Equal))
    }
}

impl Eq for Type {}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Boolean, Self::Boolean) => Some(std::cmp::Ordering::Equal),
            (Self::Char, Self::Char) => Some(std::cmp::Ordering::Equal),
            (Self::Dictionary, Self::Dictionary) => Some(std::cmp::Ordering::Equal),
            (Self::Float, Self::Float) => Some(std::cmp::Ordering::Equal),
            (Self::Fraction, Self::Fraction) => Some(std::cmp::Ordering::Equal),
            (Self::Integer, Self::Integer) => Some(std::cmp::Ordering::Equal),
            (Self::List, Self::List) => Some(std::cmp::Ordering::Equal),
            (Self::Set, Self::Set) => Some(std::cmp::Ordering::Equal),
            (Self::String, Self::String) => Some(std::cmp::Ordering::Equal),
            (Self::Unknown, Self::Unknown) => Some(std::cmp::Ordering::Equal),
            (
                Self::Function { input, output },
                Self::Function {
                    input: input2,
                    output: output2,
                },
            ) => match (input.partial_cmp(input2), output.partial_cmp(output2)) {
                (Some(std::cmp::Ordering::Equal), Some(std::cmp::Ordering::Equal)) => {
                    Some(std::cmp::Ordering::Equal)
                }
                (
                    Some(std::cmp::Ordering::Equal | std::cmp::Ordering::Less),
                    Some(std::cmp::Ordering::Equal | std::cmp::Ordering::Less),
                ) => Some(std::cmp::Ordering::Less),
                (
                    Some(std::cmp::Ordering::Equal | std::cmp::Ordering::Greater),
                    Some(std::cmp::Ordering::Equal | std::cmp::Ordering::Greater),
                ) => Some(std::cmp::Ordering::Greater),
                _ => None,
            },
            (Self::Tuple(left_tup), Self::Tuple(right_tup)) => {
                if left_tup.len() != right_tup.len() {
                    return None;
                }

                if left_tup.iter().zip(right_tup).all(|(left, right)| {
                    matches!(left.partial_cmp(right), Some(std::cmp::Ordering::Equal))
                }) {
                    Some(std::cmp::Ordering::Equal)
                } else {
                    None
                }
            }
            (Self::Either(left_either), Self::Either(right_either)) => {
                left_either.partial_cmp(right_either)
            }
            (typ, Self::Either(either)) => {
                if either.collect().contains(typ) {
                    Some(std::cmp::Ordering::Less)
                } else {
                    None
                }
            }
            (Self::Either(either), typ) => {
                if either.collect().contains(typ) {
                    Some(std::cmp::Ordering::Greater)
                } else {
                    None
                }
            }
            (_left, Self::Unknown) => Some(std::cmp::Ordering::Less),
            (Self::Unknown, _right) => Some(std::cmp::Ordering::Greater),
            (_left, _right) => None,
        }
    }
}

#[allow(dead_code)]
fn check(
    val: &ASTNode,
    expected: Type,
    env: &mut SymbolTable,
) -> Result<Type, (TypeError, Position)> {
    let actual = infer(val, env)?;

    check_type(actual, expected).map_err(|type_error| (type_error, val.position))
}

fn check_type(actual: Type, expected: Type) -> Result<Type, TypeError> {
    if actual <= expected {
        Ok(actual)
    } else {
        Err(TypeError::TypeMismatch { expected, actual })
    }
}

#[allow(dead_code)]
fn infer(val: &ASTNode, env: &mut SymbolTable) -> Result<Type, (TypeError, Position)> {
    match &val.kind {
        ASTNodeKind::Integer { .. } => Ok(Type::Integer),
        ASTNodeKind::Decimal { .. } => Ok(Type::Float),
        ASTNodeKind::Assignment { left: _, right } => infer(right, env),
        ASTNodeKind::Boolean(_) => Ok(Type::Boolean),
        ASTNodeKind::Block(block) => infer_block(block, env),
        ASTNodeKind::Call { called, args } => infer_call(called, args, env),
        ASTNodeKind::Case { .. } => todo!(),
        ASTNodeKind::Char(_) => Ok(Type::Char),
        ASTNodeKind::Comprehension { kind, .. } => Ok(infer_comprehension(*kind)),
        ASTNodeKind::DotNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::IndexNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::Dictionary { .. } => Ok(Type::Dictionary),
        ASTNodeKind::List { .. } => Ok(Type::List),
        ASTNodeKind::Set { .. } => Ok(Type::Set),
        ASTNodeKind::For { .. } => Ok(Type::Tuple(vec![])),
        ASTNodeKind::Function { params, result } => infer_function(params, result, env),
        ASTNodeKind::Fraction { .. } => Ok(Type::Fraction),
        ASTNodeKind::If {
            cond,
            positive,
            negative,
        } => infer_if(cond, positive, negative, env),
        ASTNodeKind::ImportFrom { .. } => Ok(Type::Tuple(vec![])),
        ASTNodeKind::Infix { .. } => todo!(),
        ASTNodeKind::Declaration(_) => todo!(),
        ASTNodeKind::Prefix { op, val } => infer_prefix(*op, val, env),
        ASTNodeKind::Cons { .. } => Ok(Type::List),
        ASTNodeKind::SetCons { .. } => Ok(Type::Set),
        ASTNodeKind::String { .. } => Ok(Type::String),
        ASTNodeKind::Symbol { name } => infer_symbol(name, val.position, env),
        ASTNodeKind::Tuple { list } => infer_tuple(list, env),
    }
}

fn infer_function(
    params: &[String],
    result: &ASTNode,
    env: &mut SymbolTable,
) -> Result<Type, (TypeError, Position)> {
    let input = Box::new(if params.is_empty() {
        Type::Tuple(vec![])
    } else {
        Type::Unknown
    });
    let output = Box::new(env.within_new_level(|env| {
        for name in params {
            env.set_type(name, Type::Unknown);
        }

        infer(result, env)
    })?);

    Ok(Type::Function { input, output })
}

fn infer_call(
    called: &ASTNode,
    args: &[ASTNode],
    env: &mut SymbolTable,
) -> Result<Type, (TypeError, Position)> {
    let actual_input = Box::new(
        args.first()
            .map_or(Ok(Type::Tuple(vec![])), |typ| infer(typ, env))?,
    );

    match infer(called, env)? {
        Type::Function {
            input: expected_input,
            output: expected_output,
        } => {
            check_type(*actual_input, *expected_input)
                .map_err(|type_error| (type_error, called.position))?;
            Ok(*expected_output)
        }
        typ => {
            let actual = Type::Function {
                input: actual_input,
                output: Box::new(Type::Unknown),
            };
            let expected = typ;

            Err((
                TypeError::TypeMismatch { expected, actual },
                called.position,
            ))
        }
    }
}

fn infer_prefix(
    op: PrefixOperator,
    val: &ASTNode,
    env: &mut SymbolTable,
) -> Result<Type, (TypeError, Position)> {
    match op {
        PrefixOperator::BitwiseNot => check(val, Type::Integer, env),
        PrefixOperator::LogicNot => check(val, Type::Boolean, env),
        PrefixOperator::Minus => check(val, Type::any_number(), env),
    }
}

fn infer_block(block: &[ASTNode], env: &mut SymbolTable) -> Result<Type, (TypeError, Position)> {
    infer(block.last().unwrap(), env)
}

fn infer_symbol(
    name: &str,
    name_pos: Position,
    env: &mut SymbolTable,
) -> Result<Type, (TypeError, Position)> {
    match env.get_type(name) {
        Some(typ) => Ok(typ.to_owned()),
        None => Err((TypeError::UnknownSymbol { name: name.into() }, name_pos)),
    }
}

fn infer_comprehension(kind: ComprehensionKind) -> Type {
    match kind {
        ComprehensionKind::List => Type::List,
        ComprehensionKind::Set => Type::Set,
    }
}

fn infer_tuple(vals: &[ASTNode], env: &mut SymbolTable) -> Result<Type, (TypeError, Position)> {
    let vals_types: Result<Vec<Type>, (TypeError, Position)> =
        vals.iter().map(|val| infer(val, env)).collect();

    vals_types.map(Type::Tuple)
}

fn infer_if(
    cond: &ASTNode,
    positive: &ASTNode,
    negative: &ASTNode,
    env: &mut SymbolTable,
) -> Result<Type, (TypeError, Position)> {
    check(cond, Type::Boolean, env)?;
    Ok(join(infer(positive, env)?, infer(negative, env)?))
}

fn join(left: Type, right: Type) -> Type {
    match (left, right) {
        (
            Type::Function {
                input: input1,
                output: output1,
            },
            Type::Function {
                input: input2,
                output: output2,
            },
        ) => {
            let input = Box::new(join(*input1, *input2));
            let output = Box::new(join(*output1, *output2));

            Type::Function { input, output }
        }
        (left, right) => match left.partial_cmp(&right) {
            Some(std::cmp::Ordering::Less) => right,
            Some(std::cmp::Ordering::Equal) => left,
            Some(std::cmp::Ordering::Greater) => left,
            None => Type::Either(Either::new(left, right)),
        },
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{
        ast::{
            tests::{
                _for, _if, assignable_pattern, assignment, block, boolean, call, char,
                comprehension, cons, dec_integer, decimal, dictionary, extension_list,
                extension_set, fraction, function, import_from, prefix, set_cons, string, symbol,
                symbol_pattern, tuple, wildcard,
            },
            ASTNode,
        },
        cst::{tests::dummy_pos, ComprehensionKind},
        error::Position,
        run::ModuleAddress,
        typecheck::{check, infer, SymbolTable, Type, TypeError},
    };

    fn fresh_check(val: &ASTNode, expected: Type) -> Result<Type, (TypeError, Position)> {
        check(val, expected, &mut SymbolTable::default())
    }

    fn fresh_infer(val: &ASTNode) -> Result<Type, (TypeError, Position)> {
        infer(val, &mut SymbolTable::default())
    }

    #[test]
    fn integer_check() {
        assert_eq!(
            fresh_check(&dec_integer("10", dummy_pos()), Type::Integer,),
            Ok(Type::Integer)
        )
    }

    #[test]
    fn integer_infer() {
        assert_eq!(
            fresh_infer(&dec_integer("0", dummy_pos())),
            Ok(Type::Integer),
        );
    }

    #[test]
    fn float_check() {
        assert_eq!(
            fresh_check(&decimal("0", "0", dummy_pos()), Type::Float,),
            Ok(Type::Float)
        )
    }

    #[test]
    fn float_infer() {
        assert_eq!(
            fresh_infer(&decimal("0", "0", dummy_pos())),
            Ok(Type::Float)
        );
    }

    #[test]
    fn bad_check() {
        assert_eq!(
            fresh_check(&decimal("0", "0", dummy_pos()), Type::Integer,),
            Err((
                TypeError::TypeMismatch {
                    expected: Type::Integer,
                    actual: Type::Float
                },
                dummy_pos()
            ))
        )
    }

    #[test]
    fn char_infer() {
        assert_eq!(fresh_infer(&char('x', dummy_pos())), Ok(Type::Char),);
    }

    #[test]
    fn string_infer() {
        assert_eq!(fresh_infer(&string("foo", dummy_pos())), Ok(Type::String),);
    }

    #[test]
    fn boolean_infer() {
        assert_eq!(fresh_infer(&boolean(true, dummy_pos())), Ok(Type::Boolean),);
    }

    #[test]
    fn block_infer() {
        assert_eq!(
            fresh_infer(&block(
                vec![string("foo", dummy_pos()), dec_integer("10", dummy_pos()),],
                dummy_pos()
            )),
            Ok(Type::Integer),
        );
    }

    #[test]
    fn symbol_infer() {
        let mut env = SymbolTable::default();

        env.set_type("foo", Type::Boolean);

        assert_eq!(
            infer(&symbol("foo", dummy_pos()), &mut env,),
            Ok(Type::Boolean),
        );
    }

    #[test]
    fn list_infer() {
        assert_eq!(
            fresh_infer(&extension_list(vec![], dummy_pos())),
            Ok(Type::List),
        );
    }

    #[test]
    fn list_comprehension_infer() {
        assert_eq!(
            fresh_infer(&comprehension(
                symbol("x", dummy_pos()),
                "x".into(),
                extension_list(vec![], dummy_pos()),
                ComprehensionKind::List,
                dummy_pos()
            )),
            Ok(Type::List),
        );
    }

    #[test]
    fn set_infer() {
        assert_eq!(
            fresh_infer(&extension_set(vec![], dummy_pos())),
            Ok(Type::Set),
        );
    }

    #[test]
    fn set_comprehension_infer() {
        assert_eq!(
            fresh_infer(&comprehension(
                symbol("x", dummy_pos()),
                "x".into(),
                extension_list(vec![], dummy_pos()),
                ComprehensionKind::Set,
                dummy_pos()
            )),
            Ok(Type::Set),
        );
    }

    #[test]
    fn dict_infer() {
        assert_eq!(
            fresh_infer(&dictionary(vec![], true, dummy_pos())),
            Ok(Type::Dictionary),
        );
    }

    #[test]
    fn for_infer() {
        assert_eq!(
            fresh_infer(&_for(
                wildcard(),
                extension_list(vec![], dummy_pos()),
                vec![tuple(vec![], dummy_pos())],
                dummy_pos()
            )),
            Ok(Type::Tuple(vec![])),
        );
    }

    #[test]
    fn fraction_infer() {
        assert_eq!(
            fresh_infer(&fraction(
                dec_integer("0", dummy_pos()),
                dec_integer("1", dummy_pos()),
                dummy_pos()
            )),
            Ok(Type::Fraction)
        );
    }

    #[test]
    fn import_infer() {
        assert_eq!(
            fresh_infer(&import_from(
                ModuleAddress::LocalPath {
                    path: PathBuf::new()
                },
                vec![],
                dummy_pos()
            )),
            Ok(Type::Tuple(vec![]))
        );
    }

    #[test]
    fn cons_infer() {
        assert_eq!(
            fresh_infer(&cons(
                dec_integer("1", dummy_pos()),
                symbol("tail", dummy_pos()),
                dummy_pos()
            )),
            Ok(Type::List),
        );
    }

    #[test]
    fn set_cons_infer() {
        assert_eq!(
            fresh_infer(&set_cons(
                dec_integer("1", dummy_pos()),
                symbol("tail", dummy_pos()),
                dummy_pos()
            )),
            Ok(Type::Set),
        );
    }

    #[test]
    fn tuple_infer() {
        assert_eq!(
            fresh_infer(&tuple(
                vec![dec_integer("10", dummy_pos()), string("ten", dummy_pos()),],
                dummy_pos()
            )),
            Ok(Type::Tuple(vec![Type::Integer, Type::String]))
        );
    }

    #[test]
    fn unknown_symbol() {
        assert_eq!(
            fresh_infer(&symbol("foo", Position::new(0, 3))),
            Err((
                TypeError::UnknownSymbol {
                    name: String::from("foo")
                },
                Position::new(0, 3)
            ))
        );
    }

    #[test]
    fn if_infer_single_type() {
        assert_eq!(
            fresh_infer(&_if(
                boolean(true, dummy_pos()),
                dec_integer("5", dummy_pos()),
                dec_integer("6", dummy_pos()),
                dummy_pos()
            )),
            Ok(Type::Integer),
        );
    }

    #[test]
    fn if_infer_non_boolean_condition() {
        assert_eq!(
            fresh_infer(&_if(
                dec_integer("1", Position::new(3, 1)),
                dec_integer("5", dummy_pos()),
                dec_integer("6", dummy_pos()),
                dummy_pos()
            )),
            Err((
                TypeError::TypeMismatch {
                    expected: Type::Boolean,
                    actual: Type::Integer
                },
                Position::new(3, 1)
            )),
        );
    }

    #[test]
    fn negated_boolean_infer() {
        assert_eq!(
            fresh_infer(&prefix(
                crate::cst::PrefixOperator::LogicNot,
                boolean(true, dummy_pos()),
                dummy_pos()
            )),
            Ok(Type::Boolean)
        );
    }

    #[test]
    fn negative_number_infer() {
        assert_eq!(
            fresh_infer(&prefix(
                crate::cst::PrefixOperator::Minus,
                dec_integer("1", dummy_pos()),
                dummy_pos()
            )),
            Ok(Type::Integer),
        );
    }

    #[test]
    fn infer_bitwise_not() {
        assert_eq!(
            fresh_infer(&prefix(
                crate::cst::PrefixOperator::BitwiseNot,
                dec_integer("0", dummy_pos()),
                dummy_pos()
            )),
            Ok(Type::Integer),
        );
    }

    #[test]
    fn infer_assignment() {
        assert_eq!(
            fresh_infer(&assignment(
                assignable_pattern(symbol_pattern("x")),
                dec_integer("10", dummy_pos()),
                dummy_pos()
            )),
            Ok(Type::Integer),
        );
    }

    #[test]
    fn infer_call() {
        let mut env = SymbolTable::default();

        env.set_type(
            "foo",
            Type::Function {
                input: Box::new(Type::Integer),
                output: Box::new(Type::Integer),
            },
        );

        assert_eq!(
            infer(
                &call(
                    symbol("foo", dummy_pos()),
                    vec![dec_integer("1", dummy_pos())],
                    dummy_pos()
                ),
                &mut env,
            ),
            Ok(Type::Integer),
        );
    }

    #[test]
    fn infer_call_type_mismatch() {
        let mut env = SymbolTable::default();

        env.set_type(
            "foo",
            Type::Function {
                input: Box::new(Type::Integer),
                output: Box::new(Type::Integer),
            },
        );

        assert_eq!(
            infer(
                &call(
                    symbol("foo", dummy_pos()),
                    vec![boolean(true, dummy_pos())],
                    dummy_pos()
                ),
                &mut env,
            ),
            Err((
                TypeError::TypeMismatch {
                    expected: Type::Integer,
                    actual: Type::Boolean
                },
                dummy_pos()
            )),
        );
    }

    #[test]
    fn infer_function() {
        assert_eq!(
            fresh_infer(&function(vec!["a"], symbol("a", dummy_pos()), dummy_pos())),
            Ok(Type::Function {
                input: Box::new(Type::Unknown),
                output: Box::new(Type::Unknown)
            }),
        );
    }
}
