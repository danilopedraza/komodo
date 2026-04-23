use core::fmt;
use std::{
    collections::HashMap,
    env, fs,
    ops::Deref,
    path::{Path, PathBuf},
    vec,
};

use crate::{
    ast::{ASTNode, ASTNodeKind, Constant, Declaration, InfixOperator, Pattern, TypeHint},
    cst::{ComprehensionKind, PrefixOperator},
    env::ExecContext,
    error::Position,
    lexer::Lexer,
    parser::Parser,
    run::{collect_nodes, get_std_path, ModuleAddress, STDLIB_PATH_VAR},
};

#[derive(Debug, Default)]
pub struct Environment {
    bottom: Level,
    stack: Vec<Level>,
    ctx: ExecContext,
}

impl Environment {
    fn set_type(&mut self, name: &str, typ: Type) {
        self.top_level_mut().set_type(name, typ)
    }

    fn get_type(&self, name: &str) -> Option<&Type> {
        for lvl in self.stack.iter().rev() {
            if let Some(type_hint) = lvl.get_type(name) {
                return Some(type_hint);
            }
        }

        self.bottom.get_type(name)
    }

    fn within_new_level<T, F: FnOnce(&mut Self) -> T>(&mut self, procedure: F) -> T {
        self.stack.push(Level::default());
        let res = procedure(self);
        self.stack.pop();
        res
    }

    // fn top_level(&self) -> &Level {
    //     self.stack.last().unwrap_or(&self.bottom)
    // }

    fn top_level_mut(&mut self) -> &mut Level {
        self.stack.last_mut().unwrap_or(&mut self.bottom)
    }

    pub fn std_env(ctx: ExecContext) -> Self {
        let mut res = Self::new(ctx);

        let hints = [
            ("println", Type::Unknown),
            ("print", Type::Unknown),
            ("getln", Type::Unknown),
            (
                "assert",
                Type::Function {
                    input: Box::new(Type::Boolean),
                    output: Box::new(Type::Unknown),
                },
            ),
            ("Integer", Type::Unknown),
            ("Float", Type::Unknown),
            ("List", Type::Unknown),
            ("Set", Type::Unknown),
            ("String", Type::Unknown),
            ("len", Type::Unknown),
            ("sorted", Type::Unknown),
        ];

        for (name, type_hint) in hints {
            res.bottom.set_type(name, type_hint);
        }

        res
    }

    pub fn new(ctx: ExecContext) -> Self {
        Self {
            ctx,
            ..Self::default()
        }
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
    NonExistentInfix {
        op: InfixOperator,
        lhs_type: Type,
        rhs_type: Type,
    },
    TypeMismatch {
        expected: Type,
        actual: Type,
    },
    UnknownStdModule {
        name: String,
    },
    UnknownSymbol {
        name: String,
    },
    UnknownSymbolInStd {
        symbol: String,
        module: String,
    },
    UnknownType {
        name: String,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Either {
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

        res.dedup();
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

#[derive(Clone, Debug)]
pub enum Type {
    Boolean,
    Char,
    Dictionary,
    Float,
    Fraction,
    Integer,
    List,
    Range,
    Set,
    String,
    Function { input: Box<Type>, output: Box<Type> },
    Tuple(Vec<Type>),
    Either(Either),
    Unknown,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Boolean => write!(f, "Boolean"),
            Type::Char => write!(f, "Char"),
            Type::Dictionary => write!(f, "Dictionary"),
            Type::Float => write!(f, "Float"),
            Type::Fraction => write!(f, "Fraction"),
            Type::Integer => write!(f, "Integer"),
            Type::List => write!(f, "List"),
            Type::Range => write!(f, "Range"),
            Type::Set => write!(f, "Set"),
            Type::String => write!(f, "String"),
            Type::Function { input, output } => {
                write!(f, "{} -> {}", input, output)
            }
            Type::Tuple(items) => {
                let type_strs = items
                    .iter()
                    .map(|typ| typ.to_string())
                    .reduce(|acc, cur| acc + ", " + &cur)
                    .unwrap_or(String::new());

                write!(f, "({})", type_strs)
            }
            Type::Either(either) => {
                let type_str = either
                    .collect()
                    .into_iter()
                    .map(|typ| typ.to_string())
                    .reduce(|acc, cur| acc + " | " + &cur)
                    .unwrap();

                write!(f, "{}", &type_str)
            }
            Type::Unknown => write!(f, "Unknown"),
        }
    }
}

impl Type {
    fn any_number() -> Self {
        Self::Either((Self::Integer, Self::Float, vec![Self::Fraction]).into())
    }

    fn empty_tuple() -> Self {
        Self::Tuple(vec![])
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
            (Self::Range, Self::Range) => Some(std::cmp::Ordering::Equal),
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

pub fn check(
    val: &ASTNode,
    expected: Type,
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    let actual = infer(val, env)?;

    check_type(actual, expected).map_err(|type_error| (type_error, val.position))
}

fn check_type(actual: Type, expected: Type) -> Result<Type, TypeError> {
    match (actual, expected) {
        (actual, expected) if actual == expected => Ok(actual),
        (Type::Unknown, _) => Ok(Type::Unknown),
        (actual, Type::Unknown) => Ok(actual),
        (Type::Either(e1), Type::Either(e2)) => {
            let cmp_res = e1.partial_cmp(&e2);
            let actual = Type::Either(e1);
            let expected = Type::Either(e2);
            match cmp_res {
                Some(res) => match res {
                    std::cmp::Ordering::Less => Ok(actual),
                    std::cmp::Ordering::Equal => Ok(actual),
                    std::cmp::Ordering::Greater => {
                        Err(TypeError::TypeMismatch { expected, actual })
                    }
                },
                None => Err(TypeError::TypeMismatch { expected, actual }),
            }
        }
        (actual, Type::Either(either)) => {
            for type_hint in either.collect() {
                let res = check_type(actual.clone(), type_hint);

                if res.is_ok() {
                    return res;
                }
            }

            let expected = Type::Either(either);
            Err(TypeError::TypeMismatch { expected, actual })
        }
        (Type::Either(either), expected) => {
            let actual = Type::Either(either);
            Err(TypeError::TypeMismatch { expected, actual })
        }
        (Type::Tuple(ltup), Type::Tuple(rtup)) => {
            if ltup.len() != rtup.len() {
                let actual = Type::Tuple(ltup);
                let expected = Type::Tuple(rtup);
                return Err(TypeError::TypeMismatch { expected, actual });
            }

            for (lhs, rhs) in ltup.iter().zip(&rtup) {
                if check_type(lhs.clone(), rhs.clone()).is_err() {
                    let actual = Type::Tuple(ltup);
                    let expected = Type::Tuple(rtup);
                    return Err(TypeError::TypeMismatch { expected, actual });
                }
            }

            Ok(Type::Tuple(ltup))
        }
        (actual, expected) => Err(TypeError::TypeMismatch { expected, actual }),
    }
}

pub fn infer(val: &ASTNode, env: &mut Environment) -> Result<Type, (TypeError, Position)> {
    match &val.kind {
        ASTNodeKind::Integer { .. } => Ok(Type::Integer),
        ASTNodeKind::Decimal { .. } => Ok(Type::Float),
        ASTNodeKind::Assignment { left, right } => match left {
            crate::ast::AssignableSymbol::Pattern(pattern) => {
                infer_destructuring(pattern, right, val.position, env)
            }
            crate::ast::AssignableSymbol::ObjectValue {
                object: _,
                value: _,
            } => infer(right, env),
            crate::ast::AssignableSymbol::IndexableValue {
                container: _,
                index: _,
            } => infer(right, env),
        },
        ASTNodeKind::Boolean(_) => Ok(Type::Boolean),
        ASTNodeKind::Block(block) => infer_block(block, env),
        ASTNodeKind::Call { called, args } => infer_call(called, args, env),
        ASTNodeKind::Case { expr, pairs } => infer_case(expr, pairs, env),
        ASTNodeKind::Char(_) => Ok(Type::Char),
        ASTNodeKind::Comprehension { kind, .. } => Ok(infer_comprehension(*kind)),
        ASTNodeKind::DotNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::IndexNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::Dictionary { .. } => Ok(Type::Dictionary),
        ASTNodeKind::List { .. } => Ok(Type::List),
        ASTNodeKind::Set { .. } => Ok(Type::Set),
        ASTNodeKind::For { .. } => Ok(Type::empty_tuple()),
        ASTNodeKind::Function { params, result } => infer_function(params, result, env),
        ASTNodeKind::Fraction { .. } => Ok(Type::Fraction),
        ASTNodeKind::If {
            cond,
            positive,
            negative,
        } => infer_if(cond, positive, negative, env),
        ASTNodeKind::ImportFrom { source, values } => infer_import_from(source, values, env),
        ASTNodeKind::Infix { op, lhs, rhs } => infer_infix(*op, lhs, rhs, env),
        ASTNodeKind::Declaration(declaration) => infer_declaration(declaration, val.position, env),
        ASTNodeKind::Prefix { op, val } => infer_prefix(*op, val, env),
        ASTNodeKind::Cons { .. } => Ok(Type::List),
        ASTNodeKind::SetCons { .. } => Ok(Type::Set),
        ASTNodeKind::String { .. } => Ok(Type::String),
        ASTNodeKind::Symbol { name } => infer_symbol(name, val.position, env),
        ASTNodeKind::Tuple { list } => infer_tuple(list, env),
    }
}

fn infer_import_from(
    source: &ModuleAddress,
    values: &[(String, Position)],
    // err_pos: Position,
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    match source {
        ModuleAddress::StandardLibrary { name } => match name.as_str() {
            "json" => {
                for (name, position) in values {
                    let type_hint = match name.as_str() {
                        "parse" => {
                            let input = Box::new(Type::String);
                            let output = Box::new(Type::Dictionary);
                            Ok(Type::Function { input, output })
                        }
                        "stringify" => {
                            let input = Box::new(Type::Unknown);
                            let output = Box::new(Type::String);
                            Ok(Type::Function { input, output })
                        }
                        symbol => Err((
                            TypeError::UnknownSymbolInStd {
                                symbol: symbol.to_string(),
                                module: name.to_string(),
                            },
                            *position,
                        )),
                    }?;

                    env.set_type(name, type_hint);
                }
                Ok(Type::empty_tuple())
            }
            "math" => {
                for (name, position) in values {
                    let type_hint = match name.as_str() {
                        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "ln" | "log"
                        | "cbrt" | "sqrt" | "hypot" => Ok(Type::Function {
                            input: Box::new(Type::any_number()),
                            output: Box::new(Type::Float),
                        }),
                        "abs" => Ok(Type::Function {
                            input: Box::new(Type::any_number()),
                            output: Box::new(Type::any_number()),
                        }),
                        "round" | "floor" | "ceil" => Ok(Type::Function {
                            input: Box::new(Type::any_number()),
                            output: Box::new(Type::Integer),
                        }),
                        symbol => Err((
                            TypeError::UnknownSymbolInStd {
                                symbol: symbol.to_string(),
                                module: name.to_string(),
                            },
                            *position,
                        )),
                    }?;

                    env.set_type(name, type_hint);
                }
                Ok(Type::empty_tuple())
            }
            "time" => {
                for (name, position) in values {
                    let type_hint = match name.as_str() {
                        "time" => {
                            let input = Box::new(Type::empty_tuple());
                            let output = Box::new(Type::Float);
                            Ok(Type::Function { input, output })
                        }
                        "sleep" => {
                            let input = Box::new(Type::any_number());
                            let output = Box::new(Type::empty_tuple());
                            Ok(Type::Function { input, output })
                        }
                        symbol => Err((
                            TypeError::UnknownSymbolInStd {
                                symbol: symbol.to_string(),
                                module: name.to_string(),
                            },
                            *position,
                        )),
                    }?;

                    env.set_type(name, type_hint);
                }
                Ok(Type::empty_tuple())
            }
            name => {
                let path = get_std_path(env::var(STDLIB_PATH_VAR))
                    .join(Path::new(&format!("{name}.komodo")));
                let foreign_env = infer_module(path, env)?;

                infer_imported_values(values, env, &foreign_env)
            }
        },
        ModuleAddress::LocalPath { path } => {
            let reference_path = &env.ctx.reference_path;
            let abs_path = reference_path.join(Path::new(path));
            let foreign_env = infer_module(abs_path, env)?;

            infer_imported_values(values, env, &foreign_env)
        }
    }
}

fn infer_module(
    path: PathBuf,
    env: &mut Environment,
) -> Result<Environment, (TypeError, Position)> {
    let source = fs::read_to_string(path).unwrap();
    let lexer = Lexer::from((source.as_str(), env.ctx.executed_file_path.to_path_buf()));
    let parser = Parser::from(lexer);
    let nodes = collect_nodes(parser).unwrap();

    let mut temp_env = Environment::std_env(env.ctx.clone());

    for node in nodes {
        infer(&node, &mut temp_env)?;
    }
    Ok(temp_env)
}

fn infer_imported_values(
    values: &[(String, Position)],
    env: &mut Environment,
    foreign_env: &Environment,
) -> Result<Type, (TypeError, Position)> {
    for (name, pos) in values {
        if let Some(type_hint) = foreign_env.get_type(name) {
            env.set_type(name, type_hint.to_owned());
        } else {
            let name = name.to_owned();
            return Err((TypeError::UnknownSymbol { name }, *pos));
        }
    }

    Ok(Type::empty_tuple())
}

fn infer_declaration(
    declaration: &Declaration,
    position: Position,
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    match declaration {
        Declaration::Mutable { left, right } => infer_destructuring(left, right, position, env),
        Declaration::Inmutable { left, right } => infer_destructuring(left, right, position, env),
        Declaration::MemoizedFunction {
            name,
            params,
            result,
        } => infer_function_definition(name, params, result, position, env),
        Declaration::Symbolic {
            name: _,
            constraint: _,
        } => todo!(),
        Declaration::Function {
            name,
            params,
            result,
        } => infer_function_definition(name, params, result, position, env),
    }
}

fn infer_pattern_constant(constant: &Constant) -> Type {
    match constant {
        crate::ast::Constant::Boolean(_) => Type::Boolean,
        crate::ast::Constant::Char(_) => Type::Char,
        crate::ast::Constant::Decimal { .. } => Type::Float,
        crate::ast::Constant::Dictionary { .. } => Type::Dictionary,
        crate::ast::Constant::Integer { .. } => Type::Integer,
        crate::ast::Constant::List { .. } => Type::List,
        crate::ast::Constant::Set { .. } => Type::Set,
        crate::ast::Constant::String { .. } => Type::String,
        crate::ast::Constant::Tuple { list } => {
            Type::Tuple(list.iter().map(infer_pattern_constant).collect())
        }
    }
}

fn infer_pattern(pattern: &Pattern) -> Result<Type, TypeError> {
    match pattern {
        Pattern::Constant(constant) => Ok(infer_pattern_constant(constant)),
        Pattern::Dictionary { .. } => Ok(Type::Dictionary),
        Pattern::List { .. } => Ok(Type::List),
        Pattern::Set { .. } => Ok(Type::Set),
        Pattern::ListCons { .. } => Ok(Type::Either(Either::new(Type::List, Type::String))),
        Pattern::SetCons { .. } => Ok(Type::Set),
        Pattern::Symbol { .. } => Ok(Type::Unknown),
        Pattern::Tuple { list } => Ok(Type::Tuple(
            list.iter().map(infer_pattern).collect::<Result<_, _>>()?,
        )),
        Pattern::Range { .. } => Ok(Type::Range),
        Pattern::Fraction { .. } => Ok(Type::Fraction),
        Pattern::Signature {
            pattern,
            constraint,
        } => {
            let pat_type = infer_pattern(pattern)?;

            let constraint_type = type_from_type_hint(constraint)?;

            if constraint_type <= pat_type {
                Ok(constraint_type)
            } else {
                check_type(pat_type, constraint_type)
            }
        }
        Pattern::Either { lhs, rhs } => Ok(Type::Either(Either::new(
            infer_pattern(lhs)?,
            infer_pattern(rhs)?,
        ))),
        Pattern::Wildcard => Ok(Type::Unknown),
        Pattern::AdInfinitum => Ok(Type::Unknown),
    }
}

fn type_from_type_hint(type_hint: &TypeHint) -> Result<Type, TypeError> {
    match type_hint {
        TypeHint::Simple(typename) => lookup_type(typename),
        TypeHint::Either { lhs, rhs } => Ok(Type::Either(Either::new(
            type_from_type_hint(lhs)?,
            type_from_type_hint(rhs)?,
        ))),
    }
}

fn lookup_type(typename: &str) -> Result<Type, TypeError> {
    match typename {
        "Boolean" => Ok(Type::Boolean),
        "Char" => Ok(Type::Char),
        "Dictionary" => Ok(Type::Dictionary),
        "Float" => Ok(Type::Float),
        "Fraction" => Ok(Type::Fraction),
        "Integer" => Ok(Type::Integer),
        "List" => Ok(Type::List),
        "Range" => Ok(Type::Range),
        "Set" => Ok(Type::Set),
        "String" => Ok(Type::String),
        "Function" => Ok(Type::Function {
            input: Box::new(Type::Unknown),
            output: Box::new(Type::Unknown),
        }),
        "Tuple" => Ok(Type::Tuple(vec![])),
        name => Err(TypeError::UnknownType {
            name: name.to_string(),
        }),
    }
}

fn infer_function_definition(
    name: &str,
    params: &[Pattern],
    result: &ASTNode,
    err_pos: Position,
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    let input_type = match params.first() {
        None => Ok(Type::empty_tuple()),
        Some(param) => infer_pattern(param).map_err(|err| (err, err_pos)),
    }?;

    if let Some(Type::Function { input, output }) = env.get_type(name) {
        env.set_type(
            name,
            Type::Function {
                input: Box::new(join(input_type.clone(), *input.to_owned())),
                output: Box::new(*output.to_owned()),
            },
        );
    } else {
        env.set_type(
            name,
            Type::Function {
                input: Box::new(input_type.clone()),
                output: Box::new(Type::Unknown),
            },
        );
    }

    let output_type = env.within_new_level(|env| {
        for param in params {
            let introduced_symbols = get_introduced_symbols(param, err_pos)?;
            for (name, typ) in introduced_symbols {
                env.set_type(&name, typ);
            }
        }
        infer(result, env)
    })?;

    let res = if let Some(Type::Function { input, output }) = env.get_type(name) {
        Type::Function {
            input: input.clone(),
            output: Box::new(join(output_type, *output.to_owned())),
        }
    } else {
        Type::Function {
            input: Box::new(input_type),
            output: Box::new(output_type),
        }
    };

    env.set_type(name, res.clone());

    Ok(res)
}

fn infer_destructuring(
    pattern: &Pattern,
    value: &ASTNode,
    err_pos: Position,
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    let introduced_symbols = destructure(pattern, value, err_pos, env)?;

    for (name, typ) in introduced_symbols {
        env.set_type(&name, typ);
    }

    infer(value, env)
}

fn get_introduced_symbols(
    pattern: &Pattern,
    position: Position,
) -> Result<Vec<(String, Type)>, (TypeError, Position)> {
    let mut acc = vec![];
    get_introduced_symbols_helper(pattern, &mut acc).map_err(|err| (err, position))?;

    Ok(acc)
}

fn get_introduced_symbols_helper(
    pattern: &Pattern,
    acc: &mut Vec<(String, Type)>,
) -> Result<(), TypeError> {
    match pattern {
        Pattern::Constant(_) => return Ok(()),
        Pattern::Dictionary { pairs, complete: _ } => {
            for (pattern1, pattern2) in pairs {
                get_introduced_symbols_helper(pattern1, acc)?;
                get_introduced_symbols_helper(pattern2, acc)?;
            }
        }
        Pattern::List { list } => {
            for pattern in list {
                get_introduced_symbols_helper(pattern, acc)?;
            }
        }
        Pattern::Set { list } => {
            for pattern in list {
                get_introduced_symbols_helper(pattern, acc)?;
            }
        }
        Pattern::ListCons { first, tail } => {
            get_introduced_symbols_helper(first, acc)?;
            get_introduced_symbols_helper(tail, acc)?;
        }
        Pattern::SetCons { some, most } => {
            get_introduced_symbols_helper(some, acc)?;
            get_introduced_symbols_helper(most, acc)?;
        }
        Pattern::Symbol { name } => acc.push((name.to_string(), Type::Unknown)),
        Pattern::Tuple { list } => {
            for pattern in list {
                get_introduced_symbols_helper(pattern, acc)?;
            }
        }
        Pattern::Range { start, end } => {
            get_introduced_symbols_helper(start, acc)?;
            get_introduced_symbols_helper(end, acc)?;
        }
        Pattern::Fraction { numer, denom } => {
            get_introduced_symbols_helper(numer, acc)?;
            get_introduced_symbols_helper(denom, acc)?;
        }
        Pattern::Signature {
            pattern,
            constraint,
        } => match pattern.deref() {
            Pattern::Symbol { name } => {
                acc.push((name.to_string(), type_from_type_hint(constraint)?));
            }
            pattern => get_introduced_symbols_helper(pattern, acc)?,
        },
        Pattern::Either { lhs, rhs } => {
            get_introduced_symbols_helper(lhs, acc)?;
            get_introduced_symbols_helper(rhs, acc)?;
        }
        Pattern::Wildcard => return Ok(()),
        Pattern::AdInfinitum => return Ok(()),
    };

    Ok(())
}

fn destructure(
    pattern: &Pattern,
    value: &ASTNode,
    err_pos: Position,
    env: &mut Environment,
) -> Result<Vec<(String, Type)>, (TypeError, Position)> {
    let mut acc = vec![];
    destructure_helper(pattern, value, &mut acc, err_pos, env)?;

    Ok(acc)
}

fn destructure_helper(
    pattern: &Pattern,
    value: &ASTNode,
    acc: &mut Vec<(String, Type)>,
    err_pos: Position,
    env: &mut Environment,
) -> Result<(), (TypeError, Position)> {
    // TODO: actually contrast the pattern information with the value to destructure
    if let Pattern::Symbol { name } = pattern {
        let type_hint = infer(value, env)?;
        acc.push((name.to_string(), type_hint));
        Ok(())
    } else {
        get_introduced_symbols_helper(pattern, acc).map_err(|err| (err, err_pos))
    }
}

fn infer_infix(
    op: InfixOperator,
    lhs: &ASTNode,
    rhs: &ASTNode,
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    let lhs_type = infer(lhs, env)?;
    let rhs_type = infer(rhs, env)?;
    if lhs_type == Type::Unknown {
        return Ok(Type::Unknown);
    }
    if rhs_type == Type::Unknown {
        return Ok(Type::Unknown);
    }

    match op {
        InfixOperator::BitwiseAnd => {
            check(lhs, Type::Integer, env)?;
            check(rhs, Type::Integer, env)?;
            Ok(Type::Integer)
        }
        InfixOperator::BitwiseXor => {
            check(lhs, Type::Integer, env)?;
            check(rhs, Type::Integer, env)?;
            Ok(Type::Integer)
        }
        InfixOperator::Division => {
            infer_generic_arithmetic_op(op, infer(lhs, env)?, infer(rhs, env)?)
                .map_err(|err| (err, lhs.position.join(rhs.position)))
        }
        InfixOperator::Equality => Ok(Type::Boolean),
        InfixOperator::Exponentiation => match (infer(lhs, env)?, infer(rhs, env)?) {
            (Type::Integer, Type::Integer) => {
                Ok(Type::Either(Either::new(Type::Integer, Type::Float)))
            }
            (Type::Float, Type::Integer) => Ok(Type::Float),
            (Type::Fraction, Type::Integer) => Ok(Type::Fraction),
            (lhs_type, rhs_type) => Err((
                TypeError::NonExistentInfix {
                    op,
                    lhs_type,
                    rhs_type,
                },
                lhs.position.join(rhs.position),
            )),
        },
        InfixOperator::Greater => match (infer(lhs, env)?, infer(rhs, env)?) {
            (Type::Set, Type::Set) => Ok(Type::Boolean),
            (lhs_type, rhs_type) => {
                check_type(lhs_type, Type::any_number()).map_err(|err| (err, lhs.position))?;
                check_type(rhs_type, Type::any_number()).map_err(|err| (err, rhs.position))?;
                Ok(Type::Boolean)
            }
        },
        InfixOperator::GreaterEqual => match (infer(lhs, env)?, infer(rhs, env)?) {
            (Type::Set, Type::Set) => Ok(Type::Boolean),
            (lhs_type, rhs_type) => {
                check_type(lhs_type, Type::any_number()).map_err(|err| (err, lhs.position))?;
                check_type(rhs_type, Type::any_number()).map_err(|err| (err, rhs.position))?;
                Ok(Type::Boolean)
            }
        },
        InfixOperator::In => {
            infer(lhs, env)?;
            check(rhs, Type::Either(Either::new(Type::List, Type::Set)), env)?;
            Ok(Type::Boolean)
        }
        InfixOperator::LeftShift => {
            check(lhs, Type::Integer, env)?;
            check(rhs, Type::Integer, env)?;
            Ok(Type::Integer)
        }
        InfixOperator::Less => match (infer(lhs, env)?, infer(rhs, env)?) {
            (Type::Set, Type::Set) => Ok(Type::Boolean),
            (lhs_type, rhs_type) => {
                check_type(lhs_type, Type::any_number()).map_err(|err| (err, lhs.position))?;
                check_type(rhs_type, Type::any_number()).map_err(|err| (err, rhs.position))?;
                Ok(Type::Boolean)
            }
        },
        InfixOperator::LessEqual => match (infer(lhs, env)?, infer(rhs, env)?) {
            (Type::Set, Type::Set) => Ok(Type::Boolean),
            (lhs_type, rhs_type) => {
                check_type(lhs_type, Type::any_number()).map_err(|err| (err, lhs.position))?;
                check_type(rhs_type, Type::any_number()).map_err(|err| (err, rhs.position))?;
                Ok(Type::Boolean)
            }
        },
        InfixOperator::LogicAnd => {
            check(lhs, Type::Boolean, env)?;
            check(rhs, Type::Boolean, env)?;
            Ok(Type::Boolean)
        }
        InfixOperator::Or => {
            check(lhs, Type::Boolean, env)?;
            check(rhs, Type::Boolean, env)?;
            Ok(Type::Boolean)
        }
        InfixOperator::Rem => {
            check(lhs, Type::Integer, env)?;
            check(rhs, Type::Integer, env)?;
            Ok(Type::Integer)
        }
        InfixOperator::NotEquality => Ok(Type::Boolean),
        InfixOperator::Product => match (infer(lhs, env)?, infer(rhs, env)?) {
            (Type::List, Type::Integer) => Ok(Type::List),
            (Type::Integer, Type::List) => Ok(Type::List),
            (Type::Char, Type::Integer) => Ok(Type::String),
            (Type::Integer, Type::Char) => Ok(Type::String),
            (Type::String, Type::Integer) => Ok(Type::String),
            (Type::Integer, Type::String) => Ok(Type::String),
            (lhs_type, rhs_type) => infer_generic_arithmetic_op(op, lhs_type, rhs_type)
                .map_err(|err| (err, lhs.position.join(rhs.position))),
        },
        InfixOperator::Range => {
            check(lhs, Type::Integer, env)?;
            check(rhs, Type::Integer, env)?;
            Ok(Type::Range)
        }
        InfixOperator::RightShift => {
            check(lhs, Type::Integer, env)?;
            check(rhs, Type::Integer, env)?;
            Ok(Type::Integer)
        }
        InfixOperator::Substraction => match (infer(lhs, env)?, infer(rhs, env)?) {
            (Type::Set, Type::Set) => Ok(Type::Set),
            (lhs_type, rhs_type) => infer_generic_arithmetic_op(op, lhs_type, rhs_type)
                .map_err(|err| (err, lhs.position.join(rhs.position))),
        },
        InfixOperator::Sum => match (infer(lhs, env)?, infer(rhs, env)?) {
            (Type::List, Type::List) => Ok(Type::List),
            (Type::Set, Type::Set) => Ok(Type::Set),
            (Type::String, Type::String) => Ok(Type::String),
            (Type::Char, Type::Char) => Ok(Type::String),
            (Type::String, Type::Char) => Ok(Type::String),
            (Type::Char, Type::String) => Ok(Type::String),
            (lhs_type, rhs_type) => infer_generic_arithmetic_op(op, lhs_type, rhs_type)
                .map_err(|err| (err, lhs.position.join(rhs.position))),
        },
    }
}

fn infer_generic_arithmetic_op(
    op: InfixOperator,
    lhs_type: Type,
    rhs_type: Type,
) -> Result<Type, TypeError> {
    match (lhs_type, rhs_type) {
        (Type::Integer, Type::Integer) => Ok(Type::Integer),
        (Type::Integer, Type::Float) => Ok(Type::Float),
        (Type::Integer, Type::Fraction) => Ok(Type::Fraction),
        (Type::Float, Type::Integer) => Ok(Type::Float),
        (Type::Float, Type::Float) => Ok(Type::Float),
        (Type::Float, Type::Fraction) => Ok(Type::Float),
        (Type::Fraction, Type::Integer) => Ok(Type::Fraction),
        (Type::Fraction, Type::Float) => Ok(Type::Float),
        (Type::Fraction, Type::Fraction) => Ok(Type::Fraction),
        (lhs_type, rhs_type) => Err(TypeError::NonExistentInfix {
            op,
            lhs_type,
            rhs_type,
        }),
    }
}

fn infer_case(
    expr: &ASTNode,
    pairs: &[(Pattern, ASTNode)],
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    let _expr_type = infer(expr, env)?;

    let mut pairs = pairs.iter();

    let mut result = env.within_new_level(|env| {
        let (pattern, output) = pairs.next().unwrap();
        infer_destructuring(pattern, expr, expr.position, env)?;
        infer(output, env)
    })?;

    for (pattern, output) in pairs {
        let new_output_type = env.within_new_level(|env| {
            infer_destructuring(pattern, expr, expr.position, env)?;
            infer(output, env)
        })?;

        result = join(result, new_output_type);
    }

    Ok(result)
}

fn infer_function(
    params: &[String],
    result: &ASTNode,
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    let input = Box::new(if params.is_empty() {
        Type::empty_tuple()
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
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    let actual_input = Box::new(
        args.first()
            .map_or(Ok(Type::empty_tuple()), |typ| infer(typ, env))?,
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
        Type::Unknown => Ok(Type::Unknown),
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
    env: &mut Environment,
) -> Result<Type, (TypeError, Position)> {
    match op {
        PrefixOperator::BitwiseNot => check(val, Type::Integer, env),
        PrefixOperator::LogicNot => check(val, Type::Boolean, env),
        PrefixOperator::Minus => check(val, Type::any_number(), env),
    }
}

fn infer_block(block: &[ASTNode], env: &mut Environment) -> Result<Type, (TypeError, Position)> {
    env.within_new_level(|env| {
        let mut block = block.iter();
        let mut res = infer(block.next().unwrap(), env)?;

        for expr in block {
            res = infer(expr, env)?;
        }

        Ok(res)
    })
}

fn infer_symbol(
    name: &str,
    name_pos: Position,
    env: &mut Environment,
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

fn infer_tuple(vals: &[ASTNode], env: &mut Environment) -> Result<Type, (TypeError, Position)> {
    let vals_types: Result<Vec<Type>, (TypeError, Position)> =
        vals.iter().map(|val| infer(val, env)).collect();

    vals_types.map(Type::Tuple)
}

fn infer_if(
    cond: &ASTNode,
    positive: &ASTNode,
    negative: &ASTNode,
    env: &mut Environment,
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
    use crate::{
        ast::{
            tests::{
                _for, _if, assignable_pattern, assignment, block, boolean, call, case, char,
                comprehension, cons, dec_integer, dec_integer_pattern, decimal, dictionary,
                extension_list, extension_set, fraction, function, import_from, infix, let_,
                prefix, set_cons, string, string_pattern, symbol, symbol_pattern, tuple, wildcard,
            },
            ASTNode, InfixOperator,
        },
        cst::{tests::dummy_pos, ComprehensionKind},
        error::Position,
        run::ModuleAddress,
        typecheck::{check, infer, Either, Environment, Type, TypeError},
    };

    fn fresh_check(val: &ASTNode, expected: Type) -> Result<Type, (TypeError, Position)> {
        check(val, expected, &mut Environment::default())
    }

    fn fresh_infer(val: &ASTNode) -> Result<Type, (TypeError, Position)> {
        infer(val, &mut Environment::default())
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
        let mut env = Environment::default();

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
            Ok(Type::empty_tuple()),
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
                ModuleAddress::StandardLibrary {
                    name: "math".to_string()
                },
                vec![("cos", dummy_pos())],
                dummy_pos()
            )),
            Ok(Type::empty_tuple())
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
        let mut env = Environment::default();

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
        let mut env = Environment::default();

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

    #[test]
    fn infer_case() {
        assert_eq!(
            fresh_infer(&case(
                dec_integer("2", dummy_pos()),
                vec![
                    (dec_integer_pattern("1"), string("foo", dummy_pos())),
                    (string_pattern("foo"), dec_integer("1", dummy_pos())),
                ],
                dummy_pos(),
            )),
            Ok(Type::Either(Either::new(Type::Integer, Type::String)))
        );
    }

    #[test]
    fn infer_case_with_destructuring() {
        assert_eq!(
            fresh_infer(&case(
                dec_integer("5", dummy_pos()),
                vec![(symbol_pattern("foo"), symbol("foo", dummy_pos())),],
                dummy_pos(),
            )),
            Ok(Type::Integer),
        );
    }

    #[test]
    fn simple_let() {
        assert_eq!(
            fresh_infer(&block(
                vec![
                    let_(
                        symbol_pattern("x"),
                        dec_integer("5", dummy_pos()),
                        dummy_pos()
                    ),
                    infix(
                        InfixOperator::Sum,
                        symbol("x", dummy_pos()),
                        dec_integer("1", dummy_pos()),
                        dummy_pos()
                    ),
                ],
                dummy_pos()
            )),
            Ok(Type::Integer)
        );
    }
}
