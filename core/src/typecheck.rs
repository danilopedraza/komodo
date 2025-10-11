use std::collections::HashMap;

use crate::{
    ast::{ASTNode, ASTNodeKind},
    cst::ComprehensionKind,
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
    Either(Box<Type>, Box<Type>),
    Unknown,
}

#[allow(dead_code)]
fn check(
    val: &ASTNode,
    expected: Type,
    env: &mut SymbolTable,
) -> Result<(), (TypeError, Position)> {
    let actual = infer(val, env)?;

    if expected == actual {
        Ok(())
    } else {
        Err((TypeError::TypeMismatch { expected, actual }, val.position))
    }
}

#[allow(dead_code)]
fn infer(val: &ASTNode, env: &mut SymbolTable) -> Result<Type, (TypeError, Position)> {
    match &val.kind {
        ASTNodeKind::Integer { .. } => Ok(Type::Integer),
        ASTNodeKind::Decimal { .. } => Ok(Type::Float),
        ASTNodeKind::Assignment { .. } => Ok(Type::Unknown),
        ASTNodeKind::Boolean(_) => Ok(Type::Boolean),
        ASTNodeKind::Block(block) => infer_block(block, env),
        ASTNodeKind::Call { .. } => Ok(Type::Unknown),
        ASTNodeKind::Case { .. } => Ok(Type::Unknown),
        ASTNodeKind::Char(_) => Ok(Type::Char),
        ASTNodeKind::Comprehension { kind, .. } => Ok(infer_comprehension(*kind)),
        ASTNodeKind::DotNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::IndexNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::Dictionary { .. } => Ok(Type::Dictionary),
        ASTNodeKind::List { .. } => Ok(Type::List),
        ASTNodeKind::Set { .. } => Ok(Type::Set),
        ASTNodeKind::For { .. } => Ok(Type::Tuple(vec![])),
        ASTNodeKind::Function { .. } => Ok(Type::Unknown),
        ASTNodeKind::Fraction { .. } => Ok(Type::Fraction),
        ASTNodeKind::If {
            cond,
            positive,
            negative,
        } => infer_if(cond, positive, negative, env),
        ASTNodeKind::ImportFrom { .. } => Ok(Type::Tuple(vec![])),
        ASTNodeKind::Infix { .. } => Ok(Type::Unknown),
        ASTNodeKind::Declaration(_) => Ok(Type::Unknown),
        ASTNodeKind::Prefix { .. } => Ok(Type::Unknown),
        ASTNodeKind::Cons { .. } => Ok(Type::List),
        ASTNodeKind::SetCons { .. } => Ok(Type::Set),
        ASTNodeKind::String { .. } => Ok(Type::String),
        ASTNodeKind::Symbol { name } => infer_symbol(name, val.position, env),
        ASTNodeKind::Tuple { list } => infer_tuple(list, env),
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

    vals_types.map(|types| Type::Tuple(types))
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
    if left == right {
        left
    } else {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{
        ast::{
            tests::{
                _for, _if, block, boolean, char, comprehension, cons, dec_integer, decimal,
                dictionary, extension_list, extension_set, fraction, import_from, set_cons, string,
                symbol, tuple, wildcard,
            },
            ASTNode,
        },
        cst::{tests::dummy_pos, ComprehensionKind},
        error::Position,
        run::ModuleAddress,
        typecheck::{check, infer, SymbolTable, Type, TypeError},
    };

    fn fresh_check(val: &ASTNode, expected: Type) -> Result<(), (TypeError, Position)> {
        check(val, expected, &mut SymbolTable::default())
    }

    fn fresh_infer(val: &ASTNode) -> Result<Type, (TypeError, Position)> {
        infer(val, &mut SymbolTable::default())
    }

    #[test]
    fn integer_check() {
        assert_eq!(
            fresh_check(&dec_integer("10", dummy_pos()), Type::Integer,),
            Ok(())
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
            Ok(())
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
}
