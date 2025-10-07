use std::collections::HashMap;

use crate::{
    ast::{ASTNode, ASTNodeKind},
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
    Float,
    Integer,
    List,
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
        ASTNodeKind::Char(_) => Ok(Type::Unknown),
        ASTNodeKind::Comprehension { .. } => Ok(Type::Unknown),
        ASTNodeKind::DotNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::IndexNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::Dictionary { .. } => Ok(Type::Unknown),
        ASTNodeKind::List { .. } => Ok(Type::List),
        ASTNodeKind::Set { .. } => Ok(Type::Unknown),
        ASTNodeKind::For { .. } => Ok(Type::Unknown),
        ASTNodeKind::Function { .. } => Ok(Type::Unknown),
        ASTNodeKind::Fraction { .. } => Ok(Type::Unknown),
        ASTNodeKind::If { .. } => Ok(Type::Unknown),
        ASTNodeKind::ImportFrom { .. } => Ok(Type::Unknown),
        ASTNodeKind::Infix { .. } => Ok(Type::Unknown),
        ASTNodeKind::Declaration(_) => Ok(Type::Unknown),
        ASTNodeKind::Prefix { .. } => Ok(Type::Unknown),
        ASTNodeKind::Cons { .. } => Ok(Type::Unknown),
        ASTNodeKind::SetCons { .. } => Ok(Type::Unknown),
        ASTNodeKind::String { .. } => Ok(Type::String),
        ASTNodeKind::Symbol { name } => infer_symbol(name, val.position, env),
        ASTNodeKind::Tuple { .. } => Ok(Type::Unknown),
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

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            tests::{block, boolean, dec_integer, decimal, extension_list, string, symbol},
            ASTNode,
        },
        cst::tests::dummy_pos,
        error::Position,
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
}
