use crate::{
    ast::{ASTNode, ASTNodeKind},
    error::Position,
};

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum TypeError {
    TypeMismatch {
        expected: Type,
        actual: Type,
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum Type {
    Boolean,
    Float,
    Integer,
    String,
    Function { input: Box<Type>, output: Box<Type> },
    Tuple(Vec<Type>),
    Either(Box<Type>, Box<Type>),
    Unknown,
}

#[allow(dead_code)]
fn check(val: &ASTNode, expected: Type) -> Result<(), (TypeError, Position)> {
    let actual = infer(val)?;

    if expected == actual {
        Ok(())
    } else {
        Err((TypeError::TypeMismatch { expected, actual }, val.position))
    }
}

#[allow(dead_code)]
fn infer(val: &ASTNode) -> Result<Type, (TypeError, Position)> {
    match &val.kind {
        ASTNodeKind::Integer { .. } => Ok(Type::Integer),
        ASTNodeKind::Decimal { .. } => Ok(Type::Float),
        ASTNodeKind::Assignment { .. } => Ok(Type::Unknown),
        ASTNodeKind::Boolean(_) => Ok(Type::Boolean),
        ASTNodeKind::Block(block) => infer_block(block),
        ASTNodeKind::Call { .. } => Ok(Type::Unknown),
        ASTNodeKind::Case { .. } => Ok(Type::Unknown),
        ASTNodeKind::Char(_) => Ok(Type::Unknown),
        ASTNodeKind::Comprehension { .. } => Ok(Type::Unknown),
        ASTNodeKind::DotNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::IndexNotation { .. } => Ok(Type::Unknown),
        ASTNodeKind::Dictionary { .. } => Ok(Type::Unknown),
        ASTNodeKind::List { .. } => Ok(Type::Unknown),
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
        ASTNodeKind::Symbol { .. } => Ok(Type::Unknown),
        ASTNodeKind::Tuple { .. } => Ok(Type::Unknown),
    }
}

fn infer_block(block: &[ASTNode]) -> Result<Type, (TypeError, Position)> {
    infer(block.last().unwrap())
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::tests::{block, boolean, dec_integer, decimal, string},
        cst::tests::dummy_pos,
        typecheck::{check, infer, Type, TypeError},
    };

    #[test]
    fn integer_check() {
        assert_eq!(
            check(&dec_integer("10", dummy_pos()), Type::Integer,),
            Ok(())
        )
    }

    #[test]
    fn integer_infer() {
        assert_eq!(infer(&dec_integer("0", dummy_pos())), Ok(Type::Integer),);
    }

    #[test]
    fn float_check() {
        assert_eq!(
            check(&decimal("0", "0", dummy_pos()), Type::Float,),
            Ok(())
        )
    }

    #[test]
    fn float_infer() {
        assert_eq!(infer(&decimal("0", "0", dummy_pos())), Ok(Type::Float));
    }

    #[test]
    fn bad_check() {
        assert_eq!(
            check(&decimal("0", "0", dummy_pos()), Type::Integer,),
            Err((TypeError::TypeMismatch { expected: Type::Integer, actual: Type::Float }, dummy_pos()))
        )
    }

    #[test]
    fn string_infer() {
        assert_eq!(infer(&string("foo", dummy_pos())), Ok(Type::String),);
    }

    #[test]
    fn boolean_infer() {
        assert_eq!(infer(&boolean(true, dummy_pos())), Ok(Type::Boolean),);
    }

    #[test]
    fn block_infer() {
        assert_eq!(
            infer(&block(
                vec![string("foo", dummy_pos()), dec_integer("10", dummy_pos()),],
                dummy_pos()
            )),
            Ok(Type::Integer),
        );
    }
}
