use crate::{
    ast::{ASTNode, ASTNodeKind},
    error::Position,
};

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum TypeError {}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum SingleType {
    Boolean,
    Float,
    Integer,
    String,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum Type {
    Single(SingleType),
    Function { input: Box<Type>, output: Box<Type> },
    Tuple(Vec<Type>),
    Either(Box<Type>, Box<Type>),
    Unknown,
}

#[allow(dead_code)]
fn check(val: &ASTNode, sig: Type) -> Result<bool, (TypeError, Position)> {
    Ok(infer(val)? == sig)
}

#[allow(dead_code)]
fn infer(val: &ASTNode) -> Result<Type, (TypeError, Position)> {
    match &val.kind {
        ASTNodeKind::Integer { .. } => Ok(Type::Single(SingleType::Integer)),
        ASTNodeKind::Decimal { .. } => Ok(Type::Single(SingleType::Float)),
        ASTNodeKind::Assignment { .. } => Ok(Type::Unknown),
        ASTNodeKind::Boolean(_) => Ok(Type::Single(SingleType::Boolean)),
        ASTNodeKind::Block(_) => Ok(Type::Unknown),
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
        ASTNodeKind::String { .. } => Ok(Type::Single(SingleType::String)),
        ASTNodeKind::Symbol { .. } => Ok(Type::Unknown),
        ASTNodeKind::Tuple { .. } => Ok(Type::Unknown),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::tests::{boolean, dec_integer, decimal, string},
        cst::tests::dummy_pos,
        typecheck::{check, infer, SingleType, Type},
    };

    #[test]
    fn integer_check() {
        assert_eq!(
            check(
                &dec_integer("10", dummy_pos()),
                Type::Single(SingleType::Integer),
            ),
            Ok(true)
        )
    }

    #[test]
    fn integer_infer() {
        assert_eq!(
            infer(&dec_integer("0", dummy_pos())),
            Ok(Type::Single(SingleType::Integer)),
        );
    }

    #[test]
    fn float_check() {
        assert_eq!(
            check(
                &decimal("0", "0", dummy_pos()),
                Type::Single(SingleType::Float),
            ),
            Ok(true)
        )
    }

    #[test]
    fn float_infer() {
        assert_eq!(
            infer(&decimal("0", "0", dummy_pos())),
            Ok(Type::Single(SingleType::Float))
        );
    }

    #[test]
    fn bad_check() {
        assert_eq!(
            check(
                &decimal("0", "0", dummy_pos()),
                Type::Single(SingleType::Integer),
            ),
            Ok(false)
        )
    }

    #[test]
    fn string_infer() {
        assert_eq!(
            infer(&string("foo", dummy_pos())),
            Ok(Type::Single(SingleType::String)),
        );
    }

    #[test]
    fn boolean_infer() {
        assert_eq!(
            infer(&boolean(true, dummy_pos())),
            Ok(Type::Single(SingleType::Boolean)),
        );
    }
}
