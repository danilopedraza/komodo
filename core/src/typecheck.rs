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
    Float,
    Integer,
    Unknown,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum Type {
    Single(SingleType),
    Either(SingleType, Box<Type>),
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
        ASTNodeKind::AdInfinitum => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Assignment { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Boolean(_) => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Block(_) => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Call { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Case { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Char(_) => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Comprehension { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::DotNotation { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::IndexNotation { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Dictionary { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::List { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Set { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::For { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Function { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Fraction { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::If { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::ImportFrom { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Infix { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Declaration(_) => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::TaggedExpression { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Prefix { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Cons { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::SetCons { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::String { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Symbol { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Tuple { .. } => Ok(Type::Single(SingleType::Unknown)),
        ASTNodeKind::Wildcard => Ok(Type::Single(SingleType::Unknown)),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::tests::{dec_integer, decimal},
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
            Ok(true)
        )
    }
}
