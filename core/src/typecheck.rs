use crate::ast::{ASTNode, ASTNodeKind};

#[derive(Debug, PartialEq, Eq)]
enum SingleType {
    Float,
    Integer,
    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
enum Type {
    Single(SingleType),
    Either(SingleType, Box<Type>),
}

fn check(val: &ASTNode, sig: Type) -> bool {
    infer(val) == sig
}

fn infer(val: &ASTNode) -> Type {
    match &val.kind {
        ASTNodeKind::Integer { .. } => Type::Single(SingleType::Integer),
        ASTNodeKind::Decimal { .. } => Type::Single(SingleType::Float),
        ASTNodeKind::AdInfinitum => Type::Single(SingleType::Unknown),
        ASTNodeKind::Assignment { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Boolean(_) => Type::Single(SingleType::Unknown),
        ASTNodeKind::Block(_) => Type::Single(SingleType::Unknown),
        ASTNodeKind::Call { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Case { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Char(_) => Type::Single(SingleType::Unknown),
        ASTNodeKind::Comprehension { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::DotNotation { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::IndexNotation { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Dictionary { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::List { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Set { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::For { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Function { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Fraction { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::If { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::ImportFrom { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Infix { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Declaration(_) => Type::Single(SingleType::Unknown),
        ASTNodeKind::TaggedExpression { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Prefix { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Cons { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::SetCons { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::String { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Symbol { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Tuple { .. } => Type::Single(SingleType::Unknown),
        ASTNodeKind::Wildcard => Type::Single(SingleType::Unknown),
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
        assert!(check(
            &dec_integer("10", dummy_pos()),
            Type::Single(SingleType::Integer),
        ))
    }

    #[test]
    fn integer_infer() {
        assert_eq!(
            infer(&dec_integer("0", dummy_pos())),
            Type::Single(SingleType::Integer),
        );
    }

    #[test]
    fn float_check() {
        assert!(check(
            &decimal("0", "0", dummy_pos()),
            Type::Single(SingleType::Float),
        ))
    }

    #[test]
    fn float_infer() {
        assert_eq!(
            infer(&decimal("0", "0", dummy_pos())),
            Type::Single(SingleType::Float)
        );
    }

    #[test]
    fn bad_check() {
        assert!(!check(
            &decimal("0", "0", dummy_pos()),
            Type::Single(SingleType::Integer),
        ))
    }
}
