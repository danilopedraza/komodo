use crate::ast::{ASTNode, ASTNodeKind};

#[derive(Debug, PartialEq, Eq)]
enum SingleType {
    Float,
    Integer,
}

#[derive(Debug, PartialEq, Eq)]
enum Type {
    Single(SingleType),
    Either(SingleType, Box<Type>),
}

fn check(val: ASTNode, sig: Type) -> bool {
    match sig {
        Type::Single(single_type) => check_single_type(val, single_type),
        Type::Either(lsig, rsig) => check_either_type(val, lsig, *rsig),
    }
}

fn check_single_type(val: ASTNode, sig: SingleType) -> bool {
    match (val.kind, sig) {
        (ASTNodeKind::Integer { .. }, SingleType::Integer) => true,
        (ASTNodeKind::Decimal { .. }, SingleType::Float) => true,
        _ => false,
    }
}

fn check_either_type(val: ASTNode, lsig: SingleType, rsig: Type) -> bool {
    false
}

fn infer(val: ASTNode) -> Type {
    Type::Single(SingleType::Integer)
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
            dec_integer("10", dummy_pos()),
            Type::Single(SingleType::Integer)
        ))
    }

    #[test]
    fn integer_infer() {
        assert_eq!(
            infer(dec_integer("0", dummy_pos())),
            Type::Single(SingleType::Integer)
        );
    }

    #[test]
    fn float_check() {
        assert!(check(
            decimal("0", "0", dummy_pos()),
            Type::Single(SingleType::Float)
        ))
    }

    #[test]
    fn bad_check() {
        assert!(!check(
            decimal("0", "0", dummy_pos()),
            Type::Single(SingleType::Integer)
        ))
    }
}
