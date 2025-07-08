use crate::ast::ASTNode;

#[derive(Debug, PartialEq, Eq)]
enum SingleType {
    Integer,
}

#[derive(Debug, PartialEq, Eq)]
enum Type {
    Single(SingleType),
    Either(SingleType, Box<Type>),
}

fn check(val: ASTNode, sig: Type) -> bool {
    true
}

fn infer(val: ASTNode) -> Type {
    Type::Single(SingleType::Integer)
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::tests::dec_integer,
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
}
