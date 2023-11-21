use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq)]
enum ASTNode {}

struct Parser<T: Iterator<Item = Token>> {
    tokens: T,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNode, String>;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{build_lexer, Token};
    use super::Parser;

    fn parser_from<T: Iterator<Item = Token>>(tokens: T) -> Parser<T> {
        Parser { tokens }
    }

    #[test]
    fn empty_expression() {
        assert_eq!(parser_from(build_lexer("")).next(), None);
    }
}
