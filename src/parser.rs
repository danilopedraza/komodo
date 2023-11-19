use crate::lexer::Lexer;

#[derive(Debug, PartialEq, Eq)]
enum ASTNode {}

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl Iterator for Parser<'_> {
    type Item = Result<ASTNode, String>;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, build_lexer};
    use super::Parser;

    fn parser_from(lexer: Lexer) -> Parser {
        Parser { lexer }
    }

    #[test]
    fn empty_expression() {
        assert_eq!(parser_from(build_lexer("")).next(), None);
    }
}
