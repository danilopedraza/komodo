use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq)]
enum ASTNode {
    INTEGER(i64),
}

struct Parser<T: Iterator<Item = Token>> {
    tokens: T,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNode, String>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.next() {
            Some(Token::INTEGER(int)) => Some(Ok(ASTNode::INTEGER(int))),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;
    use crate::lexer::Token;
    use super::*;

    macro_rules! token_iter {
        ($v:expr) => {
            $v.iter().map(|tok| tok.clone())
        };
    }

    fn parser_from<T: Iterator<Item = Token>>(tokens: T) -> Parser<T> {
        Parser { tokens }
    }

    #[test]
    fn empty_expression() {
        assert_eq!(parser_from(iter::empty::<Token>()).next(), None);
    }

    #[test]
    fn integer() {
        let tokens = vec![Token::INTEGER(0)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::INTEGER(0)))
        );
    }
}
