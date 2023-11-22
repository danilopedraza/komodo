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
            None => None,
            Some(Token::LPAREN) => self.parenthesis(),
            Some(tok) => Some(match tok {
                Token::INTEGER(int) => Ok(ASTNode::INTEGER(int)),
                Token::RPAREN => Err(String::from("Unexpected right parenthesis")),
                _ => todo!(),
            }),
        }
    }
}

impl <T: Iterator<Item = Token>> Parser<T> {
    fn parenthesis(&mut self) -> Option<Result<ASTNode, String>> {
        let res = self.next();
        if self.tokens.next() == Some(Token::RPAREN) {
            res
        } else {
            Some(Err(String::from("Missing left parenthesis")))
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

    #[test]
    fn integer_in_parenthesis() {
        let tokens = vec![Token::LPAREN, Token::INTEGER(365), Token::RPAREN];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::INTEGER(365)))
        );
    }

    #[test]
    fn unbalanced_left_parenthesis() {
        let tokens = vec![Token::LPAREN, Token::INTEGER(21)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(String::from("Missing left parenthesis")))
        );
    }

    #[test]
    fn unbalanced_right_parenthesis() {
        let tokens = vec![Token::RPAREN];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(String::from("Unexpected right parenthesis")))
        );
    }
}
