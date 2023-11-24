use std::iter::Peekable;

use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq)]
enum ASTNode {
    INTEGER(i64),
    SUM(Box<ASTNode>, Box<ASTNode>),
}

struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNode, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = match self.tokens.next() {
            None => None,
            Some(Token::LPAREN) => self.parenthesis(),
            Some(tok) => Some(match tok {
                Token::INTEGER(int) => Ok(ASTNode::INTEGER(int)),
                Token::RPAREN => Err(String::from("Unexpected right parenthesis")),
                _ => todo!(),
            }),
        };

        match self.tokens.next_if_eq(&Token::PLUS) {
            None => res,
            Some(_) => self.sum(res),
        }
    }
}

impl <T: Iterator<Item = Token>> Parser<T> {
    fn parenthesis(&mut self) -> Option<Result<ASTNode, String>> {
        if let Some(_) = self.tokens.next_if_eq(&Token::RPAREN) {
            return None;
        }

        let res = self.next();

        if self.tokens.next() == Some(Token::RPAREN) {
            res
        } else {
            Some(Err(String::from("Missing right parenthesis")))
        }
    }

    fn sum(&mut self, res: Option<Result<ASTNode, String>>) -> Option<Result<ASTNode, String>> {
        match (res, self.next()) {
            (Some(Ok(lhs)), Some(Ok(rhs))) => Some(Ok(
                ASTNode::SUM(
                    Box::new(lhs),
                    Box::new(rhs)
                )
            )),
            _ => todo!(),
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
        Parser { tokens: tokens.peekable() }
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
    fn empty_parenthesis() {
        let tokens = vec![Token::LPAREN, Token::RPAREN];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            None
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
            Some(Err(String::from("Missing right parenthesis")))
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

    #[test]
    fn simple_sum() {
        let tokens = vec![Token::INTEGER(1), Token::PLUS, Token::INTEGER(1)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::SUM(
                    Box::new(ASTNode::INTEGER(1)),
                    Box::new(ASTNode::INTEGER(1))
                )
            ))
        );
    }
}
