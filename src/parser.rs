use std::iter::Peekable;

use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq)]
enum ASTNode {
    INTEGER(i64),
    SUM(Box<ASTNode>, Box<ASTNode>),
    PRODUCT(Box<ASTNode>, Box<ASTNode>),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    LOWEST,
    ADDITION,
    MULTIPLICATION,
}

fn prec(tok: Token) -> Precedence {
    match tok {
        Token::PLUS => Precedence::ADDITION,
        Token::TIMES => Precedence::MULTIPLICATION,
        _ => Precedence::LOWEST,
    }
}

fn is_infix(tok: Token) -> bool {
    prec(tok) != Precedence::LOWEST // yeah lgtm
}

struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNode, String>;

    fn next(&mut self) -> Option<Self::Item> {
        self.expression(Precedence::LOWEST)
    }
}

impl <T: Iterator<Item = Token>> Parser<T> {
    fn expression(&mut self, precedence: Precedence) -> Option<Result<ASTNode, String>> {
        let res_opt = match self.tokens.next() {
            None => None,
            Some(Token::LPAREN) => self.parenthesis(),
            Some(tok) => Some(match tok {
                Token::INTEGER(int) => Ok(ASTNode::INTEGER(int)),
                Token::RPAREN => Err(String::from("Unexpected right parenthesis")),
                _ => todo!(),
            }),
        };

        match (res_opt, self.tokens.next_if(|tok| is_infix(tok.clone()) && precedence < prec(tok.clone()))) {
            (Some(Ok(lhs)), Some(op_tok)) => self.infix(lhs, op_tok.clone(), prec(op_tok)),
            (res_opt, _) => res_opt,
        }
    }

    fn parenthesis(&mut self) -> Option<Result<ASTNode, String>> {
        if let Some(_) = self.tokens.next_if_eq(&Token::RPAREN) {
            return None;
        }

        let res = self.expression(Precedence::LOWEST);

        if self.tokens.next() == Some(Token::RPAREN) {
            res
        } else {
            Some(Err(String::from("Missing right parenthesis")))
        }
    }

    fn infix(&mut self, lhs: ASTNode, op: Token, precedence: Precedence) -> Option<Result<ASTNode, String>> {
        let res_opt = match self.expression(precedence) {
            Some(Ok(rhs)) => Some(Ok(match op {
                Token::PLUS => ASTNode::SUM(
                    Box::new(lhs),
                    Box::new(rhs)
                ),
                Token::TIMES => ASTNode::PRODUCT(
                    Box::new(lhs),
                    Box::new(rhs)
                ),
                _ => todo!(),
            })),
            Some(err) => Some(err),
            None => Some(Err(match op {
                Token::PLUS => String::from("Missing right side of sum"),
                _ => todo!(),
            })),
        };

        match (res_opt, self.tokens.next_if(|tok| is_infix(tok.clone()))) {
            (Some(Ok(lhs)), Some(op_tok)) => self.infix(lhs, op_tok.clone(), precedence),
            (res_opt, _) => res_opt,
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

    #[test]
    fn incomplete_sum() {
        let tokens = vec![Token::INTEGER(1), Token::PLUS];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(String::from("Missing right side of sum")))
        );
    }

    #[test]
    fn simple_product() {
        let tokens = vec![Token::INTEGER(1), Token::TIMES, Token::INTEGER(1)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::PRODUCT(
                    Box::new(ASTNode::INTEGER(1)),
                    Box::new(ASTNode::INTEGER(1))
                )
            ))
        );
    }

    #[test]
    fn product_and_sum() {
        let tokens = vec![Token::INTEGER(1), Token::TIMES,
                                      Token::INTEGER(1), Token::PLUS, Token::INTEGER(1)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::SUM(
                    Box::new(ASTNode::PRODUCT(
                        Box::new(ASTNode::INTEGER(1)),
                        Box::new(ASTNode::INTEGER(1))
                    )),
                    Box::new(ASTNode::INTEGER(1))
                )
            )),
        );
    }
}
