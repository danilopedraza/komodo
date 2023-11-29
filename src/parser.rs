use std::iter::Peekable;

use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq)]
enum ASTNode {
    Integer(i64),
    Let(String, Box<ASTNode>),
    Sum(Box<ASTNode>, Box<ASTNode>),
    Product(Box<ASTNode>, Box<ASTNode>),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Addition,
    Multiplication,
}

fn prec(tok: Token) -> Precedence {
    match tok {
        Token::Plus => Precedence::Addition,
        Token::Times => Precedence::Multiplication,
        _ => Precedence::Lowest,
    }
}

fn is_infix(tok: Token) -> bool {
    prec(tok) != Precedence::Lowest // yeah lgtm
}

struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNode, String>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.peek() {
            Some(Token::Let) => self.let_(),
            _ => self.expression(Precedence::Lowest),
        }
    }
}

impl <T: Iterator<Item = Token>> Parser<T> {
    fn let_(&mut self) -> Option<Result<ASTNode, String>> {
        self.tokens.next();

        match self.tokens.next() {
            Some(Token::Ident(str)) => match self.tokens.next() {
                Some(Token::Assign) => match self.expression(Precedence::Lowest) {
                    Some(Ok(node)) => Some(Ok(ASTNode::Let(str, Box::new(node)))),
                    res_opt => res_opt,
                },
                _ => Some(Err(String::from("Expected an assignment symbol"))),
            },
            _ => Some(Err(String::from("Expected an identifier"))),
        }
    }

    fn expression(&mut self, precedence: Precedence) -> Option<Result<ASTNode, String>> {
        let res_opt = match self.tokens.next() {
            None => None,
            Some(Token::Lparen) => self.parenthesis(),
            Some(tok) => Some(match tok {
                Token::Integer(int) => Ok(ASTNode::Integer(int)),
                Token::Rparen => Err(String::from("Unexpected right parenthesis")),
                _ => todo!(),
            }),
        };

        match (res_opt, self.tokens.next_if(|tok| is_infix(tok.clone()) && precedence < prec(tok.clone()))) {
            (Some(Ok(lhs)), Some(op_tok)) => self.infix(lhs, op_tok.clone(), prec(op_tok)),
            (res_opt, _) => res_opt,
        }
    }

    fn parenthesis(&mut self) -> Option<Result<ASTNode, String>> {
        let res = self.expression(Precedence::Lowest);

        if self.tokens.next() == Some(Token::Rparen) {
            res
        } else {
            Some(Err(String::from("Missing right parenthesis")))
        }
    }

    fn infix(&mut self, lhs: ASTNode, op: Token, precedence: Precedence) -> Option<Result<ASTNode, String>> {
        let res_opt = match self.expression(precedence) {
            Some(Ok(rhs)) => Some(Ok(match op {
                Token::Plus => ASTNode::Sum(
                    Box::new(lhs),
                    Box::new(rhs)
                ),
                Token::Times => ASTNode::Product(
                    Box::new(lhs),
                    Box::new(rhs)
                ),
                _ => todo!(),
            })),
            Some(err) => Some(err),
            None => Some(Err(match op {
                Token::Plus => String::from("Missing right side of sum"),
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
        let tokens = vec![Token::Integer(0)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Integer(0)))
        );
    }

    // not sure about this behavior
    // #[test]
    // fn empty_parenthesis() {
    //     let tokens = vec![Token::Lparen, Token::Rparen];
    //     assert_eq!(
    //         parser_from(token_iter!(tokens)).next(),
    //         None
    //     );
    // }

    #[test]
    fn integer_in_parenthesis() {
        let tokens = vec![Token::Lparen, Token::Integer(365), Token::Rparen];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Integer(365)))
        );
    }

    #[test]
    fn unbalanced_left_parenthesis() {
        let tokens = vec![Token::Lparen, Token::Integer(21)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(String::from("Missing right parenthesis")))
        );
    }

    #[test]
    fn unbalanced_right_parenthesis() {
        let tokens = vec![Token::Rparen];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(String::from("Unexpected right parenthesis")))
        );
    }

    #[test]
    fn simple_sum() {
        let tokens = vec![Token::Integer(1), Token::Plus, Token::Integer(1)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Sum(
                    Box::new(ASTNode::Integer(1)),
                    Box::new(ASTNode::Integer(1))
                )
            ))
        );
    }

    #[test]
    fn incomplete_sum() {
        let tokens = vec![Token::Integer(1), Token::Plus];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(String::from("Missing right side of sum")))
        );
    }

    #[test]
    fn simple_product() {
        let tokens = vec![Token::Integer(1), Token::Times, Token::Integer(1)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Product(
                    Box::new(ASTNode::Integer(1)),
                    Box::new(ASTNode::Integer(1))
                )
            ))
        );
    }

    #[test]
    fn product_and_sum() {
        let tokens = vec![Token::Integer(1), Token::Times,
                                      Token::Integer(1), Token::Plus, Token::Integer(1)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Sum(
                    Box::new(ASTNode::Product(
                        Box::new(ASTNode::Integer(1)),
                        Box::new(ASTNode::Integer(1))
                    )),
                    Box::new(ASTNode::Integer(1))
                )
            )),
        );
    }

    #[test]
    fn let_statement() {
        let tokens = vec![Token::Let, Token::Ident(String::from('x')), Token::Assign, Token::Integer(1)];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Let(
                    String::from('x'),
                    Box::new(ASTNode::Integer(1))
                )
            )),
        );
    }
}
