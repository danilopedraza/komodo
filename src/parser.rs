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
            None => None,
            Some(Token::Let) => Some(self.let_()),
            _ => Some(self.expression(Precedence::Lowest)),
        }
    }
}

impl <T: Iterator<Item = Token>> Parser<T> {
    fn let_(&mut self) -> Result<ASTNode, String> {
        self.tokens.next();

        match self.tokens.next() {
            Some(Token::Ident(str)) => match self.tokens.next() {
                Some(Token::Assign) => match self.expression(Precedence::Lowest) {
                    Ok(node) => Ok(ASTNode::Let(str, Box::new(node))),
                    err => err,
                },
                _ => Err(String::from("Expected an assignment symbol")),
            },
            _ => Err(String::from("Expected an identifier")),
        }
    }

    fn expression(&mut self, precedence: Precedence) -> Result<ASTNode, String> {
        let res = match self.tokens.next() {
            None => Err(String::from("Expected an expression but reached end of program")),
            Some(Token::Lparen) => self.parenthesis(),
            Some(tok) => match tok {
                Token::Integer(int) => Ok(ASTNode::Integer(int)),
                Token::Rparen => Err(String::from("Unexpected right parenthesis")),
                _ => todo!(),
            },
        };

        match (res, self.tokens.next_if(|tok| is_infix(tok.clone()) && precedence < prec(tok.clone()))) {
            (Ok(lhs), Some(op_tok)) => self.infix(lhs, op_tok.clone(), prec(op_tok)),
            (res, _) => res,
        }
    }

    fn parenthesis(&mut self) -> Result<ASTNode, String> {
        let res = self.expression(Precedence::Lowest);

        if self.tokens.next() == Some(Token::Rparen) {
            res
        } else {
            Err(String::from("Missing right parenthesis"))
        }
    }

    fn infix(&mut self, lhs: ASTNode, op: Token, precedence: Precedence) -> Result<ASTNode, String> {
        let res = match self.expression(precedence) {
            Ok(rhs) => Ok(match op {
                Token::Plus => ASTNode::Sum(
                    Box::new(lhs),
                    Box::new(rhs)
                ),
                Token::Times => ASTNode::Product(
                    Box::new(lhs),
                    Box::new(rhs)
                ),
                _ => todo!(),
            }),
            err => err,
        };

        match (res, self.tokens.next_if(|tok| is_infix(tok.clone()))) {
            (Ok(lhs), Some(op_tok)) => self.infix(lhs, op_tok.clone(), precedence),
            (res, _) => res,
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
            Some(Err(String::from("Expected an expression but reached end of program")))
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
