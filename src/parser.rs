use std::{iter::Peekable, vec};

use crate::lexer::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ASTNode {
    Integer(i64),
    Let(Box<ASTNode>, Vec<ASTNode>, Box<ASTNode>),
    LetType(Box<ASTNode>, Box<ASTNode>),
    FunctionSignature(Box<ASTNode>, Box<ASTNode>),
    Sum(Box<ASTNode>, Box<ASTNode>),
    Symbol(String),
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
            Some(Token::Ident(name)) => match self.tokens.next() {
                Some(Token::Colon) => match self.function_signature() {
                    Ok(sig) => Ok(ASTNode::LetType(Box::new(ASTNode::Symbol(name)), Box::new(sig))),
                    err => err,
                },
                Some(Token::Lparen) => {
                    let args_res = self.arguments();

                    match (args_res, self.tokens.next()) {
                        (Ok(args), Some(Token::Assign)) => match self.expression(Precedence::Lowest) {
                            Ok(expr) => Ok(ASTNode::Let(Box::new(ASTNode::Symbol(name)), args, Box::new(expr))),
                            err => err,
                        },
                        (Err(err), _) => Err(err),
                        (_, _) => Err(String::from("Expected an assignment symbol")),
                    }
                }
                Some(Token::Assign) => match self.expression(Precedence::Lowest) {
                    Ok(expr) => Ok(ASTNode::Let(Box::new(ASTNode::Symbol(name)), vec![], Box::new(expr))),
                    err => err,
                },
                _ => Err(String::from("Expected a left parenthesis, a colon or an assignment symbol")),
            },
            _ => Err(String::from("Expected an identifier")),
        }
    }

    fn arguments(&mut self) -> Result<Vec<ASTNode>, String> {
        let mut res = vec![];
        while let Some(Token::Ident(literal)) = self.tokens.next_if(|tok| matches!(tok, Token::Ident(_))) {
            res.push(ASTNode::Symbol(literal));
            
            match self.tokens.next() {
                Some(Token::Comma) => continue,
                Some(Token::Rparen) => break,
                _ => return Err(String::from("Expected a comma or a right parenthesis")),
            }
        }

        Ok(res)
    }

    fn expression(&mut self, precedence: Precedence) -> Result<ASTNode, String> {
        let res = match self.tokens.next() {
            None => Err(String::from("Expected an expression but reached end of program")),
            Some(tok) => match tok {
                Token::Lparen => self.parenthesis(),
                Token::Integer(int) => Ok(ASTNode::Integer(int)),
                Token::Rparen => Err(String::from("Unexpected right parenthesis")),
                Token::Ident(literal) => Ok(ASTNode::Symbol(literal)),
                tok => {println!("{:?}", tok); todo!()},
            },
        };

        match (res, self.tokens.next_if(|tok| is_infix(tok.clone()) && precedence < prec(tok.clone()))) {
            (Ok(lhs), Some(op_tok)) => self.infix(lhs, op_tok.clone(), prec(op_tok)),
            (res, _) => res,
        }
    }

    fn parenthesis(&mut self) -> Result<ASTNode, String> {
        if let Some(_) = self.tokens.next_if_eq(&Token::Rparen) {
            return Err(String::from("Empty parenthesis"));
        }

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

    fn function_signature(&mut self) -> Result<ASTNode, String> {
        match self.tokens.next() {
            Some(Token::Ident(lhs)) => match self.tokens.next() {
                Some(Token::Arrow) => match self.tokens.next() {
                    Some(Token::Ident(rhs)) => Ok(ASTNode::FunctionSignature(
                            Box::new(ASTNode::Symbol(lhs)),
                            Box::new(ASTNode::Symbol(rhs))
                        )
                    ),
                    _ => todo!(),
                },
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{iter, vec};
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

    #[test]
    fn empty_parenthesis() {
        let tokens = vec![Token::Lparen, Token::Rparen];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(String::from("Empty parenthesis")))
        );
    }

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
                    Box::new(ASTNode::Symbol(String::from('x'))),
                    vec![],
                    Box::new(ASTNode::Integer(1))
                )
            )),
        );
    }

    #[test]
    fn let_function_statement() {
        let tokens = vec![
            Token::Let, Token::Ident(String::from('f')), Token::Lparen ,Token::Ident(String::from('x')), Token::Comma, Token::Ident(String::from('y')), Token::Rparen,
            Token::Assign,
            Token::Ident(String::from('x')), Token::Plus, Token::Ident(String::from('y'))
        ];
        
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::Let(
                    Box::new(ASTNode::Symbol(String::from('f'))),
                    vec![ASTNode::Symbol(String::from('x')), ASTNode::Symbol(String::from('y'))],
                    Box::new(
                        ASTNode::Sum(
                            Box::new(ASTNode::Symbol(String::from('x'))),
                            Box::new(ASTNode::Symbol(String::from('y')))
                        )
                    )
                )
            )),
        );
    }

    #[test]
    fn let_function_signature() {
        let tokens = vec![
            Token::Let, Token::Ident(String::from('f')), Token::Colon, Token::Ident(String::from('a')), Token::Arrow, Token::Ident(String::from('a'))
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(
                ASTNode::LetType(
                    Box::new(ASTNode::Symbol(String::from('f'))),
                    Box::new(
                        ASTNode::FunctionSignature(
                            Box::new(ASTNode::Symbol(String::from('a'))),
                            Box::new(ASTNode::Symbol(String::from('a')))
                        )
                    )
                )
            )),
        );
    }
}
