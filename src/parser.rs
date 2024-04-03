use std::{iter::Peekable, vec};

use crate::ast::*;
use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    ExpectedExpression(Token),
    UnexpectedToken(Vec<Token>, Token),
    EOFReached,
    EOFExpecting(Vec<Token>),
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNode, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.peek() {
            None => None,
            _ => Some(self.expression(Precedence::Lowest)),
        }
    }
}

type NodeResult = Result<ASTNode, ParserError>;

impl<T: Iterator<Item = Token>> Parser<T> {
    #[allow(unused)]
    pub fn program(&mut self) -> Vec<ASTNode> {
        let mut res = vec![];

        loop {
            let exp = self.next();

            match exp {
                Some(Ok(node)) => res.push(node),
                _ => break res,
            }
        }
    }

    fn let_(&mut self) -> NodeResult {
        let sg = self.signature()?;

        match self.tokens.next() {
            Some(Token::Assign) => self
                .expression(Precedence::Lowest)
                .map(|res| ASTNode::Let(Box::new(sg), vec![], Box::new(res))),
            _ => Ok(sg),
        }
    }

    fn signature(&mut self) -> NodeResult {
        match (self.tokens.next(), self.tokens.peek()) {
            (Some(Token::Ident(name)), Some(Token::Colon)) => {
                self.tokens.next();
                self.type_().map(|tp| {
                    ASTNode::Signature(Box::new(ASTNode::Symbol(name)), Some(Box::new(tp)))
                })
            }
            (Some(Token::Ident(name)), Some(Token::Lparen)) => {
                self.tokens.next();
                self.let_function_with_arguments(name)
            }
            (Some(Token::Ident(name)), _) => {
                Ok(ASTNode::Signature(Box::new(ASTNode::Symbol(name)), None))
            }
            (Some(tok), _) => Err(ParserError::UnexpectedToken(
                vec![Token::Ident(String::from(""))],
                tok,
            )),
            (None, _) => Err(ParserError::EOFExpecting(vec![Token::Ident(String::from(
                "",
            ))])),
        }
    }

    fn let_function_with_arguments(&mut self, name: String) -> NodeResult {
        let args_res = self.list(Token::Rparen, None);

        match (args_res, self.tokens.next()) {
            (Ok(args), Some(Token::Assign)) => match self.expression(Precedence::Lowest) {
                Ok(expr) => Ok(ASTNode::Let(
                    Box::new(ASTNode::Symbol(name)),
                    args,
                    Box::new(expr),
                )),
                err => err,
            },
            (Err(err), _) => Err(err),
            (_, Some(tok)) => Err(ParserError::UnexpectedToken(vec![Token::Assign], tok)),
            (Ok(_), None) => Err(ParserError::EOFExpecting(vec![Token::Assign])),
        }
    }

    fn list(
        &mut self,
        terminator: Token,
        first: Option<ASTNode>,
    ) -> Result<Vec<ASTNode>, ParserError> {
        let mut res = match first {
            None => vec![],
            Some(node) => vec![node],
        };

        match self.tokens.peek() {
            Some(tok) if *tok == terminator => {
                self.tokens.next();
                Ok(res)
            }
            None => Err(ParserError::EOFReached),
            _ => loop {
                let expr = self.expression(Precedence::Lowest)?;
                res.push(expr);

                match self.tokens.next() {
                    Some(Token::Comma) => continue,
                    Some(tok) if tok == terminator => break Ok(res),
                    Some(tok) => {
                        return Err(ParserError::UnexpectedToken(
                            vec![Token::Comma, terminator],
                            tok,
                        ))
                    }
                    None => return Err(ParserError::EOFExpecting(vec![Token::Comma, terminator])),
                }
            },
        }
    }

    fn expression(&mut self, precedence: Precedence) -> NodeResult {
        let mut expr = match self.tokens.next() {
            None => Err(ParserError::EOFReached),
            Some(tok) => match tok {
                Token::Char(chr) => Ok(ASTNode::Char(chr)),
                Token::For => self.for_(),
                Token::If => self.if_(),
                Token::Let => self.let_(),
                Token::True => Ok(ASTNode::Boolean(true)),
                Token::False => Ok(ASTNode::Boolean(false)),
                Token::Lparen => self.parenthesis(),
                Token::Lbrace => self.set(),
                Token::Lbrack => self.my_list(),
                Token::Integer(int) => Ok(ASTNode::Integer(int)),
                Token::Ident(literal) => Ok(ASTNode::Symbol(literal)),
                Token::String(str) => Ok(ASTNode::String(str)),
                Token::Wildcard => Ok(ASTNode::Wildcard),
                tok if PrefixOperator::from(tok.clone()).is_some() => {
                    self.prefix(PrefixOperator::from(tok).unwrap())
                }
                tok => Err(ParserError::ExpectedExpression(tok)),
            },
        }?;

        while let Some(op) = self.current_infix() {
            if precedence < op.precedence() {
                self.tokens.next();
                expr = self.infix(expr, op)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn my_list(&mut self) -> NodeResult {
        if matches!(self.tokens.peek(), Some(Token::Rbrack)) {
            self.tokens.next();
            return Ok(ASTNode::ExtensionList(vec![]));
        }

        let first = self.expression(Precedence::Lowest)?;

        match self.tokens.next() {
            Some(Token::Colon) => self
                .expression(Precedence::Lowest)
                .map(|second| ASTNode::ComprehensionList(Box::new(first), Box::new(second))),
            Some(Token::Comma) => self
                .list(Token::Rbrack, Some(first))
                .map(ASTNode::ExtensionList),
            Some(Token::Rbrack) => Ok(ASTNode::ExtensionList(vec![first])),
            Some(tok) => Err(ParserError::UnexpectedToken(
                vec![Token::Colon, Token::Comma, Token::Rbrack],
                tok,
            )),
            None => Err(ParserError::EOFExpecting(vec![
                Token::Colon,
                Token::Comma,
                Token::Comma,
                Token::Rbrack,
            ])),
        }
    }

    fn for_(&mut self) -> NodeResult {
        let ident = match self.tokens.next() {
            Some(Token::Ident(s)) => Ok(s),
            Some(tok) => Err(ParserError::UnexpectedToken(
                vec![Token::Ident(String::from(""))],
                tok,
            )),
            None => Err(ParserError::EOFExpecting(vec![Token::Ident(String::from(
                "",
            ))])),
        }?;

        self.consume(Token::In)?;

        let iter = self.expression(Precedence::Lowest)?;

        self.consume(Token::Colon)?;

        let proc = match self.expression(Precedence::Lowest)? {
            ASTNode::Tuple(v) => v,
            node => vec![node],
        };

        Ok(ASTNode::For(ident, Box::new(iter), proc))
    }

    fn consume(&mut self, expected_tok: Token) -> Result<(), ParserError> {
        match self.tokens.next() {
            Some(tok) if tok == expected_tok => Ok(()),
            Some(tok) => Err(ParserError::UnexpectedToken(vec![expected_tok], tok)),
            None => Err(ParserError::EOFExpecting(vec![expected_tok])),
        }
    }

    fn if_(&mut self) -> NodeResult {
        let cond = self.expression(Precedence::Lowest)?;

        self.consume(Token::Then)?;

        let first_res = self.expression(Precedence::Lowest)?;

        self.consume(Token::Else)?;

        let second_res = self.expression(Precedence::Lowest)?;

        Ok(ASTNode::If(
            Box::new(cond),
            Box::new(first_res),
            Box::new(second_res),
        ))
    }

    fn current_infix(&mut self) -> Option<InfixOperator> {
        self.tokens
            .peek()
            .and_then(|tok| InfixOperator::from(tok.clone()))
    }

    fn prefix(&mut self, op: PrefixOperator) -> NodeResult {
        self.expression(Precedence::Highest)
            .map(|expr| ASTNode::Prefix(op, Box::new(expr)))
    }

    fn parenthesis(&mut self) -> NodeResult {
        if matches!(self.tokens.peek(), Some(&Token::Rparen)) {
            self.tokens.next();
            return Ok(ASTNode::Tuple(vec![]));
        }

        let res = self.expression(Precedence::Lowest)?;

        match self.tokens.next() {
            Some(Token::Rparen) => Ok(res),
            Some(Token::Comma) => self.list(Token::Rparen, Some(res)).map(ASTNode::Tuple),
            Some(tok) => Err(ParserError::UnexpectedToken(vec![Token::Rparen], tok)),
            None => Err(ParserError::EOFExpecting(vec![Token::Rparen])),
        }
    }

    fn infix(&mut self, lhs: ASTNode, op: InfixOperator) -> NodeResult {
        if op == InfixOperator::Call {
            self.list(Token::Rparen, None)
                .map(|args| ASTNode::Infix(op, Box::new(lhs), Box::new(ASTNode::Tuple(args))))
        } else {
            self.expression(op.precedence())
                .map(|rhs| ASTNode::Infix(op, Box::new(lhs), Box::new(rhs)))
        }
    }

    fn type_(&mut self) -> NodeResult {
        self.expression(Precedence::Lowest)
    }

    fn set(&mut self) -> NodeResult {
        if matches!(self.tokens.peek(), Some(&Token::Rbrace)) {
            self.tokens.next();
            return Ok(ASTNode::ExtensionSet(vec![]));
        }

        let first = self.expression(Precedence::Lowest)?;

        match self.tokens.next() {
            Some(Token::Comma) => self
                .list(Token::Rbrace, Some(first))
                .map(ASTNode::ExtensionSet),
            Some(Token::Colon) => self
                .expression(Precedence::Lowest)
                .map(|second| ASTNode::ComprehensionSet(Box::new(first), Box::new(second))),
            Some(Token::Rbrace) => Ok(ASTNode::ExtensionSet(vec![first])),
            Some(tok) => Err(ParserError::UnexpectedToken(
                vec![Token::Comma, Token::Rbrace, Token::Colon],
                tok,
            )),
            None => Err(ParserError::EOFExpecting(vec![
                Token::Comma,
                Token::Rbrace,
                Token::Colon,
            ])),
        }
    }
}

pub fn parser_from<T: Iterator<Item = Token>>(tokens: T) -> Parser<T> {
    Parser {
        tokens: tokens.peekable(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{build_lexer, Token};
    use std::{iter, vec};

    macro_rules! token_iter {
        ($v:expr) => {
            $v.iter().map(|tok| tok.clone())
        };
    }

    #[test]
    fn empty_expression() {
        assert_eq!(parser_from(iter::empty::<Token>()).next(), None);
    }

    #[test]
    fn integer() {
        let tokens = [Token::Integer(String::from("0"))];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Integer(String::from("0"))))
        );
    }

    #[test]
    fn integer_in_parenthesis() {
        let tokens = [
            Token::Lparen,
            Token::Integer(String::from("365")),
            Token::Rparen,
        ];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Integer(String::from("365"))))
        );
    }

    #[test]
    fn unbalanced_left_parenthesis() {
        let tokens = [Token::Lparen, Token::Integer(String::from("65"))];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(ParserError::EOFExpecting(vec![Token::Rparen])))
        );
    }

    #[test]
    fn simple_sum() {
        let tokens = [
            Token::Integer(String::from("1")),
            Token::Plus,
            Token::Integer(String::from("1")),
        ];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Sum,
                Box::new(ASTNode::Integer(String::from("1"))),
                Box::new(ASTNode::Integer(String::from("1")))
            )))
        );
    }

    #[test]
    fn incomplete_sum() {
        let tokens = [Token::Integer(String::from("1")), Token::Plus];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Err(ParserError::EOFReached))
        );
    }

    #[test]
    fn product_and_power() {
        let tokens = [
            Token::Integer(String::from("1")),
            Token::Times,
            Token::Integer(String::from("2")),
            Token::ToThe,
            Token::Integer(String::from("2")),
        ];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Product,
                Box::new(ASTNode::Integer(String::from("1"))),
                Box::new(ASTNode::Infix(
                    InfixOperator::Exponentiation,
                    Box::new(ASTNode::Integer(String::from("2"))),
                    Box::new(ASTNode::Integer(String::from("2"))),
                ))
            )))
        );
    }

    #[test]
    fn division_and_sum() {
        let tokens = [
            Token::Integer(String::from("1")),
            Token::Over,
            Token::Integer(String::from("1")),
            Token::Plus,
            Token::Integer(String::from("1")),
        ];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Sum,
                Box::new(ASTNode::Infix(
                    InfixOperator::Division,
                    Box::new(ASTNode::Integer(String::from("1"))),
                    Box::new(ASTNode::Integer(String::from("1")))
                )),
                Box::new(ASTNode::Integer(String::from("1")))
            ))),
        );
    }

    #[test]
    fn let_statement() {
        let tokens = [
            Token::Let,
            Token::Ident(String::from('x')),
            Token::Assign,
            Token::Integer(String::from("1")),
        ];
        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Let(
                Box::new(ASTNode::Signature(
                    Box::new(ASTNode::Symbol(String::from('x'))),
                    None
                )),
                vec![],
                Box::new(ASTNode::Integer(String::from("1")))
            ))),
        );
    }

    #[test]
    fn comparison_precedence() {
        let tokens = vec![
            Token::Integer(String::from("1")),
            Token::Plus,
            Token::Integer(String::from("5")),
            Token::NotEqual,
            Token::Integer(String::from("6")),
            Token::Mod,
            Token::Integer(String::from("2")),
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::NotEquality,
                Box::new(ASTNode::Infix(
                    InfixOperator::Sum,
                    Box::new(ASTNode::Integer(String::from("1"))),
                    Box::new(ASTNode::Integer(String::from("5"))),
                )),
                Box::new(ASTNode::Infix(
                    InfixOperator::Mod,
                    Box::new(ASTNode::Integer(String::from("6"))),
                    Box::new(ASTNode::Integer(String::from("2"))),
                ))
            )))
        );
    }

    #[test]
    fn let_function_statement() {
        let tokens = vec![
            Token::Let,
            Token::Ident(String::from('f')),
            Token::Lparen,
            Token::Ident(String::from('x')),
            Token::Comma,
            Token::Ident(String::from('y')),
            Token::Rparen,
            Token::Assign,
            Token::Ident(String::from('x')),
            Token::Plus,
            Token::Ident(String::from('y')),
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Let(
                Box::new(ASTNode::Symbol(String::from('f'))),
                vec![
                    ASTNode::Symbol(String::from('x')),
                    ASTNode::Symbol(String::from('y'))
                ],
                Box::new(ASTNode::Infix(
                    InfixOperator::Sum,
                    Box::new(ASTNode::Symbol(String::from('x'))),
                    Box::new(ASTNode::Symbol(String::from('y')))
                ))
            ))),
        );
    }

    #[test]
    fn let_function_signature() {
        let tokens = [
            Token::Let,
            Token::Ident(String::from('f')),
            Token::Colon,
            Token::Ident(String::from('a')),
            Token::Arrow,
            Token::Ident(String::from('a')),
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Signature(
                Box::new(ASTNode::Symbol(String::from('f'))),
                Some(Box::new(ASTNode::Infix(
                    InfixOperator::Correspondence,
                    Box::new(ASTNode::Symbol(String::from('a'))),
                    Box::new(ASTNode::Symbol(String::from('a')))
                )))
            ))),
        );
    }

    #[test]
    fn empty_set() {
        let tokens = [Token::Lbrace, Token::Rbrace];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::ExtensionSet(vec![])))
        );
    }

    #[test]
    fn set() {
        let tokens = vec![
            Token::Lbrace,
            Token::Lparen,
            Token::True,
            Token::Rparen,
            Token::Comma,
            Token::False,
            Token::Rbrace,
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::ExtensionSet(vec![
                ASTNode::Boolean(true),
                ASTNode::Boolean(false)
            ])))
        );
    }

    #[test]
    fn empty_tuple() {
        let tokens = [Token::Lparen, Token::Rparen];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Tuple(vec![])))
        );
    }

    #[test]
    fn tuple() {
        let tokens = [
            Token::Lparen,
            Token::Ident(String::from("Real")),
            Token::Comma,
            Token::Ident(String::from("Real")),
            Token::Rparen,
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Tuple(vec![
                ASTNode::Symbol(String::from("Real")),
                ASTNode::Symbol(String::from("Real"))
            ])))
        );
    }

    #[test]
    fn set_comprehension() {
        let tokens = [
            Token::Lbrace,
            Token::Ident(String::from("a")),
            Token::Colon,
            Token::Ident(String::from("a")),
            Token::Equals,
            Token::Integer(String::from("1")),
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::ComprehensionSet(
                Box::new(ASTNode::Symbol(String::from("a"))),
                Box::new(ASTNode::Infix(
                    InfixOperator::Equality,
                    Box::new(ASTNode::Symbol(String::from("a"))),
                    Box::new(ASTNode::Integer(String::from("1")))
                ))
            )))
        );
    }

    #[test]
    fn typed_let() {
        let tokens = vec![
            Token::Let,
            Token::Ident(String::from("x")),
            Token::Colon,
            Token::Ident(String::from("Real")),
            Token::Assign,
            Token::Integer(String::from("1")),
            Token::Plus,
            Token::Integer(String::from("0")),
            Token::Mod,
            Token::Integer(String::from("2")),
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Let(
                Box::new(ASTNode::Signature(
                    Box::new(ASTNode::Symbol(String::from("x"))),
                    Some(Box::new(ASTNode::Symbol(String::from("Real"))))
                )),
                vec![],
                Box::new(ASTNode::Infix(
                    InfixOperator::Sum,
                    Box::new(ASTNode::Integer(String::from("1"))),
                    Box::new(ASTNode::Infix(
                        InfixOperator::Mod,
                        Box::new(ASTNode::Integer(String::from("0"))),
                        Box::new(ASTNode::Integer(String::from("2")))
                    ))
                ))
            )))
        );
    }

    #[test]
    fn shift_operator() {
        let tokens = [
            Token::Ident(String::from('x')),
            Token::Minus,
            Token::Integer(String::from('1')),
            Token::LeftShift,
            Token::Integer(String::from('1')),
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::LeftShift,
                Box::new(ASTNode::Infix(
                    InfixOperator::Substraction,
                    Box::new(ASTNode::Symbol(String::from('x'))),
                    Box::new(ASTNode::Integer(String::from('1')))
                )),
                Box::new(ASTNode::Integer(String::from('1')))
            )))
        );
    }

    #[test]
    fn shift_and_comparison() {
        let lexer = build_lexer("1 << 1 > 1").map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Greater,
                Box::new(ASTNode::Infix(
                    InfixOperator::LeftShift,
                    Box::new(ASTNode::Integer(String::from('1'))),
                    Box::new(ASTNode::Integer(String::from('1')))
                )),
                Box::new(ASTNode::Integer(String::from('1')))
            )))
        );
    }

    #[test]
    fn bitwise() {
        let lexer = build_lexer("a & b | c").map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::BitwiseOr,
                Box::new(ASTNode::Infix(
                    InfixOperator::BitwiseAnd,
                    Box::new(ASTNode::Symbol(String::from('a'))),
                    Box::new(ASTNode::Symbol(String::from('b')))
                )),
                Box::new(ASTNode::Symbol(String::from('c')))
            )))
        );
    }

    #[test]
    fn logic_infix_operators() {
        let lexer = build_lexer("a && b || c");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::LogicOr,
                Box::new(ASTNode::Infix(
                    InfixOperator::LogicAnd,
                    Box::new(ASTNode::Symbol(String::from('a'))),
                    Box::new(ASTNode::Symbol(String::from('b')))
                )),
                Box::new(ASTNode::Symbol(String::from('c')))
            )))
        );
    }

    #[test]
    fn complex_precedence() {
        let lexer = build_lexer("a + b || a & b << c");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::LogicOr,
                Box::new(ASTNode::Infix(
                    InfixOperator::Sum,
                    Box::new(ASTNode::Symbol(String::from('a'))),
                    Box::new(ASTNode::Symbol(String::from('b')))
                )),
                Box::new(ASTNode::Infix(
                    InfixOperator::BitwiseAnd,
                    Box::new(ASTNode::Symbol(String::from('a'))),
                    Box::new(ASTNode::Infix(
                        InfixOperator::LeftShift,
                        Box::new(ASTNode::Symbol(String::from('b'))),
                        Box::new(ASTNode::Symbol(String::from('c')))
                    ))
                ))
            )))
        );
    }

    #[test]
    fn bitwise_xor() {
        let lexer = build_lexer("a ^ b & c | d");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::BitwiseOr,
                Box::new(ASTNode::Infix(
                    InfixOperator::BitwiseXor,
                    Box::new(ASTNode::Symbol(String::from('a'))),
                    Box::new(ASTNode::Infix(
                        InfixOperator::BitwiseAnd,
                        Box::new(ASTNode::Symbol(String::from('b'))),
                        Box::new(ASTNode::Symbol(String::from('c')))
                    ))
                )),
                Box::new(ASTNode::Symbol(String::from('d')))
            )))
        );
    }

    #[test]
    fn something_after_empty_set() {
        let lexer = build_lexer("({}, 0)");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Tuple(vec![
                ASTNode::ExtensionSet(vec![]),
                ASTNode::Integer(String::from('0'))
            ])))
        );
    }

    #[test]
    fn prefixes() {
        let lexer = build_lexer("!(~1 /= -1)");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Prefix(
                PrefixOperator::LogicNot,
                Box::new(ASTNode::Infix(
                    InfixOperator::NotEquality,
                    Box::new(ASTNode::Prefix(
                        PrefixOperator::BitwiseNot,
                        Box::new(ASTNode::Integer(String::from("1"))),
                    )),
                    Box::new(ASTNode::Prefix(
                        PrefixOperator::Minus,
                        Box::new(ASTNode::Integer(String::from("1"))),
                    )),
                ))
            )))
        );
    }

    #[test]
    fn if_expr() {
        let tokens = vec![
            Token::If,
            Token::Ident(String::from("a")),
            Token::Less,
            Token::Integer(String::from("0")),
            Token::Then,
            Token::Minus,
            Token::Ident(String::from("a")),
            Token::Else,
            Token::Ident(String::from("a")),
        ];

        assert_eq!(
            parser_from(token_iter!(tokens)).next(),
            Some(Ok(ASTNode::If(
                Box::new(ASTNode::Infix(
                    InfixOperator::Less,
                    Box::new(ASTNode::Symbol(String::from("a"))),
                    Box::new(ASTNode::Integer(String::from("0")))
                )),
                Box::new(ASTNode::Prefix(
                    PrefixOperator::Minus,
                    Box::new(ASTNode::Symbol(String::from("a")))
                )),
                Box::new(ASTNode::Symbol(String::from("a"))),
            )))
        );
    }

    #[test]
    fn program() {
        let input = "let a := 5
        a * a
        ";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).program(),
            vec![
                ASTNode::Let(
                    Box::new(ASTNode::Signature(
                        Box::new(ASTNode::Symbol(String::from("a"))),
                        None,
                    )),
                    vec![],
                    Box::new(ASTNode::Integer(String::from("5"))),
                ),
                ASTNode::Infix(
                    InfixOperator::Product,
                    Box::new(ASTNode::Symbol(String::from("a"))),
                    Box::new(ASTNode::Symbol(String::from("a"))),
                )
            ],
        );
    }

    #[test]
    fn function_call() {
        let input = "f(x, y)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Call,
                Box::new(ASTNode::Symbol(String::from("f"))),
                Box::new(ASTNode::Tuple(vec![
                    ASTNode::Symbol(String::from("x")),
                    ASTNode::Symbol(String::from("y")),
                ])),
            ))),
        );
    }

    #[test]
    fn comma_last_item() {
        let input = "(1,)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Tuple(vec![ASTNode::Integer(String::from(
                "1"
            ))]))),
        );
    }

    #[test]
    fn anon_function() {
        let input = "x -> 2*x";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Correspondence,
                Box::new(ASTNode::Symbol(String::from("x"))),
                Box::new(ASTNode::Infix(
                    InfixOperator::Product,
                    Box::new(ASTNode::Integer(String::from("2"))),
                    Box::new(ASTNode::Symbol(String::from("x"))),
                ))
            )))
        );
    }

    #[test]
    fn anon_function_call() {
        let input = "((x, y) -> x)(1, 2)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Call,
                Box::new(ASTNode::Infix(
                    InfixOperator::Correspondence,
                    Box::new(ASTNode::Tuple(vec![
                        ASTNode::Symbol(String::from("x")),
                        ASTNode::Symbol(String::from("y")),
                    ])),
                    Box::new(ASTNode::Symbol(String::from("x"))),
                )),
                Box::new(ASTNode::Tuple(vec![
                    ASTNode::Integer(String::from("1")),
                    ASTNode::Integer(String::from("2")),
                ])),
            )))
        );
    }

    #[test]
    fn char_and_string() {
        let input = "('a', \"b\")";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Tuple(vec![
                ASTNode::Char('a'),
                ASTNode::String(String::from('b')),
            ])))
        );
    }

    #[test]
    fn for_loop() {
        let input = "for i in list: println(i)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::For(
                String::from("i"),
                Box::new(ASTNode::Symbol(String::from("list"))),
                vec![ASTNode::Infix(
                    InfixOperator::Call,
                    Box::new(ASTNode::Symbol(String::from("println"))),
                    Box::new(ASTNode::Tuple(vec![ASTNode::Symbol(String::from("i"))])),
                )]
            )))
        );
    }

    #[test]
    fn in_question() {
        let input = "1 in { k : k >= 1 }";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::In,
                Box::new(ASTNode::Integer(String::from("1"))),
                Box::new(ASTNode::ComprehensionSet(
                    Box::new(ASTNode::Symbol(String::from("k"))),
                    Box::new(ASTNode::Infix(
                        InfixOperator::GreaterEqual,
                        Box::new(ASTNode::Symbol(String::from("k"))),
                        Box::new(ASTNode::Integer(String::from("1"))),
                    )),
                )),
            )))
        );
    }

    #[test]
    fn list() {
        let input = "[[], 2]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::ExtensionList(vec![
                ASTNode::ExtensionList(vec![]),
                ASTNode::Integer(String::from("2")),
            ]),)),
        );
    }

    #[test]
    fn comprehension_list() {
        let input = "[ k in [1, 2] : k - 1 = 0 ]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::ComprehensionList(
                Box::new(ASTNode::Infix(
                    InfixOperator::In,
                    Box::new(ASTNode::Symbol(String::from("k"))),
                    Box::new(ASTNode::ExtensionList(vec![
                        ASTNode::Integer(String::from("1")),
                        ASTNode::Integer(String::from("2")),
                    ])),
                )),
                Box::new(ASTNode::Infix(
                    InfixOperator::Equality,
                    Box::new(ASTNode::Infix(
                        InfixOperator::Substraction,
                        Box::new(ASTNode::Symbol(String::from("k"))),
                        Box::new(ASTNode::Integer(String::from("1"))),
                    )),
                    Box::new(ASTNode::Integer(String::from("0"))),
                )),
            )))
        );
    }

    #[test]
    fn singleton_empty_set() {
        let input = "{{}}";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::ExtensionSet(vec![ASTNode::ExtensionSet(
                vec![]
            )]))),
        );
    }

    #[test]
    fn wildcard() {
        let input = "[a, 1, _]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::ExtensionList(vec![
                ASTNode::Symbol(String::from("a")),
                ASTNode::Integer(String::from("1")),
                ASTNode::Wildcard,
            ]),)),
        );
    }
}
