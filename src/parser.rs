use std::{iter::Peekable, vec};

use crate::ast::*;
use crate::lexer::{Token, TokenType};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    ExpectedExpression(TokenType),
    UnexpectedToken(Vec<TokenType>, TokenType),
    EOFReached,
    EOFExpecting(Vec<TokenType>),
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNode, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_token() {
            None => None,
            _ => Some(self.expression(Precedence::Lowest)),
        }
    }
}

type NodeResult = Result<ASTNode, ParserError>;

impl<T: Iterator<Item = Token>> Parser<T> {
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

    fn next_token(&mut self) -> Option<TokenType> {
        match self.tokens.next() {
            Some(Token { token, .. }) => Some(token),
            _ => None,
        }
    }

    fn peek_token(&mut self) -> Option<TokenType> {
        match self.tokens.peek() {
            Some(Token { token, .. }) => Some(token.to_owned()),
            _ => None,
        }
    }

    fn let_(&mut self) -> NodeResult {
        let sg = self.signature()?;

        match self.next_token() {
            Some(TokenType::Assign) => self
                .expression(Precedence::Lowest)
                .map(|res| ASTNode::Let(Box::new(sg), vec![], Box::new(res))),
            _ => Ok(sg),
        }
    }

    fn signature(&mut self) -> NodeResult {
        match (self.next_token(), self.peek_token()) {
            (Some(TokenType::Ident(name)), Some(TokenType::Colon)) => {
                self.next_token();
                self.type_().map(|tp| {
                    ASTNode::Signature(Box::new(ASTNode::Symbol(name)), Some(Box::new(tp)))
                })
            }
            (Some(TokenType::Ident(name)), Some(TokenType::Lparen)) => {
                self.next_token();
                self.let_function_with_arguments(name)
            }
            (Some(TokenType::Ident(name)), _) => {
                Ok(ASTNode::Signature(Box::new(ASTNode::Symbol(name)), None))
            }
            (Some(tok), _) => Err(ParserError::UnexpectedToken(
                vec![TokenType::Ident(String::from(""))],
                tok,
            )),
            (None, _) => Err(ParserError::EOFExpecting(vec![TokenType::Ident(
                String::from(""),
            )])),
        }
    }

    fn let_function_with_arguments(&mut self, name: String) -> NodeResult {
        let args_res = self.list(TokenType::Rparen, None);

        match (args_res, self.next_token()) {
            (Ok(args), Some(TokenType::Assign)) => match self.expression(Precedence::Lowest) {
                Ok(expr) => Ok(ASTNode::Let(
                    Box::new(ASTNode::Symbol(name)),
                    args,
                    Box::new(expr),
                )),
                err => err,
            },
            (Err(err), _) => Err(err),
            (_, Some(tok)) => Err(ParserError::UnexpectedToken(vec![TokenType::Assign], tok)),
            (Ok(_), None) => Err(ParserError::EOFExpecting(vec![TokenType::Assign])),
        }
    }

    fn list(
        &mut self,
        terminator: TokenType,
        first: Option<ASTNode>,
    ) -> Result<Vec<ASTNode>, ParserError> {
        let mut res = match first {
            None => vec![],
            Some(node) => vec![node],
        };

        match self.peek_token() {
            Some(tok) if tok == terminator => {
                self.next_token();
                Ok(res)
            }
            None => Err(ParserError::EOFReached),
            _ => loop {
                let expr = self.expression(Precedence::Lowest)?;
                res.push(expr);

                match self.next_token() {
                    Some(TokenType::Comma) => continue,
                    Some(tok) if tok == terminator => break Ok(res),
                    Some(tok) => {
                        break Err(ParserError::UnexpectedToken(
                            vec![TokenType::Comma, terminator],
                            tok,
                        ))
                    }
                    None => {
                        break Err(ParserError::EOFExpecting(vec![
                            TokenType::Comma,
                            terminator,
                        ]))
                    }
                }
            },
        }
    }

    fn expression(&mut self, precedence: Precedence) -> NodeResult {
        let mut expr = match self.next_token() {
            None => Err(ParserError::EOFReached),
            Some(tok) => match tok {
                TokenType::Char(chr) => Ok(ASTNode::Char(chr)),
                TokenType::For => self.for_(),
                TokenType::If => self.if_(),
                TokenType::Let => self.let_(),
                TokenType::True => Ok(ASTNode::Boolean(true)),
                TokenType::False => Ok(ASTNode::Boolean(false)),
                TokenType::Lparen => self.parenthesis(),
                TokenType::Lbrace => self.set(),
                TokenType::Lbrack => self.my_list(),
                TokenType::Integer(int) => Ok(ASTNode::Integer(int)),
                TokenType::Ident(literal) => Ok(ASTNode::Symbol(literal)),
                TokenType::String(str) => Ok(ASTNode::String(str)),
                TokenType::Wildcard => Ok(ASTNode::Wildcard),
                tok if PrefixOperator::from(tok.clone()).is_some() => {
                    self.prefix(PrefixOperator::from(tok).unwrap())
                }
                tok => Err(ParserError::ExpectedExpression(tok)),
            },
        }?;

        while let Some(op) = self.current_infix() {
            if precedence < op.precedence() {
                self.next_token();
                expr = self.infix(expr, op)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn my_list(&mut self) -> NodeResult {
        if matches!(self.peek_token(), Some(TokenType::Rbrack)) {
            self.next_token();
            return Ok(ASTNode::ExtensionList(vec![]));
        }

        let first = self.expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Colon) => self.comprehension_list(first),
            Some(TokenType::Comma) => self
                .list(TokenType::Rbrack, Some(first))
                .map(ASTNode::ExtensionList),
            Some(TokenType::Rbrack) => Ok(ASTNode::ExtensionList(vec![first])),
            Some(TokenType::VerticalBar) => self.prepend(first),
            Some(tok) => Err(ParserError::UnexpectedToken(
                vec![TokenType::Colon, TokenType::Comma, TokenType::Rbrack],
                tok,
            )),
            None => Err(ParserError::EOFExpecting(vec![
                TokenType::Colon,
                TokenType::Comma,
                TokenType::Comma,
                TokenType::Rbrack,
            ])),
        }
    }

    fn comprehension_list(&mut self, first: ASTNode) -> NodeResult {
        let last = self.expression(Precedence::Lowest)?;
        self.consume(TokenType::Rbrack)?;

        Ok(ASTNode::ComprehensionList(Box::new(first), Box::new(last)))
    }

    fn prepend(&mut self, first: ASTNode) -> NodeResult {
        let last = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Rbrack)?;

        Ok(ASTNode::Prepend(Box::new(first), Box::new(last)))
    }

    fn for_(&mut self) -> NodeResult {
        let ident = match self.next_token() {
            Some(TokenType::Ident(s)) => Ok(s),
            Some(tok) => Err(ParserError::UnexpectedToken(
                vec![TokenType::Ident(String::from(""))],
                tok,
            )),
            None => Err(ParserError::EOFExpecting(vec![TokenType::Ident(
                String::from(""),
            )])),
        }?;

        self.consume(TokenType::In)?;

        let iter = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Colon)?;

        let proc = match self.expression(Precedence::Lowest)? {
            ASTNode::Tuple(v) => v,
            node => vec![node],
        };

        Ok(ASTNode::For(ident, Box::new(iter), proc))
    }

    fn consume(&mut self, expected_tok: TokenType) -> Result<(), ParserError> {
        match self.next_token() {
            Some(tok) if tok == expected_tok => Ok(()),
            Some(tok) => Err(ParserError::UnexpectedToken(vec![expected_tok], tok)),
            None => Err(ParserError::EOFExpecting(vec![expected_tok])),
        }
    }

    fn if_(&mut self) -> NodeResult {
        let cond = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Then)?;

        let first_res = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Else)?;

        let second_res = self.expression(Precedence::Lowest)?;

        Ok(ASTNode::If(
            Box::new(cond),
            Box::new(first_res),
            Box::new(second_res),
        ))
    }

    fn current_infix(&mut self) -> Option<InfixOperator> {
        self.peek_token().and_then(InfixOperator::from)
    }

    fn prefix(&mut self, op: PrefixOperator) -> NodeResult {
        self.expression(Precedence::Highest)
            .map(|expr| ASTNode::Prefix(op, Box::new(expr)))
    }

    fn parenthesis(&mut self) -> NodeResult {
        if matches!(self.peek_token(), Some(TokenType::Rparen)) {
            self.next_token();
            return Ok(ASTNode::Tuple(vec![]));
        }

        let res = self.expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Rparen) => Ok(res),
            Some(TokenType::Comma) => self.list(TokenType::Rparen, Some(res)).map(ASTNode::Tuple),
            Some(tok) => Err(ParserError::UnexpectedToken(vec![TokenType::Rparen], tok)),
            None => Err(ParserError::EOFExpecting(vec![TokenType::Rparen])),
        }
    }

    fn infix(&mut self, lhs: ASTNode, op: InfixOperator) -> NodeResult {
        if op == InfixOperator::Call {
            self.list(TokenType::Rparen, None)
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
        if matches!(self.peek_token(), Some(TokenType::Rbrace)) {
            self.next_token();
            return Ok(ASTNode::ExtensionSet(vec![]));
        }

        let first = self.expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Comma) => self
                .list(TokenType::Rbrace, Some(first))
                .map(ASTNode::ExtensionSet),
            Some(TokenType::Colon) => self
                .expression(Precedence::Lowest)
                .map(|second| ASTNode::ComprehensionSet(Box::new(first), Box::new(second))),
            Some(TokenType::Rbrace) => Ok(ASTNode::ExtensionSet(vec![first])),
            Some(tok) => Err(ParserError::UnexpectedToken(
                vec![TokenType::Comma, TokenType::Rbrace, TokenType::Colon],
                tok,
            )),
            None => Err(ParserError::EOFExpecting(vec![
                TokenType::Comma,
                TokenType::Rbrace,
                TokenType::Colon,
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
    use crate::{error::Position, lexer::build_lexer};
    use std::iter;

    fn iter_from(v: Vec<TokenType>) -> impl Iterator<Item = Token> {
        v.into_iter()
            .map(|tok| Token::new(tok, Position::new(0, 0)))
    }

    #[test]
    fn empty_expression() {
        assert_eq!(parser_from(iter::empty::<Token>()).next(), None);
    }

    #[test]
    fn integer() {
        let tokens = vec![TokenType::Integer(String::from("0"))];
        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Ok(ASTNode::Integer(String::from("0"))))
        );
    }

    #[test]
    fn integer_in_parenthesis() {
        let tokens = vec![
            TokenType::Lparen,
            TokenType::Integer(String::from("365")),
            TokenType::Rparen,
        ];
        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Ok(ASTNode::Integer(String::from("365"))))
        );
    }

    #[test]
    fn unbalanced_left_parenthesis() {
        let tokens = vec![TokenType::Lparen, TokenType::Integer(String::from("65"))];
        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Err(ParserError::EOFExpecting(vec![TokenType::Rparen])))
        );
    }

    #[test]
    fn simple_sum() {
        let tokens = vec![
            TokenType::Integer(String::from("1")),
            TokenType::Plus,
            TokenType::Integer(String::from("1")),
        ];
        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Sum,
                Box::new(ASTNode::Integer(String::from("1"))),
                Box::new(ASTNode::Integer(String::from("1")))
            )))
        );
    }

    #[test]
    fn incomplete_sum() {
        let tokens = vec![TokenType::Integer(String::from("1")), TokenType::Plus];
        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Err(ParserError::EOFReached))
        );
    }

    #[test]
    fn product_and_power() {
        let tokens = vec![
            TokenType::Integer(String::from("1")),
            TokenType::Times,
            TokenType::Integer(String::from("2")),
            TokenType::ToThe,
            TokenType::Integer(String::from("2")),
        ];
        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
        let tokens = vec![
            TokenType::Integer(String::from("1")),
            TokenType::Over,
            TokenType::Integer(String::from("1")),
            TokenType::Plus,
            TokenType::Integer(String::from("1")),
        ];
        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
        let tokens = vec![
            TokenType::Let,
            TokenType::Ident(String::from('x')),
            TokenType::Assign,
            TokenType::Integer(String::from("1")),
        ];
        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
            TokenType::Integer(String::from("1")),
            TokenType::Plus,
            TokenType::Integer(String::from("5")),
            TokenType::NotEqual,
            TokenType::Integer(String::from("6")),
            TokenType::Mod,
            TokenType::Integer(String::from("2")),
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
            TokenType::Let,
            TokenType::Ident(String::from('f')),
            TokenType::Lparen,
            TokenType::Ident(String::from('x')),
            TokenType::Comma,
            TokenType::Ident(String::from('y')),
            TokenType::Rparen,
            TokenType::Assign,
            TokenType::Ident(String::from('x')),
            TokenType::Plus,
            TokenType::Ident(String::from('y')),
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
        let tokens = vec![
            TokenType::Let,
            TokenType::Ident(String::from('f')),
            TokenType::Colon,
            TokenType::Ident(String::from('a')),
            TokenType::Arrow,
            TokenType::Ident(String::from('a')),
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
        let tokens = vec![TokenType::Lbrace, TokenType::Rbrace];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Ok(ASTNode::ExtensionSet(vec![])))
        );
    }

    #[test]
    fn set() {
        let tokens = vec![
            TokenType::Lbrace,
            TokenType::Lparen,
            TokenType::True,
            TokenType::Rparen,
            TokenType::Comma,
            TokenType::False,
            TokenType::Rbrace,
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Ok(ASTNode::ExtensionSet(vec![
                ASTNode::Boolean(true),
                ASTNode::Boolean(false)
            ])))
        );
    }

    #[test]
    fn empty_tuple() {
        let tokens = vec![TokenType::Lparen, TokenType::Rparen];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Ok(ASTNode::Tuple(vec![])))
        );
    }

    #[test]
    fn tuple() {
        let tokens = vec![
            TokenType::Lparen,
            TokenType::Ident(String::from("Real")),
            TokenType::Comma,
            TokenType::Ident(String::from("Real")),
            TokenType::Rparen,
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
            Some(Ok(ASTNode::Tuple(vec![
                ASTNode::Symbol(String::from("Real")),
                ASTNode::Symbol(String::from("Real"))
            ])))
        );
    }

    #[test]
    fn set_comprehension() {
        let tokens = vec![
            TokenType::Lbrace,
            TokenType::Ident(String::from("a")),
            TokenType::Colon,
            TokenType::Ident(String::from("a")),
            TokenType::Equals,
            TokenType::Integer(String::from("1")),
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
            TokenType::Let,
            TokenType::Ident(String::from("x")),
            TokenType::Colon,
            TokenType::Ident(String::from("Real")),
            TokenType::Assign,
            TokenType::Integer(String::from("1")),
            TokenType::Plus,
            TokenType::Integer(String::from("0")),
            TokenType::Mod,
            TokenType::Integer(String::from("2")),
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
        let tokens = vec![
            TokenType::Ident(String::from('x')),
            TokenType::Minus,
            TokenType::Integer(String::from('1')),
            TokenType::LeftShift,
            TokenType::Integer(String::from('1')),
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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
        let lexer = build_lexer("a & b || c").map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Or,
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
                InfixOperator::Or,
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
                InfixOperator::Or,
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
        let lexer = build_lexer("a ^ b & c || d");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Or,
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
            TokenType::If,
            TokenType::Ident(String::from("a")),
            TokenType::Less,
            TokenType::Integer(String::from("0")),
            TokenType::Then,
            TokenType::Minus,
            TokenType::Ident(String::from("a")),
            TokenType::Else,
            TokenType::Ident(String::from("a")),
        ];

        assert_eq!(
            parser_from(iter_from(tokens)).next(),
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

    #[test]
    fn prepend() {
        let code = "[1|[2,3]]";
        let lexer = build_lexer(code).map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(ASTNode::Prepend(
                Box::new(ASTNode::Integer(String::from("1"))),
                Box::new(ASTNode::ExtensionList(vec![
                    ASTNode::Integer(String::from("2")),
                    ASTNode::Integer(String::from("3")),
                ])),
            ))),
        );
    }

    #[test]
    fn consume_comprehension_list() {
        let input = "[a : a in b] + []";
        let lexer = build_lexer(input).map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(ASTNode::Infix(
                InfixOperator::Sum,
                Box::new(ASTNode::ComprehensionList(
                    Box::new(ASTNode::Symbol(String::from("a"))),
                    Box::new(ASTNode::Infix(
                        InfixOperator::In,
                        Box::new(ASTNode::Symbol(String::from("a"))),
                        Box::new(ASTNode::Symbol(String::from("b"))),
                    ))
                )),
                Box::new(ASTNode::ExtensionList(vec![])),
            )))
        );
    }
}
