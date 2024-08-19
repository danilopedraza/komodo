use std::{iter::Peekable, vec};

use crate::cst::dictionary;
use crate::cst::*;
use crate::error::{Error, Position};
use crate::lexer::{Radix, Token, TokenType};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParserError {
    ExpectedExpression(TokenType),
    UnexpectedToken(Vec<TokenType>, TokenType),
    EOFReached,
    EOFExpecting(Vec<TokenType>),
}

pub struct Parser<T: Iterator<Item = Result<Token, Error>>> {
    tokens: Peekable<T>,
    cur_pos: Position,
}

impl<T: Iterator<Item = Result<Token, Error>>> Iterator for Parser<T> {
    type Item = Result<CSTNode, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_token() {
            Ok(None) => None,
            Ok(Some(_)) => Some(self.expression(Precedence::Lowest)),
            Err(err) => Some(Err(err)),
        }
    }
}

type _NodeResult = Result<CSTNode, Error>;

impl<T: Iterator<Item = Result<Token, Error>>> Parser<T> {
    fn peek_pos(&mut self) -> Position {
        if let Some(Ok(Token { position, .. })) = self.tokens.peek() {
            *position
        } else {
            self.cur_pos
        }
    }

    fn non_infix(&mut self) -> _NodeResult {
        match self.next_token()? {
            None => self.err_with_cur(ParserError::EOFReached),
            Some(tok) => match tok {
                TokenType::Char(chr) => self.char(chr),
                TokenType::DotDot => self.ad_infinitum(),
                TokenType::For => self.for_(),
                TokenType::From => self.import_from(),
                TokenType::If => self.if_(),
                TokenType::Import => self.import(),
                TokenType::Let => self.let_(),
                TokenType::True => self.boolean(true),
                TokenType::False => self.boolean(false),
                TokenType::Lparen => self.parenthesis(),
                TokenType::Lbrace => self.set_or_dict(),
                TokenType::Lbrack => self.list(),
                TokenType::Integer(int, radix) => self.integer(int, radix),
                TokenType::Ident(literal) => self.symbol(literal),
                TokenType::String(str) => self.string(str),
                TokenType::Wildcard => self.wildcard(),
                tok => {
                    if let Some(op) = PrefixOperator::from(&tok) {
                        self.prefix(op)
                    } else {
                        self.err_with_cur(ParserError::ExpectedExpression(tok))
                    }
                }
            },
        }
    }

    fn expression(&mut self, precedence: Precedence) -> _NodeResult {
        let start = self.peek_pos().start;

        let mut expr = self.non_infix()?;

        while let Some(op) = self.current_infix() {
            if precedence < op.precedence() {
                self.next_token()?;
                expr = self.infix(expr, op, start)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn ad_infinitum(&self) -> _NodeResult {
        self.node_with_cur(CSTNodeKind::AdInfinitum)
    }

    fn wildcard(&self) -> _NodeResult {
        self.node_with_cur(CSTNodeKind::Wildcard)
    }

    fn char(&self, chr: char) -> _NodeResult {
        self.node_with_cur(CSTNodeKind::Char(chr))
    }

    fn string(&self, str: String) -> _NodeResult {
        self.node_with_cur(CSTNodeKind::String(str))
    }

    fn boolean(&self, val: bool) -> _NodeResult {
        self.node_with_cur(CSTNodeKind::Boolean(val))
    }

    fn node_with_cur(&self, node: CSTNodeKind) -> _NodeResult {
        Ok(CSTNode::new(node, self.cur_pos))
    }

    fn err_with_cur(&self, err: ParserError) -> _NodeResult {
        Err(Error::new(err.into(), self.cur_pos))
    }

    fn symbol(&self, literal: String) -> _NodeResult {
        self.node_with_cur(CSTNodeKind::Symbol(literal))
    }

    fn let_(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        let left = Box::new(self.pattern()?);

        let right = match self.peek_token() {
            Ok(Some(TokenType::Assign)) => {
                self.next_token()?;
                Some(Box::new(self.expression(Precedence::Lowest)?))
            }
            _ => None,
        };

        Ok(CSTNode::new(
            CSTNodeKind::Let(left, right),
            self.start_to_cur(start),
        ))
    }

    fn sequence(
        &mut self,
        terminator: TokenType,
        first: Option<CSTNode>,
    ) -> Result<Vec<CSTNode>, Error> {
        let mut res = match first {
            None => vec![],
            Some(node) => vec![node],
        };

        match self.peek_token()? {
            Some(tok) if tok == terminator => {
                self.next_token()?;
                Ok(res)
            }
            None => Err(Error::new(ParserError::EOFReached.into(), self.cur_pos)),
            _ => loop {
                let expr = self.expression(Precedence::Lowest)?;
                res.push(expr);

                match self.next_token()? {
                    Some(TokenType::Comma) => match self.peek_token() {
                        Ok(Some(tok)) if tok == terminator => {
                            self.next_token()?;
                            break Ok(res);
                        }
                        _ => continue,
                    },
                    Some(tok) if tok == terminator => break Ok(res),
                    Some(tok) => {
                        break Err(Error::new(
                            ParserError::UnexpectedToken(vec![TokenType::Comma, terminator], tok)
                                .into(),
                            self.cur_pos,
                        ))
                    }
                    None => {
                        break Err(Error::new(
                            ParserError::EOFExpecting(vec![TokenType::Comma, terminator]).into(),
                            self.cur_pos,
                        ))
                    }
                }
            },
        }
    }

    fn pattern(&mut self) -> _NodeResult {
        let left = self.non_infix()?;
        match (&left.kind, self.peek_token()) {
            (_, Ok(Some(TokenType::Colon))) => {
                let start = left.position.start;

                self.next_token()?;

                match self.next_token()? {
                    Some(TokenType::Ident(name)) => Ok(CSTNode::new(
                        CSTNodeKind::Pattern(Box::new(left), Some(name)),
                        self.start_to_cur(start),
                    )),
                    Some(tok) => Err(Error::new(
                        ParserError::UnexpectedToken(vec![TokenType::Ident("".into())], tok).into(),
                        self.cur_pos,
                    )),
                    None => Err(Error::new(
                        ParserError::EOFExpecting(vec![TokenType::Ident("".into())]).into(),
                        self.cur_pos,
                    )),
                }
            }
            (CSTNodeKind::Symbol(_), Ok(Some(TokenType::Lparen))) => {
                let start = self.cur_pos.start;
                self.next_token()?;
                let tuple_start = self.cur_pos.start;
                let call_pattern = self.pattern_sequence(TokenType::Rparen, None).map(|args| {
                    infix(
                        InfixOperator::Call,
                        left,
                        tuple(args, self.start_to_cur(tuple_start)),
                        self.start_to_cur(start),
                    )
                })?;

                Ok(call_pattern)
            }
            _ => Ok(left),
        }
    }

    fn pattern_sequence(
        &mut self,
        terminator: TokenType,
        first: Option<CSTNode>,
    ) -> Result<Vec<CSTNode>, Error> {
        let mut res = match first {
            None => vec![],
            Some(node) => vec![node],
        };

        match self.peek_token()? {
            Some(tok) if tok == terminator => {
                self.next_token()?;
                Ok(res)
            }
            None => Err(Error::new(ParserError::EOFReached.into(), self.cur_pos)),
            _ => loop {
                let expr = self.pattern()?;
                res.push(expr);

                match self.next_token()? {
                    Some(TokenType::Comma) => continue,
                    Some(tok) if tok == terminator => break Ok(res),
                    Some(tok) => {
                        break Err(Error::new(
                            ParserError::UnexpectedToken(vec![TokenType::Comma, terminator], tok)
                                .into(),
                            self.cur_pos,
                        ))
                    }
                    None => {
                        break Err(Error::new(
                            ParserError::EOFExpecting(vec![TokenType::Comma, terminator]).into(),
                            self.cur_pos,
                        ))
                    }
                }
            },
        }
    }

    fn start_to_cur(&self, start: usize) -> Position {
        Position::new(start, self.cur_pos.start + self.cur_pos.length - start)
    }

    fn infix(&mut self, lhs: CSTNode, op: InfixOperator, start: usize) -> _NodeResult {
        match op {
            InfixOperator::Call => {
                let tuple_start = self.cur_pos.start;

                self.sequence(TokenType::Rparen, None).map(|args| {
                    infix(
                        op,
                        lhs,
                        tuple(args, self.start_to_cur(tuple_start)),
                        self.start_to_cur(start),
                    )
                })
            }
            InfixOperator::Element => {
                let rhs = self.expression(Precedence::Lowest)?;

                self.consume(TokenType::Rbrack)?;

                Ok(infix(op, lhs, rhs, self.start_to_cur(start)))
            }
            _ => self.expression(op.precedence()).map(|rhs| {
                CSTNode::new(
                    CSTNodeKind::Infix(op, Box::new(lhs), Box::new(rhs)),
                    self.start_to_cur(start),
                )
            }),
        }
    }

    fn integer(&mut self, int: String, radix: Radix) -> _NodeResult {
        self.node_with_cur(CSTNodeKind::Integer(int, radix))
    }

    fn parenthesis(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        if matches!(self.peek_token()?, Some(TokenType::Rparen)) {
            self.next_token()?;
            return Ok(tuple(vec![], self.start_to_cur(start)));
        }

        let res = self.expression(Precedence::Lowest)?;

        match self.next_token()? {
            Some(TokenType::Rparen) => Ok(res),
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rparen, Some(res))
                .map(|lst| tuple(lst, self.start_to_cur(start))),
            Some(tok) => {
                self.err_with_cur(ParserError::UnexpectedToken(vec![TokenType::Rparen], tok))
            }
            None => self.err_with_cur(ParserError::EOFExpecting(vec![TokenType::Rparen])),
        }
    }

    fn next_token(&mut self) -> Result<Option<TokenType>, Error> {
        match self.tokens.next() {
            Some(Ok(Token { token, position })) => {
                self.cur_pos = position;
                Ok(Some(token))
            }
            Some(Err(err)) => Err(err),
            None => Ok(None),
        }
    }

    fn peek_token(&mut self) -> Result<Option<TokenType>, Error> {
        match self.tokens.peek() {
            Some(Ok(Token { token, .. })) => Ok(Some(token.to_owned())),
            Some(Err(err)) => Err(err.to_owned()),
            None => Ok(None),
        }
    }

    fn list(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        if matches!(self.peek_token()?, Some(TokenType::Rbrack)) {
            self.next_token()?;
            return Ok(extension_list(vec![], self.start_to_cur(start)));
        }

        let first = self.expression(Precedence::Lowest)?;

        match self.next_token()? {
            Some(TokenType::For) => self.comprehension(first, ComprehensionKind::List, start),
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rbrack, Some(first))
                .map(|lst| extension_list(lst, self.start_to_cur(start))),
            Some(TokenType::Rbrack) => Ok(extension_list(vec![first], self.start_to_cur(start))),
            Some(TokenType::VerticalBar) => self.prepend_(first, start),
            Some(tok) => self.err_with_cur(ParserError::UnexpectedToken(
                vec![TokenType::For, TokenType::Comma, TokenType::Rbrack],
                tok,
            )),
            None => self.err_with_cur(ParserError::EOFExpecting(vec![
                TokenType::For,
                TokenType::Comma,
                TokenType::Rbrack,
            ])),
        }
    }

    fn comprehension(
        &mut self,
        first: CSTNode,
        kind: ComprehensionKind,
        start: usize,
    ) -> _NodeResult {
        let variable = match self.next_token()? {
            Some(TokenType::Ident(name)) => Ok(name),
            _ => todo!(),
        }?;
        self.consume(TokenType::In)?;
        let iterator = self.expression(Precedence::Lowest)?;
        self.consume(match kind {
            ComprehensionKind::List => TokenType::Rbrack,
            ComprehensionKind::Set => TokenType::Rbrace,
        })?;

        Ok(comprehension(
            first,
            variable,
            iterator,
            kind,
            self.start_to_cur(start),
        ))
    }

    fn prepend_(&mut self, first: CSTNode, start: usize) -> _NodeResult {
        let last = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Rbrack)?;

        Ok(cons(first, last, self.start_to_cur(start)))
    }

    fn for_(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        let ident = match self.next_token()? {
            Some(TokenType::Ident(s)) => Ok(s),
            Some(tok) => Err(Error::new(
                ParserError::UnexpectedToken(vec![TokenType::Ident(String::from(""))], tok).into(),
                self.cur_pos,
            )),
            None => Err(Error::new(
                ParserError::EOFExpecting(vec![TokenType::Ident(String::from(""))]).into(),
                self.cur_pos,
            )),
        }?;

        self.consume(TokenType::In)?;

        let iter = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Colon)?;

        let CSTNode { kind, position } = self.expression(Precedence::Lowest)?;

        let proc = match kind {
            CSTNodeKind::Tuple(v) => v,
            node => vec![CSTNode::new(node, position)],
        };

        Ok(_for(&ident, iter, proc, self.start_to_cur(start)))
    }

    fn import_from(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;
        match self.next_token()? {
            Some(TokenType::Ident(source)) => {
                self.consume(TokenType::Import)?;
                match self.expression(Precedence::Lowest)? {
                    CSTNode {
                        kind: CSTNodeKind::Symbol(val),
                        position: _,
                    } => Ok(CSTNode::new(
                        CSTNodeKind::ImportFrom {
                            source,
                            values: vec![val],
                        },
                        self.start_to_cur(start),
                    )),
                    CSTNode {
                        kind: CSTNodeKind::Tuple(vals),
                        position: _,
                    } => {
                        let mut values = vec![];
                        for val in vals {
                            if let CSTNode {
                                kind: CSTNodeKind::Symbol(name),
                                position: _,
                            } = val
                            {
                                values.push(name);
                            } else {
                                todo!()
                            }
                        }

                        Ok(CSTNode::new(
                            CSTNodeKind::ImportFrom { source, values },
                            self.start_to_cur(start),
                        ))
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }

    fn consume(&mut self, expected_tok: TokenType) -> Result<(), Error> {
        match self.next_token()? {
            Some(tok) if tok == expected_tok => Ok(()),
            Some(tok) => Err(Error::new(
                ParserError::UnexpectedToken(vec![expected_tok], tok).into(),
                self.cur_pos,
            )),
            None => Err(Error::new(
                ParserError::EOFExpecting(vec![expected_tok]).into(),
                self.cur_pos,
            )),
        }
    }

    fn if_(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        let cond = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Then)?;

        let first_res = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Else)?;

        let second_res = self.expression(Precedence::Lowest)?;

        Ok(_if(cond, first_res, second_res, self.start_to_cur(start)))
    }

    fn import(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        match (self.next_token()?, self.peek_token()) {
            (Some(TokenType::Ident(name)), Ok(Some(TokenType::As))) => {
                self.next_token()?;
                match self.next_token()? {
                    Some(TokenType::Ident(alias)) => Ok(CSTNode::new(
                        CSTNodeKind::Import {
                            name,
                            alias: Some(alias),
                        },
                        self.start_to_cur(start),
                    )),
                    _ => todo!(),
                }
            }
            (Some(TokenType::Ident(name)), _) => Ok(CSTNode::new(
                CSTNodeKind::Import { name, alias: None },
                self.start_to_cur(start),
            )),
            _ => todo!(),
        }
    }

    fn current_infix(&mut self) -> Option<InfixOperator> {
        match self.peek_token() {
            Ok(opt) => opt.and_then(InfixOperator::from),
            _ => None,
        }
    }

    fn prefix(&mut self, op: PrefixOperator) -> _NodeResult {
        let start = self.cur_pos.start;

        self.non_infix()
            .map(|expr| prefix(op, expr, self.start_to_cur(start)))
    }

    fn set_or_dict(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        if matches!(self.peek_token()?, Some(TokenType::Rbrace)) {
            self.next_token()?;
            return Ok(extension_set(vec![], self.start_to_cur(start)));
        }

        let first = self.expression(Precedence::Lowest)?;

        match self.next_token()? {
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rbrace, Some(first))
                .map(|lst| extension_set(lst, self.start_to_cur(start))),
            Some(TokenType::For) => self.comprehension(first, ComprehensionKind::Set, start),
            Some(TokenType::VerticalBar) => self.set_cons(first, start),
            Some(TokenType::Colon) => {
                let first = (first, self.expression(Precedence::Lowest)?);
                self.dict(first, start)
            }
            Some(TokenType::Rbrace) => Ok(extension_set(vec![first], self.start_to_cur(start))),
            Some(tok) => Err(Error::new(
                ParserError::UnexpectedToken(
                    vec![TokenType::Comma, TokenType::Rbrace, TokenType::Colon],
                    tok,
                )
                .into(),
                self.cur_pos,
            )),
            None => Err(Error::new(
                ParserError::EOFExpecting(vec![
                    TokenType::Comma,
                    TokenType::Rbrace,
                    TokenType::Colon,
                ])
                .into(),
                self.cur_pos,
            )),
        }
    }

    fn set_cons(&mut self, first: CSTNode, start: usize) -> _NodeResult {
        let some = Box::new(first);
        let most = Box::new(self.expression(Precedence::Lowest)?);
        self.consume(TokenType::Rbrace)?;

        Ok(CSTNode::new(
            CSTNodeKind::SetCons { some, most },
            self.start_to_cur(start),
        ))
    }

    fn dict(&mut self, first: (CSTNode, CSTNode), start: usize) -> _NodeResult {
        let mut pairs = vec![first];

        loop {
            match self.peek_token()? {
                Some(TokenType::Comma) => {
                    self.next_token()?;
                    continue;
                }
                Some(TokenType::Rbrace) => {
                    self.next_token()?;
                    break;
                }
                Some(_) => {
                    let left = self.expression(Precedence::Lowest)?;
                    if left.kind == CSTNodeKind::AdInfinitum {
                        self.consume(TokenType::Rbrace)?;
                        return Ok(dictionary(pairs, false, self.start_to_cur(start)));
                    }

                    self.consume(TokenType::Colon)?;
                    let right = self.expression(Precedence::Lowest)?;
                    pairs.push((left, right));
                }
                None => {
                    return Err(Error::new(
                        ParserError::EOFExpecting(vec![
                            TokenType::Comma,
                            TokenType::Colon,
                            TokenType::Rbrace,
                        ])
                        .into(),
                        self.cur_pos,
                    ))
                }
            }
        }

        Ok(dictionary(pairs, true, self.start_to_cur(start)))
    }
}

pub fn parser_from<T: Iterator<Item = Result<Token, Error>>>(tokens: T) -> Parser<T> {
    Parser {
        tokens: tokens.peekable(),
        cur_pos: Position::new(0, 0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cst::tests::{
            _pos, ad_infinitum, boolean, char, dec_integer, import, import_from, let_, pattern,
            set_cons, string, symbol, wildcard,
        },
        error::Position,
        lexer::{Lexer, Radix},
    };
    use std::iter;

    #[test]
    fn empty_expression() {
        assert_eq!(
            parser_from(iter::empty::<Result<Token, Error>>()).next(),
            None
        );
    }

    #[test]
    fn integer_alone() {
        let tokens = vec![Token::new(
            TokenType::Integer(String::from("0"), Radix::Decimal),
            _pos(0, 1),
        )];
        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(dec_integer("0", _pos(0, 1))))
        );
    }

    #[test]
    fn integer_in_parenthesis() {
        let tokens = vec![
            Token::new(TokenType::Lparen, _pos(0, 1)),
            Token::new(
                TokenType::Integer(String::from("365"), Radix::Decimal),
                _pos(1, 3),
            ),
            Token::new(TokenType::Rparen, _pos(4, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(dec_integer("365", _pos(1, 3)))),
        );
    }

    #[test]
    fn unbalanced_left_parenthesis() {
        let tokens = vec![
            Token::new(TokenType::Lparen, _pos(0, 1)),
            Token::new(
                TokenType::Integer(String::from("65"), Radix::Decimal),
                _pos(1, 2),
            ),
        ];
        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Err(Error::new(
                ParserError::EOFExpecting(vec![TokenType::Rparen,]).into(),
                _pos(1, 2),
            )))
        );
    }

    #[test]
    fn simple_sum() {
        let tokens = vec![
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(0, 1),
            ),
            Token::new(TokenType::Plus, _pos(2, 1)),
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(4, 1),
            ),
        ];
        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(infix(
                InfixOperator::Sum,
                dec_integer("1", _pos(0, 1)),
                dec_integer("1", _pos(4, 1)),
                _pos(0, 5)
            )))
        );
    }

    #[test]
    fn incomplete_sum() {
        let tokens = vec![
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(0, 1),
            ),
            Token::new(TokenType::Plus, _pos(1, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Err(Error::new(ParserError::EOFReached.into(), _pos(1, 1),)))
        );
    }

    #[test]
    fn product_and_power() {
        let tokens = vec![
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(0, 1),
            ),
            Token::new(TokenType::Times, _pos(1, 1)),
            Token::new(
                TokenType::Integer(String::from("2"), Radix::Decimal),
                _pos(2, 1),
            ),
            Token::new(TokenType::ToThe, _pos(4, 2)),
            Token::new(
                TokenType::Integer(String::from("2"), Radix::Decimal),
                _pos(7, 1),
            ),
        ];
        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(infix(
                InfixOperator::Product,
                dec_integer("1", _pos(0, 1)),
                infix(
                    InfixOperator::Exponentiation,
                    dec_integer("2", _pos(2, 1)),
                    dec_integer("2", _pos(7, 1)),
                    _pos(2, 6),
                ),
                _pos(0, 8)
            )))
        );
    }

    #[test]
    fn division_and_sum() {
        let tokens = vec![
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(0, 1),
            ),
            Token::new(TokenType::Over, _pos(1, 1)),
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(2, 1),
            ),
            Token::new(TokenType::Plus, _pos(4, 1)),
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(6, 1),
            ),
        ];
        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(infix(
                InfixOperator::Sum,
                infix(
                    InfixOperator::Division,
                    dec_integer("1", _pos(0, 1)),
                    dec_integer("1", _pos(2, 1)),
                    _pos(0, 3)
                ),
                dec_integer("1", _pos(6, 1)),
                _pos(0, 7),
            )))
        );
    }

    #[test]
    fn let_statement() {
        let tokens = vec![
            Token::new(TokenType::Let, Position::new(0, 3)),
            Token::new(TokenType::Ident(String::from('x')), Position::new(4, 1)),
            Token::new(TokenType::Assign, Position::new(6, 2)),
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                Position::new(9, 1),
            ),
        ];
        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(let_(
                symbol("x", _pos(4, 1)),
                Some(dec_integer("1", _pos(9, 1))),
                _pos(0, 10)
            )))
        );
    }

    #[test]
    fn comparison_precedence() {
        let tokens = vec![
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(0, 1),
            ),
            Token::new(TokenType::Plus, _pos(2, 1)),
            Token::new(
                TokenType::Integer(String::from("5"), Radix::Decimal),
                _pos(4, 1),
            ),
            Token::new(TokenType::NotEqual, _pos(6, 2)),
            Token::new(
                TokenType::Integer(String::from("6"), Radix::Decimal),
                _pos(9, 1),
            ),
            Token::new(TokenType::Percent, _pos(11, 1)),
            Token::new(
                TokenType::Integer(String::from("2"), Radix::Decimal),
                _pos(13, 1),
            ),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(infix(
                InfixOperator::NotEquality,
                infix(
                    InfixOperator::Sum,
                    dec_integer("1", _pos(0, 1)),
                    dec_integer("5", _pos(4, 1)),
                    _pos(0, 5)
                ),
                infix(
                    InfixOperator::Rem,
                    dec_integer("6", _pos(9, 1)),
                    dec_integer("2", _pos(13, 1)),
                    _pos(9, 5)
                ),
                _pos(0, 14)
            )))
        );
    }

    #[test]
    fn let_function_statement() {
        let tokens = vec![
            Token::new(TokenType::Let, _pos(0, 3)),
            Token::new(TokenType::Ident(String::from('f')), _pos(4, 1)),
            Token::new(TokenType::Lparen, _pos(5, 1)),
            Token::new(TokenType::Ident(String::from('x')), _pos(6, 1)),
            Token::new(TokenType::Comma, _pos(7, 1)),
            Token::new(TokenType::Ident(String::from('y')), _pos(9, 1)),
            Token::new(TokenType::Rparen, _pos(10, 1)),
            Token::new(TokenType::Assign, _pos(12, 2)),
            Token::new(TokenType::Ident(String::from('x')), _pos(15, 1)),
            Token::new(TokenType::Plus, _pos(17, 1)),
            Token::new(TokenType::Ident(String::from('y')), _pos(19, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(let_(
                infix(
                    InfixOperator::Call,
                    symbol("f", _pos(4, 1)),
                    tuple(
                        vec![symbol("x", _pos(6, 1)), symbol("y", _pos(9, 1))],
                        _pos(5, 6)
                    ),
                    _pos(4, 7)
                ),
                Some(infix(
                    InfixOperator::Sum,
                    symbol("x", _pos(15, 1)),
                    symbol("y", _pos(19, 1)),
                    _pos(15, 5)
                )),
                _pos(0, 20),
            ))),
        );
    }

    #[test]
    fn empty_set() {
        let tokens = vec![
            Token::new(TokenType::Lbrace, Position::new(0, 1)),
            Token::new(TokenType::Rbrace, Position::new(1, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(extension_set(vec![], _pos(0, 2))))
        );
    }

    #[test]
    fn set() {
        let tokens = vec![
            Token::new(TokenType::Lbrace, Position::new(0, 1)),
            Token::new(TokenType::Lparen, Position::new(1, 1)),
            Token::new(TokenType::True, Position::new(2, 4)),
            Token::new(TokenType::Rparen, Position::new(6, 1)),
            Token::new(TokenType::Comma, Position::new(7, 1)),
            Token::new(TokenType::False, Position::new(9, 4)),
            Token::new(TokenType::Rbrace, Position::new(13, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(extension_set(
                vec![boolean(true, _pos(2, 4)), boolean(false, _pos(9, 4)),],
                _pos(0, 14)
            )))
        );
    }

    #[test]
    fn empty_tuple() {
        let tokens = vec![
            Token::new(TokenType::Lparen, Position::new(0, 1)),
            Token::new(TokenType::Rparen, Position::new(1, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(tuple(vec![], _pos(0, 2))))
        );
    }

    #[test]
    fn tuple_only() {
        let tokens = vec![
            Token::new(TokenType::Lparen, Position::new(0, 1)),
            Token::new(TokenType::Ident(String::from("Real")), Position::new(1, 4)),
            Token::new(TokenType::Comma, Position::new(5, 1)),
            Token::new(TokenType::Ident(String::from("Real")), Position::new(7, 4)),
            Token::new(TokenType::Rparen, Position::new(11, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(tuple(
                vec![symbol("Real", _pos(1, 4)), symbol("Real", _pos(7, 4))],
                _pos(0, 12)
            )))
        );
    }

    #[test]
    fn set_comprehension() {
        let input = "{a for a in S}";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(comprehension(
                symbol("a", _pos(1, 1)),
                "a".into(),
                symbol("S", _pos(12, 1)),
                ComprehensionKind::Set,
                _pos(0, 14),
            )))
        );
    }

    #[test]
    fn typed_let() {
        let tokens = vec![
            Token::new(TokenType::Let, _pos(0, 3)),
            Token::new(TokenType::Ident(String::from("x")), _pos(4, 1)),
            Token::new(TokenType::Colon, _pos(6, 1)),
            Token::new(TokenType::Ident(String::from("Real")), _pos(8, 4)),
            Token::new(TokenType::Assign, _pos(13, 2)),
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(16, 1),
            ),
            Token::new(TokenType::Plus, _pos(18, 1)),
            Token::new(
                TokenType::Integer(String::from("0"), Radix::Decimal),
                _pos(20, 1),
            ),
            Token::new(TokenType::Percent, _pos(22, 1)),
            Token::new(
                TokenType::Integer(String::from("2"), Radix::Decimal),
                _pos(24, 1),
            ),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(let_(
                pattern(symbol("x", _pos(4, 1)), Some("Real"), _pos(4, 8)),
                Some(infix(
                    InfixOperator::Sum,
                    dec_integer("1", _pos(16, 1)),
                    infix(
                        InfixOperator::Rem,
                        dec_integer("0", _pos(20, 1)),
                        dec_integer("2", _pos(24, 1)),
                        _pos(20, 5)
                    ),
                    _pos(16, 9),
                )),
                _pos(0, 25),
            )))
        );
    }

    #[test]
    fn shift_operator() {
        let tokens = vec![
            Token::new(TokenType::Ident(String::from('x')), _pos(0, 1)),
            Token::new(TokenType::Minus, _pos(2, 1)),
            Token::new(
                TokenType::Integer(String::from('1'), Radix::Decimal),
                _pos(4, 1),
            ),
            Token::new(TokenType::LeftShift, _pos(6, 2)),
            Token::new(
                TokenType::Integer(String::from('1'), Radix::Decimal),
                _pos(9, 1),
            ),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(infix(
                InfixOperator::LeftShift,
                infix(
                    InfixOperator::Substraction,
                    symbol("x", _pos(0, 1)),
                    dec_integer("1", _pos(4, 1)),
                    _pos(0, 5)
                ),
                dec_integer("1", _pos(9, 1)),
                _pos(0, 10),
            )))
        );
    }

    #[test]
    fn shift_and_comparison() {
        let lexer = Lexer::new("1 << 1 > 1");

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Greater,
                infix(
                    InfixOperator::LeftShift,
                    dec_integer("1", _pos(0, 1)),
                    dec_integer("1", _pos(5, 1)),
                    _pos(0, 6)
                ),
                dec_integer("1", _pos(9, 1)),
                _pos(0, 10)
            )))
        );
    }

    #[test]
    fn bitwise() {
        let lexer = Lexer::new("a & b || c");

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Or,
                infix(
                    InfixOperator::BitwiseAnd,
                    symbol("a", _pos(0, 1)),
                    symbol("b", _pos(4, 1)),
                    _pos(0, 5)
                ),
                symbol("c", _pos(9, 1)),
                _pos(0, 10),
            )))
        );
    }

    #[test]
    fn logic_infix_operators() {
        let lexer = Lexer::new("a && b || c");

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Or,
                infix(
                    InfixOperator::LogicAnd,
                    symbol("a", _pos(0, 1)),
                    symbol("b", _pos(5, 1)),
                    _pos(0, 6)
                ),
                symbol("c", _pos(10, 1)),
                _pos(0, 11),
            )))
        );
    }

    #[test]
    fn complex_precedence() {
        let lexer = Lexer::new("  a + b || a & b << c");

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Or,
                infix(
                    InfixOperator::Sum,
                    symbol("a", _pos(2, 1)),
                    symbol("b", _pos(6, 1)),
                    _pos(2, 5)
                ),
                infix(
                    InfixOperator::BitwiseAnd,
                    symbol("a", _pos(11, 1)),
                    infix(
                        InfixOperator::LeftShift,
                        symbol("b", _pos(15, 1)),
                        symbol("c", _pos(20, 1)),
                        _pos(15, 6)
                    ),
                    _pos(11, 10),
                ),
                _pos(2, 19)
            )))
        );
    }

    #[test]
    fn bitwise_xor() {
        let lexer = Lexer::new("a ^ b & c || d");

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Or,
                infix(
                    InfixOperator::BitwiseXor,
                    symbol("a", _pos(0, 1)),
                    infix(
                        InfixOperator::BitwiseAnd,
                        symbol("b", _pos(4, 1)),
                        symbol("c", _pos(8, 1)),
                        _pos(4, 5)
                    ),
                    _pos(0, 9),
                ),
                symbol("d", _pos(13, 1)),
                _pos(0, 14)
            )))
        );
    }

    #[test]
    fn something_after_empty_set() {
        let lexer = Lexer::new("({}, 0)");

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(tuple(
                vec![
                    extension_set(vec![], _pos(1, 2)),
                    dec_integer("0", _pos(5, 1))
                ],
                _pos(0, 7)
            )))
        );
    }

    #[test]
    fn prefixes() {
        let lexer = Lexer::new("!(~1 /= -1)");

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(prefix(
                PrefixOperator::LogicNot,
                infix(
                    InfixOperator::NotEquality,
                    prefix(
                        PrefixOperator::BitwiseNot,
                        dec_integer("1", _pos(3, 1)),
                        _pos(2, 2)
                    ),
                    prefix(
                        PrefixOperator::Minus,
                        dec_integer("1", _pos(9, 1)),
                        _pos(8, 2)
                    ),
                    _pos(2, 8),
                ),
                _pos(0, 11),
            )))
        );
    }

    #[test]
    fn if_expr() {
        let tokens = vec![
            Token::new(TokenType::If, _pos(0, 2)),
            Token::new(TokenType::Ident(String::from("a")), _pos(3, 1)),
            Token::new(TokenType::Less, _pos(5, 1)),
            Token::new(
                TokenType::Integer(String::from("0"), Radix::Decimal),
                _pos(7, 1),
            ),
            Token::new(TokenType::Then, _pos(9, 4)),
            Token::new(TokenType::Minus, _pos(14, 1)),
            Token::new(TokenType::Ident(String::from("a")), _pos(15, 1)),
            Token::new(TokenType::Else, _pos(17, 4)),
            Token::new(TokenType::Ident(String::from("a")), _pos(22, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter().map(Ok)).next(),
            Some(Ok(_if(
                infix(
                    InfixOperator::Less,
                    symbol("a", _pos(3, 1)),
                    dec_integer("0", _pos(7, 1)),
                    _pos(3, 5)
                ),
                prefix(PrefixOperator::Minus, symbol("a", _pos(15, 1)), _pos(14, 2)),
                symbol("a", _pos(22, 1)),
                _pos(0, 23),
            )))
        );
    }

    #[test]
    fn function_call() {
        let input = "f(x, y)";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Call,
                symbol("f", _pos(0, 1)),
                tuple(
                    vec![symbol("x", _pos(2, 1)), symbol("y", _pos(5, 1)),],
                    _pos(1, 6)
                ),
                _pos(0, 7),
            ))),
        );
    }

    #[test]
    fn comma_last_item() {
        let input = "(1,)";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(tuple(vec![dec_integer("1", _pos(1, 1))], _pos(0, 4)))),
        );
    }

    #[test]
    fn anon_function() {
        let input = "x -> 2*x";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Correspondence,
                symbol("x", _pos(0, 1)),
                infix(
                    InfixOperator::Product,
                    dec_integer("2", _pos(5, 1)),
                    symbol("x", _pos(7, 1)),
                    _pos(5, 3)
                ),
                _pos(0, 8),
            )))
        );
    }

    #[test]
    fn anon_function_call() {
        let input = "((x, y) -> x)(1, 2)";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Call,
                infix(
                    InfixOperator::Correspondence,
                    tuple(
                        vec![symbol("x", _pos(2, 1)), symbol("y", _pos(5, 1)),],
                        _pos(1, 6)
                    ),
                    symbol("x", _pos(11, 1)),
                    _pos(1, 11),
                ),
                tuple(
                    vec![dec_integer("1", _pos(14, 1)), dec_integer("2", _pos(17, 1)),],
                    _pos(13, 6)
                ),
                _pos(0, 19)
            )))
        );
    }

    #[test]
    fn char_and_string() {
        let input = "('a', \"b\")";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(tuple(
                vec![char('a', _pos(1, 3)), string("b", _pos(6, 3)),],
                _pos(0, 10),
            )))
        );
    }

    #[test]
    fn for_loop() {
        let input = "for i in list: println(i)";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(_for(
                "i",
                symbol("list", _pos(9, 4)),
                vec![infix(
                    InfixOperator::Call,
                    symbol("println", _pos(15, 7)),
                    tuple(vec![symbol("i", _pos(23, 1))], _pos(22, 3)),
                    _pos(15, 10)
                )],
                _pos(0, 25)
            )))
        );
    }

    #[test]
    fn list() {
        let input = "[[], 2]";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(extension_list(
                vec![
                    extension_list(vec![], _pos(1, 2)),
                    dec_integer("2", _pos(5, 1)),
                ],
                _pos(0, 7),
            ))),
        );
    }

    #[test]
    fn singleton_empty_set() {
        let input = "{{}}";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(extension_set(
                vec![extension_set(vec![], _pos(1, 2))],
                _pos(0, 4)
            ))),
        );
    }

    #[test]
    fn wildcard_() {
        let input = "[a, 1, _]";

        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(extension_list(
                vec![
                    symbol("a", _pos(1, 1)),
                    dec_integer("1", _pos(4, 1)),
                    CSTNode::new(CSTNodeKind::Wildcard, _pos(7, 1)),
                ],
                _pos(0, 9)
            ),)),
        );
    }

    #[test]
    fn prepend_only() {
        let code = "[1|[2,3]]";
        let lexer = Lexer::new(code);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(cons(
                dec_integer("1", _pos(1, 1)),
                extension_list(
                    vec![dec_integer("2", _pos(4, 1)), dec_integer("3", _pos(6, 1)),],
                    _pos(3, 5)
                ),
                _pos(0, 9)
            ))),
        );
    }

    #[test]
    fn consume_comprehension_list() {
        let input = "[a for a in b] + []";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Sum,
                comprehension(
                    symbol("a", _pos(1, 1)),
                    "a".into(),
                    symbol("b", _pos(12, 1)),
                    ComprehensionKind::List,
                    _pos(0, 14),
                ),
                extension_list(vec![], _pos(17, 2)),
                _pos(0, 19)
            )))
        );
    }

    #[test]
    fn expected_rparen() {
        let input = "(15]";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Err(Error::new(
                ParserError::UnexpectedToken(vec![TokenType::Rparen], TokenType::Rbrack).into(),
                _pos(3, 1),
            ))),
        );
    }

    #[test]
    fn expected_expression() {
        let input = "1 + )";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Err(Error::new(
                ParserError::ExpectedExpression(TokenType::Rparen).into(),
                _pos(4, 1)
            ))),
        );
    }

    #[test]
    fn oop_function_call() {
        let input = "list.map(func) + []";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Sum,
                infix(
                    InfixOperator::Dot,
                    symbol("list", _pos(0, 4)),
                    infix(
                        InfixOperator::Call,
                        symbol("map", _pos(5, 3)),
                        tuple(vec![symbol("func", _pos(9, 4))], _pos(8, 6)),
                        _pos(5, 9)
                    ),
                    _pos(0, 14)
                ),
                extension_list(vec![], _pos(17, 2)),
                _pos(0, 19),
            )))
        );
    }

    #[test]
    fn decimal() {
        let input = "1.5";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Dot,
                dec_integer("1", _pos(0, 1)),
                dec_integer("5", _pos(2, 1)),
                _pos(0, 3)
            ))),
        );
    }

    #[test]
    fn range() {
        let input = "5 in 0..10+1";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::In,
                dec_integer("5", _pos(0, 1)),
                infix(
                    InfixOperator::Range,
                    dec_integer("0", _pos(5, 1)),
                    infix(
                        InfixOperator::Sum,
                        dec_integer("10", _pos(8, 2)),
                        dec_integer("1", _pos(11, 1)),
                        _pos(8, 4)
                    ),
                    _pos(5, 7)
                ),
                _pos(0, 12)
            )))
        );
    }

    #[test]
    fn fraction() {
        let input = "1 // 2";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Fraction,
                dec_integer("1", _pos(0, 1)),
                dec_integer("2", _pos(5, 1)),
                _pos(0, 6)
            )))
        );
    }

    #[test]
    fn oop_cal_with_int() {
        let input = "2.f()";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Dot,
                dec_integer("2", _pos(0, 1)),
                infix(
                    InfixOperator::Call,
                    symbol("f", _pos(2, 1)),
                    tuple(vec![], _pos(3, 2)),
                    _pos(2, 3)
                ),
                _pos(0, 5)
            )))
        );
    }

    #[test]
    fn dictionary_() {
        let input = "{'a': 2, 1:5}";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(dictionary(
                vec![
                    (char('a', _pos(1, 3)), dec_integer("2", _pos(6, 1))),
                    (dec_integer("1", _pos(9, 1)), dec_integer("5", _pos(11, 1)))
                ],
                true,
                _pos(0, 13)
            )))
        );
    }

    #[test]
    fn container_element() {
        let input = "(list[0])";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Element,
                symbol("list", _pos(1, 4)),
                dec_integer("0", _pos(6, 1)),
                _pos(1, 7)
            )))
        );
    }

    #[test]
    fn ad_infinitum_() {
        let input = "[1, 2,..]";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(extension_list(
                vec![
                    dec_integer("1", _pos(1, 1)),
                    dec_integer("2", _pos(4, 1)),
                    ad_infinitum(_pos(6, 2)),
                ],
                _pos(0, 9)
            )))
        );
    }

    #[test]
    fn ad_infinitum_dict() {
        let input = "{1: 5,..}";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(dictionary(
                vec![(dec_integer("1", _pos(1, 1)), dec_integer("5", _pos(4, 1)),),],
                false,
                _pos(0, 9)
            ))),
        );
    }

    #[test]
    fn set_cons_() {
        let input = "{first|_}";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(set_cons(
                symbol("first", _pos(1, 5)),
                wildcard(_pos(7, 1)),
                _pos(0, 9)
            ))),
        );
    }

    #[test]
    fn import_statement() {
        let input = "import foo";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(import("foo", None, _pos(0, 10)))),
        );
    }

    #[test]
    fn import_with_alias() {
        let input = "import foo as bar";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(import("foo", Some("bar"), _pos(0, 17))))
        );
    }

    #[test]
    fn import_from_() {
        let input = "from foo import bar";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(import_from("foo", vec!["bar"], _pos(0, 19)))),
        );
    }

    #[test]
    fn import_several_values() {
        let input = "from foo import (bar, baz)";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(import_from("foo", vec!["bar", "baz"], _pos(0, 26))))
        );
    }

    #[test]
    fn let_with_type() {
        let input = "let map(iter: List, fn: Function) := iter";
        let lexer = Lexer::new(input);

        assert_eq!(
            parser_from(lexer).next(),
            Some(Ok(let_(
                infix(
                    InfixOperator::Call,
                    symbol("map", _pos(4, 3)),
                    tuple(
                        vec![
                            pattern(symbol("iter", _pos(8, 4)), Some("List"), _pos(8, 10)),
                            pattern(symbol("fn", _pos(20, 2)), Some("Function"), _pos(20, 12)),
                        ],
                        _pos(7, 26)
                    ),
                    _pos(4, 29)
                ),
                Some(symbol("iter", _pos(37, 4))),
                _pos(0, 41)
            )))
        );
    }
}
