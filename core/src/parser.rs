use std::path::PathBuf;
use std::{iter::Peekable, vec};

use crate::cst::dictionary;
use crate::cst::*;
use crate::error::{Error, Position};
use crate::lexer::{Lexer, Radix, Token, TokenType};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParserError {
    ExpectedExpression(TokenType),
    MemoizeInVarExpression,
    UnexpectedToken(Vec<TokenType>, TokenType),
    EOFReached,
    EOFExpecting(Vec<TokenType>),
}

pub struct Parser<T: Iterator<Item = Result<Token, Error>>> {
    path: PathBuf,
    tokens: Peekable<T>,
    cur_pos: Position,
    ignore_whitespace: bool,
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

type NodeResult = Result<CSTNode, Error>;

impl<T: Iterator<Item = Result<Token, Error>>> Parser<T> {
    pub fn path(&self) -> PathBuf {
        self.path.to_path_buf()
    }

    fn peek_pos(&mut self) -> Position {
        if let Some(Ok(Token { position, .. })) = self.tokens.peek() {
            *position
        } else {
            self.cur_pos
        }
    }

    fn non_infix(&mut self) -> NodeResult {
        match self.next_token()? {
            None => self.err_with_cur(ParserError::EOFReached),
            Some(tok) => match tok {
                TokenType::Char(chr) => self.char(chr),
                TokenType::Case => self.case(),
                TokenType::DotDot => self.ad_infinitum(),
                TokenType::For => self.for_(),
                TokenType::From => self.import_from(),
                TokenType::If => self.if_(),
                TokenType::Import => self.import(),
                TokenType::Let => self.let_expression(),
                TokenType::Var => self.var_expression(),
                TokenType::True => self.boolean(true),
                TokenType::False => self.boolean(false),
                TokenType::Lparen => self.parenthesis(),
                TokenType::Lbrace => self.set_or_dict(),
                TokenType::Lbrack => self.list(),
                TokenType::Integer(int, radix) => self.integer(int, radix),
                TokenType::Ident(literal) => self.symbol(literal),
                TokenType::Indent => self.block(),
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

    fn case(&mut self) -> NodeResult {
        let start = self.cur_pos.start;
        let expr = self.expression(Precedence::Lowest)?;
        self.consume(TokenType::Do)?;
        self.consume(TokenType::Indent)?;

        let mut pairs = vec![];
        loop {
            match self.peek_token()? {
                Some(TokenType::Dedent) => {
                    self.next_token()?;
                    break;
                }
                Some(_) => {
                    let left = self.expression(Precedence::Lowest)?;
                    self.consume(TokenType::FatArrow)?;
                    let right = self.expression(Precedence::Lowest)?;

                    pairs.push((left, right));
                }
                None => {
                    return Err(Error::with_position(
                        ParserError::EOFReached.into(),
                        self.cur_pos,
                        self.path.to_path_buf(),
                    ))
                }
            }
        }

        let expr = Box::new(expr);
        Ok(CSTNode::new(
            CSTNodeKind::Case { expr, pairs },
            self.start_to_cur(start),
        ))
    }

    fn let_expression(&mut self) -> NodeResult {
        if let Ok(Some(TokenType::Memoize)) = self.peek_token() {
            let start = self.cur_pos.start;
            self.next_token()?;
            self.declaration(DeclarationKind::InmutableMemoized, start)
        } else {
            self.declaration(DeclarationKind::Inmutable, self.cur_pos.start)
        }
    }

    fn var_expression(&mut self) -> NodeResult {
        if let Ok(Some(TokenType::Memoize)) = self.peek_token() {
            self.next_token()?;
            Err(Error::with_position(
                ParserError::MemoizeInVarExpression.into(),
                self.cur_pos,
                self.path.to_path_buf(),
            ))
        } else {
            self.declaration(DeclarationKind::Mutable, self.cur_pos.start)
        }
    }

    fn block(&mut self) -> NodeResult {
        let first = self.expression(Precedence::Lowest)?;
        let start = first.position.start;
        let mut exprs = vec![first];

        loop {
            match self.peek_token() {
                Ok(Some(TokenType::Dedent)) => {
                    let res = Ok(CSTNode::new(
                        CSTNodeKind::Block(exprs),
                        self.start_to_cur(start),
                    ));

                    self.next_token()?;

                    break res;
                }
                _ => {
                    exprs.push(self.expression(Precedence::Lowest)?);
                    continue;
                }
            }
        }
    }

    fn expression(&mut self, precedence: Precedence) -> NodeResult {
        while let Ok(Some(TokenType::Newline)) = self.peek_token() {
            self.next_token()?;
        }

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

    fn ad_infinitum(&self) -> NodeResult {
        self.node_with_cur(CSTNodeKind::AdInfinitum)
    }

    fn wildcard(&self) -> NodeResult {
        self.node_with_cur(CSTNodeKind::Wildcard)
    }

    fn char(&self, chr: char) -> NodeResult {
        self.node_with_cur(CSTNodeKind::Char(chr))
    }

    fn string(&self, str: String) -> NodeResult {
        self.node_with_cur(CSTNodeKind::String(str))
    }

    fn boolean(&self, val: bool) -> NodeResult {
        self.node_with_cur(CSTNodeKind::Boolean(val))
    }

    fn node_with_cur(&self, node: CSTNodeKind) -> NodeResult {
        Ok(CSTNode::new(node, self.cur_pos))
    }

    fn err_with_cur<P>(&self, err: ParserError) -> Result<P, Error> {
        Err(Error::with_position(
            err.into(),
            self.cur_pos,
            self.path.to_path_buf(),
        ))
    }

    fn symbol(&self, literal: String) -> NodeResult {
        self.node_with_cur(CSTNodeKind::Symbol(literal))
    }

    fn declaration(&mut self, kind: DeclarationKind, start: usize) -> NodeResult {
        let expr = self.expression(Precedence::Lowest)?;
        let last_pos = expr.position;

        Ok(CSTNode::new(
            CSTNodeKind::Declaration(Box::new(expr), kind),
            Self::start_to_pos(start, last_pos),
        ))
    }

    fn sequence(
        &mut self,
        terminator: TokenType,
        first: Option<CSTNode>,
    ) -> Result<Vec<CSTNode>, Error> {
        self.ignoring_whitespace(|parser| {
            let mut res = match first {
                None => vec![],
                Some(node) => vec![node],
            };

            let path = parser.path.to_path_buf();

            match parser.peek_token()? {
                Some(tok) if tok == terminator => {
                    parser.next_token()?;
                    Ok(res)
                }
                None => Err(Error::with_position(
                    ParserError::EOFReached.into(),
                    parser.cur_pos,
                    path,
                )),
                _ => loop {
                    let expr = parser.expression(Precedence::Lowest)?;
                    res.push(expr);

                    match parser.next_token()? {
                        Some(TokenType::Comma) => match parser.peek_token() {
                            Ok(Some(tok)) if tok == terminator => {
                                parser.next_token()?;
                                break Ok(res);
                            }
                            _ => continue,
                        },
                        Some(tok) if tok == terminator => break Ok(res),
                        Some(tok) => {
                            break Err(Error::with_position(
                                ParserError::UnexpectedToken(
                                    vec![TokenType::Comma, terminator],
                                    tok,
                                )
                                .into(),
                                parser.cur_pos,
                                path,
                            ))
                        }
                        None => {
                            break Err(Error::with_position(
                                ParserError::EOFExpecting(vec![TokenType::Comma, terminator])
                                    .into(),
                                parser.cur_pos,
                                path,
                            ))
                        }
                    }
                },
            }
        })
    }

    fn start_to_cur(&self, start: usize) -> Position {
        Self::start_to_pos(start, self.cur_pos)
    }

    fn start_to_pos(start: usize, pos: Position) -> Position {
        Position::new(start, pos.start + pos.length - start)
    }

    fn infix(&mut self, lhs: CSTNode, op: InfixOperator, start: usize) -> NodeResult {
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
                let last_pos = rhs.position;
                CSTNode::new(
                    CSTNodeKind::Infix(op, Box::new(lhs), Box::new(rhs)),
                    Self::start_to_pos(start, last_pos),
                )
            }),
        }
    }

    fn integer(&mut self, int: String, radix: Radix) -> NodeResult {
        self.node_with_cur(CSTNodeKind::Integer(int, radix))
    }

    fn ignoring_whitespace<F: FnOnce(&mut Parser<T>) -> Result<P, Error>, P>(
        &mut self,
        f: F,
    ) -> Result<P, Error> {
        let last = self.ignore_whitespace;
        self.ignore_whitespace = true;
        self.skip_whitespace();

        let res = f(self);

        self.ignore_whitespace = last;

        res
    }

    fn parenthesis(&mut self) -> NodeResult {
        let start = self.cur_pos.start;

        if matches!(self.peek_token()?, Some(TokenType::Rparen)) {
            self.next_token()?;
            return Ok(tuple(vec![], self.start_to_cur(start)));
        }

        self.ignoring_whitespace(|parser: &mut Parser<T>| {
            let res = parser.expression(Precedence::Lowest)?;

            match parser.next_token()? {
                Some(TokenType::Rparen) => Ok(res),
                Some(TokenType::Comma) => parser
                    .sequence(TokenType::Rparen, Some(res))
                    .map(|lst| tuple(lst, parser.start_to_cur(start))),
                Some(tok) => {
                    parser.err_with_cur(ParserError::UnexpectedToken(vec![TokenType::Rparen], tok))
                }
                None => parser.err_with_cur(ParserError::EOFExpecting(vec![TokenType::Rparen])),
            }
        })
    }

    fn skip_whitespace(&mut self) {
        while let Some(Ok(Token {
            token: TokenType::Indent | TokenType::Dedent | TokenType::Newline,
            ..
        })) = self.tokens.peek()
        {
            self.tokens.next();
        }
    }

    fn skip_newlines(&mut self) {
        while let Some(Ok(Token {
            token: TokenType::Newline,
            ..
        })) = self.tokens.peek()
        {
            self.tokens.next();
        }
    }

    fn next_token(&mut self) -> Result<Option<TokenType>, Error> {
        if self.ignore_whitespace {
            self.skip_whitespace();
        }

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
        if self.ignore_whitespace {
            self.skip_whitespace();
        }

        match self.tokens.peek() {
            Some(Ok(Token { token, .. })) => Ok(Some(token.to_owned())),
            Some(Err(err)) => Err(err.to_owned()),
            None => Ok(None),
        }
    }

    fn list(&mut self) -> NodeResult {
        self.ignoring_whitespace(|parser| {
            let start = parser.cur_pos.start;

            if matches!(parser.peek_token()?, Some(TokenType::Rbrack)) {
                parser.next_token()?;
                return Ok(extension_list(vec![], parser.start_to_cur(start)));
            }

            let first = parser.expression(Precedence::Lowest)?;

            match parser.next_token()? {
                Some(TokenType::For) => parser.comprehension(first, ComprehensionKind::List, start),
                Some(TokenType::Comma) => parser
                    .sequence(TokenType::Rbrack, Some(first))
                    .map(|lst| extension_list(lst, parser.start_to_cur(start))),
                Some(TokenType::Rbrack) => {
                    Ok(extension_list(vec![first], parser.start_to_cur(start)))
                }
                Some(TokenType::VerticalBar) => parser.prepend_(first, start),
                Some(tok) => parser.err_with_cur(ParserError::UnexpectedToken(
                    vec![TokenType::For, TokenType::Comma, TokenType::Rbrack],
                    tok,
                )),
                None => parser.err_with_cur(ParserError::EOFExpecting(vec![
                    TokenType::For,
                    TokenType::Comma,
                    TokenType::Rbrack,
                ])),
            }
        })
    }

    fn comprehension(
        &mut self,
        first: CSTNode,
        kind: ComprehensionKind,
        start: usize,
    ) -> NodeResult {
        let variable = self.consume_symbol()?;
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

    fn consume_symbol(&mut self) -> Result<String, Error> {
        match self.next_token()? {
            Some(TokenType::Ident(name)) => Ok(name),
            Some(tok) => self.err_with_cur(ParserError::UnexpectedToken(
                vec![TokenType::Ident("".into())],
                tok,
            )),
            None => self.err_with_cur(ParserError::EOFExpecting(vec![TokenType::Ident("".into())])),
        }
    }

    fn prepend_(&mut self, first: CSTNode, start: usize) -> NodeResult {
        let last = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Rbrack)?;

        Ok(cons(first, last, self.start_to_cur(start)))
    }

    fn for_(&mut self) -> NodeResult {
        let start = self.cur_pos.start;

        let expr = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Do)?;

        let proc = self.expression(Precedence::Lowest)?;

        Ok(CSTNode::new(
            CSTNodeKind::For(Box::new(expr), Box::new(proc)),
            self.start_to_cur(start),
        ))
    }

    fn import_from(&mut self) -> NodeResult {
        let start = self.cur_pos.start;

        let source = Box::new(self.non_infix()?);
        self.consume(TokenType::Import)?;
        let values = Box::new(self.non_infix()?);

        Ok(CSTNode::new(
            CSTNodeKind::ImportFrom { source, values },
            self.start_to_cur(start),
        ))
    }

    fn consume(&mut self, expected_tok: TokenType) -> Result<(), Error> {
        self.skip_newlines();
        match self.next_token()? {
            Some(tok) if tok == expected_tok => Ok(()),
            Some(tok) => self.err_with_cur(ParserError::UnexpectedToken(vec![expected_tok], tok)),
            None => self.err_with_cur(ParserError::EOFExpecting(vec![expected_tok])),
        }
    }

    fn if_(&mut self) -> NodeResult {
        let start = self.cur_pos.start;

        let cond = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Then)?;

        let first_res = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Else)?;

        let second_res = self.expression(Precedence::Lowest)?;

        Ok(_if(cond, first_res, second_res, self.start_to_cur(start)))
    }

    fn import(&mut self) -> NodeResult {
        let start = self.cur_pos.start;

        let name = Box::new(self.non_infix()?);
        match self.peek_token() {
            Ok(Some(TokenType::As)) => {
                self.next_token()?;
                let alias = Box::new(self.non_infix()?);
                Ok(CSTNode::new(
                    CSTNodeKind::Import {
                        name,
                        alias: Some(alias),
                    },
                    self.start_to_cur(start),
                ))
            }
            _ => Ok(CSTNode::new(
                CSTNodeKind::Import { name, alias: None },
                self.start_to_cur(start),
            )),
        }
    }

    fn current_infix(&mut self) -> Option<InfixOperator> {
        match self.peek_token() {
            Ok(opt) => opt.and_then(InfixOperator::from),
            _ => None,
        }
    }

    fn prefix(&mut self, op: PrefixOperator) -> NodeResult {
        let start = self.cur_pos.start;

        self.expression(Precedence::Prefix)
            .map(|expr| prefix(op, expr, self.start_to_cur(start)))
    }

    fn set_or_dict(&mut self) -> NodeResult {
        self.ignoring_whitespace(|parser| {
            let start = parser.cur_pos.start;

            if matches!(parser.peek_token()?, Some(TokenType::Rbrace)) {
                parser.next_token()?;
                return Ok(extension_set(vec![], parser.start_to_cur(start)));
            }

            let first = parser.expression(Precedence::Lowest)?;

            match parser.next_token()? {
                Some(TokenType::Comma) => parser
                    .sequence(TokenType::Rbrace, Some(first))
                    .map(|lst| extension_set(lst, parser.start_to_cur(start))),
                Some(TokenType::For) => parser.comprehension(first, ComprehensionKind::Set, start),
                Some(TokenType::VerticalBar) => parser.set_cons(first, start),
                Some(TokenType::FatArrow) => {
                    let first = (first, parser.expression(Precedence::Lowest)?);
                    parser.dict(first, start)
                }
                Some(TokenType::Rbrace) => {
                    Ok(extension_set(vec![first], parser.start_to_cur(start)))
                }
                Some(tok) => parser.err_with_cur(ParserError::UnexpectedToken(
                    vec![TokenType::Comma, TokenType::Rbrace, TokenType::Colon],
                    tok,
                )),
                None => parser.err_with_cur(ParserError::EOFExpecting(vec![
                    TokenType::Comma,
                    TokenType::Rbrace,
                    TokenType::Colon,
                ])),
            }
        })
    }

    fn set_cons(&mut self, first: CSTNode, start: usize) -> NodeResult {
        let some = Box::new(first);
        let most = Box::new(self.expression(Precedence::Lowest)?);
        self.consume(TokenType::Rbrace)?;

        Ok(CSTNode::new(
            CSTNodeKind::SetCons { some, most },
            self.start_to_cur(start),
        ))
    }

    fn dict(&mut self, first: (CSTNode, CSTNode), start: usize) -> NodeResult {
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

                    self.consume(TokenType::FatArrow)?;
                    let right = self.expression(Precedence::Lowest)?;
                    pairs.push((left, right));
                }
                None => {
                    return self.err_with_cur(ParserError::EOFExpecting(vec![
                        TokenType::Comma,
                        TokenType::Colon,
                        TokenType::Rbrace,
                    ]))
                }
            }
        }

        Ok(dictionary(pairs, true, self.start_to_cur(start)))
    }
}

impl<T: Iterator<Item = Result<Token, Error>>> From<(T, PathBuf)> for Parser<T> {
    fn from((tokens, path): (T, PathBuf)) -> Self {
        Self {
            path,
            tokens: tokens.peekable(),
            cur_pos: Position::new(0, 0),
            ignore_whitespace: false,
        }
    }
}

impl<'a> From<Lexer<'a>> for Parser<Lexer<'a>> {
    fn from(lexer: Lexer<'a>) -> Self {
        Self {
            path: lexer.path(),
            tokens: lexer.peekable(),
            cur_pos: Position::new(0, 0),
            ignore_whitespace: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::tests::pos,
        cst::tests::{
            _pos, ad_infinitum, block, boolean, case, char, dec_integer, import_from, integer,
            let_, let_memoize, pattern, set_cons, simple_import, string, symbol, var, wildcard,
        },
        error::Position,
        lexer::{Lexer, Radix},
    };
    use pretty_assertions::assert_eq;
    use std::iter;
    use unindent::unindent;

    fn lexer_from(source: &str) -> Lexer<'_> {
        Lexer::from((source, PathBuf::default()))
    }

    fn parser_from<T: Iterator<Item = Result<Token, Error>>>(iter: T) -> Parser<T> {
        Parser::from((iter, PathBuf::default()))
    }

    #[test]
    fn empty_expression() {
        assert_eq!(
            Parser::from((iter::empty::<Result<Token, Error>>(), PathBuf::default())).next(),
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
            Some(Err(Error::with_position(
                ParserError::EOFExpecting(vec![TokenType::Rparen,]).into(),
                _pos(1, 2),
                PathBuf::default(),
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
            Some(Err(Error::with_position(
                ParserError::EOFReached.into(),
                _pos(1, 1),
                PathBuf::default(),
            )))
        );
    }

    #[test]
    fn product_and_power() {
        let tokens = vec![
            Token::new(
                TokenType::Integer(String::from("1"), Radix::Decimal),
                _pos(0, 1),
            ),
            Token::new(TokenType::Star, _pos(1, 1)),
            Token::new(
                TokenType::Integer(String::from("2"), Radix::Decimal),
                _pos(2, 1),
            ),
            Token::new(TokenType::StarStar, _pos(4, 2)),
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
            Token::new(TokenType::Slash, _pos(1, 1)),
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
        let lexer = lexer_from(input);

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
                pattern(
                    symbol("x", _pos(4, 1)),
                    Some(symbol("Real", _pos(8, 4))),
                    _pos(4, 8)
                ),
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
        let lexer = lexer_from("1 << 1 > 1");

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from("a & b || c");

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from("a && b || c");

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from("  a + b || a & b << c");

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from("a ^ b & c || d");

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from("({}, 0)");

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from("!(~1 /= -1)");

        assert_eq!(
            Parser::from(lexer).next(),
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

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(tuple(vec![dec_integer("1", _pos(1, 1))], _pos(0, 4)))),
        );
    }

    #[test]
    fn anon_function() {
        let input = "x -> 2*x";

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(tuple(
                vec![char('a', _pos(1, 3)), string("b", _pos(6, 3)),],
                _pos(0, 10),
            )))
        );
    }

    #[test]
    fn for_loop() {
        let input = "for i in list do println(i)";

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(_for_single_instruction(
                symbol("i", _pos(4, 1)),
                symbol("list", _pos(9, 4)),
                infix(
                    InfixOperator::Call,
                    symbol("println", _pos(17, 7)),
                    tuple(vec![symbol("i", _pos(25, 1))], _pos(24, 3)),
                    _pos(17, 10)
                ),
                _pos(0, 27)
            )))
        );
    }

    #[test]
    fn list() {
        let input = "[[], 2]";

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(extension_set(
                vec![extension_set(vec![], _pos(1, 2))],
                _pos(0, 4)
            ))),
        );
    }

    #[test]
    fn wildcard_() {
        let input = "[a, 1, _]";

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from(code);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Err(Error::with_position(
                ParserError::UnexpectedToken(vec![TokenType::Rparen], TokenType::Rbrack).into(),
                _pos(3, 1),
                PathBuf::default()
            ))),
        );
    }

    #[test]
    fn expected_expression() {
        let input = "1 + )";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Err(Error::with_position(
                ParserError::ExpectedExpression(TokenType::Rparen).into(),
                _pos(4, 1),
                PathBuf::default(),
            ))),
        );
    }

    #[test]
    fn oop_function_call() {
        let input = "list.map(func) + []";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Sum,
                infix(
                    InfixOperator::Call,
                    infix(
                        InfixOperator::Dot,
                        symbol("list", _pos(0, 4)),
                        symbol("map", _pos(5, 3)),
                        _pos(0, 8)
                    ),
                    tuple(vec![symbol("func", _pos(9, 4))], _pos(8, 6)),
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
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Call,
                infix(
                    InfixOperator::Dot,
                    dec_integer("2", _pos(0, 1)),
                    symbol("f", _pos(2, 1)),
                    _pos(0, 3)
                ),
                tuple(vec![], _pos(3, 2)),
                _pos(0, 5)
            )))
        );
    }

    #[test]
    fn dictionary_() {
        let input = "{'a' => 2, 1 => 5}";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(dictionary(
                vec![
                    (char('a', _pos(1, 3)), dec_integer("2", _pos(8, 1))),
                    (dec_integer("1", _pos(11, 1)), dec_integer("5", _pos(16, 1)))
                ],
                true,
                _pos(0, 18)
            )))
        );
    }

    #[test]
    fn container_element() {
        let input = "(list[0])";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let input = "{1 => 5,..}";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(dictionary(
                vec![(dec_integer("1", _pos(1, 1)), dec_integer("5", _pos(6, 1)),),],
                false,
                _pos(0, 11)
            ))),
        );
    }

    #[test]
    fn set_cons_() {
        let input = "{first|_}";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
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
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(simple_import(
                symbol("foo", _pos(7, 3)),
                None,
                _pos(0, 10)
            ))),
        );
    }

    #[test]
    fn import_with_alias() {
        let input = "import foo as bar";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(simple_import(
                symbol("foo", _pos(7, 3)),
                Some(symbol("bar", _pos(14, 3))),
                _pos(0, 17)
            )))
        );
    }

    #[test]
    fn import_from_() {
        let input = "from foo import bar";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(import_from(
                symbol("foo", _pos(5, 3)),
                symbol("bar", _pos(16, 3)),
                _pos(0, 19)
            ))),
        );
    }

    #[test]
    fn import_several_values() {
        let input = "from foo import (bar, baz)";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(import_from(
                symbol("foo", _pos(5, 3)),
                tuple(
                    vec![symbol("bar", _pos(17, 3)), symbol("baz", _pos(22, 3))],
                    _pos(16, 10)
                ),
                _pos(0, 26)
            )))
        );
    }

    #[test]
    fn let_with_type() {
        let input = "let map(iter: List, fn: Function) := iter";
        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(let_(
                infix(
                    InfixOperator::Call,
                    symbol("map", _pos(4, 3)),
                    tuple(
                        vec![
                            pattern(
                                symbol("iter", _pos(8, 4)),
                                Some(symbol("List", _pos(14, 4))),
                                _pos(8, 10)
                            ),
                            pattern(
                                symbol("fn", _pos(20, 2)),
                                Some(symbol("Function", _pos(24, 8))),
                                _pos(20, 12)
                            ),
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

    #[test]
    fn set_several_lines() {
        let input = unindent(
            "
        let eights := {
            0b1000,
            0o10,
            8,
            0x8,
        }
        ",
        );

        let lexer = lexer_from(input.as_str());

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(let_(
                symbol("eights", _pos(4, 6)),
                Some(extension_set(
                    vec![
                        integer("1000", Radix::Binary, _pos(20, 6)),
                        integer("10", Radix::Octal, _pos(32, 4)),
                        integer("8", Radix::Decimal, _pos(42, 1)),
                        integer("8", Radix::Hex, _pos(49, 3)),
                    ],
                    _pos(14, 41)
                )),
                _pos(0, 55)
            ))),
        );
    }

    #[test]
    fn indented_block() {
        let input = unindent(
            "
        let a := n ->
            let k := n + 1
            k*k
        ",
        );

        let lexer = lexer_from(input.as_str());

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(let_(
                symbol("a", _pos(4, 1)),
                Some(infix(
                    InfixOperator::Correspondence,
                    symbol("n", _pos(9, 1)),
                    block(
                        vec![
                            let_(
                                symbol("k", _pos(22, 1)),
                                Some(infix(
                                    InfixOperator::Sum,
                                    symbol("n", _pos(27, 1)),
                                    dec_integer("1", _pos(31, 1)),
                                    _pos(27, 5)
                                )),
                                _pos(18, 14)
                            ),
                            infix(
                                InfixOperator::Product,
                                symbol("k", _pos(37, 1)),
                                symbol("k", _pos(39, 1)),
                                _pos(37, 3)
                            )
                        ],
                        _pos(18, 22)
                    ),
                    _pos(9, 31)
                )),
                _pos(0, 40)
            ))),
        );
    }

    #[test]
    fn parens_in_indented_list() {
        let input = unindent(
            "[
            (n)
        ]",
        );

        let lexer = lexer_from(input.as_str());

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(extension_list(
                vec![symbol("n", _pos(7, 1))],
                _pos(0, 11)
            ))),
        );
    }

    #[test]
    fn prefix_and_call() {
        let input = "-f()";

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(prefix(
                PrefixOperator::Minus,
                infix(
                    InfixOperator::Call,
                    symbol("f", _pos(1, 1)),
                    tuple(vec![], _pos(2, 2)),
                    _pos(1, 3)
                ),
                _pos(0, 4)
            )))
        );
    }

    #[test]
    fn var_() {
        let input = "var x := 0";

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(var(
                infix(
                    InfixOperator::Assignment,
                    symbol("x", _pos(4, 1)),
                    dec_integer("0", _pos(9, 1)),
                    _pos(4, 6)
                ),
                _pos(0, 10)
            )))
        );
    }

    #[test]
    fn memoize() {
        let input = "let memoize f(x) := 1";

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(let_memoize(
                infix(
                    InfixOperator::Assignment,
                    infix(
                        InfixOperator::Call,
                        symbol("f", pos(12, 1)),
                        tuple(vec![symbol("x", pos(14, 1))], pos(13, 3)),
                        pos(12, 4),
                    ),
                    dec_integer("1", pos(20, 1)),
                    pos(12, 9),
                ),
                pos(0, 21),
            )))
        );
    }

    #[test]
    fn _case() {
        let input = unindent(
            "
        case expr do
            [a|_] => a
        ",
        );

        let lexer = lexer_from(input.as_str());

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(case(
                symbol("expr", pos(5, 4)),
                vec![(
                    cons(symbol("a", pos(18, 1)), wildcard(pos(20, 1)), pos(17, 5)),
                    symbol("a", pos(26, 1))
                )],
                pos(0, 28),
            )))
        );
    }

    #[test]
    fn signature_conjunction_pattern() {
        let input = "n: Integer || Float";

        let lexer = lexer_from(input);

        assert_eq!(
            Parser::from(lexer).next(),
            Some(Ok(infix(
                InfixOperator::Constraint,
                symbol("n", _pos(0, 1)),
                infix(
                    InfixOperator::Or,
                    symbol("Integer", _pos(3, 7)),
                    symbol("Float", _pos(14, 5)),
                    _pos(3, 16)
                ),
                _pos(0, 19)
            ))),
        );
    }

    #[test]
    fn rparen_after_block() {
        let input = unindent(
            "
        for i in 0..1 do
            println(i)
        (a * 2)
        ",
        );

        let lexer = lexer_from(&input);
        let mut parser = Parser::from(lexer);

        assert_eq!(
            parser.next(),
            Some(Ok(_for(
                symbol("i", _pos(4, 1)),
                infix(
                    InfixOperator::Range,
                    dec_integer("0", _pos(9, 1)),
                    dec_integer("1", _pos(12, 1)),
                    _pos(9, 4),
                ),
                vec![infix(
                    InfixOperator::Call,
                    symbol("println", _pos(21, 7)),
                    tuple(vec![symbol("i", _pos(29, 1))], _pos(28, 3)),
                    _pos(21, 10)
                )],
                _pos(0, 32)
            ))),
        );

        assert_eq!(
            parser.next(),
            Some(Ok(infix(
                InfixOperator::Product,
                symbol("a", _pos(33, 1)),
                dec_integer("2", _pos(37, 1)),
                _pos(33, 5)
            ))),
        );
    }
}
