use std::{iter::Peekable, vec};

use crate::ast::*;
use crate::error::Position;
use crate::lexer::{Token, TokenType};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    ExpectedExpression(TokenType),
    UnexpectedToken(Vec<TokenType>, TokenType),
    EOFReached,
    // EOFReached_(Position),
    EOFExpecting(Vec<TokenType>),
    EOFExpecting_(Vec<TokenType>, Position),
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    cur_pos: Position,
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ASTNodeType, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_token() {
            None => None,
            _ => Some(self.expression(Precedence::Lowest)),
        }
    }
}

fn if_(cond: ASTNodeType, first_res: ASTNodeType, second_res: ASTNodeType) -> ASTNodeType {
    ASTNodeType::If(Box::new(cond), Box::new(first_res), Box::new(second_res))
}

fn _if_(cond: ASTNode, first_res: ASTNode, second_res: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::If(Box::new(cond), Box::new(first_res), Box::new(second_res)),
        position,
    )
}

fn tuple(list: Vec<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::Tuple(list)
}

fn _tuple(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Tuple(list), position)
}

fn comprehension_set(val: ASTNodeType, prop: ASTNodeType) -> ASTNodeType {
    ASTNodeType::ComprehensionSet(Box::new(val), Box::new(prop))
}

fn _comprehension_set(val: ASTNode, prop: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::ComprehensionSet(Box::new(val), Box::new(prop)),
        position,
    )
}

fn comprehension_list(val: ASTNodeType, prop: ASTNodeType) -> ASTNodeType {
    ASTNodeType::ComprehensionList(Box::new(val), Box::new(prop))
}

fn _comprehension_list(val: ASTNode, prop: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::ComprehensionList(Box::new(val), Box::new(prop)),
        position,
    )
}

fn prepend(first: ASTNodeType, most: ASTNodeType) -> ASTNodeType {
    ASTNodeType::Prepend(Box::new(first), Box::new(most))
}

fn _prepend(first: ASTNode, most: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::Prepend(Box::new(first), Box::new(most)),
        position,
    )
}

fn extension_list(list: Vec<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::ExtensionList(list)
}

fn _extension_list(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::ExtensionList(list), position)
}

fn extension_set(list: Vec<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::ExtensionSet(list)
}

fn _extension_set(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::ExtensionSet(list), position)
}

fn _for(var: &str, iter: ASTNode, block: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::For(var.into(), Box::new(iter), block),
        position,
    )
}

type NodeResult = Result<ASTNodeType, ParserError>;
type _NodeResult = Result<ASTNode, ParserError>;

#[allow(dead_code)]
impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn _next(&mut self) -> Option<_NodeResult> {
        match self.peek_token() {
            None => None,
            _ => Some(self._expression(Precedence::Lowest)),
        }
    }

    fn _peek_pos(&mut self) -> Position {
        if let Some(Token { position, .. }) = self.tokens.peek() {
            *position
        } else {
            self.cur_pos
        }
    }

    fn _expression(&mut self, precedence: Precedence) -> _NodeResult {
        let start = self._peek_pos().start;

        let mut expr = match self.next_token() {
            None => Err(ParserError::EOFReached),
            Some(tok) => match tok {
                TokenType::Char(chr) => self.char(chr),
                TokenType::For => self.for__(),
                TokenType::If => self.if__(),
                TokenType::Let => self.let__(),
                TokenType::True => self.boolean(true),
                TokenType::False => self.boolean(false),
                TokenType::Lparen => self.parenthesis_(),
                TokenType::Lbrace => self.set_(),
                TokenType::Lbrack => self.list_(),
                TokenType::Integer(int) => self.integer(int),
                TokenType::Ident(literal) => self.symbol(literal),
                TokenType::String(str) => self.string(str),
                TokenType::Wildcard => self.wildcard(),
                tok if PrefixOperator::from(tok.clone()).is_some() => {
                    self.prefix_(PrefixOperator::from(tok).unwrap())
                }
                tok => Err(ParserError::ExpectedExpression(tok)),
            },
        }?;

        while let Some(op) = self.current_infix() {
            if precedence < op.precedence() {
                self.next_token();
                expr = self.infix_(expr, op, start)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn wildcard(&self) -> _NodeResult {
        self.node_with_cur(ASTNodeType_::Wildcard)
    }

    fn char(&self, chr: char) -> _NodeResult {
        self.node_with_cur(ASTNodeType_::Char(chr))
    }

    fn string(&self, str: String) -> _NodeResult {
        self.node_with_cur(ASTNodeType_::String(str))
    }

    fn boolean(&self, val: bool) -> _NodeResult {
        self.node_with_cur(ASTNodeType_::Boolean(val))
    }

    fn node_with_cur(&self, node: ASTNodeType_) -> _NodeResult {
        Ok(ASTNode::new(node, self.cur_pos))
    }

    fn symbol(&self, literal: String) -> _NodeResult {
        self.node_with_cur(ASTNodeType_::Symbol(literal))
    }

    fn let__(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        let sg = self.signature_()?;

        match self.next_token() {
            Some(TokenType::Assign) => self
                ._expression(Precedence::Lowest)
                .map(|res| _let_(sg, vec![], res, self.start_to_cur(start))),
            Some(TokenType::Lparen) => {
                let (sg, params, val) = self.let_function_with_arguments_(sg)?;

                Ok(_let_(sg, params, val, self.start_to_cur(start)))
            }
            _ => Ok(sg),
        }
    }

    fn let_function_with_arguments_(
        &mut self,
        name: ASTNode,
    ) -> Result<(ASTNode, Vec<ASTNode>, ASTNode), ParserError> {
        let args_res = self.sequence(TokenType::Rparen, None);

        match (args_res, self.next_token()) {
            (Ok(args), Some(TokenType::Assign)) => match self._expression(Precedence::Lowest) {
                Ok(expr) => Ok((name, args, expr)),
                Err(err) => Err(err),
            },
            (Err(err), _) => Err(err),
            (_, Some(tok)) => Err(ParserError::UnexpectedToken(vec![TokenType::Assign], tok)),
            (Ok(_), None) => Err(ParserError::EOFExpecting(vec![TokenType::Assign])),
        }
    }

    fn sequence(
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
                let expr = self._expression(Precedence::Lowest)?;
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

    fn signature_(&mut self) -> _NodeResult {
        match (self.next_token(), self.peek_token()) {
            (Some(TokenType::Ident(name)), Some(TokenType::Colon)) => {
                let symbol_pos = self.cur_pos;
                self.next_token();
                self.type__().map(|tp| {
                    _signature(
                        _symbol(&name, symbol_pos),
                        Some(tp),
                        self.start_to_cur(symbol_pos.start),
                    )
                })
            }
            (Some(TokenType::Ident(name)), _) => Ok(_symbol(&name, self.cur_pos)),
            (Some(tok), _) => Err(ParserError::UnexpectedToken(
                vec![TokenType::Ident(String::from(""))],
                tok,
            )),
            (None, _) => Err(ParserError::EOFExpecting(vec![TokenType::Ident(
                String::from(""),
            )])),
        }
    }

    fn type__(&mut self) -> _NodeResult {
        self._expression(Precedence::Lowest)
    }

    fn start_to_cur(&self, start: u32) -> Position {
        Position::new(start, self.cur_pos.start + self.cur_pos.length - start)
    }

    fn infix_(&mut self, lhs: ASTNode, op: InfixOperator, start: u32) -> _NodeResult {
        if op == InfixOperator::Call {
            let tuple_start = self.cur_pos.start;

            self.sequence(TokenType::Rparen, None).map(|args| {
                _infix(
                    op,
                    lhs,
                    _tuple(args, self.start_to_cur(tuple_start)),
                    self.start_to_cur(start),
                )
            })
        } else {
            self._expression(op.precedence()).map(|rhs| {
                ASTNode::new(
                    ASTNodeType_::Infix(op, Box::new(lhs), Box::new(rhs)),
                    self.start_to_cur(start),
                )
            })
        }
    }

    fn integer(&self, int: String) -> _NodeResult {
        self.node_with_cur(ASTNodeType_::Integer(int))
    }

    fn parenthesis_(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        if matches!(self.peek_token(), Some(TokenType::Rparen)) {
            self.next_token();
            return Ok(_tuple(vec![], self.start_to_cur(start)));
        }

        let res = self._expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Rparen) => Ok(res),
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rparen, Some(res))
                .map(|lst| _tuple(lst, self.start_to_cur(start))),
            Some(tok) => Err(ParserError::UnexpectedToken(vec![TokenType::Rparen], tok)),
            None => Err(ParserError::EOFExpecting_(
                vec![TokenType::Rparen],
                self.start_to_cur(start),
            )),
        }
    }

    pub fn program(&mut self) -> Vec<ASTNodeType> {
        let mut res = vec![];

        loop {
            let exp = self.next();

            match exp {
                Some(Ok(node)) => res.push(node),
                _ => break res,
            }
        }
    }

    pub fn program_(&mut self) -> Vec<ASTNode> {
        let mut res = vec![];

        loop {
            let exp = self._next();

            match exp {
                Some(Ok(node)) => res.push(node),
                _ => break res,
            }
        }
    }

    fn next_token(&mut self) -> Option<TokenType> {
        match self.tokens.next() {
            Some(Token { token, position }) => {
                self.cur_pos = position;
                Some(token)
            }
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
                .map(|res| let_(sg, vec![], res)),
            Some(TokenType::Lparen) => self.let_function_with_arguments(sg),
            _ => Ok(sg),
        }
    }

    fn signature(&mut self) -> NodeResult {
        match (self.next_token(), self.peek_token()) {
            (Some(TokenType::Ident(name)), Some(TokenType::Colon)) => {
                self.next_token();
                self.type_().map(|tp| signature(symbol(&name), Some(tp)))
            }
            (Some(TokenType::Ident(name)), _) => Ok(symbol(&name)),
            (Some(tok), _) => Err(ParserError::UnexpectedToken(
                vec![TokenType::Ident(String::from(""))],
                tok,
            )),
            (None, _) => Err(ParserError::EOFExpecting(vec![TokenType::Ident(
                String::from(""),
            )])),
        }
    }

    fn let_function_with_arguments(&mut self, name: ASTNodeType) -> NodeResult {
        let args_res = self.list(TokenType::Rparen, None);

        match (args_res, self.next_token()) {
            (Ok(args), Some(TokenType::Assign)) => match self.expression(Precedence::Lowest) {
                Ok(expr) => Ok(let_(name, args, expr)),
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
        first: Option<ASTNodeType>,
    ) -> Result<Vec<ASTNodeType>, ParserError> {
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
                TokenType::Char(chr) => Ok(ASTNodeType::Char(chr)),
                TokenType::For => self.for_(),
                TokenType::If => self.if_(),
                TokenType::Let => self.let_(),
                TokenType::True => Ok(ASTNodeType::Boolean(true)),
                TokenType::False => Ok(ASTNodeType::Boolean(false)),
                TokenType::Lparen => self.parenthesis(),
                TokenType::Lbrace => self.set(),
                TokenType::Lbrack => self.my_list(),
                TokenType::Integer(int) => Ok(integer(&int)),
                TokenType::Ident(literal) => Ok(symbol(&literal)),
                TokenType::String(str) => Ok(ASTNodeType::String(str)),
                TokenType::Wildcard => Ok(ASTNodeType::Wildcard),
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
            return Ok(extension_list(vec![]));
        }

        let first = self.expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Colon) => self.comprehension_list(first),
            Some(TokenType::Comma) => self
                .list(TokenType::Rbrack, Some(first))
                .map(extension_list),
            Some(TokenType::Rbrack) => Ok(extension_list(vec![first])),
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

    fn list_(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        if matches!(self.peek_token(), Some(TokenType::Rbrack)) {
            self.next_token();
            return Ok(_extension_list(vec![], self.start_to_cur(start)));
        }

        let first = self._expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Colon) => self.comprehension_list_(first, start),
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rbrack, Some(first))
                .map(|lst| _extension_list(lst, self.start_to_cur(start))),
            Some(TokenType::Rbrack) => Ok(_extension_list(vec![first], self.start_to_cur(start))),
            Some(TokenType::VerticalBar) => self.prepend_(first, start),
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

    fn comprehension_list(&mut self, first: ASTNodeType) -> NodeResult {
        let last = self.expression(Precedence::Lowest)?;
        self.consume(TokenType::Rbrack)?;

        Ok(comprehension_list(first, last))
    }

    fn comprehension_list_(&mut self, first: ASTNode, start: u32) -> _NodeResult {
        let last = self._expression(Precedence::Lowest)?;
        self.consume(TokenType::Rbrack)?;

        Ok(_comprehension_list(first, last, self.start_to_cur(start)))
    }

    fn prepend(&mut self, first: ASTNodeType) -> NodeResult {
        let last = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Rbrack)?;

        Ok(prepend(first, last))
    }

    fn prepend_(&mut self, first: ASTNode, start: u32) -> _NodeResult {
        let last = self._expression(Precedence::Lowest)?;

        self.consume(TokenType::Rbrack)?;

        Ok(_prepend(first, last, self.start_to_cur(start)))
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
            ASTNodeType::Tuple(v) => v,
            node => vec![node],
        };

        Ok(ASTNodeType::For(ident, Box::new(iter), proc))
    }

    fn for__(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

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

        let iter = self._expression(Precedence::Lowest)?;

        self.consume(TokenType::Colon)?;

        let ASTNode { _type, position } = self._expression(Precedence::Lowest)?;

        let proc = match _type {
            ASTNodeType_::Tuple(v) => v,
            node => vec![ASTNode::new(node, position)],
        };

        Ok(_for(&ident, iter, proc, self.start_to_cur(start)))
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

        Ok(if_(cond, first_res, second_res))
    }

    fn if__(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        let cond = self._expression(Precedence::Lowest)?;

        self.consume(TokenType::Then)?;

        let first_res = self._expression(Precedence::Lowest)?;

        self.consume(TokenType::Else)?;

        let second_res = self._expression(Precedence::Lowest)?;

        Ok(_if_(cond, first_res, second_res, self.start_to_cur(start)))
    }

    fn current_infix(&mut self) -> Option<InfixOperator> {
        self.peek_token().and_then(InfixOperator::from)
    }

    fn prefix(&mut self, op: PrefixOperator) -> NodeResult {
        self.expression(Precedence::Highest)
            .map(|expr| prefix(op, expr))
    }

    fn prefix_(&mut self, op: PrefixOperator) -> _NodeResult {
        let start = self.cur_pos.start;

        self._expression(Precedence::Highest)
            .map(|expr| _prefix(op, expr, self.start_to_cur(start)))
    }

    fn parenthesis(&mut self) -> NodeResult {
        if matches!(self.peek_token(), Some(TokenType::Rparen)) {
            self.next_token();
            return Ok(tuple(vec![]));
        }

        let res = self.expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Rparen) => Ok(res),
            Some(TokenType::Comma) => self.list(TokenType::Rparen, Some(res)).map(tuple),
            Some(tok) => Err(ParserError::UnexpectedToken(vec![TokenType::Rparen], tok)),
            None => Err(ParserError::EOFExpecting(vec![TokenType::Rparen])),
        }
    }

    fn infix(&mut self, lhs: ASTNodeType, op: InfixOperator) -> NodeResult {
        if op == InfixOperator::Call {
            self.list(TokenType::Rparen, None)
                .map(|args| infix(op, lhs, tuple(args)))
        } else {
            self.expression(op.precedence())
                .map(|rhs| infix(op, lhs, rhs))
        }
    }

    fn type_(&mut self) -> NodeResult {
        self.expression(Precedence::Lowest)
    }

    fn set(&mut self) -> NodeResult {
        if matches!(self.peek_token(), Some(TokenType::Rbrace)) {
            self.next_token();
            return Ok(extension_set(vec![]));
        }

        let first = self.expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Comma) => self.list(TokenType::Rbrace, Some(first)).map(extension_set),
            Some(TokenType::Colon) => self
                .expression(Precedence::Lowest)
                .map(|second| comprehension_set(first, second)),
            Some(TokenType::Rbrace) => Ok(extension_set(vec![first])),
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

    fn set_(&mut self) -> _NodeResult {
        let start = self.cur_pos.start;

        if matches!(self.peek_token(), Some(TokenType::Rbrace)) {
            self.next_token();
            return Ok(_extension_set(vec![], self.start_to_cur(start)));
        }

        let first = self._expression(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rbrace, Some(first))
                .map(|lst| _extension_set(lst, self.start_to_cur(start))),
            Some(TokenType::Colon) => {
                let second = self._expression(Precedence::Lowest)?;

                self.consume(TokenType::Rbrace)?;

                Ok(_comprehension_set(first, second, self.start_to_cur(start)))
            }
            Some(TokenType::Rbrace) => Ok(_extension_set(vec![first], self.start_to_cur(start))),
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
        cur_pos: Position::new(0, 0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::Position, lexer::build_lexer};
    use std::iter;

    #[test]
    fn empty_expression() {
        assert_eq!(parser_from(iter::empty::<Token>())._next(), None);
    }

    #[test]
    fn integer_alone() {
        let tokens = vec![Token::new(
            TokenType::Integer(String::from("0")),
            _pos(0, 1),
        )];
        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_integer("0", _pos(0, 1))))
        );
    }

    #[test]
    fn integer_in_parenthesis() {
        let tokens = vec![
            Token::new(TokenType::Lparen, _pos(0, 1)),
            Token::new(TokenType::Integer(String::from("365")), _pos(1, 3)),
            Token::new(TokenType::Rparen, _pos(4, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_integer("365", _pos(1, 3)))),
        );
    }

    #[test]
    fn unbalanced_left_parenthesis() {
        let tokens = vec![
            Token::new(TokenType::Lparen, _pos(0, 1)),
            Token::new(TokenType::Integer(String::from("65")), _pos(1, 2)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Err(ParserError::EOFExpecting_(
                vec![TokenType::Rparen,],
                _pos(0, 3)
            )))
        );
    }

    #[test]
    fn simple_sum() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), _pos(0, 1)),
            Token::new(TokenType::Plus, _pos(2, 1)),
            Token::new(TokenType::Integer(String::from("1")), _pos(4, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_infix(
                InfixOperator::Sum,
                _integer("1", _pos(0, 1)),
                _integer("1", _pos(4, 1)),
                _pos(0, 5)
            )))
        );
    }

    #[test]
    fn incomplete_sum() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), _pos(0, 1)),
            Token::new(TokenType::Plus, _pos(1, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Err(ParserError::EOFReached))
        );
    }

    #[test]
    fn product_and_power() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), _pos(0, 1)),
            Token::new(TokenType::Times, _pos(1, 1)),
            Token::new(TokenType::Integer(String::from("2")), _pos(2, 1)),
            Token::new(TokenType::ToThe, _pos(4, 2)),
            Token::new(TokenType::Integer(String::from("2")), _pos(7, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_infix(
                InfixOperator::Product,
                _integer("1", _pos(0, 1)),
                _infix(
                    InfixOperator::Exponentiation,
                    _integer("2", _pos(2, 1)),
                    _integer("2", _pos(7, 1)),
                    _pos(2, 6),
                ),
                _pos(0, 8)
            )))
        );
    }

    #[test]
    fn division_and_sum() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), _pos(0, 1)),
            Token::new(TokenType::Over, _pos(1, 1)),
            Token::new(TokenType::Integer(String::from("1")), _pos(2, 1)),
            Token::new(TokenType::Plus, _pos(4, 1)),
            Token::new(TokenType::Integer(String::from("1")), _pos(6, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_infix(
                InfixOperator::Sum,
                _infix(
                    InfixOperator::Division,
                    _integer("1", _pos(0, 1)),
                    _integer("1", _pos(2, 1)),
                    _pos(0, 3)
                ),
                _integer("1", _pos(6, 1)),
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
            Token::new(TokenType::Integer(String::from("1")), Position::new(9, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_let_(
                _symbol("x", _pos(4, 1)),
                vec![],
                _integer("1", _pos(9, 1)),
                _pos(0, 10)
            )))
        );
    }

    #[test]
    fn comparison_precedence() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), _pos(0, 1)),
            Token::new(TokenType::Plus, _pos(2, 1)),
            Token::new(TokenType::Integer(String::from("5")), _pos(4, 1)),
            Token::new(TokenType::NotEqual, _pos(6, 2)),
            Token::new(TokenType::Integer(String::from("6")), _pos(9, 1)),
            Token::new(TokenType::Mod, _pos(11, 1)),
            Token::new(TokenType::Integer(String::from("2")), _pos(13, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_infix(
                InfixOperator::NotEquality,
                _infix(
                    InfixOperator::Sum,
                    _integer("1", _pos(0, 1)),
                    _integer("5", _pos(4, 1)),
                    _pos(0, 5)
                ),
                _infix(
                    InfixOperator::Mod,
                    _integer("6", _pos(9, 1)),
                    _integer("2", _pos(13, 1)),
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
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_let_(
                _symbol("f", _pos(4, 1)),
                vec![_symbol("x", _pos(6, 1)), _symbol("y", _pos(9, 1))],
                _infix(
                    InfixOperator::Sum,
                    _symbol("x", _pos(15, 1)),
                    _symbol("y", _pos(19, 1)),
                    _pos(15, 5)
                ),
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
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_extension_set(vec![], _pos(0, 2))))
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
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_extension_set(
                vec![_boolean(true, _pos(2, 4)), _boolean(false, _pos(9, 4)),],
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
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_tuple(vec![], _pos(0, 2))))
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
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_tuple(
                vec![_symbol("Real", _pos(1, 4)), _symbol("Real", _pos(7, 4))],
                _pos(0, 12)
            )))
        );
    }

    #[test]
    fn set_comprehension() {
        let tokens = vec![
            Token::new(TokenType::Lbrace, _pos(0, 1)),
            Token::new(TokenType::Ident(String::from("a")), _pos(1, 1)),
            Token::new(TokenType::Colon, _pos(3, 1)),
            Token::new(TokenType::Ident(String::from("a")), _pos(5, 1)),
            Token::new(TokenType::Equals, _pos(7, 1)),
            Token::new(TokenType::Integer(String::from("1")), _pos(9, 1)),
            Token::new(TokenType::Rbrace, _pos(10, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_comprehension_set(
                _symbol("a", _pos(1, 1)),
                _infix(
                    InfixOperator::Equality,
                    _symbol("a", _pos(5, 1)),
                    _integer("1", _pos(9, 1)),
                    _pos(5, 5)
                ),
                _pos(0, 11),
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
            Token::new(TokenType::Integer(String::from("1")), _pos(16, 1)),
            Token::new(TokenType::Plus, _pos(18, 1)),
            Token::new(TokenType::Integer(String::from("0")), _pos(20, 1)),
            Token::new(TokenType::Mod, _pos(22, 1)),
            Token::new(TokenType::Integer(String::from("2")), _pos(24, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_let_(
                _signature(
                    _symbol("x", _pos(4, 1)),
                    Some(_symbol("Real", _pos(8, 4))),
                    _pos(4, 8)
                ),
                vec![],
                _infix(
                    InfixOperator::Sum,
                    _integer("1", _pos(16, 1)),
                    _infix(
                        InfixOperator::Mod,
                        _integer("0", _pos(20, 1)),
                        _integer("2", _pos(24, 1)),
                        _pos(20, 5)
                    ),
                    _pos(16, 9),
                ),
                _pos(0, 25),
            )))
        );
    }

    #[test]
    fn shift_operator() {
        let tokens = vec![
            Token::new(TokenType::Ident(String::from('x')), _pos(0, 1)),
            Token::new(TokenType::Minus, _pos(2, 1)),
            Token::new(TokenType::Integer(String::from('1')), _pos(4, 1)),
            Token::new(TokenType::LeftShift, _pos(6, 2)),
            Token::new(TokenType::Integer(String::from('1')), _pos(9, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_infix(
                InfixOperator::LeftShift,
                _infix(
                    InfixOperator::Substraction,
                    _symbol("x", _pos(0, 1)),
                    _integer("1", _pos(4, 1)),
                    _pos(0, 5)
                ),
                _integer("1", _pos(9, 1)),
                _pos(0, 10),
            )))
        );
    }

    #[test]
    fn shift_and_comparison() {
        let lexer = build_lexer("1 << 1 > 1").map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer)._next(),
            Some(Ok(_infix(
                InfixOperator::Greater,
                _infix(
                    InfixOperator::LeftShift,
                    _integer("1", _pos(0, 1)),
                    _integer("1", _pos(5, 1)),
                    _pos(0, 6)
                ),
                _integer("1", _pos(9, 1)),
                _pos(0, 10)
            )))
        );
    }

    #[test]
    fn bitwise() {
        let lexer = build_lexer("a & b || c").map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer)._next(),
            Some(Ok(_infix(
                InfixOperator::Or,
                _infix(
                    InfixOperator::BitwiseAnd,
                    _symbol("a", _pos(0, 1)),
                    _symbol("b", _pos(4, 1)),
                    _pos(0, 5)
                ),
                _symbol("c", _pos(9, 1)),
                _pos(0, 10),
            )))
        );
    }

    #[test]
    fn logic_infix_operators() {
        let lexer = build_lexer("a && b || c");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_infix(
                InfixOperator::Or,
                _infix(
                    InfixOperator::LogicAnd,
                    _symbol("a", _pos(0, 1)),
                    _symbol("b", _pos(5, 1)),
                    _pos(0, 6)
                ),
                _symbol("c", _pos(10, 1)),
                _pos(0, 11),
            )))
        );
    }

    #[test]
    fn complex_precedence() {
        let lexer = build_lexer("  a + b || a & b << c");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_infix(
                InfixOperator::Or,
                _infix(
                    InfixOperator::Sum,
                    _symbol("a", _pos(2, 1)),
                    _symbol("b", _pos(6, 1)),
                    _pos(2, 5)
                ),
                _infix(
                    InfixOperator::BitwiseAnd,
                    _symbol("a", _pos(11, 1)),
                    _infix(
                        InfixOperator::LeftShift,
                        _symbol("b", _pos(15, 1)),
                        _symbol("c", _pos(20, 1)),
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
        let lexer = build_lexer("a ^ b & c || d");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_infix(
                InfixOperator::Or,
                _infix(
                    InfixOperator::BitwiseXor,
                    _symbol("a", _pos(0, 1)),
                    _infix(
                        InfixOperator::BitwiseAnd,
                        _symbol("b", _pos(4, 1)),
                        _symbol("c", _pos(8, 1)),
                        _pos(4, 5)
                    ),
                    _pos(0, 9),
                ),
                _symbol("d", _pos(13, 1)),
                _pos(0, 14)
            )))
        );
    }

    #[test]
    fn something_after_empty_set() {
        let lexer = build_lexer("({}, 0)");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_tuple(
                vec![
                    _extension_set(vec![], _pos(1, 2)),
                    _integer("0", _pos(5, 1))
                ],
                _pos(0, 7)
            )))
        );
    }

    #[test]
    fn prefixes() {
        let lexer = build_lexer("!(~1 /= -1)");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_prefix(
                PrefixOperator::LogicNot,
                _infix(
                    InfixOperator::NotEquality,
                    _prefix(
                        PrefixOperator::BitwiseNot,
                        _integer("1", _pos(3, 1)),
                        _pos(2, 2)
                    ),
                    _prefix(PrefixOperator::Minus, _integer("1", _pos(9, 1)), _pos(8, 2)),
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
            Token::new(TokenType::Integer(String::from("0")), _pos(7, 1)),
            Token::new(TokenType::Then, _pos(9, 4)),
            Token::new(TokenType::Minus, _pos(14, 1)),
            Token::new(TokenType::Ident(String::from("a")), _pos(15, 1)),
            Token::new(TokenType::Else, _pos(17, 4)),
            Token::new(TokenType::Ident(String::from("a")), _pos(22, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter())._next(),
            Some(Ok(_if_(
                _infix(
                    InfixOperator::Less,
                    _symbol("a", _pos(3, 1)),
                    _integer("0", _pos(7, 1)),
                    _pos(3, 5)
                ),
                _prefix(
                    PrefixOperator::Minus,
                    _symbol("a", _pos(15, 1)),
                    _pos(14, 2)
                ),
                _symbol("a", _pos(22, 1)),
                _pos(0, 23),
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
            parser_from(lexer.map(|res| res.unwrap())).program_(),
            vec![
                _let_(
                    _symbol("a", _pos(4, 1)),
                    vec![],
                    _integer("5", _pos(9, 1)),
                    _pos(0, 10)
                ),
                _infix(
                    InfixOperator::Product,
                    _symbol("a", _pos(19, 1)),
                    _symbol("a", _pos(23, 1)),
                    _pos(19, 5)
                )
            ],
        );
    }

    #[test]
    fn function_call() {
        let input = "f(x, y)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_infix(
                InfixOperator::Call,
                _symbol("f", _pos(0, 1)),
                _tuple(
                    vec![_symbol("x", _pos(2, 1)), _symbol("y", _pos(5, 1)),],
                    _pos(1, 6)
                ),
                _pos(0, 7),
            ))),
        );
    }

    #[test]
    fn comma_last_item() {
        let input = "(1,)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_tuple(vec![_integer("1", _pos(1, 1))], _pos(0, 4)))),
        );
    }

    #[test]
    fn anon_function() {
        let input = "x -> 2*x";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_infix(
                InfixOperator::Correspondence,
                _symbol("x", _pos(0, 1)),
                _infix(
                    InfixOperator::Product,
                    _integer("2", _pos(5, 1)),
                    _symbol("x", _pos(7, 1)),
                    _pos(5, 3)
                ),
                _pos(0, 8),
            )))
        );
    }

    #[test]
    fn anon_function_call() {
        let input = "((x, y) -> x)(1, 2)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_infix(
                InfixOperator::Call,
                _infix(
                    InfixOperator::Correspondence,
                    _tuple(
                        vec![_symbol("x", _pos(2, 1)), _symbol("y", _pos(5, 1)),],
                        _pos(1, 6)
                    ),
                    _symbol("x", _pos(11, 1)),
                    _pos(1, 11),
                ),
                _tuple(
                    vec![_integer("1", _pos(14, 1)), _integer("2", _pos(17, 1)),],
                    _pos(13, 6)
                ),
                _pos(0, 19)
            )))
        );
    }

    #[test]
    fn char_and_string() {
        let input = "('a', \"b\")";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_tuple(
                vec![_char('a', _pos(1, 3)), _string("b", _pos(6, 3)),],
                _pos(0, 10),
            )))
        );
    }

    #[test]
    fn for_loop() {
        let input = "for i in list: println(i)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_for(
                "i",
                _symbol("list", _pos(9, 4)),
                vec![_infix(
                    InfixOperator::Call,
                    _symbol("println", _pos(15, 7)),
                    _tuple(vec![_symbol("i", _pos(23, 1))], _pos(22, 3)),
                    _pos(15, 10)
                )],
                _pos(0, 25)
            )))
        );
    }

    #[test]
    fn in_question() {
        let input = "1 in { k : k >= 1 }";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_infix(
                InfixOperator::In,
                _integer("1", _pos(0, 1)),
                _comprehension_set(
                    _symbol("k", _pos(7, 1)),
                    _infix(
                        InfixOperator::GreaterEqual,
                        _symbol("k", _pos(11, 1)),
                        _integer("1", _pos(16, 1)),
                        _pos(11, 6)
                    ),
                    _pos(5, 14),
                ),
                _pos(0, 19),
            )))
        );
    }

    #[test]
    fn list() {
        let input = "[[], 2]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_extension_list(
                vec![
                    _extension_list(vec![], _pos(1, 2)),
                    _integer("2", _pos(5, 1)),
                ],
                _pos(0, 7),
            ))),
        );
    }

    #[test]
    fn comprehension_list_only() {
        let input = "[ k in [1, 2] : k - 1 = 0 ]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_comprehension_list(
                _infix(
                    InfixOperator::In,
                    _symbol("k", _pos(2, 1)),
                    _extension_list(
                        vec![_integer("1", _pos(8, 1)), _integer("2", _pos(11, 1)),],
                        _pos(7, 6)
                    ),
                    _pos(2, 11)
                ),
                _infix(
                    InfixOperator::Equality,
                    _infix(
                        InfixOperator::Substraction,
                        _symbol("k", _pos(16, 1)),
                        _integer("1", _pos(20, 1)),
                        _pos(16, 5)
                    ),
                    _integer("0", _pos(24, 1)),
                    _pos(16, 9)
                ),
                _pos(0, 27)
            )))
        );
    }

    #[test]
    fn singleton_empty_set() {
        let input = "{{}}";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_extension_set(
                vec![_extension_set(vec![], _pos(1, 2))],
                _pos(0, 4)
            ))),
        );
    }

    #[test]
    fn wildcard() {
        let input = "[a, 1, _]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap()))._next(),
            Some(Ok(_extension_list(
                vec![
                    _symbol("a", _pos(1, 1)),
                    _integer("1", _pos(4, 1)),
                    ASTNode::new(ASTNodeType_::Wildcard, _pos(7, 1)),
                ],
                _pos(0, 9)
            ),)),
        );
    }

    #[test]
    fn prepend_only() {
        let code = "[1|[2,3]]";
        let lexer = build_lexer(code).map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer)._next(),
            Some(Ok(_prepend(
                _integer("1", _pos(1, 1)),
                _extension_list(
                    vec![_integer("2", _pos(4, 1)), _integer("3", _pos(6, 1)),],
                    _pos(3, 5)
                ),
                _pos(0, 9)
            ))),
        );
    }

    #[test]
    fn consume_comprehension_list() {
        let input = "[a : a in b] + []";
        let lexer = build_lexer(input).map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer)._next(),
            Some(Ok(_infix(
                InfixOperator::Sum,
                _comprehension_list(
                    _symbol("a", _pos(1, 1)),
                    _infix(
                        InfixOperator::In,
                        _symbol("a", _pos(5, 1)),
                        _symbol("b", _pos(10, 1)),
                        _pos(5, 6)
                    ),
                    _pos(0, 12)
                ),
                _extension_list(vec![], _pos(15, 2)),
                _pos(0, 17)
            )))
        );
    }

    #[test]
    fn expected_rparen() {
        let input = "(15]";
        let lexer = build_lexer(input).map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer)._next(),
            Some(Err(ParserError::UnexpectedToken(
                vec![TokenType::Rparen],
                TokenType::Rbrack
            ))),
        );
    }

    #[test]
    fn expected_expression() {
        let input = "1 + )";
        let lexer = build_lexer(input).map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer)._next(),
            Some(Err(ParserError::ExpectedExpression(TokenType::Rparen))),
        );
    }
}
