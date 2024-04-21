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
    pos_stack: Vec<Position>,
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

fn pos(start: u32, length: u32) -> Position {
    Position::new(start, length)
}

fn boolean_(val: bool, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Boolean(val), position)
}

fn char_(val: char, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Char(val), position)
}

fn string_(str: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::String(str.into()), position)
}

fn symbol(name: &str) -> ASTNodeType {
    ASTNodeType::Symbol(name.to_string())
}

fn symbol_(name: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Symbol(name.into()), position)
}

fn integer(int: &str) -> ASTNodeType {
    ASTNodeType::Integer(int.to_string())
}

fn integer_(int: &str, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Integer(int.into()), position)
}

fn infix(op: InfixOperator, lhs: ASTNodeType, rhs: ASTNodeType) -> ASTNodeType {
    ASTNodeType::Infix(op, Box::new(lhs), Box::new(rhs))
}

fn infix_(op: InfixOperator, lhs: ASTNode, rhs: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::Infix(op, Box::new(lhs), Box::new(rhs)),
        position,
    )
}

fn prefix(op: PrefixOperator, val: ASTNodeType) -> ASTNodeType {
    ASTNodeType::Prefix(op, Box::new(val))
}

fn prefix_(op: PrefixOperator, val: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Prefix(op, Box::new(val)), position)
}

fn let_(ident: ASTNodeType, params: Vec<ASTNodeType>, val: ASTNodeType) -> ASTNodeType {
    ASTNodeType::Let(Box::new(ident), params, Box::new(val))
}

fn let__(ident: ASTNode, params: Vec<ASTNode>, val: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::Let(Box::new(ident), params, Box::new(val)),
        position,
    )
}

fn signature(symbol: ASTNodeType, type_: Option<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::Signature(Box::new(symbol), type_.map(Box::new))
}

fn signature_(symbol: ASTNode, type_: Option<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::Signature(Box::new(symbol), type_.map(Box::new)),
        position,
    )
}

fn if_(cond: ASTNodeType, first_res: ASTNodeType, second_res: ASTNodeType) -> ASTNodeType {
    ASTNodeType::If(Box::new(cond), Box::new(first_res), Box::new(second_res))
}

fn if__(cond: ASTNode, first_res: ASTNode, second_res: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::If(Box::new(cond), Box::new(first_res), Box::new(second_res)),
        position,
    )
}

fn tuple(list: Vec<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::Tuple(list)
}

fn tuple_(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::Tuple(list), position)
}

fn comprehension_set(val: ASTNodeType, prop: ASTNodeType) -> ASTNodeType {
    ASTNodeType::ComprehensionSet(Box::new(val), Box::new(prop))
}

fn comprehension_set_(val: ASTNode, prop: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::ComprehensionSet(Box::new(val), Box::new(prop)),
        position,
    )
}

fn comprehension_list(val: ASTNodeType, prop: ASTNodeType) -> ASTNodeType {
    ASTNodeType::ComprehensionList(Box::new(val), Box::new(prop))
}

fn comprehension_list_(val: ASTNode, prop: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::ComprehensionList(Box::new(val), Box::new(prop)),
        position,
    )
}

fn prepend(first: ASTNodeType, most: ASTNodeType) -> ASTNodeType {
    ASTNodeType::Prepend(Box::new(first), Box::new(most))
}

fn prepend_(first: ASTNode, most: ASTNode, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::Prepend(Box::new(first), Box::new(most)),
        position,
    )
}

fn extension_list(list: Vec<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::ExtensionList(list)
}

fn extension_list_(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::ExtensionList(list), position)
}

fn extension_set(list: Vec<ASTNodeType>) -> ASTNodeType {
    ASTNodeType::ExtensionSet(list)
}

fn extension_set_(list: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(ASTNodeType_::ExtensionSet(list), position)
}

fn for_(var: &str, iter: ASTNode, block: Vec<ASTNode>, position: Position) -> ASTNode {
    ASTNode::new(
        ASTNodeType_::For(var.into(), Box::new(iter), block),
        position,
    )
}

type NodeResult = Result<ASTNodeType, ParserError>;
type NodeResult_ = Result<ASTNode, ParserError>;

impl<T: Iterator<Item = Token>> Parser<T> {
    fn next_(&mut self) -> Option<NodeResult_> {
        match self.peek_token() {
            None => None,
            _ => Some(self.expression_(Precedence::Lowest)),
        }
    }

    fn peek_pos(&mut self) -> Position {
        if let Some(Token { position, .. }) = self.tokens.peek() {
            *position
        } else {
            pos(0, 0)
        }
    }

    fn expression_(&mut self, precedence: Precedence) -> NodeResult_ {
        let start = self.peek_pos().start;

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
                // tok => Err(ParserError::ExpectedExpression(tok)),
                _ => todo!(),
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

    fn wildcard(&self) -> NodeResult_ {
        self.node_with_cur(ASTNodeType_::Wildcard)
    }

    fn char(&self, chr: char) -> NodeResult_ {
        self.node_with_cur(ASTNodeType_::Char(chr))
    }

    fn string(&self, str: String) -> NodeResult_ {
        self.node_with_cur(ASTNodeType_::String(str))
    }

    fn boolean(&self, val: bool) -> NodeResult_ {
        self.node_with_cur(ASTNodeType_::Boolean(val))
    }

    fn node_with_cur(&self, node: ASTNodeType_) -> NodeResult_ {
        Ok(ASTNode::new(node, self.cur_pos))
    }

    fn symbol(&self, literal: String) -> NodeResult_ {
        self.node_with_cur(ASTNodeType_::Symbol(literal))
    }

    fn let__(&mut self) -> NodeResult_ {
        let start = self.cur_pos.start;

        let sg = self.signature_()?;

        match self.next_token() {
            Some(TokenType::Assign) => self
                .expression_(Precedence::Lowest)
                .map(|res| let__(sg, vec![], res, self.start_to_cur(start))),
            Some(TokenType::Lparen) => {
                let (sg, params, val) = self.let_function_with_arguments_(sg)?;

                Ok(let__(sg, params, val, self.start_to_cur(start)))
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
            (Ok(args), Some(TokenType::Assign)) => match self.expression_(Precedence::Lowest) {
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
                let expr = self.expression_(Precedence::Lowest)?;
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

    fn signature_(&mut self) -> NodeResult_ {
        match (self.next_token(), self.peek_token()) {
            (Some(TokenType::Ident(name)), Some(TokenType::Colon)) => {
                let symbol_pos = self.cur_pos;
                self.next_token();
                self.type__().map(|tp| {
                    signature_(
                        symbol_(&name, symbol_pos),
                        Some(tp),
                        self.start_to_cur(symbol_pos.start),
                    )
                })
            }
            (Some(TokenType::Ident(name)), _) => Ok(symbol_(&name, self.cur_pos)),
            (Some(tok), _) => Err(ParserError::UnexpectedToken(
                vec![TokenType::Ident(String::from(""))],
                tok,
            )),
            (None, _) => Err(ParserError::EOFExpecting(vec![TokenType::Ident(
                String::from(""),
            )])),
        }
    }

    fn type__(&mut self) -> NodeResult_ {
        self.expression_(Precedence::Lowest)
    }

    fn start_to_cur(&self, start: u32) -> Position {
        Position::new(start, self.cur_pos.start + self.cur_pos.length - start)
    }

    fn infix_(&mut self, lhs: ASTNode, op: InfixOperator, start: u32) -> NodeResult_ {
        if op == InfixOperator::Call {
            let tuple_start = self.cur_pos.start;

            self.sequence(TokenType::Rparen, None).map(|args| {
                infix_(
                    op,
                    lhs,
                    tuple_(args, self.start_to_cur(tuple_start)),
                    self.start_to_cur(start),
                )
            })
        } else {
            self.expression_(op.precedence()).map(|rhs| {
                ASTNode::new(
                    ASTNodeType_::Infix(op, Box::new(lhs), Box::new(rhs)),
                    self.start_to_cur(start),
                )
            })
        }
    }

    fn integer(&self, int: String) -> NodeResult_ {
        self.node_with_cur(ASTNodeType_::Integer(int))
    }

    fn parenthesis_(&mut self) -> NodeResult_ {
        let start = self.cur_pos.start;

        if matches!(self.peek_token(), Some(TokenType::Rparen)) {
            self.next_token();
            return Ok(tuple_(vec![], self.start_to_cur(start)));
        }

        let res = self.expression_(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Rparen) => Ok(res),
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rparen, Some(res))
                .map(|lst| tuple_(lst, self.start_to_cur(start))),
            // Some(tok) => Err(ParserError::UnexpectedToken(vec![TokenType::Rparen], tok)),
            None => Err(ParserError::EOFExpecting_(
                vec![TokenType::Rparen],
                self.start_to_cur(start),
            )),
            _ => todo!(),
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
            let exp = self.next_();

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

    fn list_(&mut self) -> NodeResult_ {
        let start = self.cur_pos.start;

        if matches!(self.peek_token(), Some(TokenType::Rbrack)) {
            self.next_token();
            return Ok(extension_list_(vec![], self.start_to_cur(start)));
        }

        let first = self.expression_(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Colon) => self.comprehension_list_(first, start),
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rbrack, Some(first))
                .map(|lst| extension_list_(lst, self.start_to_cur(start))),
            Some(TokenType::Rbrack) => Ok(extension_list_(vec![first], self.start_to_cur(start))),
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

    fn comprehension_list_(&mut self, first: ASTNode, start: u32) -> NodeResult_ {
        let last = self.expression_(Precedence::Lowest)?;
        self.consume(TokenType::Rbrack)?;

        Ok(comprehension_list_(first, last, self.start_to_cur(start)))
    }

    fn prepend(&mut self, first: ASTNodeType) -> NodeResult {
        let last = self.expression(Precedence::Lowest)?;

        self.consume(TokenType::Rbrack)?;

        Ok(prepend(first, last))
    }

    fn prepend_(&mut self, first: ASTNode, start: u32) -> NodeResult_ {
        let last = self.expression_(Precedence::Lowest)?;

        self.consume(TokenType::Rbrack)?;

        Ok(prepend_(first, last, self.start_to_cur(start)))
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

    fn for__(&mut self) -> NodeResult_ {
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

        let iter = self.expression_(Precedence::Lowest)?;

        self.consume(TokenType::Colon)?;

        let proc = match self.expression_(Precedence::Lowest)? {
            ASTNode { node, position } => match node {
                ASTNodeType_::Tuple(v) => v,
                node => vec![ASTNode::new(node, position)],
            },
        };

        Ok(for_(&ident, iter, proc, self.start_to_cur(start)))
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

    fn if__(&mut self) -> NodeResult_ {
        let start = self.cur_pos.start;

        let cond = self.expression_(Precedence::Lowest)?;

        self.consume(TokenType::Then)?;

        let first_res = self.expression_(Precedence::Lowest)?;

        self.consume(TokenType::Else)?;

        let second_res = self.expression_(Precedence::Lowest)?;

        Ok(if__(cond, first_res, second_res, self.start_to_cur(start)))
    }

    fn current_infix(&mut self) -> Option<InfixOperator> {
        self.peek_token().and_then(InfixOperator::from)
    }

    fn prefix(&mut self, op: PrefixOperator) -> NodeResult {
        self.expression(Precedence::Highest)
            .map(|expr| prefix(op, expr))
    }

    fn prefix_(&mut self, op: PrefixOperator) -> NodeResult_ {
        let start = self.cur_pos.start;

        self.expression_(Precedence::Highest)
            .map(|expr| prefix_(op, expr, self.start_to_cur(start)))
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

    fn set_(&mut self) -> NodeResult_ {
        let start = self.cur_pos.start;

        if matches!(self.peek_token(), Some(TokenType::Rbrace)) {
            self.next_token();
            return Ok(extension_set_(vec![], self.start_to_cur(start)));
        }

        let first = self.expression_(Precedence::Lowest)?;

        match self.next_token() {
            Some(TokenType::Comma) => self
                .sequence(TokenType::Rbrace, Some(first))
                .map(|lst| extension_set_(lst, self.start_to_cur(start))),
            Some(TokenType::Colon) => {
                let second = self.expression_(Precedence::Lowest)?;

                self.consume(TokenType::Rbrace)?;

                Ok(comprehension_set_(first, second, self.start_to_cur(start)))
            }
            Some(TokenType::Rbrace) => Ok(extension_set_(vec![first], self.start_to_cur(start))),
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
        pos_stack: vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::Position, lexer::build_lexer, parser::integer};
    use std::iter;

    fn iter_from(v: Vec<TokenType>) -> impl Iterator<Item = Token> {
        v.into_iter()
            .map(|tok| Token::new(tok, Position::new(0, 0)))
    }

    #[test]
    fn empty_expression() {
        assert_eq!(parser_from(iter::empty::<Token>()).next_(), None);
    }

    #[test]
    fn integer_alone() {
        let tokens = vec![Token::new(TokenType::Integer(String::from("0")), pos(0, 1))];
        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(integer_("0", pos(0, 1))))
        );
    }

    #[test]
    fn integer_in_parenthesis() {
        let tokens = vec![
            Token::new(TokenType::Lparen, pos(0, 1)),
            Token::new(TokenType::Integer(String::from("365")), pos(1, 3)),
            Token::new(TokenType::Rparen, pos(4, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(integer_("365", pos(1, 3)))),
        );
    }

    #[test]
    fn unbalanced_left_parenthesis() {
        let tokens = vec![
            Token::new(TokenType::Lparen, pos(0, 1)),
            Token::new(TokenType::Integer(String::from("65")), pos(1, 2)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Err(ParserError::EOFExpecting_(
                vec![TokenType::Rparen,],
                pos(0, 3)
            )))
        );
    }

    #[test]
    fn simple_sum() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), pos(0, 1)),
            Token::new(TokenType::Plus, pos(2, 1)),
            Token::new(TokenType::Integer(String::from("1")), pos(4, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(infix_(
                InfixOperator::Sum,
                integer_("1", pos(0, 1)),
                integer_("1", pos(4, 1)),
                pos(0, 5)
            )))
        );
    }

    #[test]
    fn incomplete_sum() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), pos(0, 1)),
            Token::new(TokenType::Plus, pos(1, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Err(ParserError::EOFReached))
        );
    }

    #[test]
    fn product_and_power() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), pos(0, 1)),
            Token::new(TokenType::Times, pos(1, 1)),
            Token::new(TokenType::Integer(String::from("2")), pos(2, 1)),
            Token::new(TokenType::ToThe, pos(4, 2)),
            Token::new(TokenType::Integer(String::from("2")), pos(7, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(infix_(
                InfixOperator::Product,
                integer_("1", pos(0, 1)),
                infix_(
                    InfixOperator::Exponentiation,
                    integer_("2", pos(2, 1)),
                    integer_("2", pos(7, 1)),
                    pos(2, 6),
                ),
                pos(0, 8)
            )))
        );
    }

    #[test]
    fn division_and_sum() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), pos(0, 1)),
            Token::new(TokenType::Over, pos(1, 1)),
            Token::new(TokenType::Integer(String::from("1")), pos(2, 1)),
            Token::new(TokenType::Plus, pos(4, 1)),
            Token::new(TokenType::Integer(String::from("1")), pos(6, 1)),
        ];
        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(infix_(
                InfixOperator::Sum,
                infix_(
                    InfixOperator::Division,
                    integer_("1", pos(0, 1)),
                    integer_("1", pos(2, 1)),
                    pos(0, 3)
                ),
                integer_("1", pos(6, 1)),
                pos(0, 7),
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
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(let__(
                symbol_("x", pos(4, 1)),
                vec![],
                integer_("1", pos(9, 1)),
                pos(0, 10)
            )))
        );
    }

    #[test]
    fn comparison_precedence() {
        let tokens = vec![
            Token::new(TokenType::Integer(String::from("1")), pos(0, 1)),
            Token::new(TokenType::Plus, pos(2, 1)),
            Token::new(TokenType::Integer(String::from("5")), pos(4, 1)),
            Token::new(TokenType::NotEqual, pos(6, 2)),
            Token::new(TokenType::Integer(String::from("6")), pos(9, 1)),
            Token::new(TokenType::Mod, pos(11, 1)),
            Token::new(TokenType::Integer(String::from("2")), pos(13, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(infix_(
                InfixOperator::NotEquality,
                infix_(
                    InfixOperator::Sum,
                    integer_("1", pos(0, 1)),
                    integer_("5", pos(4, 1)),
                    pos(0, 5)
                ),
                infix_(
                    InfixOperator::Mod,
                    integer_("6", pos(9, 1)),
                    integer_("2", pos(13, 1)),
                    pos(9, 5)
                ),
                pos(0, 14)
            )))
        );
    }

    #[test]
    fn let_function_statement() {
        let tokens = vec![
            Token::new(TokenType::Let, pos(0, 3)),
            Token::new(TokenType::Ident(String::from('f')), pos(4, 1)),
            Token::new(TokenType::Lparen, pos(5, 1)),
            Token::new(TokenType::Ident(String::from('x')), pos(6, 1)),
            Token::new(TokenType::Comma, pos(7, 1)),
            Token::new(TokenType::Ident(String::from('y')), pos(9, 1)),
            Token::new(TokenType::Rparen, pos(10, 1)),
            Token::new(TokenType::Assign, pos(12, 2)),
            Token::new(TokenType::Ident(String::from('x')), pos(15, 1)),
            Token::new(TokenType::Plus, pos(17, 1)),
            Token::new(TokenType::Ident(String::from('y')), pos(19, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(let__(
                symbol_("f", pos(4, 1)),
                vec![symbol_("x", pos(6, 1)), symbol_("y", pos(9, 1))],
                infix_(
                    InfixOperator::Sum,
                    symbol_("x", pos(15, 1)),
                    symbol_("y", pos(19, 1)),
                    pos(15, 5)
                ),
                pos(0, 20),
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
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(extension_set_(vec![], pos(0, 2))))
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
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(extension_set_(
                vec![boolean_(true, pos(2, 4)), boolean_(false, pos(9, 4)),],
                pos(0, 14)
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
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(tuple_(vec![], pos(0, 2))))
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
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(tuple_(
                vec![symbol_("Real", pos(1, 4)), symbol_("Real", pos(7, 4))],
                pos(0, 12)
            )))
        );
    }

    #[test]
    fn set_comprehension() {
        let tokens = vec![
            Token::new(TokenType::Lbrace, pos(0, 1)),
            Token::new(TokenType::Ident(String::from("a")), pos(1, 1)),
            Token::new(TokenType::Colon, pos(3, 1)),
            Token::new(TokenType::Ident(String::from("a")), pos(5, 1)),
            Token::new(TokenType::Equals, pos(7, 1)),
            Token::new(TokenType::Integer(String::from("1")), pos(9, 1)),
            Token::new(TokenType::Rbrace, pos(10, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(comprehension_set_(
                symbol_("a", pos(1, 1)),
                infix_(
                    InfixOperator::Equality,
                    symbol_("a", pos(5, 1)),
                    integer_("1", pos(9, 1)),
                    pos(5, 5)
                ),
                pos(0, 11),
            )))
        );
    }

    #[test]
    fn typed_let() {
        let tokens = vec![
            Token::new(TokenType::Let, pos(0, 3)),
            Token::new(TokenType::Ident(String::from("x")), pos(4, 1)),
            Token::new(TokenType::Colon, pos(6, 1)),
            Token::new(TokenType::Ident(String::from("Real")), pos(8, 4)),
            Token::new(TokenType::Assign, pos(13, 2)),
            Token::new(TokenType::Integer(String::from("1")), pos(16, 1)),
            Token::new(TokenType::Plus, pos(18, 1)),
            Token::new(TokenType::Integer(String::from("0")), pos(20, 1)),
            Token::new(TokenType::Mod, pos(22, 1)),
            Token::new(TokenType::Integer(String::from("2")), pos(24, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(let__(
                signature_(
                    symbol_("x", pos(4, 1)),
                    Some(symbol_("Real", pos(8, 4))),
                    pos(4, 8)
                ),
                vec![],
                infix_(
                    InfixOperator::Sum,
                    integer_("1", pos(16, 1)),
                    infix_(
                        InfixOperator::Mod,
                        integer_("0", pos(20, 1)),
                        integer_("2", pos(24, 1)),
                        pos(20, 5)
                    ),
                    pos(16, 9),
                ),
                pos(0, 25),
            )))
        );
    }

    #[test]
    fn shift_operator() {
        let tokens = vec![
            Token::new(TokenType::Ident(String::from('x')), pos(0, 1)),
            Token::new(TokenType::Minus, pos(2, 1)),
            Token::new(TokenType::Integer(String::from('1')), pos(4, 1)),
            Token::new(TokenType::LeftShift, pos(6, 2)),
            Token::new(TokenType::Integer(String::from('1')), pos(9, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(infix_(
                InfixOperator::LeftShift,
                infix_(
                    InfixOperator::Substraction,
                    symbol_("x", pos(0, 1)),
                    integer_("1", pos(4, 1)),
                    pos(0, 5)
                ),
                integer_("1", pos(9, 1)),
                pos(0, 10),
            )))
        );
    }

    #[test]
    fn shift_and_comparison() {
        let lexer = build_lexer("1 << 1 > 1").map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next_(),
            Some(Ok(infix_(
                InfixOperator::Greater,
                infix_(
                    InfixOperator::LeftShift,
                    integer_("1", pos(0, 1)),
                    integer_("1", pos(5, 1)),
                    pos(0, 6)
                ),
                integer_("1", pos(9, 1)),
                pos(0, 10)
            )))
        );
    }

    #[test]
    fn bitwise() {
        let lexer = build_lexer("a & b || c").map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next_(),
            Some(Ok(infix_(
                InfixOperator::Or,
                infix_(
                    InfixOperator::BitwiseAnd,
                    symbol_("a", pos(0, 1)),
                    symbol_("b", pos(4, 1)),
                    pos(0, 5)
                ),
                symbol_("c", pos(9, 1)),
                pos(0, 10),
            )))
        );
    }

    #[test]
    fn logic_infix_operators() {
        let lexer = build_lexer("a && b || c");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(infix_(
                InfixOperator::Or,
                infix_(
                    InfixOperator::LogicAnd,
                    symbol_("a", pos(0, 1)),
                    symbol_("b", pos(5, 1)),
                    pos(0, 6)
                ),
                symbol_("c", pos(10, 1)),
                pos(0, 11),
            )))
        );
    }

    #[test]
    fn complex_precedence() {
        let lexer = build_lexer("  a + b || a & b << c");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(infix_(
                InfixOperator::Or,
                infix_(
                    InfixOperator::Sum,
                    symbol_("a", pos(2, 1)),
                    symbol_("b", pos(6, 1)),
                    pos(2, 5)
                ),
                infix_(
                    InfixOperator::BitwiseAnd,
                    symbol_("a", pos(11, 1)),
                    infix_(
                        InfixOperator::LeftShift,
                        symbol_("b", pos(15, 1)),
                        symbol_("c", pos(20, 1)),
                        pos(15, 6)
                    ),
                    pos(11, 10),
                ),
                pos(2, 19)
            )))
        );
    }

    #[test]
    fn bitwise_xor() {
        let lexer = build_lexer("a ^ b & c || d");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(infix_(
                InfixOperator::Or,
                infix_(
                    InfixOperator::BitwiseXor,
                    symbol_("a", pos(0, 1)),
                    infix_(
                        InfixOperator::BitwiseAnd,
                        symbol_("b", pos(4, 1)),
                        symbol_("c", pos(8, 1)),
                        pos(4, 5)
                    ),
                    pos(0, 9),
                ),
                symbol_("d", pos(13, 1)),
                pos(0, 14)
            )))
        );
    }

    #[test]
    fn something_after_empty_set() {
        let lexer = build_lexer("({}, 0)");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(tuple_(
                vec![extension_set_(vec![], pos(1, 2)), integer_("0", pos(5, 1))],
                pos(0, 7)
            )))
        );
    }

    #[test]
    fn prefixes() {
        let lexer = build_lexer("!(~1 /= -1)");

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(prefix_(
                PrefixOperator::LogicNot,
                infix_(
                    InfixOperator::NotEquality,
                    prefix_(
                        PrefixOperator::BitwiseNot,
                        integer_("1", pos(3, 1)),
                        pos(2, 2)
                    ),
                    prefix_(PrefixOperator::Minus, integer_("1", pos(9, 1)), pos(8, 2)),
                    pos(2, 8),
                ),
                pos(0, 11),
            )))
        );
    }

    #[test]
    fn if_expr() {
        let tokens = vec![
            Token::new(TokenType::If, pos(0, 2)),
            Token::new(TokenType::Ident(String::from("a")), pos(3, 1)),
            Token::new(TokenType::Less, pos(5, 1)),
            Token::new(TokenType::Integer(String::from("0")), pos(7, 1)),
            Token::new(TokenType::Then, pos(9, 4)),
            Token::new(TokenType::Minus, pos(14, 1)),
            Token::new(TokenType::Ident(String::from("a")), pos(15, 1)),
            Token::new(TokenType::Else, pos(17, 4)),
            Token::new(TokenType::Ident(String::from("a")), pos(22, 1)),
        ];

        assert_eq!(
            parser_from(tokens.into_iter()).next_(),
            Some(Ok(if__(
                infix_(
                    InfixOperator::Less,
                    symbol_("a", pos(3, 1)),
                    integer_("0", pos(7, 1)),
                    pos(3, 5)
                ),
                prefix_(PrefixOperator::Minus, symbol_("a", pos(15, 1)), pos(14, 2)),
                symbol_("a", pos(22, 1)),
                pos(0, 23),
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
                let__(
                    symbol_("a", pos(4, 1)),
                    vec![],
                    integer_("5", pos(9, 1)),
                    pos(0, 10)
                ),
                infix_(
                    InfixOperator::Product,
                    symbol_("a", pos(19, 1)),
                    symbol_("a", pos(23, 1)),
                    pos(19, 5)
                )
            ],
        );
    }

    #[test]
    fn function_call() {
        let input = "f(x, y)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(infix_(
                InfixOperator::Call,
                symbol_("f", pos(0, 1)),
                tuple_(
                    vec![symbol_("x", pos(2, 1)), symbol_("y", pos(5, 1)),],
                    pos(1, 6)
                ),
                pos(0, 7),
            ))),
        );
    }

    #[test]
    fn comma_last_item() {
        let input = "(1,)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(tuple_(vec![integer_("1", pos(1, 1))], pos(0, 4)))),
        );
    }

    #[test]
    fn anon_function() {
        let input = "x -> 2*x";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(infix_(
                InfixOperator::Correspondence,
                symbol_("x", pos(0, 1)),
                infix_(
                    InfixOperator::Product,
                    integer_("2", pos(5, 1)),
                    symbol_("x", pos(7, 1)),
                    pos(5, 3)
                ),
                pos(0, 8),
            )))
        );
    }

    #[test]
    fn anon_function_call() {
        let input = "((x, y) -> x)(1, 2)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(infix_(
                InfixOperator::Call,
                infix_(
                    InfixOperator::Correspondence,
                    tuple_(
                        vec![symbol_("x", pos(2, 1)), symbol_("y", pos(5, 1)),],
                        pos(1, 6)
                    ),
                    symbol_("x", pos(11, 1)),
                    pos(1, 11),
                ),
                tuple_(
                    vec![integer_("1", pos(14, 1)), integer_("2", pos(17, 1)),],
                    pos(13, 6)
                ),
                pos(0, 19)
            )))
        );
    }

    #[test]
    fn char_and_string() {
        let input = "('a', \"b\")";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(tuple_(
                vec![char_('a', pos(1, 3)), string_("b", pos(6, 3)),],
                pos(0, 10),
            )))
        );
    }

    #[test]
    fn for_loop() {
        let input = "for i in list: println(i)";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(for_(
                "i",
                symbol_("list", pos(9, 4)),
                vec![infix_(
                    InfixOperator::Call,
                    symbol_("println", pos(15, 7)),
                    tuple_(vec![symbol_("i", pos(23, 1))], pos(22, 3)),
                    pos(15, 10)
                )],
                pos(0, 25)
            )))
        );
    }

    #[test]
    fn in_question() {
        let input = "1 in { k : k >= 1 }";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(infix_(
                InfixOperator::In,
                integer_("1", pos(0, 1)),
                comprehension_set_(
                    symbol_("k", pos(7, 1)),
                    infix_(
                        InfixOperator::GreaterEqual,
                        symbol_("k", pos(11, 1)),
                        integer_("1", pos(16, 1)),
                        pos(11, 6)
                    ),
                    pos(5, 14),
                ),
                pos(0, 19),
            )))
        );
    }

    #[test]
    fn list() {
        let input = "[[], 2]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(extension_list_(
                vec![extension_list_(vec![], pos(1, 2)), integer_("2", pos(5, 1)),],
                pos(0, 7),
            ))),
        );
    }

    #[test]
    fn comprehension_list_only() {
        let input = "[ k in [1, 2] : k - 1 = 0 ]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(comprehension_list_(
                infix_(
                    InfixOperator::In,
                    symbol_("k", pos(2, 1)),
                    extension_list_(
                        vec![integer_("1", pos(8, 1)), integer_("2", pos(11, 1)),],
                        pos(7, 6)
                    ),
                    pos(2, 11)
                ),
                infix_(
                    InfixOperator::Equality,
                    infix_(
                        InfixOperator::Substraction,
                        symbol_("k", pos(16, 1)),
                        integer_("1", pos(20, 1)),
                        pos(16, 5)
                    ),
                    integer_("0", pos(24, 1)),
                    pos(16, 9)
                ),
                pos(0, 27)
            )))
        );
    }

    #[test]
    fn singleton_empty_set() {
        let input = "{{}}";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(extension_set_(
                vec![extension_set_(vec![], pos(1, 2))],
                pos(0, 4)
            ))),
        );
    }

    #[test]
    fn wildcard() {
        let input = "[a, 1, _]";

        let lexer = build_lexer(input);

        assert_eq!(
            parser_from(lexer.map(|res| res.unwrap())).next_(),
            Some(Ok(extension_list_(
                vec![
                    symbol_("a", pos(1, 1)),
                    integer_("1", pos(4, 1)),
                    ASTNode::new(ASTNodeType_::Wildcard, pos(7, 1)),
                ],
                pos(0, 9)
            ),)),
        );
    }

    #[test]
    fn prepend_only() {
        let code = "[1|[2,3]]";
        let lexer = build_lexer(code).map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next_(),
            Some(Ok(prepend_(
                integer_("1", pos(1, 1)),
                extension_list_(
                    vec![integer_("2", pos(4, 1)), integer_("3", pos(6, 1)),],
                    pos(3, 5)
                ),
                pos(0, 9)
            ))),
        );
    }

    #[test]
    fn consume_comprehension_list() {
        let input = "[a : a in b] + []";
        let lexer = build_lexer(input).map(|res| res.unwrap());

        assert_eq!(
            parser_from(lexer).next_(),
            Some(Ok(infix_(
                InfixOperator::Sum,
                comprehension_list_(
                    symbol_("a", pos(1, 1)),
                    infix_(
                        InfixOperator::In,
                        symbol_("a", pos(5, 1)),
                        symbol_("b", pos(10, 1)),
                        pos(5, 6)
                    ),
                    pos(0, 12)
                ),
                extension_list_(vec![], pos(15, 2)),
                pos(0, 17)
            )))
        );
    }
}
