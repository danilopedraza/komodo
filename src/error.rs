use crate::{
    ast::_dummy_pos,
    exec::EvalError,
    lexer::{LexerError, TokenType},
    parser::ParserError,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Error(pub ErrorType, pub Position);

impl Error {
    pub fn new(err: ErrorType, pos: Position) -> Self {
        Self(err, pos)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    _Lexer(LexerError),
    Parser(ParserError),
    _Exec(EvalError),
}

impl From<ParserError> for ErrorType {
    fn from(err: ParserError) -> Self {
        Self::Parser(err)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Position {
    pub start: u32,
    pub length: u32,
}

impl Position {
    pub fn new(start: u32, length: u32) -> Self {
        Self { start, length }
    }
}

pub fn error_msg(Error(err, _pos): Error) -> ErrorMessage {
    match err {
        ErrorType::Parser(err) => parser_error_msg(err),
        _ => ErrorMessage("Unknown error".into(), _dummy_pos()),
    }
}

fn found_a(tok: TokenType) -> String {
    match tok {
        TokenType::Rparen => "a left parenthesis: `)`".into(),
        TokenType::Arrow => "an arrow: `->`".into(),
        TokenType::Assign => "an assignment symbol: `:=`".into(),
        TokenType::Bang => "a bang: `!`".into(),
        TokenType::BitwiseAnd => "an ampersand: `&`".into(),
        TokenType::VerticalBar => "a pipe symbol: `|`".into(),
        TokenType::BitwiseXor => "a caret: `^`".into(),
        TokenType::Char(chr) => format!("a character: `{chr}`"),
        TokenType::Colon => "a colon: `:`".into(),
        TokenType::Comma => "a comma: `,`".into(),
        TokenType::Dot => "a dot: `.`".into(),
        TokenType::Else => "the `else` keyword".into(),
        TokenType::Equals => "an equality operator: `=`".into(),
        TokenType::False => "a boolean: `false`".into(),
        TokenType::For => "a keyword: `for`".into(),
        TokenType::Greater => "an operator: `>`".into(),
        TokenType::GreaterEqual => "an operator: `>=`".into(),
        TokenType::Ident(ident) => format!("a symbol: `{ident}`"),
        TokenType::If => "the `if` keyword".into(),
        TokenType::In => "the `in` keyword".into(),
        TokenType::Integer(val) => format!("an integer: `{val}`"),
        TokenType::Lbrace => "a left brace: `{`".into(),
        TokenType::Lbrack => "a left bracket: `[`".into(),
        TokenType::LeftShift => "a left shift operator: `<<`".into(),
        TokenType::Less => "a less-than comparator: `<`".into(),
        TokenType::LessEqual => "a less-than-or-equal comparator: `<=`".into(),
        TokenType::Let => "the `let` keyword".into(),
        TokenType::LogicAnd => "a double ampersand: `&&`".into(),
        TokenType::LogicOr => "a double pipe symbol: `||`".into(),
        TokenType::Lparen => "a left parenthesis: `(`".into(),
        TokenType::Minus => "a minus operator: `-`".into(),
        TokenType::Mod => "a modulo operator: `%`".into(),
        TokenType::NotEqual => "a non-equality operator: `/=`".into(),
        TokenType::Over => "a division operator: `/`".into(),
        TokenType::Plus => "a plus operator: `+`".into(),
        TokenType::Rbrace => "a right brace: `}`".into(),
        TokenType::Rbrack => "a right bracket: `{`".into(),
        TokenType::RightShift => "a right shift operator: `>>`".into(),
        TokenType::String(val) => format!("a string: `{val}`"),
        TokenType::Then => "the `then` keyword".into(),
        TokenType::Tilde => "a negation: `~`".into(),
        TokenType::Times => "a multiplication operator: `*`".into(),
        TokenType::ToThe => "an exponentiation operator: `**`".into(),
        TokenType::True => "a boolean: `true`".into(),
        TokenType::Unknown => "an unknown symbol".into(),
        TokenType::Wildcard => "a wildcard: `_`".into(),
    }
}

fn parser_error_msg(err: ParserError) -> ErrorMessage {
    match err {
        ParserError::ExpectedExpression(tok, pos) => {
            let tok_str = found_a(tok);
            ErrorMessage(format!("Expected an expression, but found {tok_str}"), pos)
        }
        _ => ErrorMessage("Unknown parser error".into(), _dummy_pos()),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ErrorMessage(pub String, pub Position);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenType;

    #[test]
    fn expected_expression() {
        assert_eq!(
            error_msg(Error::new(
                ParserError::ExpectedExpression(TokenType::Rparen, Position::new(3, 1)).into(),
                Position::new(3, 1)
            )),
            ErrorMessage(
                String::from("Expected an expression, but found a left parenthesis: `)`"),
                Position::new(3, 1),
            ),
        );
    }
}
