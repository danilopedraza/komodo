use crate::{ast::_dummy_pos, exec::EvalError, lexer::LexerError, parser::ParserError};

// struct Error(ErrorType, Position);

pub enum Error {
    _Lexer(LexerError),
    Parser(ParserError),
    _Exec(EvalError),
}

impl From<ParserError> for Error {
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

pub fn error_msg(err: Error) -> ErrorMessage {
    match err {
        Error::Parser(err) => parser_error_msg(err),
        _ => ErrorMessage("Unknown error".into(), _dummy_pos()),
    }
}

fn parser_error_msg(err: ParserError) -> ErrorMessage {
    match err {
        ParserError::ExpectedExpression(_, pos) => {
            ErrorMessage("Expected an expression, but found a `)`".into(), pos)
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
            error_msg(Error::Parser(ParserError::ExpectedExpression(
                TokenType::Rparen,
                Position::new(3, 1)
            ))),
            ErrorMessage(
                String::from("Expected an expression, but found a `)`"),
                Position::new(3, 1),
            ),
        );
    }
}
