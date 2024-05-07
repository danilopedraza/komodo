use crate::{exec::EvalError, lexer::LexerError, parser::ParserError};

// struct Error(ErrorType, Position);

enum Error {
    LexerError(LexerError),
    ParserError(ParserError),
    ExecError(EvalError),
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

fn error_msg(err: Error) -> ErrorMessage {
    match err {
        Error::ParserError(err) => parser_error_msg(err),
        _ => todo!(),
    }
}

fn parser_error_msg(err: ParserError) -> ErrorMessage {
    match err {
        ParserError::ExpectedExpression(_, pos) => {
            ErrorMessage("Expected an expression, but got a ')'".into(), pos)
        }
        _ => todo!(),
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ErrorMessage(String, Position);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenType;

    #[test]
    fn expected_expression() {
        assert_eq!(
            error_msg(Error::ParserError(ParserError::ExpectedExpression(
                TokenType::Rparen,
                Position::new(3, 1)
            ))),
            ErrorMessage(
                String::from("Expected an expression, but got a ')'"),
                Position::new(3, 1),
            ),
        );
    }
}
