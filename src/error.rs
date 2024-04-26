// use crate::{exec::EvalError, lexer::LexerError, parser::ParserError};

// struct Error(ErrorType, Position);

// enum ErrorType {
//     LexerError(LexerError),
//     ParserError(ParserError),
//     ExecError(EvalError),
// }

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

// struct ErrorMessage(Error, Position);

// #[cfg(test)]
// mod tests {
// }
