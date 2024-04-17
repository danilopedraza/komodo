use crate::{exec::EvalError, lexer::LexerError, parser::ParserError};

enum Error {
    LexerError(LexerError),
    ParserError(ParserError),
    ExecError(EvalError),
}

#[derive(Debug)]
pub struct Position {
    start: u32,
    length: u32,
}

impl Position {
    pub fn new(start: u32, length: u32) -> Self {
        Self { start, length, }
    }
}

struct ErrorMessage(Error, Position);
