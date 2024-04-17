use crate::{exec::EvalError, lexer::LexerError, parser::ParserError};

enum Error {
    LexerError(LexerError),
    ParserError(ParserError),
    ExecError(EvalError),
}

struct Position {
    start: u32,
    length: u32,
}

struct ErrorMessage(Error, Position);
