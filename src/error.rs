use crate::{exec::EvalError, lexer::LexerError, parser::ParserError};

enum Error {
    LexerError(LexerError),
    ParserError(ParserError),
    ExecError(EvalError),
}

struct Position {
    row: u32,
    col: u32,
}

struct Range(Position, Position);

struct ErrorMessage(Error, Range);
