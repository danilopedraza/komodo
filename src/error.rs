use crate::{
    exec::EvalError,
    lexer::{LexerError, TokenType},
    parser::ParserError,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error(pub ErrorType, pub Position);

impl Error {
    pub fn new(err: ErrorType, pos: Position) -> Self {
        Self(err, pos)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorType {
    Lexer(LexerError),
    Parser(ParserError),
    Exec(EvalError),
}

impl From<LexerError> for ErrorType {
    fn from(err: LexerError) -> Self {
        Self::Lexer(err)
    }
}

impl From<ParserError> for ErrorType {
    fn from(err: ParserError) -> Self {
        Self::Parser(err)
    }
}

impl From<EvalError> for ErrorType {
    fn from(err: EvalError) -> Self {
        Self::Exec(err)
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

pub fn error_msg(Error(err, pos): &Error) -> ErrorMessage {
    let msg = match err {
        ErrorType::Parser(err) => parser_error_msg(err),
        ErrorType::Lexer(err) => lexer_error_msg(err),
        ErrorType::Exec(err) => exec_error_msg(err),
    };

    ErrorMessage(msg, *pos)
}

fn found_a(tok: &TokenType) -> String {
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

fn parser_error_msg(err: &ParserError) -> String {
    match err {
        ParserError::ExpectedExpression(tok) => expected_expression(found_a(tok)),
        ParserError::UnexpectedToken(expected_vec, actual) => {
            unexpected_token(expected_vec.iter().map(found_a).collect(), found_a(actual))
        }
        ParserError::EOFReached => todo!(),
        ParserError::EOFExpecting(_) => todo!(),
    }
}

fn expected_expression(tok_msg: String) -> String {
    format!("Expected an expression, but found {tok_msg}")
}

fn unexpected_token(expected_msgs: Vec<String>, actual_msg: String) -> String {
    let mut expected_str = expected_msgs[0].to_owned();

    for expected_msg in expected_msgs.iter().take(expected_msgs.len() - 1).skip(1) {
        expected_str.push_str(", ");
        expected_str.push_str(expected_msg);
    }

    if expected_msgs.len() > 1 {
        expected_str.push_str(" or ");
        expected_str.push_str(expected_msgs.last().unwrap());
    }

    format!("Expected {expected_str}, but found {actual_msg}")
}

fn lexer_error_msg(err: &LexerError) -> String {
    match err {
        LexerError::UnexpectedChar(_) => todo!(),
        LexerError::UnexpectedEOF => todo!(),
        LexerError::UnterminatedChar => todo!(),
        LexerError::UnterminatedString => todo!(),
    }
}

fn exec_error_msg(err: &EvalError) -> String {
    match err {
        EvalError::MissingFunctionArguments => todo!(),
        EvalError::NonCallableObject => todo!(),
        EvalError::NonExistentOperation => todo!(),
        EvalError::NonIterableObject => todo!(),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ErrorMessage(pub String, pub Position);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expected_expression_() {
        assert_eq!(
            expected_expression("a left parenthesis: `)`".into()),
            String::from("Expected an expression, but found a left parenthesis: `)`"),
        );
    }

    #[test]
    fn unexpected_token_single_expected() {
        assert_eq!(
            unexpected_token(
                vec!["a colon: `:`".into()],
                "an assignment symbol: `:=`".into()
            ),
            String::from("Expected a colon: `:`, but found an assignment symbol: `:=`"),
        );
    }

    #[test]
    fn unexpected_token_two_expected() {
        assert_eq!(
            unexpected_token(
                vec!["a colon: `:`".into(), "a comma: `,`".into()],
                "an assignment symbol: `:=`".into()
            ),
            String::from(
                "Expected a colon: `:` or a comma: `,`, but found an assignment symbol: `:=`"
            ),
        );
    }

    #[test]
    fn unexpected_token_three_expected() {
        assert_eq!(
            unexpected_token(
                vec![
                    "a dot: `.`".into(),
                    "a bang: `!`".into(),
                    "a comma: `,`".into()
                ],
                "a symbol: `foo`".into()
            ),
            String::from(
                "Expected a dot: `.`, a bang: `!` or a comma: `,`, but found a symbol: `foo`"
            ),
        );
    }
}
