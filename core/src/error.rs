use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{Buffer, StandardStream},
    },
};

use crate::{
    exec::EvalError,
    lexer::{LexerError, TokenType},
    parser::ParserError,
    weeder::WeederError,
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
    Weeder(WeederError),
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

impl From<WeederError> for ErrorType {
    fn from(err: WeederError) -> Self {
        Self::Weeder(err)
    }
}

impl From<EvalError> for ErrorType {
    fn from(err: EvalError) -> Self {
        Self::Exec(err)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Position {
    pub start: usize,
    pub length: usize,
}

impl Position {
    pub fn new(start: usize, length: usize) -> Self {
        Self { start, length }
    }
}

pub fn error_msg(Error(err, pos): &Error) -> ErrorMessage {
    let msg = match err {
        ErrorType::Lexer(err) => lexer_error_msg(err),
        ErrorType::Parser(err) => parser_error_msg(err),
        ErrorType::Weeder(err) => weeder_error_msg(err),
        ErrorType::Exec(err) => exec_error_msg(err),
    };

    ErrorMessage(msg, *pos)
}

fn found_a(tok: &TokenType) -> String {
    match tok {
        TokenType::Rparen => "a right parenthesis: `)`".into(),
        TokenType::Arrow => "an arrow: `->`".into(),
        TokenType::Assign => "an assignment symbol: `:=`".into(),
        TokenType::Bang => "a bang: `!`".into(),
        TokenType::BitwiseAnd => "an ampersand: `&`".into(),
        TokenType::VerticalBar => "a pipe symbol: `|`".into(),
        TokenType::BitwiseXor => "a caret: `^`".into(),
        TokenType::Char(chr) => format!("a character: `{chr}`"),
        TokenType::Colon => "a colon: `:`".into(),
        TokenType::Comma => "a comma: `,`".into(),
        TokenType::Do => "the `do` keyword".into(),
        TokenType::Dot => "a dot: `.`".into(),
        TokenType::DotDot => "a range operator: `..`".into(),
        TokenType::Else => "the `else` keyword".into(),
        TokenType::Equals => "an equality operator: `=`".into(),
        TokenType::False => "a boolean: `false`".into(),
        TokenType::For => "a keyword: `for`".into(),
        TokenType::Greater => "an operator: `>`".into(),
        TokenType::GreaterEqual => "an operator: `>=`".into(),
        TokenType::Ident(ident) => format!("a symbol: `{ident}`"),
        TokenType::If => "the `if` keyword".into(),
        TokenType::In => "the `in` keyword".into(),
        TokenType::Integer(val, _) => format!("an integer: `{val}`"),
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
        TokenType::Percent => "a remainder operator: `%`".into(),
        TokenType::NotEqual => "a non-equality operator: `/=`".into(),
        TokenType::Over => "a division operator: `/`".into(),
        TokenType::Plus => "a plus operator: `+`".into(),
        TokenType::Rbrace => "a right brace: `}`".into(),
        TokenType::Rbrack => "a right bracket: `]`".into(),
        TokenType::RightShift => "a right shift operator: `>>`".into(),
        TokenType::SlashSlash => "a fraction operator: `//`".into(),
        TokenType::String(val) => format!("a string: `{val}`"),
        TokenType::Then => "the `then` keyword".into(),
        TokenType::Tilde => "a negation: `~`".into(),
        TokenType::Times => "a multiplication operator: `*`".into(),
        TokenType::ToThe => "an exponentiation operator: `**`".into(),
        TokenType::True => "a boolean: `true`".into(),
        TokenType::Unknown => "an unknown symbol".into(),
        TokenType::Wildcard => "a wildcard: `_`".into(),
        TokenType::As => "the `as` keyword".into(),
        TokenType::From => "the `from` keyword".into(),
        TokenType::Import => "the `import` keyword".into(),
        TokenType::Dedent => "the end of an indentation block".into(),
        TokenType::Indent => "the beggining of an indentation block".into(),
    }
}

fn lexer_error_msg(err: &LexerError) -> String {
    match err {
        LexerError::EmptyChar => "This character is empty".into(),
        LexerError::UnexpectedChar(chr) => unexpected_char(*chr),
        LexerError::UnterminatedChar => {
            "The end of the program was reached while reading a character".into()
        }
        LexerError::UnterminatedString => {
            "The end of the program was reached while reading a string".into()
        }
        LexerError::LeadingZeros => "Decimal numbers cannot have leading zeros".into(),
        LexerError::EmptyPrefixedInteger => "There is an integer prefix, but nothing more".into(),
    }
}

fn parser_error_msg(err: &ParserError) -> String {
    match err {
        ParserError::ExpectedExpression(tok) => expected_expression(found_a(tok)),
        ParserError::UnexpectedToken(expected_vec, actual) => {
            unexpected_token(expected_vec.iter().map(found_a).collect(), found_a(actual))
        }
        ParserError::EOFReached => eof_reached(),
        ParserError::EOFExpecting(expected) => {
            eof_expecting(expected.iter().map(found_a).collect())
        }
    }
}

fn weeder_error_msg(_err: &WeederError) -> String {
    todo!()
}

fn exec_error_msg(err: &EvalError) -> String {
    match err {
        EvalError::BadFraction {
            numer_kind,
            denom_kind,
        } => bad_fraction(numer_kind, denom_kind),
        EvalError::DenominatorZero => "Division by zero".into(),
        EvalError::FailedAssertion(msg) => failed_assertion(msg),
        EvalError::MissingFunctionArguments { expected, actual } => {
            missing_func_arguments(*expected, *actual)
        }
        EvalError::NonCallableObject(kind) => non_callable_object(kind),
        EvalError::NonIterableObject(kind) => non_iterable_object(kind),
        EvalError::NonPrependableObject(kind) => non_prependable_object(kind),
        EvalError::NonExistentPrefixOperation { op, rhs } => non_existent_prefix(op, rhs),
        EvalError::NonExistentInfixOperation { op, lhs, rhs } => non_existent_infix(op, lhs, rhs),
        EvalError::IndexingNonContainer { kind } => indexing_non_container(kind),
        EvalError::ListIndexOutOfBounds => "List index out of bounds".into(),
        EvalError::InvalidIndex { kind } => invalid_index(kind),
        EvalError::NonExistentKey { key } => non_existent_key(key),
        EvalError::UnknownValue(value) => unknown_value(value),
    }
}

fn unknown_value(value: &str) -> String {
    format!("Unknown value: `{value}`")
}

fn indexing_non_container(kind: &str) -> String {
    format!("Cannot get elements from `{kind}`")
}

fn invalid_index(kind: &str) -> String {
    format!("Cannot use `{kind}` as an index")
}

fn non_existent_key(key: &str) -> String {
    format!("The key `{key}` is not in the dictionary")
}

fn failed_assertion(msg: &Option<String>) -> String {
    match msg {
        None => "Failed assertion".into(),
        Some(msg) => format!("Failed assertion: {msg}"),
    }
}

fn bad_fraction(numer_kind: &str, denom_kind: &str) -> String {
    format!("Cannot create a fraction from `{numer_kind}` and `{denom_kind}`")
}

fn non_prependable_object(kind: &str) -> String {
    format!("Cannot prepend elements to `{kind}`")
}

fn non_existent_infix(op: &str, lhs: &str, rhs: &str) -> String {
    format!("The {op} operation does not exist for `{lhs}` and `{rhs}`")
}

fn non_existent_prefix(op: &str, rhs: &str) -> String {
    format!("The {op} operation does not exist for `{rhs}`")
}

fn non_iterable_object(kind: &str) -> String {
    format!("`{kind}` cannot be iterated through")
}

fn missing_func_arguments(expected: usize, actual: usize) -> String {
    format!("Expected {expected} arguments for this function call, but found {actual}")
}

fn non_callable_object(kind: &str) -> String {
    format!("`{kind}` cannot be called like a function")
}

fn unexpected_char(chr: char) -> String {
    format!("Expected a `'` to close the character, but found `{chr}`")
}

fn eof_expecting(expected_msgs: Vec<String>) -> String {
    let expected_str = disjunction(expected_msgs);
    format!("The end of the program was reached while expecting {expected_str}")
}

fn eof_reached() -> String {
    "The end of the program was reached while reading an expression".into()
}

fn expected_expression(tok_msg: String) -> String {
    format!("Expected an expression, but found {tok_msg}")
}

fn disjunction(msgs: Vec<String>) -> String {
    let mut res = msgs[0].to_owned();

    for msg in msgs.iter().take(msgs.len() - 1).skip(1) {
        res.push_str(", ");
        res.push_str(msg);
    }

    if msgs.len() > 1 {
        res.push_str(" or ");
        res.push_str(msgs.last().unwrap());
    }

    res
}

fn unexpected_token(expected_msgs: Vec<String>, actual_msg: String) -> String {
    let expected_str = disjunction(expected_msgs);
    format!("Expected {expected_str}, but found {actual_msg}")
}

#[derive(Debug, PartialEq, Eq)]
pub struct ErrorMessage(pub String, pub Position);

impl ErrorMessage {
    pub fn as_bytes(&self, filename: &str, source: &str) -> Vec<u8> {
        let mut files = SimpleFiles::new();
        let mut writer = Buffer::no_color();
        let config = codespan_reporting::term::Config::default();

        let file_id = files.add(filename, source);
        let diagnostic = Diagnostic::error()
            .with_message(self.0.to_owned())
            .with_labels(vec![Label::primary(
                file_id,
                self.1.start..(self.1.start + self.1.length),
            )]);

        let _ = term::emit(&mut writer, &config, &files, &diagnostic);

        writer.into_inner()
    }

    pub fn emit(&self, filename: &str, source: &str) {
        let mut files = SimpleFiles::new();
        let writer = StandardStream::stderr(term::termcolor::ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let file_id = files.add(filename, source);
        let diagnostic = Diagnostic::error()
            .with_message(self.0.to_owned())
            .with_labels(vec![Label::primary(
                file_id,
                self.1.start..(self.1.start + self.1.length),
            )]);

        let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
    }
}

#[cfg(test)]
mod tests {
    use crate::cst::tests::dummy_pos;

    use super::*;

    #[test]
    fn expected_expression_() {
        assert_eq!(
            expected_expression("a right parenthesis: `)`".into()),
            String::from("Expected an expression, but found a right parenthesis: `)`"),
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

    #[test]
    fn eof_reached() {
        assert_eq!(
            error_msg(&Error(ParserError::EOFReached.into(), dummy_pos())),
            ErrorMessage(
                "The end of the program was reached while reading an expression".into(),
                dummy_pos()
            ),
        );
    }

    #[test]
    fn eof_expecting_() {
        assert_eq!(
            eof_expecting(vec![
                String::from("a dot: `.`"),
                String::from("a comma: `,`")
            ]),
            String::from(
                "The end of the program was reached while expecting a dot: `.` or a comma: `,`"
            ),
        );
    }

    #[test]
    fn non_callable_object_() {
        assert_eq!(
            non_callable_object("extension set"),
            String::from("`extension set` cannot be called like a function")
        );
    }

    #[test]
    fn missing_func_arguments_() {
        assert_eq!(
            missing_func_arguments(3, 1),
            String::from("Expected 3 arguments for this function call, but found 1"),
        );
    }

    #[test]
    fn non_iterable_object_() {
        assert_eq!(
            non_iterable_object("foo"),
            String::from("`foo` cannot be iterated through")
        );
    }

    #[test]
    fn non_existent_prefix_() {
        assert_eq!(
            non_existent_prefix("bitwise negation", "set"),
            String::from("The bitwise negation operation does not exist for `set`")
        );
    }

    #[test]
    fn non_existent_infix_() {
        assert_eq!(
            non_existent_infix("bitwise AND", "integer", "set"),
            String::from("The bitwise AND operation does not exist for `integer` and `set`")
        );
    }

    #[test]
    fn non_prependable_obj() {
        assert_eq!(
            non_prependable_object("integer"),
            String::from("Cannot prepend elements to `integer`")
        );
    }

    #[test]
    fn _bad_fraction() {
        assert_eq!(
            bad_fraction("symbol", "list"),
            String::from("Cannot create a fraction from `symbol` and `list`"),
        );
    }

    #[test]
    fn _failed_assertion_no_message() {
        assert_eq!(failed_assertion(&None), String::from("Failed assertion"),);
    }

    #[test]
    fn _failed_assertion() {
        assert_eq!(
            failed_assertion(&Some(String::from("this is not what I want"))),
            String::from("Failed assertion: this is not what I want"),
        );
    }
}
