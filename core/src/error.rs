use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{Buffer, StandardStream},
    },
};

use unindent::unindent;

use crate::{
    exec::EvalError,
    lexer::{LexerError, TokenType},
    parser::ParserError,
    run::ImportError,
    weeder::WeederError,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    WithPosition(ErrorKind, Position),
    IO(String),
}

impl Error {
    pub fn with_position(err: ErrorKind, pos: Position) -> Self {
        Self::WithPosition(err, pos)
    }

    pub fn as_bytes(&self, filename: &str, source: &str) -> Vec<u8> {
        match self {
            Self::WithPosition(err, pos) => {
                let mut files = SimpleFiles::new();
                let mut writer = Buffer::no_color();
                let config = codespan_reporting::term::Config::default();

                let file_id = files.add(filename, source);
                let diagnostic = Diagnostic::error()
                    .with_message(error_msg(err))
                    .with_labels(vec![Label::primary(
                        file_id,
                        pos.start..(pos.start + pos.length),
                    )]);

                let _ = term::emit(&mut writer, &config, &files, &diagnostic);

                writer.into_inner()
            }
            Self::IO(err) => err.as_bytes().to_vec(),
        }
    }

    pub fn emit(&self, filename: &str, source: &str) {
        match self {
            Self::WithPosition(err, pos) => {
                let mut files = SimpleFiles::new();
                let writer = StandardStream::stderr(term::termcolor::ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                let file_id = files.add(filename, source);
                let diagnostic = Diagnostic::error()
                    .with_message(error_msg(err))
                    .with_labels(vec![Label::primary(
                        file_id,
                        pos.start..(pos.start + pos.length),
                    )]);

                let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
            }
            Self::IO(err) => eprintln!("{err}"),
        }
    }

    pub fn msg(&self) -> String {
        match self {
            Self::IO(err) => err.to_string(),
            Self::WithPosition(err, _) => error_msg(err),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::IO(value.to_string())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    Lexer(LexerError),
    Parser(ParserError),
    Weeder(WeederError),
    Exec(EvalError),
    Import(ImportError),
}

impl From<LexerError> for ErrorKind {
    fn from(err: LexerError) -> Self {
        Self::Lexer(err)
    }
}

impl From<ParserError> for ErrorKind {
    fn from(err: ParserError) -> Self {
        Self::Parser(err)
    }
}

impl From<WeederError> for ErrorKind {
    fn from(err: WeederError) -> Self {
        Self::Weeder(err)
    }
}

impl From<EvalError> for ErrorKind {
    fn from(err: EvalError) -> Self {
        Self::Exec(err)
    }
}

impl From<ImportError> for ErrorKind {
    fn from(err: ImportError) -> Self {
        Self::Import(err)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Position {
    pub start: usize,
    pub length: usize,
}

impl Position {
    pub fn new(start: usize, length: usize) -> Self {
        Self { start, length }
    }

    pub fn join(&self, other: Self) -> Self {
        Self {
            start: self.start,
            length: other.start + other.length - self.start,
        }
    }
}

pub fn error_msg(err: &ErrorKind) -> String {
    match err {
        ErrorKind::Lexer(err) => lexer_error_msg(err),
        ErrorKind::Parser(err) => parser_error_msg(err),
        ErrorKind::Weeder(err) => weeder_error_msg(err),
        ErrorKind::Exec(err) => exec_error_msg(err),
        ErrorKind::Import(err) => import_error_msg(err),
    }
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
        TokenType::Case => "the `case` keyword".into(),
        TokenType::Char(chr) => format!("a character: `{chr}`"),
        TokenType::Colon => "a colon: `:`".into(),
        TokenType::Comma => "a comma: `,`".into(),
        TokenType::Do => "the `do` keyword".into(),
        TokenType::Dot => "a dot: `.`".into(),
        TokenType::DotDot => "a range operator: `..`".into(),
        TokenType::Else => "the `else` keyword".into(),
        TokenType::Equals => "an equality operator: `=`".into(),
        TokenType::False => "a boolean: `false`".into(),
        TokenType::FatArrow => "a fat arrow: `=>`".into(),
        TokenType::For => "the `for` keyword".into(),
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
        TokenType::Memoize => "the `memoize` keyword".into(),
        TokenType::Minus => "a minus operator: `-`".into(),
        TokenType::Newline => "a newline character".into(),
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
        TokenType::Var => "the `var` keyword".into(),
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
        ParserError::MemoizeInVarExpression => unindent(
            "
            You can only memoize `let` expressions that declare functions.
            Turn this into a `let` expression declaring a function, or remove the `memoize`",
        ),
    }
}

fn weeder_error_msg(err: &WeederError) -> String {
    match err {
        WeederError::BadDeclaration => unindent(
            "
            Expressions inside a declaration must be symbols with properties or assignments.
            Replace this with an assignment or a constrained symbol (i.e. name: Property)",
        ),
        WeederError::BadDot => unindent(
            "
            The dot is allowed for decimal numbers and calls only.
            At the right of the dot there should be a function call or a decimal number",
        ),
        WeederError::BadImportOrigin => {
            "The module from where you want to import can only be represented with a name".into()
        }
        WeederError::BadImportSymbol => unindent(
            "
            The thing you want to import must be represented with a name or a tuple of names.
            Replace this with a name or a tuple of names",
        ),
        WeederError::BadSymbolicDeclaration => {
            "Only names can be declared without a value. Replace this with a name".into()
        }
        WeederError::BadSymbolInImportTuple => {
            "You can only import named things from a module. Replace this with a name".into()
        }
        WeederError::BadAnonFunctionLHS => unindent(
            "
            The left hand side of an anonymous function can be a tuple or a symbol, nothing else.
            Replace this with a tuple of names or a name",
        ),
        WeederError::BadAnonFunctionParameter => {
            "Only names can be parameters of anonymous functions. Replace this with a name".into()
        }
        WeederError::PlainImportNotImplemented => {
            "Jokes on you, this is not yet implemented. Use `from module import stuff`".into()
        }
        WeederError::MutableFunctionDeclaration => {
            "Functions cannot be declared as mutable. Use `let` instead".into()
        }
        WeederError::MemoizedNonFunctionDeclaration => {
            "You can only memoize functions. Remove the `memoize` from the declaration".into()
        }
    }
}

fn exec_error_msg(err: &EvalError) -> String {
    match err {
        EvalError::BadFraction {
            numer_kind,
            denom_kind,
        } => bad_fraction(numer_kind, denom_kind),
        EvalError::BadMatch => "The pattern did not match its assigned value".into(),
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
        EvalError::InmutableAssign(value) => inmutable_assign(value),
        EvalError::UnmatchedCall => {
            "None of the patterns in the function matched the arguments of this call".into()
        }
        EvalError::UnmatchedExpression => "None of the patterns matched the expression".into(),
    }
}

fn inmutable_assign(value: &str) -> String {
    format!("`{value}` is inmutable, so it cannot be used in assignments")
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

fn import_error_msg(err: &ImportError) -> String {
    match err {
        ImportError::SymbolNotFound { module, symbol } => symbol_not_found(module, symbol),
    }
}

fn symbol_not_found(module: &str, symbol: &str) -> String {
    format!("`{symbol}` was not found in the `{module}` module")
}

#[cfg(test)]
mod tests {
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
            error_msg(&ParserError::EOFReached.into()),
            "The end of the program was reached while reading an expression".to_string(),
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
