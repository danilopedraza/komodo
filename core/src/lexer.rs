use std::{collections::VecDeque, iter::Peekable, str::Chars, vec};

use crate::error::{Error, Position};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexerError {
    EmptyChar,
    EmptyPrefixedInteger,
    LeadingZeros,
    UnexpectedChar(char),
    UnterminatedChar,
    UnterminatedString,
}

enum IndentLevel {
    Zero,
    NonZero(usize, Token),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub token: TokenType,
    pub position: Position,
}

impl Token {
    pub fn new(token: TokenType, position: Position) -> Self {
        Self { token, position }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Radix {
    Binary,
    Decimal,
    Hex,
    Octal,
}

impl From<Radix> for u32 {
    fn from(value: Radix) -> Self {
        match value {
            Radix::Binary => 2,
            Radix::Decimal => 10,
            Radix::Hex => 16,
            Radix::Octal => 8,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Arrow,
    As,
    Assign,
    Bang,
    BitwiseAnd,
    VerticalBar,
    BitwiseXor,
    Char(char),
    Colon,
    Comma,
    Dedent,
    Dot,
    DotDot,
    Else,
    Equals,
    False,
    For,
    From,
    Greater,
    GreaterEqual,
    Ident(String),
    If,
    Import,
    In,
    Indent,
    Integer(String, Radix),
    Lbrace,
    Lbrack,
    LeftShift,
    Less,
    LessEqual,
    Let,
    LogicAnd,
    LogicOr,
    Lparen,
    Minus,
    Percent,
    NotEqual,
    Over,
    Plus,
    Rbrace,
    Rbrack,
    RightShift,
    Rparen,
    SlashSlash,
    String(String),
    Then,
    Tilde,
    Times,
    ToThe,
    True,
    Unknown,
    Wildcard,
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    cur_pos: usize,
    token_queue: VecDeque<Result<Token, Error>>,
    indent_level: usize,
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(res) = self.token_queue.pop_front() {
            return Some(res);
        }

        self.consume_indent();

        let start = self.cur_pos;

        let res = match self.next_token() {
            Some(Ok(token)) => Some(Ok(Token::new(
                token,
                Position::new(start, self.cur_pos - start),
            ))),
            Some(Err(err)) => Some(Err(Error::new(
                err.into(),
                Position::new(start, self.cur_pos - start),
            ))),
            None => None,
        };

        if let Some(res) = res {
            self.token_queue.push_back(res);
        };

        self.token_queue.pop_front()
    }
}

type LexerResult = Result<TokenType, LexerError>;

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            cur_pos: 0,
            token_queue: VecDeque::default(),
            indent_level: 0,
        }
    }

    fn emit_indents(&mut self) -> IndentLevel {
        let mut start = self.cur_pos;
        let mut spaces = 0;
        let mut new_indent_level = 0;
        let mut token = None;

        while let Some(chr) = self.input.peek() {
            match chr {
                ' ' => {
                    self.next_char();
                    spaces += 1;
                }
                '\n' => {
                    self.next_char();
                    spaces = 0;
                    new_indent_level = 0;
                }
                _ => break,
            }

            if spaces == 4 {
                new_indent_level += 1;
                token = Some(Token::new(TokenType::Indent, Position::new(start, 4)));
                start = self.cur_pos;
                spaces = 0;
            }
        }

        if new_indent_level == 0 {
            IndentLevel::Zero
        } else {
            IndentLevel::NonZero(new_indent_level, token.unwrap())
        }
    }

    fn push_dedents(&mut self, amount: usize) {
        for _ in 0..amount {
            self.token_queue.push_back(Ok(Token::new(
                TokenType::Dedent,
                Position::new(self.cur_pos, 0),
            )));
        }
    }

    fn consume_indent(&mut self) {
        loop {
            match self.input.peek() {
                Some('\n') => {
                    self.next_char();

                    let indent_res = self.emit_indents();

                    match indent_res {
                        IndentLevel::Zero => {
                            self.push_dedents(self.indent_level);
                            self.indent_level = 0;
                        }
                        IndentLevel::NonZero(new_indent_level, indent_token) => {
                            if new_indent_level > self.indent_level {
                                self.token_queue.push_back(Ok(indent_token));
                            } else {
                                self.push_dedents(self.indent_level - new_indent_level);
                            }

                            self.indent_level = new_indent_level;
                        }
                    }

                    break;
                }
                Some(chr) if chr.is_whitespace() && *chr != '\n' => {
                    self.skip_non_linefeed_whitespace()
                }
                Some('#') => self.skip_comment(),
                _ => break,
            }
        }
    }

    fn next_char(&mut self) -> Option<char> {
        match self.input.next() {
            Some(chr) => {
                self.cur_pos += 1;
                Some(chr)
            }
            None => None,
        }
    }

    fn next_token(&mut self) -> Option<LexerResult> {
        match self.next_char() {
            None => None,
            Some('\'') => self.char(),
            Some('"') => Some(self.string_()),
            Some(chr) if chr.is_ascii_digit() => Some(self.integer(chr)),
            Some(chr) => Some(Ok(match chr {
                '!' => TokenType::Bang,
                '^' => TokenType::BitwiseXor,
                ',' => TokenType::Comma,
                '.' => self.fork(TokenType::Dot, vec![('.', TokenType::DotDot)]),
                '=' => TokenType::Equals,
                '&' => self.fork(TokenType::BitwiseAnd, vec![('&', TokenType::LogicAnd)]),
                '|' => self.fork(TokenType::VerticalBar, vec![('|', TokenType::LogicOr)]),
                ':' => self.fork(TokenType::Colon, vec![('=', TokenType::Assign)]),
                '>' => self.fork(
                    TokenType::Greater,
                    vec![('>', TokenType::RightShift), ('=', TokenType::GreaterEqual)],
                ),
                '<' => self.fork(
                    TokenType::Less,
                    vec![('<', TokenType::LeftShift), ('=', TokenType::LessEqual)],
                ),
                '-' => self.fork(TokenType::Minus, vec![('>', TokenType::Arrow)]),
                '/' => self.fork(
                    TokenType::Over,
                    vec![('=', TokenType::NotEqual), ('/', TokenType::SlashSlash)],
                ),
                '{' => TokenType::Lbrace,
                '[' => TokenType::Lbrack,
                '(' => TokenType::Lparen,
                '%' => TokenType::Percent,
                '+' => TokenType::Plus,
                '*' => self.fork(TokenType::Times, vec![('*', TokenType::ToThe)]),
                '}' => TokenType::Rbrace,
                ']' => TokenType::Rbrack,
                ')' => TokenType::Rparen,
                '~' => TokenType::Tilde,
                '_' => TokenType::Wildcard,
                chr if chr.is_alphabetic() => self.identifier_or_keyword(chr),
                _ => TokenType::Unknown,
            })),
        }
    }
    fn string_(&mut self) -> LexerResult {
        let mut str = String::new();

        loop {
            match self.next_char() {
                Some('\\') => match self.next_char() {
                    Some(c) => str.push(c),
                    None => break Err(LexerError::UnterminatedString),
                },
                Some('"') => break Ok(TokenType::String(str)),
                Some(c) => str.push(c),
                None => break Err(LexerError::UnterminatedString),
            }
        }
    }

    fn char(&mut self) -> Option<LexerResult> {
        if let Some('\'') = self.input.peek() {
            self.next_token();
            return Some(Err(LexerError::EmptyChar));
        }

        if let Some('\\') = self.input.peek() {
            self.next_token();
        }

        match (self.next_char(), self.next_char()) {
            (Some(chr), Some('\'')) => Some(Ok(TokenType::Char(chr))),
            (Some(_), Some(c)) => Some(Err(LexerError::UnexpectedChar(c))),
            _ => Some(Err(LexerError::UnterminatedChar)),
        }
    }

    fn skip_comment(&mut self) {
        for chr in self.input.by_ref() {
            self.cur_pos += 1;
            if chr == 0xA as char {
                break;
            } else {
                continue;
            }
        }
    }

    fn skip_non_linefeed_whitespace(&mut self) {
        while let Some(chr) = self.input.peek() {
            if *chr != '\n' && chr.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    fn identifier_or_keyword(&mut self, first: char) -> TokenType {
        let mut literal = String::from(first);
        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_alphanumeric()) {
            // is (kinda) intentional to accept non-ascii symbols
            self.cur_pos += 1;
            literal.push(chr);
        }

        match Self::keyword(&literal) {
            Some(tok) => tok,
            None => TokenType::Ident(literal),
        }
    }

    fn keyword(literal: &str) -> Option<TokenType> {
        match literal {
            "as" => Some(TokenType::As),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "from" => Some(TokenType::From),
            "if" => Some(TokenType::If),
            "import" => Some(TokenType::Import),
            "in" => Some(TokenType::In),
            "let" => Some(TokenType::Let),
            "then" => Some(TokenType::Then),
            "true" => Some(TokenType::True),
            _ => None,
        }
    }

    fn fork(&mut self, def: TokenType, alts: Vec<(char, TokenType)>) -> TokenType {
        for (chr, tok) in alts {
            if matches!(self.input.peek(), Some(c) if *c == chr) {
                self.next_char();
                return tok;
            }
        }

        def
    }

    fn is_digit(chr: char, radix: Radix) -> bool {
        match radix {
            Radix::Binary => matches!(chr, '0' | '1'),
            Radix::Decimal => chr.is_ascii_digit(),
            Radix::Hex => chr.is_ascii_digit() || matches!(chr, 'a'..='f'),
            Radix::Octal => matches!(chr, '0'..='7'),
        }
    }

    fn integer(&mut self, first: char) -> Result<TokenType, LexerError> {
        let radix = match first {
            '0' => match self.input.peek() {
                Some('b' | 'B') => Radix::Binary,
                Some('x' | 'X') => Radix::Hex,
                Some('o' | 'O') => Radix::Octal,
                _ => Radix::Decimal,
            },
            _ => Radix::Decimal,
        };

        let mut literal = String::new();
        if radix != Radix::Decimal {
            self.next_char();
        } else {
            literal.push(first);
        }

        while let Some(chr) = self
            .input
            .by_ref()
            .next_if(|c| Self::is_digit(*c, radix) || *c == '_')
        {
            self.cur_pos += 1;
            if chr != '_' {
                literal.push(chr);
            }
        }

        match (literal.chars().next(), radix) {
            (None, _) => Err(LexerError::EmptyPrefixedInteger),
            (Some('0'), Radix::Decimal) if literal.len() > 1 => Err(LexerError::LeadingZeros),
            _ => Ok(TokenType::Integer(literal, radix)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use unindent::unindent;

    use crate::cst::tests::_pos;

    use super::*;

    fn tokens_from(source: &str) -> Result<Vec<Token>, Error> {
        Lexer::new(source).collect()
    }

    fn token_types_from(source: &str) -> Result<Vec<TokenType>, Error> {
        match tokens_from(source) {
            Ok(tokens) => Ok(tokens.into_iter().map(|t| t.token).collect()),
            Err(err) => Err(err),
        }
    }

    #[test]
    fn empty_string() {
        assert!(Lexer::new("").next().is_none());
    }

    #[test]
    fn plus_operator() {
        assert_eq!(
            Lexer::new("+").next(),
            Some(Ok(Token::new(TokenType::Plus, Position::new(0, 1))))
        );
    }

    #[test]
    fn whitespace() {
        assert_eq!(Lexer::new(" \t").next(), None);
    }

    #[test]
    fn simple_expression() {
        assert_eq!(
            tokens_from("x + y /= a"),
            Ok(vec![
                Token::new(TokenType::Ident(String::from('x')), Position::new(0, 1)),
                Token::new(TokenType::Plus, Position::new(2, 1)),
                Token::new(TokenType::Ident(String::from('y')), Position::new(4, 1)),
                Token::new(TokenType::NotEqual, Position::new(6, 2)),
                Token::new(TokenType::Ident(String::from('a')), Position::new(9, 1)),
            ]),
        );
    }

    #[test]
    fn simple_statement() {
        assert_eq!(
            tokens_from("let x := 1 / 2."),
            Ok(vec![
                Token::new(TokenType::Let, Position::new(0, 3)),
                Token::new(TokenType::Ident(String::from('x')), Position::new(4, 1)),
                Token::new(TokenType::Assign, Position::new(6, 2)),
                Token::new(
                    TokenType::Integer(String::from('1'), Radix::Decimal),
                    Position::new(9, 1)
                ),
                Token::new(TokenType::Over, Position::new(11, 1)),
                Token::new(
                    TokenType::Integer(String::from('2'), Radix::Decimal),
                    Position::new(13, 1)
                ),
                Token::new(TokenType::Dot, Position::new(14, 1)),
            ]),
        );
    }

    #[test]
    fn complex_expression() {
        assert_eq!(
            tokens_from("(x * y) = 23 % 2"),
            Ok(vec![
                Token::new(TokenType::Lparen, Position::new(0, 1)),
                Token::new(TokenType::Ident(String::from('x')), Position::new(1, 1)),
                Token::new(TokenType::Times, Position::new(3, 1)),
                Token::new(TokenType::Ident(String::from('y')), Position::new(5, 1)),
                Token::new(TokenType::Rparen, Position::new(6, 1)),
                Token::new(TokenType::Equals, Position::new(8, 1)),
                Token::new(
                    TokenType::Integer(String::from("23"), Radix::Decimal),
                    Position::new(10, 2)
                ),
                Token::new(TokenType::Percent, Position::new(13, 1)),
                Token::new(
                    TokenType::Integer(String::from("2"), Radix::Decimal),
                    Position::new(15, 1)
                ),
            ]),
        );
    }

    #[test]
    fn leq_comparisons() {
        assert_eq!(
            token_types_from("a < b <= c"),
            Ok(vec![
                TokenType::Ident(String::from("a")),
                TokenType::Less,
                TokenType::Ident(String::from("b")),
                TokenType::LessEqual,
                TokenType::Ident(String::from("c"))
            ]),
        );
    }

    #[test]
    fn geq_comparisons() {
        assert_eq!(
            token_types_from("a > b >= c"),
            Ok(vec![
                TokenType::Ident(String::from("a")),
                TokenType::Greater,
                TokenType::Ident(String::from("b")),
                TokenType::GreaterEqual,
                TokenType::Ident(String::from("c"))
            ]),
        );
    }

    #[test]
    fn function_declaration() {
        assert_eq!(
            token_types_from("let f: a -> a"),
            Ok(vec![
                TokenType::Let,
                TokenType::Ident(String::from('f')),
                TokenType::Colon,
                TokenType::Ident(String::from('a')),
                TokenType::Arrow,
                TokenType::Ident(String::from('a'))
            ]),
        );
    }

    #[test]
    fn shift_operator() {
        assert_eq!(
            token_types_from("(1 << 2) >> 2"),
            Ok(vec![
                TokenType::Lparen,
                TokenType::Integer(String::from("1"), Radix::Decimal),
                TokenType::LeftShift,
                TokenType::Integer(String::from("2"), Radix::Decimal),
                TokenType::Rparen,
                TokenType::RightShift,
                TokenType::Integer(String::from("2"), Radix::Decimal)
            ]),
        );
    }

    #[test]
    fn function_call() {
        assert_eq!(
            tokens_from("f(x,y ** 2)"),
            Ok(vec![
                Token::new(TokenType::Ident(String::from('f')), Position::new(0, 1)),
                Token::new(TokenType::Lparen, Position::new(1, 1)),
                Token::new(TokenType::Ident(String::from('x')), Position::new(2, 1)),
                Token::new(TokenType::Comma, Position::new(3, 1)),
                Token::new(TokenType::Ident(String::from('y')), Position::new(4, 1)),
                Token::new(TokenType::ToThe, Position::new(6, 2)),
                Token::new(
                    TokenType::Integer(String::from('2'), Radix::Decimal),
                    Position::new(9, 1)
                ),
                Token::new(TokenType::Rparen, Position::new(10, 1)),
            ]),
        );
    }

    #[test]
    fn set() {
        assert_eq!(
            token_types_from("{true, false}"),
            Ok(vec![
                TokenType::Lbrace,
                TokenType::True,
                TokenType::Comma,
                TokenType::False,
                TokenType::Rbrace
            ]),
        );
    }

    #[test]
    fn leading_zeros() {
        assert_eq!(
            token_types_from("01"),
            Err(Error::new(LexerError::LeadingZeros.into(), _pos(0, 2))),
        );
    }

    #[test]
    fn comment() {
        let code = "input() # get input\nprint() # print";

        assert_eq!(
            tokens_from(code),
            Ok(vec![
                Token::new(TokenType::Ident(String::from("input")), Position::new(0, 5)),
                Token::new(TokenType::Lparen, Position::new(5, 1)),
                Token::new(TokenType::Rparen, Position::new(6, 1)),
                Token::new(
                    TokenType::Ident(String::from("print")),
                    Position::new(20, 5)
                ),
                Token::new(TokenType::Lparen, Position::new(25, 1)),
                Token::new(TokenType::Rparen, Position::new(26, 1)),
            ]),
        );
    }

    #[test]
    fn if_expr() {
        let code = "if a < 0 then -a else a";

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::If,
                TokenType::Ident(String::from("a")),
                TokenType::Less,
                TokenType::Integer(String::from("0"), Radix::Decimal),
                TokenType::Then,
                TokenType::Minus,
                TokenType::Ident(String::from("a")),
                TokenType::Else,
                TokenType::Ident(String::from("a")),
            ]),
        );
    }

    #[test]
    fn character() {
        let code = "'x'";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Ok(Token::new(TokenType::Char('x'), Position::new(0, 3)))),
        );
    }

    #[test]
    fn unexpected_char_eof() {
        let code = "'x";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Err(Error::new(
                LexerError::UnterminatedChar.into(),
                _pos(0, 2)
            ))),
        );
    }

    #[test]
    fn bad_delimiter() {
        let code = "'x)";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Err(Error::new(
                LexerError::UnexpectedChar(')').into(),
                _pos(0, 3)
            )))
        );
    }

    #[test]
    fn string() {
        let code = "\"abc\"";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Ok(Token::new(
                TokenType::String(String::from("abc")),
                Position::new(0, 5)
            ))),
        );
    }

    #[test]
    fn unterminated_string() {
        let code = "\"a";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Err(Error::new(
                LexerError::UnterminatedString.into(),
                _pos(0, 2)
            ))),
        );
    }

    #[test]
    fn list() {
        let code = "[1, 2]";

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::Lbrack,
                TokenType::Integer(String::from("1"), Radix::Decimal),
                TokenType::Comma,
                TokenType::Integer(String::from("2"), Radix::Decimal),
                TokenType::Rbrack,
            ]),
        );
    }

    #[test]
    fn wildcard() {
        let code = "f(x, _) := x";

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::Ident(String::from("f")),
                TokenType::Lparen,
                TokenType::Ident(String::from("x")),
                TokenType::Comma,
                TokenType::Wildcard,
                TokenType::Rparen,
                TokenType::Assign,
                TokenType::Ident(String::from("x")),
            ]),
        );
    }

    #[test]
    fn range() {
        let code = "0..10";

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::Integer(String::from("0"), Radix::Decimal),
                TokenType::DotDot,
                TokenType::Integer(String::from("10"), Radix::Decimal),
            ]),
        );
    }

    #[test]
    fn fraction() {
        let code = "1 // 2";

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::Integer(String::from("1"), Radix::Decimal),
                TokenType::SlashSlash,
                TokenType::Integer(String::from("2"), Radix::Decimal),
            ]),
        );
    }

    #[test]
    fn escape_char() {
        let code = "'\\''";

        assert_eq!(token_types_from(code), Ok(vec![TokenType::Char('\''),]),);
    }

    #[test]
    fn empty_char() {
        let code = "''";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Err(Error::new(LexerError::EmptyChar.into(), _pos(0, 2)))),
        );
    }

    #[test]
    fn escape_string() {
        let code = "\"\\\"\"";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Ok(Token::new(
                TokenType::String(String::from("\"")),
                _pos(0, 4)
            )))
        );
    }

    #[test]
    fn ident_with_number() {
        let code = "s1";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Ok(Token::new(
                TokenType::Ident(String::from("s1")),
                _pos(0, 2),
            )))
        );
    }

    #[test]
    fn container_element() {
        let code = "list[0]";

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::Ident(String::from("list")),
                TokenType::Lbrack,
                TokenType::Integer(String::from("0"), Radix::Decimal),
                TokenType::Rbrack,
            ])
        );
    }

    #[test]
    fn import_statement() {
        let code = "from foo import bar as baz";

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::From,
                TokenType::Ident(String::from("foo")),
                TokenType::Import,
                TokenType::Ident(String::from("bar")),
                TokenType::As,
                TokenType::Ident(String::from("baz"))
            ])
        );
    }

    #[test]
    fn hex() {
        let code = "0x123456789abcdef0";

        assert_eq!(
            token_types_from(code),
            Ok(vec![TokenType::Integer(
                String::from("123456789abcdef0"),
                Radix::Hex
            )])
        );
    }

    #[test]
    fn underscore_between_digits() {
        let code = "0b0010_0001";

        assert_eq!(
            token_types_from(code),
            Ok(vec![TokenType::Integer("00100001".into(), Radix::Binary),])
        );
    }

    #[test]
    fn one_line_indent() {
        let code = &unindent(
            "
        let f := n ->

            n
        f
        ",
        );

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::Let,
                TokenType::Ident("f".into()),
                TokenType::Assign,
                TokenType::Ident("n".into()),
                TokenType::Arrow,
                TokenType::Indent,
                TokenType::Ident("n".into()),
                TokenType::Dedent,
                TokenType::Ident("f".into()),
            ])
        );
    }

    #[test]
    fn integer_position() {
        let code = "0x8";

        assert_eq!(
            Lexer::new(code).next(),
            Some(Ok(Token {
                token: TokenType::Integer("8".into(), Radix::Hex),
                position: Position {
                    start: 0,
                    length: 3
                }
            })),
        );
    }

    #[test]
    fn two_line_indent() {
        let code = &unindent(
            "
        let f := n ->
            let a := n + 1
            a
        ",
        );

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::Let,
                TokenType::Ident("f".into()),
                TokenType::Assign,
                TokenType::Ident("n".into()),
                TokenType::Arrow,
                TokenType::Indent,
                TokenType::Let,
                TokenType::Ident("a".into()),
                TokenType::Assign,
                TokenType::Ident("n".into()),
                TokenType::Plus,
                TokenType::Integer("1".into(), Radix::Decimal),
                TokenType::Ident("a".into()),
                TokenType::Dedent,
            ])
        );
    }

    #[test]
    fn indent_position() {
        let code = &unindent(
            "
        let f := n ->
            n
        ",
        );

        assert_eq!(
            tokens_from(code),
            Ok(vec![
                Token {
                    token: TokenType::Let,
                    position: Position {
                        start: 0,
                        length: 3
                    }
                },
                Token {
                    token: TokenType::Ident("f".into()),
                    position: Position {
                        start: 4,
                        length: 1
                    }
                },
                Token {
                    token: TokenType::Assign,
                    position: Position {
                        start: 6,
                        length: 2
                    }
                },
                Token {
                    token: TokenType::Ident("n".into()),
                    position: Position {
                        start: 9,
                        length: 1
                    }
                },
                Token {
                    token: TokenType::Arrow,
                    position: Position {
                        start: 11,
                        length: 2
                    }
                },
                Token {
                    token: TokenType::Indent,
                    position: Position {
                        start: 14,
                        length: 4
                    }
                },
                Token {
                    token: TokenType::Ident("n".into()),
                    position: Position {
                        start: 18,
                        length: 1
                    }
                },
                Token {
                    token: TokenType::Dedent,
                    position: Position {
                        start: 20,
                        length: 0
                    }
                },
            ]),
        );
    }

    #[test]
    fn double_block() {
        let code = &unindent(
            "
        for _ in foo:
            for _ in bar:
                baz()
        ",
        );

        assert_eq!(
            token_types_from(code),
            Ok(vec![
                TokenType::For,
                TokenType::Wildcard,
                TokenType::In,
                TokenType::Ident("foo".into()),
                TokenType::Colon,
                TokenType::Indent,
                TokenType::For,
                TokenType::Wildcard,
                TokenType::In,
                TokenType::Ident("bar".into()),
                TokenType::Colon,
                TokenType::Indent,
                TokenType::Ident("baz".into()),
                TokenType::Lparen,
                TokenType::Rparen,
                TokenType::Dedent,
                TokenType::Dedent,
            ]),
        );
    }
}
