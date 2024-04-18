use std::{iter::Peekable, str::Chars, vec};

use crate::error::Position;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedChar(char),
    UnexpectedEOF,
    UnterminatedChar,
    UnterminatedString,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TokenInfo {
    pub token: Token,
    pub position: Position,
}

impl TokenInfo {
    pub fn new(token: Token, position: Position) -> Self {
        Self { token, position }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Arrow,
    Assign,
    Bang,
    BitwiseAnd,
    VerticalBar,
    BitwiseXor,
    Char(char),
    Colon,
    Comma,
    Dot,
    Else,
    Equals,
    False,
    For,
    Greater,
    GreaterEqual,
    Ident(String),
    If,
    In,
    Integer(String),
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
    Mod,
    NotEqual,
    Over,
    Plus,
    Rbrace,
    Rbrack,
    RightShift,
    Rparen,
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
    cur_pos: u32,
}

impl Iterator for Lexer<'_> {
    type Item = Result<TokenInfo, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.input.peek() {
                Some(chr) if chr.is_whitespace() => self.skip_whitespace(),
                Some('#') => self.skip_comment(),
                _ => break,
            }
        }

        let start = self.cur_pos;

        match self.next_token() {
            Some(Ok(token)) => Some(Ok(TokenInfo::new(
                token,
                Position::new(start, self.cur_pos - start),
            ))),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }
}

type LexerResult = Result<Token, LexerError>;

impl Lexer<'_> {
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
            Some(chr) => Some(Ok(match chr {
                '!' => Token::Bang,
                '^' => Token::BitwiseXor,
                ',' => Token::Comma,
                '.' => Token::Dot,
                '=' => Token::Equals,
                '&' => self.fork(Token::BitwiseAnd, vec![('&', Token::LogicAnd)]),
                '|' => self.fork(Token::VerticalBar, vec![('|', Token::LogicOr)]),
                ':' => self.fork(Token::Colon, vec![('=', Token::Assign)]),
                '>' => self.fork(
                    Token::Greater,
                    vec![('>', Token::RightShift), ('=', Token::GreaterEqual)],
                ),
                '<' => self.fork(
                    Token::Less,
                    vec![('<', Token::LeftShift), ('=', Token::LessEqual)],
                ),
                '-' => self.fork(Token::Minus, vec![('>', Token::Arrow)]),
                '/' => self.fork(Token::Over, vec![('=', Token::NotEqual)]),
                '{' => Token::Lbrace,
                '[' => Token::Lbrack,
                '(' => Token::Lparen,
                '%' => Token::Mod,
                '+' => Token::Plus,
                '*' => self.fork(Token::Times, vec![('*', Token::ToThe)]),
                '}' => Token::Rbrace,
                ']' => Token::Rbrack,
                ')' => Token::Rparen,
                '~' => Token::Tilde,
                '_' => Token::Wildcard,
                '0'..='9' => self.integer(chr),
                chr if chr.is_alphabetic() => self.identifier_or_keyword(chr),
                _ => Token::Unknown,
            })),
        }
    }
    fn string_(&mut self) -> LexerResult {
        let mut str = String::new();

        loop {
            match self.next_char() {
                Some('"') => break Ok(Token::String(str)),
                Some(c) => str.push(c),
                None => break Err(LexerError::UnterminatedString),
            }
        }
    }

    fn char(&mut self) -> Option<LexerResult> {
        let chr = self.next_char();
        if chr.is_none() {
            return Some(Err(LexerError::UnexpectedEOF));
        }

        Some(match self.next_char() {
            Some('\'') => Ok(Token::Char(chr.unwrap())),
            Some(c) => Err(LexerError::UnexpectedChar(c)),
            None => Err(LexerError::UnterminatedChar),
        })
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

    fn skip_whitespace(&mut self) {
        while let Some(chr) = self.input.peek() {
            if chr.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    fn identifier_or_keyword(&mut self, first: char) -> Token {
        let mut literal = String::from(first);
        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_alphabetic()) {
            self.cur_pos += 1;
            literal.push(chr);
        }

        match Self::keyword(&literal) {
            Some(tok) => tok,
            None => Token::Ident(literal),
        }
    }

    fn keyword(literal: &str) -> Option<Token> {
        match literal {
            "else" => Some(Token::Else),
            "false" => Some(Token::False),
            "for" => Some(Token::For),
            "if" => Some(Token::If),
            "in" => Some(Token::In),
            "let" => Some(Token::Let),
            "then" => Some(Token::Then),
            "true" => Some(Token::True),
            _ => None,
        }
    }

    fn fork(&mut self, def: Token, alts: Vec<(char, Token)>) -> Token {
        for (chr, tok) in alts {
            if matches!(self.input.peek(), Some(c) if *c == chr) {
                self.next_char();
                return tok;
            }
        }

        def
    }

    fn integer(&mut self, first: char) -> Token {
        if first == '0' {
            return Token::Integer(String::from(first));
        }

        let mut number = String::from(first);

        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_ascii_digit()) {
            self.cur_pos += 1;
            number.push(chr);
        }

        Token::Integer(number)
    }
}

pub fn build_lexer(input: &str) -> Lexer {
    Lexer {
        input: input.chars().peekable(),
        cur_pos: 0,
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn empty_string() {
        assert!(build_lexer("").next().is_none());
    }

    #[test]
    fn plus_operator() {
        assert_eq!(
            build_lexer("+").next(),
            Some(Ok(TokenInfo::new(Token::Plus, Position::new(0, 1))))
        );
    }

    #[test]
    fn whitespace() {
        assert_eq!(build_lexer(" \t").next(), None);
    }

    #[test]
    fn simple_expression() {
        assert_eq!(
            build_lexer("x + y /= a")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap())
                .collect::<Vec<_>>(),
            vec![
                TokenInfo::new(Token::Ident(String::from('x')), Position::new(0, 1)),
                TokenInfo::new(Token::Plus, Position::new(2, 1)),
                TokenInfo::new(Token::Ident(String::from('y')), Position::new(4, 1)),
                TokenInfo::new(Token::NotEqual, Position::new(6, 2)),
                TokenInfo::new(Token::Ident(String::from('a')), Position::new(9, 1)),
            ],
        );
    }

    #[test]
    fn simple_statement() {
        assert_eq!(
            build_lexer("let x := 1 / 2.")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap())
                .collect::<Vec<_>>(),
            vec![
                TokenInfo::new(Token::Let, Position::new(0, 3)),
                TokenInfo::new(Token::Ident(String::from('x')), Position::new(4, 1)),
                TokenInfo::new(Token::Assign, Position::new(6, 2)),
                TokenInfo::new(Token::Integer(String::from('1')), Position::new(9, 1)),
                TokenInfo::new(Token::Over, Position::new(11, 1)),
                TokenInfo::new(Token::Integer(String::from('2')), Position::new(13, 1)),
                TokenInfo::new(Token::Dot, Position::new(14, 1)),
            ],
        );
    }

    #[test]
    fn complex_expression() {
        assert_eq!(
            build_lexer("(x * y) = 23 % 2")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap())
                .collect::<Vec<_>>(),
            vec![
                TokenInfo::new(Token::Lparen, Position::new(0, 1)),
                TokenInfo::new(Token::Ident(String::from('x')), Position::new(1, 1)),
                TokenInfo::new(Token::Times, Position::new(3, 1)),
                TokenInfo::new(Token::Ident(String::from('y')), Position::new(5, 1)),
                TokenInfo::new(Token::Rparen, Position::new(6, 1)),
                TokenInfo::new(Token::Equals, Position::new(8, 1)),
                TokenInfo::new(Token::Integer(String::from("23")), Position::new(10, 2)),
                TokenInfo::new(Token::Mod, Position::new(13, 1)),
                TokenInfo::new(Token::Integer(String::from("2")), Position::new(15, 1)),
            ],
        );
    }

    #[test]
    fn leq_comparisons() {
        assert_eq!(
            build_lexer("a < b <= c")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::Ident(String::from("a")),
                Token::Less,
                Token::Ident(String::from("b")),
                Token::LessEqual,
                Token::Ident(String::from("c"))
            ],
        );
    }

    #[test]
    fn geq_comparisons() {
        assert_eq!(
            build_lexer("a > b >= c")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::Ident(String::from("a")),
                Token::Greater,
                Token::Ident(String::from("b")),
                Token::GreaterEqual,
                Token::Ident(String::from("c"))
            ],
        );
    }

    #[test]
    fn function_declaration() {
        assert_eq!(
            build_lexer("let f: a -> a")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::Let,
                Token::Ident(String::from('f')),
                Token::Colon,
                Token::Ident(String::from('a')),
                Token::Arrow,
                Token::Ident(String::from('a'))
            ],
        );
    }

    #[test]
    fn shift_operator() {
        assert_eq!(
            build_lexer("(1 << 2) >> 2")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::Lparen,
                Token::Integer(String::from("1")),
                Token::LeftShift,
                Token::Integer(String::from("2")),
                Token::Rparen,
                Token::RightShift,
                Token::Integer(String::from("2"))
            ]
        );
    }

    #[test]
    fn function_call() {
        assert_eq!(
            build_lexer("f(x,y ** 2)")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap())
                .collect::<Vec<_>>(),
            vec![
                TokenInfo::new(Token::Ident(String::from('f')), Position::new(0, 1)),
                TokenInfo::new(Token::Lparen, Position::new(1, 1)),
                TokenInfo::new(Token::Ident(String::from('x')), Position::new(2, 1)),
                TokenInfo::new(Token::Comma, Position::new(3, 1)),
                TokenInfo::new(Token::Ident(String::from('y')), Position::new(4, 1)),
                TokenInfo::new(Token::ToThe, Position::new(6, 2)),
                TokenInfo::new(Token::Integer(String::from('2')), Position::new(9, 1)),
                TokenInfo::new(Token::Rparen, Position::new(10, 1)),
            ],
        );
    }

    #[test]
    fn set() {
        assert_eq!(
            build_lexer("{true, false}")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::Lbrace,
                Token::True,
                Token::Comma,
                Token::False,
                Token::Rbrace
            ],
        );
    }

    #[test]
    fn leading_zeros() {
        assert_eq!(
            build_lexer("01")
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::Integer(String::from('0')),
                Token::Integer(String::from('1'))
            ]
        );
    }

    #[test]
    fn comment() {
        let code = "input() # get input\nprint() # print";

        assert_eq!(
            build_lexer(code)
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap())
                .collect::<Vec<_>>(),
            vec![
                TokenInfo::new(Token::Ident(String::from("input")), Position::new(0, 5)),
                TokenInfo::new(Token::Lparen, Position::new(5, 1)),
                TokenInfo::new(Token::Rparen, Position::new(6, 1)),
                TokenInfo::new(Token::Ident(String::from("print")), Position::new(20, 5)),
                TokenInfo::new(Token::Lparen, Position::new(25, 1)),
                TokenInfo::new(Token::Rparen, Position::new(26, 1)),
            ],
        );
    }

    #[test]
    fn if_expr() {
        let code = "if a < 0 then -a else a";

        assert_eq!(
            build_lexer(code)
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::If,
                Token::Ident(String::from("a")),
                Token::Less,
                Token::Integer(String::from("0")),
                Token::Then,
                Token::Minus,
                Token::Ident(String::from("a")),
                Token::Else,
                Token::Ident(String::from("a")),
            ]
        );
    }

    #[test]
    fn character() {
        let code = "'x'";

        assert_eq!(
            build_lexer(code).next(),
            Some(Ok(TokenInfo::new(Token::Char('x'), Position::new(0, 3)))),
        );
    }

    #[test]
    fn unexpected_char_eof() {
        let code = "'x";

        assert_eq!(
            build_lexer(code).next(),
            Some(Err(LexerError::UnterminatedChar)),
        );
    }

    #[test]
    fn bad_delimiter() {
        let code = "'x)";

        assert_eq!(
            build_lexer(code).next(),
            Some(Err(LexerError::UnexpectedChar(')')))
        );
    }

    #[test]
    fn string() {
        let code = "\"abc\"";

        assert_eq!(
            build_lexer(code).next(),
            Some(Ok(TokenInfo::new(
                Token::String(String::from("abc")),
                Position::new(0, 5)
            ))),
        );
    }

    #[test]
    fn unterminated_string() {
        let code = "\"a";

        assert_eq!(
            build_lexer(code).next(),
            Some(Err(LexerError::UnterminatedString)),
        );
    }

    #[test]
    fn list() {
        let code = "[1, 2]";

        assert_eq!(
            build_lexer(code)
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::Lbrack,
                Token::Integer(String::from("1")),
                Token::Comma,
                Token::Integer(String::from("2")),
                Token::Rbrack,
            ],
        );
    }

    #[test]
    fn wildcard() {
        let code = "f(x, _) := x";

        assert_eq!(
            build_lexer(code)
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                Token::Ident(String::from("f")),
                Token::Lparen,
                Token::Ident(String::from("x")),
                Token::Comma,
                Token::Wildcard,
                Token::Rparen,
                Token::Assign,
                Token::Ident(String::from("x")),
            ],
        );
    }
}
