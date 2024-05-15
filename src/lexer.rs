use std::{iter::Peekable, str::Chars, vec};

use crate::error::{Error, Position};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedChar(char),
    UnexpectedEOF,
    UnterminatedChar,
    UnterminatedString,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
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
    type Item = Result<Token, Error>;

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
            Some(Ok(token)) => Some(Ok(Token::new(
                token,
                Position::new(start, self.cur_pos - start),
            ))),
            Some(Err(err)) => Some(Err(Error::new(
                err.into(),
                Position::new(start, self.cur_pos - start),
            ))),
            None => None,
        }
    }
}

type LexerResult = Result<TokenType, LexerError>;

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
                '!' => TokenType::Bang,
                '^' => TokenType::BitwiseXor,
                ',' => TokenType::Comma,
                '.' => TokenType::Dot,
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
                '/' => self.fork(TokenType::Over, vec![('=', TokenType::NotEqual)]),
                '{' => TokenType::Lbrace,
                '[' => TokenType::Lbrack,
                '(' => TokenType::Lparen,
                '%' => TokenType::Mod,
                '+' => TokenType::Plus,
                '*' => self.fork(TokenType::Times, vec![('*', TokenType::ToThe)]),
                '}' => TokenType::Rbrace,
                ']' => TokenType::Rbrack,
                ')' => TokenType::Rparen,
                '~' => TokenType::Tilde,
                '_' => TokenType::Wildcard,
                '0'..='9' => self.integer(chr),
                chr if chr.is_alphabetic() => self.identifier_or_keyword(chr),
                _ => TokenType::Unknown,
            })),
        }
    }
    fn string_(&mut self) -> LexerResult {
        let mut str = String::new();

        loop {
            match self.next_char() {
                Some('"') => break Ok(TokenType::String(str)),
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
            Some('\'') => Ok(TokenType::Char(chr.unwrap())),
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

    fn identifier_or_keyword(&mut self, first: char) -> TokenType {
        let mut literal = String::from(first);
        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_alphabetic()) {
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
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "if" => Some(TokenType::If),
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

    fn integer(&mut self, first: char) -> TokenType {
        if first == '0' {
            return TokenType::Integer(String::from(first));
        }

        let mut number = String::from(first);

        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_ascii_digit()) {
            self.cur_pos += 1;
            number.push(chr);
        }

        TokenType::Integer(number)
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

    use crate::ast::_pos;

    use super::*;

    #[test]
    fn empty_string() {
        assert!(build_lexer("").next().is_none());
    }

    #[test]
    fn plus_operator() {
        assert_eq!(
            build_lexer("+").next(),
            Some(Ok(Token::new(TokenType::Plus, Position::new(0, 1))))
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
                Token::new(TokenType::Ident(String::from('x')), Position::new(0, 1)),
                Token::new(TokenType::Plus, Position::new(2, 1)),
                Token::new(TokenType::Ident(String::from('y')), Position::new(4, 1)),
                Token::new(TokenType::NotEqual, Position::new(6, 2)),
                Token::new(TokenType::Ident(String::from('a')), Position::new(9, 1)),
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
                Token::new(TokenType::Let, Position::new(0, 3)),
                Token::new(TokenType::Ident(String::from('x')), Position::new(4, 1)),
                Token::new(TokenType::Assign, Position::new(6, 2)),
                Token::new(TokenType::Integer(String::from('1')), Position::new(9, 1)),
                Token::new(TokenType::Over, Position::new(11, 1)),
                Token::new(TokenType::Integer(String::from('2')), Position::new(13, 1)),
                Token::new(TokenType::Dot, Position::new(14, 1)),
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
                Token::new(TokenType::Lparen, Position::new(0, 1)),
                Token::new(TokenType::Ident(String::from('x')), Position::new(1, 1)),
                Token::new(TokenType::Times, Position::new(3, 1)),
                Token::new(TokenType::Ident(String::from('y')), Position::new(5, 1)),
                Token::new(TokenType::Rparen, Position::new(6, 1)),
                Token::new(TokenType::Equals, Position::new(8, 1)),
                Token::new(TokenType::Integer(String::from("23")), Position::new(10, 2)),
                Token::new(TokenType::Mod, Position::new(13, 1)),
                Token::new(TokenType::Integer(String::from("2")), Position::new(15, 1)),
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
                TokenType::Ident(String::from("a")),
                TokenType::Less,
                TokenType::Ident(String::from("b")),
                TokenType::LessEqual,
                TokenType::Ident(String::from("c"))
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
                TokenType::Ident(String::from("a")),
                TokenType::Greater,
                TokenType::Ident(String::from("b")),
                TokenType::GreaterEqual,
                TokenType::Ident(String::from("c"))
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
                TokenType::Let,
                TokenType::Ident(String::from('f')),
                TokenType::Colon,
                TokenType::Ident(String::from('a')),
                TokenType::Arrow,
                TokenType::Ident(String::from('a'))
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
                TokenType::Lparen,
                TokenType::Integer(String::from("1")),
                TokenType::LeftShift,
                TokenType::Integer(String::from("2")),
                TokenType::Rparen,
                TokenType::RightShift,
                TokenType::Integer(String::from("2"))
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
                Token::new(TokenType::Ident(String::from('f')), Position::new(0, 1)),
                Token::new(TokenType::Lparen, Position::new(1, 1)),
                Token::new(TokenType::Ident(String::from('x')), Position::new(2, 1)),
                Token::new(TokenType::Comma, Position::new(3, 1)),
                Token::new(TokenType::Ident(String::from('y')), Position::new(4, 1)),
                Token::new(TokenType::ToThe, Position::new(6, 2)),
                Token::new(TokenType::Integer(String::from('2')), Position::new(9, 1)),
                Token::new(TokenType::Rparen, Position::new(10, 1)),
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
                TokenType::Lbrace,
                TokenType::True,
                TokenType::Comma,
                TokenType::False,
                TokenType::Rbrace
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
                TokenType::Integer(String::from('0')),
                TokenType::Integer(String::from('1'))
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
                Token::new(TokenType::Ident(String::from("input")), Position::new(0, 5)),
                Token::new(TokenType::Lparen, Position::new(5, 1)),
                Token::new(TokenType::Rparen, Position::new(6, 1)),
                Token::new(
                    TokenType::Ident(String::from("print")),
                    Position::new(20, 5)
                ),
                Token::new(TokenType::Lparen, Position::new(25, 1)),
                Token::new(TokenType::Rparen, Position::new(26, 1)),
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
                TokenType::If,
                TokenType::Ident(String::from("a")),
                TokenType::Less,
                TokenType::Integer(String::from("0")),
                TokenType::Then,
                TokenType::Minus,
                TokenType::Ident(String::from("a")),
                TokenType::Else,
                TokenType::Ident(String::from("a")),
            ]
        );
    }

    #[test]
    fn character() {
        let code = "'x'";

        assert_eq!(
            build_lexer(code).next(),
            Some(Ok(Token::new(TokenType::Char('x'), Position::new(0, 3)))),
        );
    }

    #[test]
    fn unexpected_char_eof() {
        let code = "'x";

        assert_eq!(
            build_lexer(code).next(),
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
            build_lexer(code).next(),
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
            build_lexer(code).next(),
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
            build_lexer(code).next(),
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
            build_lexer(code)
                .collect::<Vec<_>>()
                .into_iter()
                .map(|res| res.unwrap().token)
                .collect::<Vec<_>>(),
            vec![
                TokenType::Lbrack,
                TokenType::Integer(String::from("1")),
                TokenType::Comma,
                TokenType::Integer(String::from("2")),
                TokenType::Rbrack,
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
                TokenType::Ident(String::from("f")),
                TokenType::Lparen,
                TokenType::Ident(String::from("x")),
                TokenType::Comma,
                TokenType::Wildcard,
                TokenType::Rparen,
                TokenType::Assign,
                TokenType::Ident(String::from("x")),
            ],
        );
    }
}
