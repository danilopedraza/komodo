use std::{iter::Peekable, str::Chars, vec};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedChar(char),
    UnexpectedEOF,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Arrow,
    Assign,
    Bang,
    BitwiseAnd,
    BitwiseOr,
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
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.input.next() {
            None => None,
            Some(chr) if chr.is_whitespace() => {
                self.skip_whitespace();
                self.next()
            }
            Some('#') => {
                self.skip_comment();
                self.next()
            }
            Some('\'') => self.char(),
            Some('"') => Some(self.string_()),
            Some(chr) => Some(Ok(match chr {
                '!' => Token::Bang,
                '^' => Token::BitwiseXor,
                ',' => Token::Comma,
                '.' => Token::Dot,
                '=' => Token::Equals,
                '&' => self.fork(Token::BitwiseAnd, vec![('&', Token::LogicAnd)]),
                '|' => self.fork(Token::BitwiseOr, vec![('|', Token::LogicOr)]),
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
                '*' => self.stars(),
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
}

impl Lexer<'_> {
    fn string_(&mut self) -> Result<Token, LexerError> {
        let mut str = String::new();

        loop {
            match self.input.next() {
                Some('"') => break Ok(Token::String(str)),
                Some(c) => str.push(c),
                None => break Err(LexerError::UnexpectedEOF),
            }
        }
    }

    fn char(&mut self) -> Option<Result<Token, LexerError>> {
        let chr = self.input.next();
        if chr.is_none() {
            return Some(Err(LexerError::UnexpectedEOF));
        }

        Some(match self.input.next() {
            Some('\'') => Ok(Token::Char(chr.unwrap())),
            Some(c) => Err(LexerError::UnexpectedChar(c)),
            None => Err(LexerError::UnexpectedEOF),
        })
    }

    fn skip_comment(&mut self) {
        for chr in self.input.by_ref() {
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
                self.input.next();
            } else {
                break;
            }
        }
    }

    fn identifier_or_keyword(&mut self, first: char) -> Token {
        let mut literal = String::from(first);
        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_alphabetic()) {
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

    fn stars(&mut self) -> Token {
        match self.input.peek() {
            Some('*') => {
                self.input.next();
                Token::ToThe
            }
            _ => Token::Times,
        }
    }

    fn fork(&mut self, def: Token, alts: Vec<(char, Token)>) -> Token {
        for (chr, tok) in alts {
            if matches!(self.input.peek(), Some(c) if *c == chr) {
                self.input.next();
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
            number.push(chr);
        }

        Token::Integer(number)
    }
}

pub fn build_lexer(input: &str) -> Lexer {
    Lexer {
        input: input.chars().peekable(),
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
        assert_eq!(build_lexer("+").next(), Some(Ok(Token::Plus)));
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
                .iter()
                .map(|res| res.clone().unwrap())
                .collect::<Vec<_>>(),
            vec![
                Token::Ident(String::from('x')),
                Token::Plus,
                Token::Ident(String::from('y')),
                Token::NotEqual,
                Token::Ident(String::from('a'))
            ],
        );
    }

    #[test]
    fn simple_statement() {
        assert_eq!(
            build_lexer("let x := 1 / 2.")
                .collect::<Vec<_>>()
                .iter()
                .map(|res| res.clone().unwrap())
                .collect::<Vec<_>>(),
            vec![
                Token::Let,
                Token::Ident(String::from('x')),
                Token::Assign,
                Token::Integer(String::from('1')),
                Token::Over,
                Token::Integer(String::from('2')),
                Token::Dot
            ],
        );
    }

    #[test]
    fn complex_expression() {
        assert_eq!(
            build_lexer("(x * y) = 23 % 2")
                .collect::<Vec<_>>()
                .iter()
                .map(|res| res.clone().unwrap())
                .collect::<Vec<_>>(),
            vec![
                Token::Lparen,
                Token::Ident(String::from('x')),
                Token::Times,
                Token::Ident(String::from('y')),
                Token::Rparen,
                Token::Equals,
                Token::Integer(String::from("23")),
                Token::Mod,
                Token::Integer(String::from('2')),
            ],
        );
    }

    #[test]
    fn leq_comparisons() {
        assert_eq!(
            build_lexer("a < b <= c")
                .collect::<Vec<_>>()
                .iter()
                .map(|res| res.clone().unwrap())
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
                .iter()
                .map(|res| res.clone().unwrap())
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
                .iter()
                .map(|res| res.clone().unwrap())
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
                .iter()
                .map(|res| res.clone().unwrap())
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
                .iter()
                .map(|res| res.clone().unwrap())
                .collect::<Vec<_>>(),
            vec![
                Token::Ident(String::from('f')),
                Token::Lparen,
                Token::Ident(String::from('x')),
                Token::Comma,
                Token::Ident(String::from('y')),
                Token::ToThe,
                Token::Integer(String::from('2')),
                Token::Rparen
            ],
        );
    }

    #[test]
    fn set() {
        assert_eq!(
            build_lexer("{true, false}")
                .collect::<Vec<_>>()
                .iter()
                .map(|res| res.clone().unwrap())
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
                .iter()
                .map(|res| res.clone().unwrap())
                .collect::<Vec<_>>(),
            vec![
                Token::Integer(String::from('0')),
                Token::Integer(String::from('1'))
            ]
        );
    }

    #[test]
    fn comment() {
        let code = "input() # get input
        print() # print";

        assert_eq!(
            build_lexer(code)
                .collect::<Vec<_>>()
                .iter()
                .map(|res| res.clone().unwrap())
                .collect::<Vec<_>>(),
            vec![
                Token::Ident(String::from("input")),
                Token::Lparen,
                Token::Rparen,
                Token::Ident(String::from("print")),
                Token::Lparen,
                Token::Rparen,
            ],
        );
    }

    #[test]
    fn if_expr() {
        let code = "if a < 0 then -a else a";

        assert_eq!(
            build_lexer(code)
                .collect::<Vec<_>>()
                .iter()
                .map(|res| res.clone().unwrap())
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

        assert_eq!(build_lexer(code).next(), Some(Ok(Token::Char('x'))),);
    }

    #[test]
    fn unexpected_char_eof() {
        let code = "'x";

        assert_eq!(
            build_lexer(code).next(),
            Some(Err(LexerError::UnexpectedEOF)),
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
            Some(Ok(Token::String(String::from("abc")))),
        );
    }

    #[test]
    fn list() {
        let code = "[1, 2]";

        assert_eq!(
            build_lexer(code)
                .collect::<Vec<_>>()
                .iter()
                .map(|res| res.clone().unwrap())
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
                .iter()
                .map(|res| res.clone().unwrap())
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
