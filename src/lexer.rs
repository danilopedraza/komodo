use std::{iter::Peekable, str::Chars, vec};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Arrow,
    Assign,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Colon,
    Comma,
    Dot,
    Equals,
    False,
    Greater,
    GreaterEqual,
    Ident(String),
    Integer(String),
    Lbrace,
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
    Plus,
    Rbrace,
    RightShift,
    Rparen,
    Over,
    Times,
    ToThe,
    True,
    Unknown,
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.input.next() {
            None => None,
            Some(chr) if chr.is_whitespace() => {
                self.skip_whitespace();
                self.next()
            }
            Some(chr) => Some(match chr {
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
                '(' => Token::Lparen,
                '%' => Token::Mod,
                '+' => Token::Plus,
                '*' => self.stars(),
                '}' => Token::Rbrace,
                ')' => Token::Rparen,
                '0'..='9' => self.integer(chr),
                chr if chr.is_alphabetic() => self.identifier_or_keyword(chr),
                _ => Token::Unknown,
            }),
        }
    }
}

impl Lexer<'_> {
    fn skip_whitespace(&mut self) {
        while self.input.by_ref().next_if(|c| c.is_whitespace()).is_some() {
            // this skips all the whitespaces. Kinda obscure,
            // but better than recursion, or even worse, unwrapping and breaking several times
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
            "let" => Some(Token::Let),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
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
        assert_eq!(build_lexer("+").next(), Some(Token::Plus));
    }

    #[test]
    fn whitespace() {
        assert_eq!(build_lexer(" \t").next(), None);
    }

    #[test]
    fn simple_expression() {
        assert_eq!(
            build_lexer("x + y /= a").collect::<Vec<_>>(),
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
            build_lexer("let x := 1 / 2.").collect::<Vec<_>>(),
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
            build_lexer("(x * y) = 23 % 2").collect::<Vec<_>>(),
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
            build_lexer("a < b <= c").collect::<Vec<_>>(),
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
            build_lexer("a > b >= c").collect::<Vec<_>>(),
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
            build_lexer("let f: a -> a").collect::<Vec<_>>(),
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
            build_lexer("(1 << 2) >> 2").collect::<Vec<_>>(),
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
            build_lexer("f(x,y ** 2)").collect::<Vec<_>>(),
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
            build_lexer("{true, false}").collect::<Vec<_>>(),
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
            build_lexer("01").collect::<Vec<_>>(),
            vec![
                Token::Integer(String::from('0')),
                Token::Integer(String::from('1'))
            ]
        );
    }
}
