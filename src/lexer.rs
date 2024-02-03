use std::{str::Chars, iter::Peekable};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Arrow,
    Assign,
    Colon,
    Comma,
    Dot,
    Equals,
    False,
    Ident(String),
    Integer(String),
    Lbrace,
    Let,
    Lparen,
    Minus,
    Percent,
    Plus,
    Rbrace,
    Rparen,
    Slash,
    Star,
    StarStar,
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
            },
            Some(chr) => Some(match chr {
                ',' => Token::Comma,
                '+' => Token::Plus,
                '*' => self.stars(),
                '.' => Token::Dot,
                '{' => Token::Lbrace,
                '(' => Token::Lparen,
                '%' => Token::Percent,
                '}' => Token::Rbrace,
                ')' => Token::Rparen,
                '/' => Token::Slash,
                '=' => Token::Equals,
                '-' => self.minus_or_arrow(),
                ':' => self.assign_or_colon(),
                chr if chr.is_numeric() => self.integer(chr),
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

    fn assign_or_colon(&mut self) -> Token {
        match self.input.peek() {
            Some('=') => {
                self.input.next();
                Token::Assign
            },
            _ => Token::Colon,
        }
    }

    fn stars(&mut self) -> Token {
        match self.input.peek() {
            Some('*') => {
                self.input.next();
                Token::StarStar
            },
            _ => Token::Star,
        }
    }

    fn integer(&mut self, first: char) -> Token {
        let mut number = String::from(first);
        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_ascii_digit()) {
            number.push(chr);
        }

        Token::Integer(number)
    }

    fn minus_or_arrow(&mut self) -> Token {
        match self.input.peek() {
            Some('>') => {
                self.input.next();
                Token::Arrow
            }
            _ => Token::Minus,
        }
    }
}

pub fn build_lexer(input: &str) -> Lexer {
    Lexer { input: input.chars().peekable() }
}

#[cfg(test)]
mod tests {
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
            build_lexer("x + y").collect::<Vec<_>>(),
            vec![Token::Ident(String::from('x')), Token::Plus, Token::Ident(String::from('y'))],
        );
    }

    #[test]
    fn simple_statement() {
        assert_eq!(
            build_lexer("let x := 1 / 2.").collect::<Vec<_>>(),
            vec![
                Token::Let, Token::Ident(String::from('x')),
                Token::Assign, Token::Integer(String::from('1')),
                Token::Slash, Token::Integer(String::from('2')), Token::Dot
            ],
        );
    }

    #[test]
    fn complex_expression() {
        assert_eq!(
            build_lexer("(x * y) = 23 % 2").collect::<Vec<_>>(),
            vec![
                Token::Lparen, Token::Ident(String::from('x')),
                Token::Star, Token::Ident(String::from('y')), Token::Rparen,
                Token::Equals, Token::Integer(String::from("23")),
                Token::Percent, Token::Integer(String::from('2')),
            ],
        );
    }

    #[test]
    fn function_declaration() {
        assert_eq!(
            build_lexer("let f: a -> a").collect::<Vec<_>>(),
            vec![Token::Let, Token::Ident(String::from('f')), Token::Colon, Token::Ident(String::from('a')), Token::Arrow, Token::Ident(String::from('a'))],
        );
    }

    #[test]
    fn function_call() {
        assert_eq!(
            build_lexer("f(x,y ** 2)").collect::<Vec<_>>(),
            vec![
                Token::Ident(String::from('f')),
                Token::Lparen, Token::Ident(String::from('x')), Token::Comma,
                Token::Ident(String::from('y')), Token::StarStar, Token::Integer(String::from('2')),
                Token::Rparen
            ],
        );
    }

    #[test]
    fn set() {
        assert_eq!(
            build_lexer("{true, false}").collect::<Vec<_>>(),
            vec![Token::Lbrace, Token::True, Token::Comma, Token::False, Token::Rbrace],
        );
    }
}
