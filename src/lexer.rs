use std::{str::Chars, iter::Peekable};

#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    ASSIGN,
    COLON,
    DOT,
    EQUALS,
    IDENT(String),
    INTEGER(i64),
    LET,
    LPAREN,
    PLUS,
    RPAREN,
    TIMES,
    UNKNOWN,
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.input.next() {
            None => None,
            Some(chr) if chr.is_whitespace() => self.whitespace(),
            Some(chr) => Some(match chr {
                '+' => Token::PLUS,
                '*' => Token::TIMES,
                '.' => Token::DOT,
                '(' => Token::LPAREN,
                ')' => Token::RPAREN,
                '=' => Token::EQUALS,
                ':' => self.assign_or_colon(),
                chr if chr.is_numeric() => self.integer(chr),
                chr if chr.is_alphabetic() => self.identifier(chr),
                _ => Token::UNKNOWN,
            }),
        }
    }
}

impl Lexer<'_> {
    fn whitespace(&mut self) -> Option<Token> {
        while let Some(_) = self.input.by_ref().next_if(|c| c.is_whitespace()) {
            // this skips all the whitespaces. Kinda obscure,
            // but better than recursion, or even worse, unwrapping and breaking several times
        }
        self.next()
    }

    fn identifier(&mut self, first: char) -> Token {
        let mut literal = String::from(first);
        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_alphabetic()) {
            literal.push(chr);
        }

        match literal.as_str() {
            "sea" => Token::LET,
            _ => Token::IDENT(literal),
        }
    }

    fn assign_or_colon(&mut self) -> Token {
        match self.input.peek() {
            Some('=') => {
                self.input.next();
                Token::ASSIGN
            },
            _ => Token::COLON,
        }
    }

    fn integer(&mut self, first: char) -> Token {
        let mut number = String::from(first);
        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_numeric()) {
            number.push(chr);
        }

        Token::INTEGER(number.parse().unwrap())
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
        assert_eq!(build_lexer("+").next(), Some(Token::PLUS));
    }

    #[test]
    fn whitespace() {
        assert_eq!(build_lexer(" \t").next(), None);
    }

    #[test]
    fn simple_expression() {
        assert_eq!(
            build_lexer("x + y").collect::<Vec<_>>(),
            vec![Token::IDENT(String::from('x')), Token::PLUS, Token::IDENT(String::from('y'))],
        );
    }

    #[test]
    fn simple_statement() {
        assert_eq!(
            build_lexer("sea x := 1.").collect::<Vec<_>>(),
            vec![
                Token::LET, Token::IDENT(String::from('x')),
                Token::ASSIGN, Token::INTEGER(1), Token::DOT
            ],
        );
    }

    #[test]
    fn complex_expression() {
        assert_eq!(
            build_lexer("(x * y) = 23").collect::<Vec<_>>(),
            vec![
                Token::LPAREN, Token::IDENT(String::from('x')),
                Token::TIMES, Token::IDENT(String::from('y')), Token::RPAREN,
                Token::EQUALS, Token::INTEGER(23)
            ],
        );
    }
}
