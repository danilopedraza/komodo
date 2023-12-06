use std::{str::Chars, iter::Peekable};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Arrow,
    Assign,
    Colon,
    Dot,
    Equals,
    Ident(String),
    Integer(i64),
    Let,
    Lparen,
    Minus,
    Plus,
    Rparen,
    Times,
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
            Some(chr) if chr.is_whitespace() => self.whitespace(),
            Some(chr) => Some(match chr {
                '+' => Token::Plus,
                '*' => Token::Times,
                '.' => Token::Dot,
                '(' => Token::Lparen,
                ')' => Token::Rparen,
                '=' => Token::Equals,
                '-' => self.minus_or_arrow(),
                ':' => self.assign_or_colon(),
                chr if chr.is_numeric() => self.integer(chr),
                chr if chr.is_alphabetic() => self.identifier(chr),
                _ => Token::Unknown,
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
            "sea" => Token::Let,
            _ => Token::Ident(literal),
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

    fn integer(&mut self, first: char) -> Token {
        let mut number = String::from(first);
        while let Some(chr) = self.input.by_ref().next_if(|c| c.is_numeric()) {
            number.push(chr);
        }

        Token::Integer(number.parse().unwrap())
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


#[cfg(test)]
mod tests {
    use super::*;
    
    pub fn build_lexer(input: &str) -> Lexer {
        Lexer { input: input.chars().peekable() }
    }

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
            build_lexer("sea x := 1.").collect::<Vec<_>>(),
            vec![
                Token::Let, Token::Ident(String::from('x')),
                Token::Assign, Token::Integer(1), Token::Dot
            ],
        );
    }

    #[test]
    fn complex_expression() {
        assert_eq!(
            build_lexer("(x * y) = 23").collect::<Vec<_>>(),
            vec![
                Token::Lparen, Token::Ident(String::from('x')),
                Token::Times, Token::Ident(String::from('y')), Token::Rparen,
                Token::Equals, Token::Integer(23)
            ],
        );
    }

    #[test]
    fn function_declaration() {
        assert_eq!(
            build_lexer("sea f: a -> a").collect::<Vec<_>>(),
            vec![Token::Let, Token::Ident(String::from('f')), Token::Colon, Token::Ident(String::from('a')), Token::Arrow, Token::Ident(String::from('a'))],
        );
    }
}
