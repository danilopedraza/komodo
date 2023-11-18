use std::{str::Chars, iter::Peekable};

#[derive(PartialEq, Eq, Debug)]
enum Token {
    ASSIGN,
    COLON,
    DOT,
    EQUALS,
    LET,
    LPAREN,
    PLUS,
    RPAREN,
    IDENT(String),
    INTEGER(i64),
}

struct Lexer<'a> {
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
                '.' => Token::DOT,
                '(' => Token::LPAREN,
                ')' => Token::RPAREN,
                '=' => Token::EQUALS,
                ':' => self.assign_or_colon(),
                '0'..='9' => self.integer(chr),
                _ => self.identifier(chr),
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

#[cfg(test)]
mod tests {
    use super::*;
    fn lexer_from(string: &str) -> Lexer {
        Lexer { input: string.chars().peekable() }
    }

    #[test]
    fn empty_string() {
        assert!(lexer_from("").next().is_none());
    }

    #[test]
    fn plus_operator() {
        assert_eq!(lexer_from("+").next(), Some(Token::PLUS));
    }

    #[test]
    fn whitespace() {
        assert_eq!(lexer_from(" \t").next(), None);
    }

    #[test]
    fn simple_expression() {
        assert_eq!(
            lexer_from("x + y").collect::<Vec<_>>(),
            vec![Token::IDENT(String::from('x')), Token::PLUS, Token::IDENT(String::from('y'))],
        );
    }

    #[test]
    fn simple_statement() {
        assert_eq!(
            lexer_from("sea x := 1.").collect::<Vec<_>>(),
            vec![
                Token::LET, Token::IDENT(String::from('x')),
                Token::ASSIGN, Token::INTEGER(1), Token::DOT
            ],
        );
    }
    #[test]
    fn complex_expression() {
        assert_eq!(
            lexer_from("(x + y) = 23").collect::<Vec<_>>(),
            vec![
                Token::LPAREN, Token::IDENT(String::from('x')),
                Token::PLUS, Token::IDENT(String::from('y')), Token::RPAREN,
                Token::EQUALS, Token::INTEGER(23)
            ],
        );
    }
}
