use std::{str::Chars, iter::Peekable};

#[derive(PartialEq, Eq, Debug)]
enum Token {
    ASSIGN,
    COLON,
    DOT,
    LET,
    PLUS,
    IDENT(String),
    INTEGER(i64),
}

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.input.peek() {
            None => None,
            Some(' ') | Some('\t') => self.whitespace(),
            Some(chr) => Some(match chr {
                '+' => self.plus(),
                ':' => self.assign_or_colon(),
                '.' => self.dot(),
                '0'..='9' => self.integer(),
                _ => self.identifier(),
            }),
        }
    }
}

impl Lexer<'_> {
    fn whitespace(&mut self) -> Option<Token> {
        self.input.next();
        self.next()
    }

    fn plus(&mut self) -> Token {
        self.input.next();
        Token::PLUS
    }

    fn identifier(&mut self) -> Token {
        let literal = self.input
            .by_ref()
            .take_while(|chr| chr.is_alphabetic())
            .collect::<String>();

        match literal.as_str() {
            "sea" => Token::LET,
            _ => Token::IDENT(literal),
        }
    }

    fn assign_or_colon(&mut self) -> Token {
        self.input.next();
        match self.input.peek() {
            Some('=') => {
                self.input.next();
                Token::ASSIGN
            },
            _ => Token::COLON,
        }
    }

    fn dot(&mut self) -> Token {
        self.input.next();
        Token::DOT
    }

    fn integer(&mut self) -> Token {
        Token::INTEGER(self.input.next().unwrap().to_digit(10).unwrap() as i64)
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
}
