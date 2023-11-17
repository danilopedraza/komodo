use std::str::Chars;

#[derive(PartialEq, Eq, Debug)]
enum Token {
    PLUS,
    IDENT(char),
}

struct Lexer<'a> {
    input: Chars<'a>,
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.input.next() {
            None => None,
            Some(' ') | Some('\t') => self.whitespace(),
            Some(chr) => Some(match chr {
                '+' => self.plus(),
                _ => self.identifier(chr),
            }),
        }
    }
}

impl Lexer<'_> {
    fn whitespace(&mut self) -> Option<Token> {
        self.next()
    }

    fn plus(&self) -> Token {
        Token::PLUS
    }

    fn identifier(&self, chr: char) -> Token {
        Token::IDENT(chr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn lexer_from(string: &str) -> Lexer {
        Lexer { input: string.chars() }
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
            vec![Token::IDENT('x'), Token::PLUS, Token::IDENT('y')],
        );
    }
}
