use std::str::Chars;

#[derive(PartialEq, Eq, Debug)]
enum Token {
    PLUS,
}

struct Lexer<'a> {
    input: Chars<'a>,
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.input.next() {
            None => None,
            Some(' ') => self.next(),
            Some('+') => Some(Token::PLUS),
            _ => todo!()
        }
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
        assert_eq!(lexer_from(" ").next(), None);
    }
}
