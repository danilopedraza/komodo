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
        self.input.next().map(|_| Token::PLUS)
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
}
