use std::str::Chars;

#[derive(PartialEq, Eq, Debug)]
enum Token {
    EOF,
    PLUS,
}

struct Lexer<'a> {
    input: Chars<'a>,
}

impl Lexer<'_> {
    fn next_token(mut self) -> Token {
        match self.input.next() {
            None => Token::EOF,
            Some(_) => Token::PLUS,
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
        assert_eq!(Lexer::next_token(lexer_from("")), Token::EOF);
    }

    #[test]
    fn plus_operator() {
        assert_eq!(Lexer::next_token(lexer_from("+")), Token::PLUS);
    }
}
