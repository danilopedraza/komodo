use crate::{env::Environment, exec::exec, lexer::build_lexer, parser::parser_from};
use rustyline::error::ReadlineError;

#[derive(Debug, PartialEq, Eq)]
pub enum ReplResponse {
    Break,
    Continue,
    Error,
}

#[derive(Default)]
pub struct Repl {
    // env: Environment,
    // code: String,
}

impl Repl {
    pub fn eval(&self, input: Result<String, ReadlineError>) -> (String, ReplResponse) {
        match input {
            Ok(line) => {
                let lexer = build_lexer(&line);
                let mut parser = parser_from(lexer.map(|res| res.unwrap()));

                match parser.next() {
                    Some(Ok(node)) => match exec(&node, &mut Environment::default()) {
                        Ok(obj) => (obj.to_string(), ReplResponse::Continue),
                        _ => (String::from("error"), ReplResponse::Break),
                    },
                    None => (String::from(""), ReplResponse::Continue),
                    _ => (String::from("error"), ReplResponse::Error),
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                (String::from(""), ReplResponse::Break)
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_string() {
        let input = Ok(String::from(""));
        let repl = Repl::default();

        assert_eq!(repl.eval(input), (String::from(""), ReplResponse::Continue));
    }

    #[test]
    fn integer() {
        let input = Ok(String::from("0"));
        let repl = Repl::default();

        assert_eq!(
            repl.eval(input),
            (String::from("0"), ReplResponse::Continue)
        );
    }

    #[test]
    fn symbol() {
        let input = Ok(String::from("x"));
        let repl = Repl::default();

        assert_eq!(
            repl.eval(input),
            (String::from("x"), ReplResponse::Continue)
        );
    }

    #[test]
    fn error() {
        let input = Ok(String::from("("));
        let repl = Repl::default();

        assert_eq!(
            repl.eval(input),
            (String::from("error"), ReplResponse::Error)
        );
    }
}
