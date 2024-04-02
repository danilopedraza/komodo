use crate::{env::Environment, exec::exec, lexer::build_lexer, parser::parser_from};

#[derive(Default)]
pub struct Repl {
    // env: Environment,
    // code: String,
}

impl Repl {
    pub fn eval(&self, input: &str) -> String {
        let lexer = build_lexer(input);
        let mut parser = parser_from(lexer.map(|res| res.unwrap()));

        match parser.next() {
            Some(Ok(node)) => match exec(&node, &mut Environment::default()) {
                Ok(obj) => obj.to_string(),
                _ => String::from("error"),
            },
            None => String::from(""),
            _ => String::from("error"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_string() {
        let code = "";
        let repl = Repl::default();

        assert_eq!(repl.eval(code), "".to_string());
    }

    #[test]
    fn integer() {
        let code = "0";
        let repl = Repl::default();

        assert_eq!(repl.eval(code), "0".to_string());
    }

    #[test]
    fn symbol() {
        let code = "x";
        let repl = Repl::default();

        assert_eq!(repl.eval(code), "x".to_string());
    }

    #[test]
    fn error() {
        let code = "(";
        let repl = Repl::default();

        assert_eq!(repl.eval(code), "error".to_string());
    }
}
