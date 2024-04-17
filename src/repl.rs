use crate::{
    ast::ASTNode,
    builtin::standard_env,
    env::Environment,
    exec::exec,
    lexer::build_lexer,
    parser::{parser_from, ParserError},
    semantic::postprocess,
};
use rustyline::error::ReadlineError;

#[derive(Debug, PartialEq, Eq)]
pub enum ReplResponse {
    Break,
    Continue,
    Error,
    WaitForMore,
}

#[derive(Default)]
struct Repl {
    env: Environment,
    code: String,
}

impl Repl {
    fn standard_repl() -> Self {
        Self {
            env: standard_env(),
            code: String::new(),
        }
    }
}

impl Repl {
    pub fn response(&mut self, input: Result<String, ReadlineError>) -> (String, ReplResponse) {
        match input {
            Ok(line) => {
                if !self.code.is_empty() {
                    self.code.push(' ');
                }
                self.code.push_str(&line);
                let lexer = build_lexer(&self.code);
                let mut parser = parser_from(lexer.map(|res| res.unwrap().token));

                match parser.next() {
                    None => (String::from(""), ReplResponse::Continue),
                    Some(res) => self.ast_response(res),
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                (String::from(""), ReplResponse::Break)
            }
            _ => todo!(),
        }
    }

    fn ast_response(&mut self, res: Result<ASTNode, ParserError>) -> (String, ReplResponse) {
        match res {
            Ok(node) => match exec(&postprocess(node), &mut self.env) {
                Ok(obj) => {
                    self.code.clear();
                    (obj.to_string(), ReplResponse::Continue)
                }
                Err(err) => (format!("{:?}", err), ReplResponse::Error),
            },
            Err(ParserError::EOFExpecting(_)) => (String::from(""), ReplResponse::WaitForMore),
            Err(err) => (format!("{:?}", err), ReplResponse::Error),
        }
    }
}

pub trait Cli {
    fn input(&mut self, msg: &str) -> Result<String, ReadlineError>;

    fn println(&self, msg: &str);

    fn add_history_entry(&mut self, entry: &str);
}

pub fn repl<T: Cli>(interface: &mut T) -> Result<(), ()> {
    let mut wait_for_more = false;
    let mut repl = Repl::standard_repl();

    loop {
        let readline = match wait_for_more {
            false => interface.input(">>> "),
            true => interface.input("... "),
        };

        if let Ok(line) = &readline {
            interface.add_history_entry(line);
        }

        let (line, response) = repl.response(readline);

        interface.println(&line);

        wait_for_more = response == ReplResponse::WaitForMore;

        match response {
            ReplResponse::Break => break Ok(()),
            ReplResponse::Continue => continue,
            ReplResponse::Error => break Err(()),
            ReplResponse::WaitForMore => continue,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_string() {
        let input = Ok(String::from(""));
        let mut repl = Repl::default();

        assert_eq!(
            repl.response(input),
            (String::from(""), ReplResponse::Continue)
        );
    }

    #[test]
    fn integer() {
        let input = Ok(String::from("0"));
        let mut repl = Repl::default();

        assert_eq!(
            repl.response(input),
            (String::from("0"), ReplResponse::Continue)
        );
    }

    #[test]
    fn symbol() {
        let input = Ok(String::from("x"));
        let mut repl = Repl::default();

        assert_eq!(
            repl.response(input),
            (String::from("x"), ReplResponse::Continue)
        );
    }

    #[test]
    fn error() {
        let input = Ok(String::from("("));
        let mut repl = Repl::default();

        assert!(matches!(repl.response(input), (_, ReplResponse::Error)));
    }

    #[test]
    fn memory() {
        let mut repl = Repl::default();
        repl.response(Ok(String::from("let x := 1")));

        assert_eq!(
            repl.response(Ok(String::from("x"))),
            (String::from("1"), ReplResponse::Continue)
        );
    }

    #[test]
    fn wait_for_completeness() {
        let mut repl = Repl::default();

        assert_eq!(
            repl.response(Ok(String::from("if 1 + 1 = 2 then a"))),
            (String::from(""), ReplResponse::WaitForMore)
        );
    }

    #[test]
    fn eval_completed_expression() {
        let mut repl = Repl::default();

        repl.response(Ok(String::from("if 1 + 1 = 2 then a")));

        assert_eq!(
            repl.response(Ok(String::from("else b"))),
            (String::from("a"), ReplResponse::Continue),
        );
    }

    #[test]
    fn match_initial_case() {
        let mut repl = Repl::default();

        repl.response(Ok(String::from("let f(0) := 1")));

        assert_eq!(
            repl.response(Ok(String::from("f(0)"))),
            (String::from("1"), ReplResponse::Continue),
        );
    }

    #[test]
    fn match_anything() {
        let mut repl = Repl::default();

        repl.response(Ok(String::from("let f(_) := 0")));

        assert_eq!(
            repl.response(Ok(String::from("f(1) = f(2) && f(1) = 0"))),
            (String::from("true"), ReplResponse::Continue),
        );
    }

    #[test]
    fn match_singleton() {
        let mut repl = Repl::default();

        repl.response(Ok(String::from("let f([val]) := val")));

        assert_eq!(
            repl.response(Ok(String::from("f([x])"))),
            (String::from("x"), ReplResponse::Continue),
        );
    }

    #[test]
    fn match_two_args() {
        let mut repl = Repl::default();

        repl.response(Ok(String::from("let f([a], [b]) := a + b")));

        assert_eq!(
            repl.response(Ok(String::from("f([1], [2])"))),
            (String::from("3"), ReplResponse::Continue),
        );
    }

    #[test]
    fn match_pair() {
        let mut repl = Repl::default();

        repl.response(Ok(String::from("let f([a, b]) := a + b")));

        assert_eq!(
            repl.response(Ok(String::from("f([1, 2])"))),
            (String::from("3"), ReplResponse::Continue),
        );
    }

    #[test]
    fn match_prefix() {
        let mut repl = Repl::default();

        repl.response(Ok(String::from("let f([a|[b]]) := a*b")));

        assert_eq!(
            repl.response(Ok(String::from("f([3,2])"))),
            (String::from("6"), ReplResponse::Continue),
        );
    }
}
