use crate::{
    ast::ASTNode,
    builtin::standard_env,
    env::Environment,
    error::{error_msg, Error, ErrorMessage, ErrorType},
    lexer::build_lexer,
    new_weeder::rewrite,
    object::Object,
    parser::{parser_from, ParserError},
    run,
};
use rustyline::{error::ReadlineError, DefaultEditor};

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
                let mut parser = parser_from(lexer);

                match parser.next() {
                    None => (String::from(""), ReplResponse::Continue),
                    Some(res) => self.exec_response(res.and_then(rewrite)),
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                (String::from(""), ReplResponse::Break)
            }
            _ => todo!(),
        }
    }

    fn exec_response(&mut self, res: Result<ASTNode, Error>) -> (String, ReplResponse) {
        match self.exec_result(res) {
            Ok(obj) => {
                self.code.clear();
                (obj.to_string(), ReplResponse::Continue)
            }
            Err(Error(ErrorType::Parser(ParserError::EOFExpecting(_)), _)) => {
                (String::from(""), ReplResponse::WaitForMore)
            }
            Err(err) => {
                self.code.clear();
                let ErrorMessage(msg, _) = error_msg(&err);
                (msg, ReplResponse::Error)
            }
        }
    }

    fn exec_result(&mut self, node_res: Result<ASTNode, Error>) -> Result<Object, Error> {
        run::run_node(node_res?, &mut self.env)
    }
}

pub trait Cli {
    fn input(&mut self, msg: &str) -> Result<String, ReadlineError>;

    fn println(&mut self, msg: &str);

    fn add_history_entry(&mut self, entry: &str);
}

pub struct MyCLI {
    rl: DefaultEditor,
}

impl Default for MyCLI {
    fn default() -> Self {
        Self {
            rl: DefaultEditor::new().unwrap(),
        }
    }
}

impl Cli for MyCLI {
    fn input(&mut self, msg: &str) -> Result<String, rustyline::error::ReadlineError> {
        self.rl.readline(msg)
    }

    fn println(&mut self, msg: &str) {
        println!("{msg}")
    }

    fn add_history_entry(&mut self, entry: &str) {
        let _ = self.rl.add_history_entry(entry);
    }
}

pub fn repl<T: Cli>(interface: &mut T) {
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

        if !line.is_empty() {
            interface.println(&line);
        }

        wait_for_more = response == ReplResponse::WaitForMore;

        match response {
            ReplResponse::Break => break,
            ReplResponse::Continue => continue,
            ReplResponse::Error => continue,
            ReplResponse::WaitForMore => continue,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct CliMock {
        input_index: usize,
        inputs: Vec<Result<String, ReadlineError>>,
        lines_printed: Vec<String>,
        prompt_prefixes: Vec<String>,
        history: Vec<String>,
    }

    impl CliMock {
        fn _new(inputs: Vec<Result<String, ReadlineError>>) -> Self {
            Self {
                input_index: 0,
                inputs,
                lines_printed: vec![],
                prompt_prefixes: vec![],
                history: vec![],
            }
        }
    }

    impl Cli for CliMock {
        fn input(&mut self, msg: &str) -> Result<String, ReadlineError> {
            self.prompt_prefixes.push(msg.to_owned());
            let res = &self.inputs[self.input_index];
            self.input_index += 1;

            // I'm just cloning here
            match res {
                Err(ReadlineError::Interrupted) => Err(ReadlineError::Interrupted),
                Ok(str) => Ok(str.to_owned()),
                _ => unimplemented!(),
            }
        }

        fn println(&mut self, msg: &str) {
            self.lines_printed.push(msg.to_owned());
        }

        fn add_history_entry(&mut self, entry: &str) {
            self.history.push(entry.to_owned());
        }
    }

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
    fn language_error_communicated() {
        let mut repl = Repl::standard_repl();

        assert!(matches!(
            repl.response(Ok(")".into())),
            (_, ReplResponse::Error),
        ));
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

    #[test]
    fn empty_response() {
        let mut repl = Repl::standard_repl();

        assert_eq!(
            repl.response(Err(ReadlineError::Interrupted)),
            (String::from(""), ReplResponse::Break)
        );
    }

    #[test]
    fn not_printing_empty_response() {
        let mut cli = CliMock::_new(vec![Ok("".into()), Err(ReadlineError::Interrupted)]);
        repl(&mut cli);

        assert!(cli.lines_printed.is_empty());
    }

    #[test]
    fn continue_after_error() {
        let mut cli = CliMock::_new(vec![Ok(")".into()), Err(ReadlineError::Interrupted)]);
        repl(&mut cli);

        let consumed_inputs = cli.input_index;

        assert_eq!(consumed_inputs, 2);
    }

    #[test]
    fn clear_autocomplete_after_error() {
        let mut repl = Repl::standard_repl();
        repl.response(Ok("1 + )".into()));
        assert!(repl.code.is_empty());
    }
}
