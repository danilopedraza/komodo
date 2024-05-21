mod ast;
mod builtin;
mod env;
mod error;
mod exec;
mod lexer;
mod matcher;
mod object;
mod parser;
mod repl;
mod run;
mod weeder;

use std::fs;

use error::{error_msg, ErrorMessage};
use exec::exec;
use repl::{repl, Cli};
use rustyline::DefaultEditor;

struct MyCLI {
    rl: DefaultEditor,
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

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        repl(&mut MyCLI {
            rl: DefaultEditor::new().unwrap(),
        })
        .unwrap();
    } else {
        let input = fs::read_to_string(&args[1]).unwrap();
        let res = run::run(&input);
        if let Err(err) = res {
            let ErrorMessage(msg, _) = error_msg(&err);
            println!("{msg}");
        }
    }
}
