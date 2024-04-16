mod ast;
mod builtin;
mod env;
mod exec;
mod file;
mod lexer;
mod matcher;
mod object;
mod parser;
mod repl;
mod semantic;

use builtin::standard_env;
use exec::exec;
use file::parse_file;
use repl::{repl, IOInterface};
use rustyline::DefaultEditor;
use semantic::postprocess;

struct MyIOInterface {
    rl: DefaultEditor,
}

impl IOInterface for MyIOInterface {
    fn input(&mut self, msg: &str) -> Result<String, rustyline::error::ReadlineError> {
        self.rl.readline(msg)
    }

    fn println(&self, msg: &str) {
        println!("{msg}")
    }

    fn add_history_entry(&mut self, entry: &str) {
        let _ = self.rl.add_history_entry(entry);
    }
}

fn main() -> Result<(), ()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        repl(&mut MyIOInterface {
            rl: DefaultEditor::new().unwrap(),
        })?;
    } else {
        let nodes = parse_file(&args[1]);
        for node in nodes {
            exec(&postprocess(node), &mut standard_env()).unwrap();
        }
    }

    Ok(())
}
