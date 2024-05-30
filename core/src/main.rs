use std::fs;

use rustyline::DefaultEditor;
use symstatic::error::error_msg;
use symstatic::repl::{repl, Cli};
use symstatic::run::run;

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
        let res = run(&input);
        if let Err(err) = res {
            error_msg(&err).emit(&args[1], &input);
        }
    }
}
