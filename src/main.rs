mod ast;
mod builtin;
mod env;
mod exec;
mod file;
mod lexer;
mod object;
mod parser;
mod repl;
mod semantic;

use builtin::standard_env;
use exec::exec;
use file::parse_file;
use repl::Repl;
use semantic::postprocess;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline(">>> ");

        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);
                let res = Repl::default().eval(&line);
                println!("{res}");
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break Ok(()),
            _ => todo!(),
        }
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        repl()?;
    } else {
        let nodes = parse_file(&args[1]);
        for node in nodes {
            exec(&postprocess(node), &mut standard_env()).unwrap();
        }
    }

    Ok(())
}
