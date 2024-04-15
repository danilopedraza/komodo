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
use repl::{Repl, ReplResponse};
use semantic::postprocess;

use rustyline::DefaultEditor;

fn repl() -> Result<(), ()> {
    let mut rl = DefaultEditor::new().unwrap();
    let mut wait_for_more = false;
    let mut repl = Repl::default();

    loop {
        let readline = match wait_for_more {
            false => rl.readline(">>> "),
            true => rl.readline("... "),
        };

        if let Ok(line) = &readline {
            let _ = rl.add_history_entry(line);
        }

        let (line, response) = repl.response(readline);

        println!("{line}");

        wait_for_more = response == ReplResponse::WaitForMore;

        match response {
            ReplResponse::Break => break Ok(()),
            ReplResponse::Continue => continue,
            ReplResponse::Error => break Err(()),
            ReplResponse::WaitForMore => continue,
        }
    }
}

fn main() -> Result<(), ()> {
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
