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

use std::io::{stdin, stdout, Write};

fn repl() -> std::io::Result<()> {
    let handle = stdin();
    loop {
        let mut line = String::new();
        print!(">>> ");
        stdout().flush().unwrap();
        handle.read_line(&mut line)?;
        let res = Repl::default().eval(&line);
        println!("{res}");
    }
}

fn main() -> std::io::Result<()> {
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
