mod lexer;
mod parser;
// mod semantic;
mod eval;

use eval::{eval, to_string};
use lexer::build_lexer;
use parser::parser_from;

use std::io::{stdin, stdout, BufRead, Write};

fn eval_line(line: &str) -> String {
    let lexer = build_lexer(line);
    let mut parser = parser_from(lexer);
    match parser.next() {
        None => String::from(""),
        Some(Ok(node)) => to_string(&eval(&node)),
        Some(Err(_)) => String::from("error"),
    }
}

fn main() -> std::io::Result<()> {
    let mut handle = stdin().lock();
    loop {
        let mut line = String::new();
        print!(">>> ");
        stdout().flush().unwrap();
        handle.read_line(&mut line)?;
        let res = eval_line(&line);
        println!("{res}");
    }
}
