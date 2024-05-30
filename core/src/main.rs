use std::fs;

use symstatic::error::error_msg;
use symstatic::repl::{repl, MyCLI};
use symstatic::run::run;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        repl(&mut MyCLI::default());
    } else {
        let input = fs::read_to_string(&args[1]).unwrap();
        let res = run(&input);
        if let Err(err) = res {
            error_msg(&err).emit(&args[1], &input);
        }
    }
}
