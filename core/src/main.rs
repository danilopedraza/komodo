use std::fs;
use std::process::ExitCode;

use symstatic::error::error_msg;
use symstatic::repl::{repl, MyCLI};
use symstatic::run::run;

fn run_smtc(args: &[String]) -> ExitCode {
    if args.len() == 1 {
        repl(&mut MyCLI::default());
        ExitCode::SUCCESS
    } else {
        let input = fs::read_to_string(&args[1]).unwrap();
        let res = run(&input);
        if let Err(err) = res {
            error_msg(&err).emit(&args[1], &input);
            ExitCode::FAILURE
        } else {
            ExitCode::SUCCESS
        }
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    run_smtc(&args)
}
