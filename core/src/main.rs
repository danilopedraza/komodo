use std::env::current_dir;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

use symstatic::error::error_msg;
#[cfg(feature = "repl")]
use symstatic::repl::{repl, MyCLI};
use symstatic::run::run;
use symstatic::{builtin::standard_env, env::ExecContext};

fn get_reference_path(path: &str) -> String {
    let path = Path::new(path);
    if path.is_absolute() {
        match path.parent() {
            None => String::from("/"),
            Some(parent) => parent.to_str().unwrap().to_owned(),
        }
    } else {
        let exec_dir = current_dir().unwrap().to_str().unwrap().to_owned();
        let path_parent = path.parent().unwrap().to_str().unwrap().to_owned();
        format!("{exec_dir}/{path_parent}")
    }
}

fn run_file(path: &str) -> ExitCode {
    let input_res = fs::read_to_string(path);

    match input_res {
        Ok(input) => {
            let reference_path = get_reference_path(path);
            let mut env = standard_env(ExecContext::File { reference_path });
            let res = run(&input, &mut env);
            if let Err(err) = res {
                error_msg(&err).emit(path, &input);
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Err(err) => {
            let msg = err.to_string();
            eprintln!("Error reading {path}: {msg}");
            ExitCode::FAILURE
        }
    }
}

fn run_smtc(args: &[String]) -> ExitCode {
    if args.len() == 1 {
        #[cfg(feature = "repl")]
        repl(&mut MyCLI::default());
        ExitCode::SUCCESS
    } else {
        run_file(&args[1])
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    run_smtc(&args)
}
