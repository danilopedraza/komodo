use std::env::current_dir;
use std::fs;
use std::io::{Error, ErrorKind};
use std::path::{Path, PathBuf};

use komodo::error::error_msg;
#[cfg(feature = "repl")]
use komodo::repl::{repl, MyCLI};
use komodo::run::run;
use komodo::{builtin::standard_env, env::ExecContext};

fn get_reference_path(path: PathBuf) -> std::io::Result<PathBuf> {
    if path.is_absolute() {
        match path.parent() {
            None => Ok(Path::new("/").to_owned()),
            Some(parent) => Ok(parent.to_path_buf()),
        }
    } else {
        let exec_dir = current_dir()?;
        let path_parent = path.parent().unwrap_or(Path::new(""));
        Ok(exec_dir.join(path_parent))
    }
}

fn run_file(path: &str) -> std::io::Result<()> {
    let input_res = fs::read_to_string(path);

    match input_res {
        Ok(input) => {
            let reference_path = get_reference_path(path.into())?;
            let mut env = standard_env(ExecContext::new(reference_path));
            let res = run(&input, &mut env);
            if let Err(err) = res {
                error_msg(&err).emit(path, &input);
                Err(Error::new(ErrorKind::Other, ""))
            } else {
                Ok(())
            }
        }
        Err(err) => Err(err),
    }
}

fn run_komodo(args: &[String]) -> std::io::Result<()> {
    if let Some(path) = args.get(1) {
        run_file(path)
    } else {
        #[cfg(feature = "repl")]
        repl(
            &mut MyCLI::default(),
            ExecContext::new(get_reference_path(".".into())?),
        );
        Ok(())
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    run_komodo(&args)
}
