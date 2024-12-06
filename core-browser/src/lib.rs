mod builtin;

use builtin::{standard_env, STDIN, STDOUT};
use komodo::run::run;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run_code(source: &str, stdin: &str) -> String {
    STDIN.lock().unwrap().clear();
    STDOUT.lock().unwrap().clear();

    let mut guard = STDIN.lock().unwrap();

    for line in stdin.lines().rev() {
        guard.push(line.to_string());
    }

    std::mem::drop(guard);

    let mut env = standard_env();

    let run_res = run(source, &mut env);
    let mut res = STDOUT.lock().unwrap().clone();
    if let Err(err) = run_res {
        res.push_str(&String::from_utf8(err.as_bytes()).unwrap_or_default());
    }

    res
}

#[cfg(test)]
mod tests {
    use crate::run_code;

    #[test]
    fn correct_code() {
        assert_eq!(run_code("2 + 2", ""), String::from(""));
    }

    #[test]
    fn println() {
        assert_eq!(run_code("println(\"hello\")", ""), "hello\n");
    }

    #[test]
    fn getln() {
        assert_eq!(run_code("println(getln())", "hello\n"), "hello\n");
    }
}
