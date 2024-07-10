mod utils;

use std::sync::Mutex;

use symstatic::{
    builtin::smtc_assert,
    env::Environment,
    error::error_msg,
    object::{Effect, Function, Object},
    run::run,
};
use wasm_bindgen::prelude::*;

static STDOUT: Mutex<String> = Mutex::new(String::new());
static STDIN: Mutex<Vec<String>> = Mutex::new(vec![]);

#[wasm_bindgen]
pub fn run_code(source: &str, stdin: &str) -> String {
    STDIN.lock().unwrap().clear();
    STDOUT.lock().unwrap().clear();

    let mut guard = STDIN.lock().unwrap();

    for line in stdin.lines().rev() {
        guard.push(line.to_string());
    }

    std::mem::drop(guard);

    fn smtc_println(args: &[Object]) -> Object {
        let mut guard = STDOUT.lock().unwrap();
        guard.push_str(&args[0].to_string());
        guard.push('\n');

        Object::empty_tuple()
    }

    fn smtc_getln(_args: &[Object]) -> Object {
        let mut guard = STDIN.lock().unwrap();

        let res = guard.pop().unwrap_or_default();

        Object::String(res.into())
    }

    let mut env = Environment::default();
    env.set(
        "println",
        Object::Function(Function::Effect(Effect::new(smtc_println, 1))),
    );
    env.set(
        "getln",
        Object::Function(Function::Effect(Effect::new(smtc_getln, 0))),
    );

    env.set(
        "assert",
        Object::Function(Function::Effect(Effect::new(smtc_assert, 1))),
    );

    let run_res = run(source, &mut env);
    let mut res = STDOUT.lock().unwrap().clone();
    if let Err(err) = run_res {
        res.push_str(&String::from_utf8(error_msg(&err).as_bytes("source.smtc", source)).unwrap());
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
