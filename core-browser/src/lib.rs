mod utils;

use symstatic::{env::Environment, error::error_msg, run::run};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn run_code(source: &str) -> Option<String> {
    if let Err(err) = run(source, &mut Environment::default()) {
        Some(String::from_utf8(error_msg(&err).as_bytes("foo", source)).unwrap())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::run_code;

    #[test]
    fn correct_code() {
        assert!(run_code("2 + 2").is_none());
    }

    #[test]
    fn incorrect_code() {
        assert!(run_code("3 + (").is_some());
    }
}
