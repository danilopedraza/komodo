mod utils;

use symstatic::{env::Environment, error::error_msg, run::run};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn run_code(source: &str) -> String {
    if let Err(err) = run(source, &mut Environment::default()) {
        String::from_utf8(error_msg(&err).as_bytes("source.smtc", source)).unwrap()
    } else {
        String::from("")
    }
}

#[cfg(test)]
mod tests {
    use crate::run_code;

    #[test]
    fn correct_code() {
        assert_eq!(run_code("2 + 2"), String::from(""));
    }
}
