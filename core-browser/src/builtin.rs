use std::{
    path::{Path, PathBuf},
    sync::Mutex,
};

use komodo::{
    builtin::komodo_assert,
    env::{Environment, ExecContext},
    object::{ExternFunction, Function, Object},
};

pub static STDOUT: Mutex<String> = Mutex::new(String::new());
pub static STDIN: Mutex<Vec<String>> = Mutex::new(vec![]);

pub fn standard_env() -> Environment {
    fn komodo_println(arg: &Object) -> Object {
        let mut guard = STDOUT.lock().unwrap();
        guard.push_str(&arg.to_string());
        guard.push('\n');

        Object::empty_tuple()
    }

    fn komodo_getln(_arg: &Object) -> Object {
        let mut guard = STDIN.lock().unwrap();

        let res = guard.pop().unwrap_or_default();

        Object::String(res.into())
    }

    let assets = vec![
        (
            "println",
            Object::Function(Function::Extern(ExternFunction::new(
                komodo_println,
                "println".into(),
            ))),
        ),
        (
            "getln",
            Object::Function(Function::Extern(ExternFunction::new(
                komodo_getln,
                "getln".into(),
            ))),
        ),
        (
            "assert",
            Object::Function(Function::Extern(ExternFunction::new(
                komodo_assert,
                "assert".into(),
            ))),
        ),
    ];

    let mut env = Environment::new(ExecContext::new(
        Path::new("source.komodo").to_path_buf(),
        PathBuf::default(),
    ));
    for (name, obj) in assets {
        env.set_inmutable(name, obj);
    }

    env
}
