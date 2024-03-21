use crate::{
    env::Environment,
    object::{Bool, Effect, Object},
};

fn smtc_println(args: &[Object]) -> Object {
    let str = args[0].to_string();
    println!("{str}");
    Object::Boolean(Bool::from(true))
}

pub fn standard_env() -> Environment {
    let mut env = Environment::default();

    env.set(
        "println",
        crate::object::Object::Function(crate::object::Function::Effect(Effect {
            func: smtc_println,
        })),
    );

    env
}
