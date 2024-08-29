use std::{collections::BTreeMap, path::PathBuf};

use crate::object::Object;

#[derive(Debug, PartialEq, Eq)]
pub enum EnvResponse<'a> {
    Mutable(&'a mut Object),
    Inmutable(&'a Object),
    NotFound,
}

#[derive(Debug)]
enum ValueKind {
    Inmutable,
    Mutable,
}

#[derive(Debug, Default)]
pub struct Scope {
    dict: BTreeMap<String, (ValueKind, Object)>,
}

impl Scope {
    fn get(&mut self, name: &str) -> EnvResponse {
        match self.dict.get_mut(name) {
            Some((ValueKind::Inmutable, value)) => EnvResponse::Inmutable(value),
            Some((ValueKind::Mutable, value)) => EnvResponse::Mutable(value),
            None => EnvResponse::NotFound,
        }
    }

    fn set_mutable(&mut self, name: &str, val: Object) {
        self.dict
            .insert(name.to_string(), (ValueKind::Mutable, val));
    }

    fn set_inmutable(&mut self, name: &str, val: Object) {
        self.dict
            .insert(name.to_string(), (ValueKind::Inmutable, val));
    }
}

pub enum ExecContext {
    Repl,
    File { reference_path: PathBuf },
}

pub struct Environment {
    scopes: Vec<Scope>,
    pub ctx: ExecContext,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            ctx: ExecContext::Repl,
        }
    }
}

impl Environment {
    pub fn new(ctx: ExecContext) -> Self {
        Self {
            scopes: vec![Scope::default()],
            ctx,
        }
    }

    pub fn get(&mut self, name: &str) -> EnvResponse<'_> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get(name) {
                EnvResponse::NotFound => continue,
                response => return response,
            }
        }

        EnvResponse::NotFound
    }

    pub fn set_mutable(&mut self, name: &str, val: Object) {
        self.scopes.last_mut().unwrap().set_mutable(name, val);
    }

    pub fn set_inmutable(&mut self, name: &str, val: Object) {
        self.scopes.last_mut().unwrap().set_inmutable(name, val);
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
}
