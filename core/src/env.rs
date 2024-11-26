use std::{collections::BTreeMap, path::PathBuf};

use crate::object::Object;

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeDepth(pub usize);

#[derive(Debug, PartialEq, Eq)]
pub enum EnvResponse<'a> {
    Mutable(&'a mut Object, ScopeDepth),
    Inmutable(&'a Object, ScopeDepth),
    NotFound,
}

#[derive(Debug, PartialEq, Eq)]
enum ScopeResponse<'a> {
    Mutable(&'a mut Object),
    Inmutable(&'a Object),
    NotFound,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValueKind {
    Inmutable,
    Mutable,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Scope {
    dict: BTreeMap<String, (ValueKind, Object)>,
}

impl Scope {
    fn get(&mut self, name: &str) -> ScopeResponse {
        match self.dict.get_mut(name) {
            Some((ValueKind::Inmutable, value)) => ScopeResponse::Inmutable(value),
            Some((ValueKind::Mutable, value)) => ScopeResponse::Mutable(value),
            None => ScopeResponse::NotFound,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct ExecContext {
    pub reference_path: PathBuf,
}

impl ExecContext {
    pub fn new(reference_path: PathBuf) -> Self {
        Self { reference_path }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Environment {
    base: Scope,
    scopes: Vec<Scope>,
    pub ctx: ExecContext,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            base: Scope::default(),
            scopes: vec![Scope::default()],
            ctx: ExecContext::default(),
        }
    }
}

impl Environment {
    pub fn new(ctx: ExecContext) -> Self {
        Self {
            base: Scope::default(),
            scopes: vec![Scope::default()],
            ctx,
        }
    }

    pub fn get(&mut self, name: &str) -> EnvResponse<'_> {
        let mut depth = 0;
        for scope in self.scopes.iter_mut().rev().chain([&mut self.base]) {
            match scope.get(name) {
                ScopeResponse::NotFound => {
                    depth += 1;
                    continue;
                }
                ScopeResponse::Inmutable(val) => {
                    return EnvResponse::Inmutable(val, ScopeDepth(depth))
                }
                ScopeResponse::Mutable(val) => return EnvResponse::Mutable(val, ScopeDepth(depth)),
            }
        }

        EnvResponse::NotFound
    }

    pub fn get_current_scope(&mut self, name: &str) -> EnvResponse<'_> {
        match self.scopes.last_mut().unwrap_or(&mut self.base).get(name) {
            ScopeResponse::Mutable(val) => EnvResponse::Mutable(val, ScopeDepth(0)),
            ScopeResponse::Inmutable(val) => EnvResponse::Inmutable(val, ScopeDepth(0)),
            ScopeResponse::NotFound => EnvResponse::NotFound,
        }
    }

    pub fn set_mutable(&mut self, name: &str, val: Object) {
        self.scopes
            .last_mut()
            .unwrap_or(&mut self.base)
            .set_mutable(name, val);
    }

    pub fn set_inmutable(&mut self, name: &str, val: Object) {
        self.scopes
            .last_mut()
            .unwrap_or(&mut self.base)
            .set_inmutable(name, val);
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}
