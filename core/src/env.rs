use std::{collections::BTreeMap, path::PathBuf};

use crate::object::Object;

#[derive(Debug, PartialEq, Eq)]
pub enum EnvResponse<'a> {
    Mutable(&'a mut Object),
    Inmutable(&'a Object),
    MutableOriginally(&'a Object),
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

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ScopeKind {
    #[default]
    Root,
    Function,
    Loop,
    Block,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Scope {
    dict: BTreeMap<String, (ValueKind, Object)>,
    kind: ScopeKind,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            dict: BTreeMap::default(),
            kind,
        }
    }

    fn get(&mut self, name: &str) -> ScopeResponse<'_> {
        match self.dict.get_mut(name) {
            Some((ValueKind::Inmutable, value)) => ScopeResponse::Inmutable(value),
            Some((ValueKind::Mutable, value)) => ScopeResponse::Mutable(value),
            None => ScopeResponse::NotFound,
        }
    }

    fn set_mutable(&mut self, name: &str, obj: Object) {
        self.dict
            .insert(name.to_string(), (ValueKind::Mutable, obj));
    }

    fn set_inmutable(&mut self, name: &str, obj: Object) {
        self.dict
            .insert(name.to_string(), (ValueKind::Inmutable, obj));
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct ExecContext {
    pub executed_file_path: PathBuf,
    pub reference_path: PathBuf,
}

impl ExecContext {
    pub fn new(executed_file_path: PathBuf, reference_path: PathBuf) -> Self {
        Self {
            executed_file_path,
            reference_path,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct Environment {
    base: Scope,
    scopes: Vec<Scope>,
    ctx: ExecContext,
}

impl Environment {
    pub fn new(ctx: ExecContext) -> Self {
        Self {
            ctx,
            ..Self::default()
        }
    }

    pub fn ctx(&self) -> ExecContext {
        self.ctx.clone()
    }

    pub fn file_path(&self) -> PathBuf {
        self.ctx.executed_file_path.to_path_buf()
    }

    pub fn get(&mut self, name: &str) -> EnvResponse<'_> {
        let mut mutable_reachable = true;

        for scope in self.scopes.iter_mut().rev().chain([&mut self.base]) {
            let scope_kind = scope.kind;

            let res = match scope.get(name) {
                ScopeResponse::NotFound => None,
                ScopeResponse::Inmutable(val) => Some(EnvResponse::Inmutable(val)),
                ScopeResponse::Mutable(val) => {
                    if mutable_reachable {
                        Some(EnvResponse::Mutable(val))
                    } else {
                        Some(EnvResponse::MutableOriginally(val))
                    }
                }
            };

            if scope_kind != ScopeKind::Loop {
                mutable_reachable = false;
            }

            if let Some(res) = res {
                return res;
            } else {
                continue;
            }
        }

        EnvResponse::NotFound
    }

    pub fn get_current_scope(&mut self, name: &str) -> EnvResponse<'_> {
        match self.scopes.last_mut().unwrap_or(&mut self.base).get(name) {
            ScopeResponse::Mutable(val) => EnvResponse::Mutable(val),
            ScopeResponse::Inmutable(val) => EnvResponse::Inmutable(val),
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

    pub fn push_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind));
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

pub fn env_with(assets: Vec<(&str, Object)>, ctx: ExecContext) -> Environment {
    let mut env = Environment::new(ctx);

    for (name, value) in assets {
        env.set_inmutable(name, value);
    }

    env
}
