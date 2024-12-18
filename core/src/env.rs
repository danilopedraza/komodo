use std::{collections::BTreeMap, path::PathBuf};

use crate::object::Object;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Address(usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Store {
    memory: Vec<Object>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum EnvResponse<'a> {
    Mutable((&'a mut Object, Address)),
    Inmutable((&'a Object, Address)),
    MutableOriginally((&'a Object, Address)),
    NotFound,
}

#[derive(Debug, PartialEq, Eq)]
enum ScopeResponse<'a> {
    Mutable((&'a mut Object, Address)),
    Inmutable((&'a Object, Address)),
    NotFound,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValueKind {
    Inmutable,
    Mutable,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ScopeKind {
    Root,
    Function,
    Loop,
    Block,
}

impl Default for ScopeKind {
    fn default() -> Self {
        Self::Root
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Scope {
    dict: BTreeMap<String, (ValueKind, Object, Address)>,
    kind: ScopeKind,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            dict: BTreeMap::default(),
            kind,
        }
    }

    fn get(&mut self, name: &str) -> ScopeResponse {
        match self.dict.get_mut(name) {
            Some((ValueKind::Inmutable, value, addr)) => ScopeResponse::Inmutable((value, *addr)),
            Some((ValueKind::Mutable, value, addr)) => ScopeResponse::Mutable((value, *addr)),
            None => ScopeResponse::NotFound,
        }
    }

    fn set_mutable(&mut self, name: &str, (obj, addr): (Object, Address)) {
        self.dict
            .insert(name.to_string(), (ValueKind::Mutable, obj, addr));
    }

    fn set_inmutable(&mut self, name: &str, (obj, addr): (Object, Address)) {
        self.dict
            .insert(name.to_string(), (ValueKind::Inmutable, obj, addr));
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
    store: Store,
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
            if scope.kind == ScopeKind::Function {
                mutable_reachable = false;
            }

            match scope.get(name) {
                ScopeResponse::NotFound => continue,
                ScopeResponse::Inmutable(val) => return EnvResponse::Inmutable(val),
                ScopeResponse::Mutable(val) => {
                    if mutable_reachable {
                        return EnvResponse::Mutable(val);
                    } else {
                        return EnvResponse::MutableOriginally((val.0, val.1));
                    }
                }
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

    pub fn set_mutable(&mut self, name: &str, val: (Object, Address)) {
        self.scopes
            .last_mut()
            .unwrap_or(&mut self.base)
            .set_mutable(name, val);
    }

    pub fn set_inmutable(&mut self, name: &str, val: (Object, Address)) {
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
