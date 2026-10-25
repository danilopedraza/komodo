use std::{collections::BTreeMap, path::PathBuf, rc::Rc};

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
    // Captures share their bindings until a scope is changed.
    dict: Rc<BTreeMap<String, (ValueKind, Object)>>,
    kind: ScopeKind,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            dict: Rc::default(),
            kind,
        }
    }

    fn get(&mut self, name: &str) -> ScopeResponse<'_> {
        match Rc::make_mut(&mut self.dict).get_mut(name) {
            Some((ValueKind::Inmutable, value)) => ScopeResponse::Inmutable(value),
            Some((ValueKind::Mutable, value)) => ScopeResponse::Mutable(value),
            None => ScopeResponse::NotFound,
        }
    }

    fn set_mutable(&mut self, name: &str, obj: Object) {
        Rc::make_mut(&mut self.dict).insert(name.to_string(), (ValueKind::Mutable, obj));
    }

    fn set_inmutable(&mut self, name: &str, obj: Object) {
        Rc::make_mut(&mut self.dict).insert(name.to_string(), (ValueKind::Inmutable, obj));
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

    /// Read a value without copying a shared captured scope to obtain a mutable reference.
    pub fn lookup(&self, name: &str) -> Option<&Object> {
        self.scopes
            .iter()
            .rev()
            .chain([&self.base])
            .find_map(|scope| scope.dict.get(name).map(|(_, value)| value))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn captures_keep_bindings_and_mutable_collections_isolated() {
        let mut original = Environment::default();
        original.set_mutable(
            "items",
            Object::List(vec![Object::Integer(1.into())].into()),
        );
        original.push_scope(ScopeKind::Block);
        original.set_inmutable("local", Object::Integer(2.into()));
        let mut captured = original.clone();

        original.set_inmutable("local", Object::Integer(4.into()));
        assert_eq!(captured.lookup("local"), Some(&Object::Integer(2.into())));
        assert!(matches!(
            captured.get("items"),
            EnvResponse::MutableOriginally(_)
        ));
        captured.pop_scope();

        // Mutable references to nested objects must detach the captured scope.
        let EnvResponse::Mutable(Object::List(items)) = captured.get("items") else {
            panic!("expected mutable list");
        };
        items.list[0] = Object::Integer(3.into());
        assert_eq!(
            original.lookup("items"),
            Some(&Object::List(vec![Object::Integer(1.into())].into()))
        );
        captured.set_inmutable("added", Object::true_obj());
        assert_eq!(original.lookup("added"), None);
    }
}
