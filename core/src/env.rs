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
    MutableOriginally(&'a Object),
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

/// Compact binding storage with stable slots; names are consulted only on the
/// dynamic lookup path. Rebinding preserves the original slot.
#[derive(Clone, Debug, Default)]
struct Bindings {
    index: BTreeMap<Rc<str>, usize>,
    values: Vec<(Rc<str>, (ValueKind, Object))>,
}
impl Bindings {
    fn get(&self, name: &str) -> Option<&(ValueKind, Object)> {
        self.index.get(name).map(|&slot| &self.values[slot].1)
    }
    fn insert(&mut self, name: Rc<str>, value: (ValueKind, Object)) {
        if let Some(&slot) = self.index.get(&name) {
            self.values[slot].1 = value;
        } else {
            self.index.insert(name.clone(), self.values.len());
            self.values.push((name, value));
        }
    }
    fn ordered(&self) -> impl Iterator<Item = (&Rc<str>, &(ValueKind, Object))> {
        self.index
            .iter()
            .map(|(name, &slot)| (name, &self.values[slot].1))
    }
}
impl std::ops::Index<&str> for Bindings {
    type Output = (ValueKind, Object);
    fn index(&self, name: &str) -> &Self::Output {
        self.get(name).expect("binding exists")
    }
}
impl FromIterator<(Rc<str>, (ValueKind, Object))> for Bindings {
    fn from_iter<I: IntoIterator<Item = (Rc<str>, (ValueKind, Object))>>(items: I) -> Self {
        let mut result = Self::default();
        for (name, value) in items {
            result.insert(name, value);
        }
        result
    }
}
// Storage order is an implementation detail, not part of closure equality.
impl PartialEq for Bindings {
    fn eq(&self, other: &Self) -> bool {
        self.ordered().eq(other.ordered())
    }
}
impl Eq for Bindings {}
impl PartialOrd for Bindings {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Bindings {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ordered().cmp(other.ordered())
    }
}
impl std::hash::Hash for Bindings {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&self.values.len(), state);
        for entry in self.ordered() {
            std::hash::Hash::hash(&entry, state);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Scope {
    // Captures share their bindings until a scope is changed.
    dict: Rc<Bindings>,
    kind: ScopeKind,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            dict: Rc::default(),
            kind,
        }
    }

    fn get(&mut self, name: &str, mutable_reachable: bool) -> ScopeResponse<'_> {
        let Some(&slot) = self.dict.index.get(name) else {
            return ScopeResponse::NotFound;
        };
        match self.dict.values[slot].1 .0 {
            ValueKind::Inmutable => ScopeResponse::Inmutable(&self.dict.values[slot].1 .1),
            ValueKind::Mutable if !mutable_reachable => {
                ScopeResponse::MutableOriginally(&self.dict.values[slot].1 .1)
            }
            ValueKind::Mutable => {
                ScopeResponse::Mutable(&mut Rc::make_mut(&mut self.dict).values[slot].1 .1)
            }
        }
    }

    fn set_mutable(&mut self, name: &str, obj: Object) {
        if matches!(self.dict.get(name), Some((ValueKind::Mutable, value)) if value == &obj) {
            return;
        }
        Rc::make_mut(&mut self.dict).insert(crate::names::intern(name), (ValueKind::Mutable, obj));
    }

    fn set_inmutable(&mut self, name: &str, obj: Object) {
        Rc::make_mut(&mut self.dict)
            .insert(crate::names::intern(name), (ValueKind::Inmutable, obj));
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
    ctx: Rc<ExecContext>,
    resolved: ResolvedSlots,
}

// Resolved addresses are execution caches, not part of closure identity.
type BindingAddress = (Option<usize>, usize);
#[derive(Clone, Debug, Default)]
struct ResolvedSlots(Option<Rc<[Option<BindingAddress>]>>);
impl PartialEq for ResolvedSlots {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
impl Eq for ResolvedSlots {}
impl PartialOrd for ResolvedSlots {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for ResolvedSlots {
    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}
impl std::hash::Hash for ResolvedSlots {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
}

impl Environment {
    pub(crate) fn resolve_captures(&mut self, body: &crate::ir::Block) {
        self.resolved = if body.capture_names.is_empty() {
            ResolvedSlots::default()
        } else {
            ResolvedSlots(Some(
                body.capture_names
                    .iter()
                    .map(|name| {
                        self.scopes
                            .iter()
                            .enumerate()
                            .rev()
                            .map(|(index, scope)| (Some(index), scope))
                            .chain([(None, &self.base)])
                            .find_map(|(scope, bindings)| {
                                bindings
                                    .dict
                                    .index
                                    .get(name.as_ref())
                                    .map(|&slot| (scope, slot))
                            })
                    })
                    .collect(),
            ))
        };
    }

    pub(crate) fn lookup_capture(&self, index: usize, name: &str) -> Option<&Object> {
        let (scope, slot) = self.resolved.0.as_ref()?.get(index).copied()??;
        let scope = scope.map(|index| &self.scopes[index]).unwrap_or(&self.base);
        let (stored_name, (_, value)) = scope.dict.values.get(slot)?;
        (stored_name.as_ref() == name).then_some(value)
    }

    pub fn new(ctx: ExecContext) -> Self {
        Self {
            ctx: Rc::new(ctx),
            ..Self::default()
        }
    }

    pub fn capture(self, body: &crate::ir::Block, param: &crate::ast::Pattern) -> Self {
        let mut bound = std::collections::BTreeSet::new();
        crate::ir::pattern_names(param, &mut bound);
        let mut captured = self;
        for scope in std::iter::once(&mut captured.base).chain(captured.scopes.iter_mut()) {
            let dict: Bindings = body
                .references
                .iter()
                .filter(|name| !bound.contains(*name))
                .filter_map(|name| {
                    scope
                        .dict
                        .get(name)
                        .map(|value| (name.clone(), (value.0, value.1.snapshot())))
                })
                .collect();
            if dict.values.len() != scope.dict.values.len()
                || scope
                    .dict
                    .values
                    .iter()
                    .any(|(_, (_, value))| value.is_recursive_reference())
            {
                scope.dict = Rc::new(dict);
            }
        }
        captured
    }

    pub fn snapshot(&self) -> Self {
        let mut snapshot = self.clone();
        for scope in std::iter::once(&mut snapshot.base).chain(snapshot.scopes.iter_mut()) {
            if scope
                .dict
                .values
                .iter()
                .any(|(_, (_, value))| value.is_recursive_reference())
            {
                for (_, (_, value)) in &mut Rc::make_mut(&mut scope.dict).values {
                    if value.is_recursive_reference() {
                        *value = value.snapshot();
                    }
                }
            }
        }
        snapshot
    }

    pub fn ctx(&self) -> ExecContext {
        (*self.ctx).clone()
    }

    pub fn context(&self) -> &ExecContext {
        &self.ctx
    }

    pub fn shared_context(&self) -> Rc<ExecContext> {
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

    pub fn lookup_slot(&self, depth: usize, slot: usize, name: &str) -> Option<&Object> {
        let scope = self
            .scopes
            .len()
            .checked_sub(depth + 1)
            .map(|i| &self.scopes[i])?;
        let (stored_name, (_, value)) = scope.dict.values.get(slot)?;
        (stored_name.as_ref() == name).then_some(value)
    }

    pub fn get(&mut self, name: &str) -> EnvResponse<'_> {
        let mut mutable_reachable = true;

        for scope in self.scopes.iter_mut().rev().chain([&mut self.base]) {
            let scope_kind = scope.kind;

            let res = match scope.get(name, mutable_reachable) {
                ScopeResponse::NotFound => None,
                ScopeResponse::Inmutable(val) => Some(EnvResponse::Inmutable(val)),
                ScopeResponse::MutableOriginally(val) => Some(EnvResponse::MutableOriginally(val)),
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
        match self
            .scopes
            .last_mut()
            .unwrap_or(&mut self.base)
            .get(name, true)
        {
            ScopeResponse::Mutable(val) => EnvResponse::Mutable(val),
            ScopeResponse::Inmutable(val) => EnvResponse::Inmutable(val),
            ScopeResponse::NotFound => EnvResponse::NotFound,
            ScopeResponse::MutableOriginally(val) => EnvResponse::MutableOriginally(val),
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

    pub fn push_bindings(&mut self, kind: ScopeKind, bindings: BTreeMap<String, Object>) {
        let dict = bindings
            .into_iter()
            .map(|(name, value)| (crate::names::intern(&name), (ValueKind::Inmutable, value)))
            .collect();
        self.scopes.push(Scope {
            kind,
            dict: Rc::new(dict),
        });
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
    fn read_and_rejected_mutation_do_not_detach_scopes() {
        let mut env = Environment::default();
        env.set_mutable("outer", Object::true_obj());
        env.set_inmutable("constant", Object::false_obj());
        env.push_scope(ScopeKind::Block);
        let snapshot = env.clone();
        assert!(matches!(env.get("missing"), EnvResponse::NotFound));
        assert!(matches!(env.get("constant"), EnvResponse::Inmutable(_)));
        assert!(matches!(
            env.get("outer"),
            EnvResponse::MutableOriginally(_)
        ));
        assert!(Rc::ptr_eq(&env.base.dict, &snapshot.base.dict));
        assert!(Rc::ptr_eq(&env.scopes[0].dict, &snapshot.scopes[0].dict));
    }

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
