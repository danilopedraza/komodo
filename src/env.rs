use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Default)]
pub struct Scope {
    dict: HashMap<String, Object>,
}

impl Scope {
    fn get(&self, name: &str) -> Option<&Object> {
        self.dict.get(name)
    }

    fn set(&mut self, name: &str, val: Object) {
        self.dict.insert(name.to_string(), val);
    }
}

pub struct Environment {
    scopes: Vec<Scope>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }
}

impl Environment {
    pub fn get(&self, name: &str) -> Option<&Object> {
        for scope in self.scopes.iter().rev() {
            match scope.get(name) {
                None => continue,
                obj_opt => return obj_opt,
            }
        }

        None
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.scopes.last_mut().unwrap().set(name, val);
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }
}
