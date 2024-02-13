use std::collections::HashMap;

use crate::ast::ASTNode;

#[derive(Debug, Default)]
pub struct Scope {
    dict: HashMap<String, ASTNode>,
}

impl Scope {
    fn get(&self, name: &str) -> Option<&ASTNode> {
        self.dict.get(name)
    }

    fn set(&mut self, name: &str, val: ASTNode) {
        self.dict.insert(name.to_string(), val);
    }
}

#[allow(unused)]
pub enum Environment {
    Child(Scope, Box<Environment>),
    Root(Scope),
}

impl Default for Environment {
    fn default() -> Self {
        Self::Root(Default::default())
    }
}

#[allow(unused)]
impl Environment {
    pub fn get(&self, name: &str) -> Option<&ASTNode> {
        match self {
            Self::Root(scope) => scope.get(name),
            Self::Child(scope, inner) => match scope.get(name) {
                Some(val) => Some(val),
                None => inner.get(name),
            },
        }
    }

    pub fn set(&mut self, name: &str, val: ASTNode) {
        match self {
            Self::Root(scope) => scope.set(name, val),
            Self::Child(scope, _) => scope.set(name, val),
        }
    }

    pub fn new_scope(self) -> Self {
        Self::Child(Default::default(), Box::new(self))
    }
}
