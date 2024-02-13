use std::collections::HashMap;

use crate::ast::ASTNode;

#[derive(Debug, Default)]
pub struct Scope {
    dict: HashMap<String, ASTNode>,
}

impl Scope {
    pub fn get(&self, name: &str) -> Option<&ASTNode> {
        self.dict.get(name)
    }
}

pub enum Environment {
    Child(Scope, Box<Environment>),
    Root(Scope),
}

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
}
