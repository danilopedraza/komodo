use crate::{
    ast::ASTNode,
    env::Environment,
    exec,
    object::{ExtensionList, Object},
};

pub enum MatchResult {
    Match(Vec<(String, Object)>),
    NotAMatch,
}

pub fn match_and_map(pattern: &ASTNode, val: &Object) -> MatchResult {
    match pattern {
        ASTNode::Wildcard => empty_match(),
        ASTNode::ExtensionList(l) => match_list(l, val),
        _ => match_constant(pattern, val),
    }
}

fn empty_match() -> MatchResult {
    MatchResult::Match(vec![])
}

fn match_list(pattern: &[ASTNode], val: &Object) -> MatchResult {
    match val {
        Object::ExtensionList(ExtensionList { list: al }) => match &pattern[0] {
            ASTNode::Symbol(s) => MatchResult::Match(vec![(s.to_string(), al[0].clone())]),
            _ => todo!(),
        },
        _ => MatchResult::NotAMatch,
    }
}

fn match_constant(pattern: &ASTNode, val: &Object) -> MatchResult {
    if exec(pattern, &mut Environment::default()).unwrap() == *val {
        empty_match()
    } else {
        MatchResult::NotAMatch
    }
}
