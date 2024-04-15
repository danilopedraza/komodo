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
        ASTNode::Symbol(s) => MatchResult::Match(vec![(s.to_string(), val.clone())]),
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
        Object::ExtensionList(ExtensionList { list: al }) => match_and_map(&pattern[0], &al[0]),
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
