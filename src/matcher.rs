use std::iter::zip;

use crate::{
    ast::ASTNode,
    env::Environment,
    exec,
    object::{ExtensionList, Object},
};

#[derive(Debug, PartialEq, Eq)]
pub enum MatchResult {
    Match(Vec<(String, Object)>),
    NotAMatch,
}

pub fn match_call(patterns: &[ASTNode], args: &[Object]) -> MatchResult {
    match_list(patterns, args)
}

fn match_list(patterns: &[ASTNode], vals: &[Object]) -> MatchResult {
    let mut map = vec![];

    for (pattern, arg) in zip(patterns, vals) {
        if let MatchResult::Match(v) = match_and_map(pattern, arg) {
            map.extend(v);
        } else {
            return MatchResult::NotAMatch;
        }
    }

    MatchResult::Match(map)
}

fn match_and_map(pattern: &ASTNode, val: &Object) -> MatchResult {
    match pattern {
        ASTNode::Symbol(s) => single_match(s, val),
        ASTNode::Wildcard => empty_match(),
        ASTNode::ExtensionList(l) => match_extension_list(l, val),
        _ => match_constant(pattern, val),
    }
}

fn single_match(name: &str, val: &Object) -> MatchResult {
    MatchResult::Match(vec![(name.to_string(), val.clone())])
}

fn empty_match() -> MatchResult {
    MatchResult::Match(vec![])
}

fn match_extension_list(pattern: &[ASTNode], val: &Object) -> MatchResult {
    match val {
        Object::ExtensionList(ExtensionList { list: al }) => match_list(pattern, al),
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

#[cfg(test)]
mod tests {
    use crate::object::Integer;

    use super::*;

    #[test]
    fn two_args() {
        let patterns = [
            ASTNode::ExtensionList(vec![ASTNode::Symbol(String::from("a"))]),
            ASTNode::ExtensionList(vec![ASTNode::Symbol(String::from("b"))]),
        ];

        let args = [
            Object::ExtensionList(ExtensionList {
                list: vec![Object::Integer(Integer::from(1))],
            }),
            Object::ExtensionList(ExtensionList {
                list: vec![Object::Integer(Integer::from(2))],
            }),
        ];

        assert_eq!(
            match_list(&patterns, &args),
            MatchResult::Match(vec![
                (String::from("a"), Object::Integer(Integer::from(1))),
                (String::from("b"), Object::Integer(Integer::from(2)))
            ])
        );
    }
}
