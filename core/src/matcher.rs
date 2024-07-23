use std::{
    collections::{BTreeMap, BTreeSet},
    iter::zip,
};

use crate::{
    ast::{ASTNode, ASTNodeType},
    env::Environment,
    exec::exec,
    object::{ExtensionList, Object},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Match(pub BTreeMap<String, Object>);

fn key_intersection(m1: &Match, m2: &Match) -> BTreeSet<String> {
    let Match(m1) = m1;
    let Match(m2) = m2;

    let mut res = BTreeSet::new();

    for key in m1.keys() {
        if m2.contains_key(key) {
            res.insert(key.to_string());
        }
    }

    for key in m2.keys() {
        if m1.contains_key(key) {
            res.insert(key.to_string());
        }
    }

    res
}

impl From<Vec<(String, Object)>> for Match {
    fn from(map: Vec<(String, Object)>) -> Self {
        let mut res = BTreeMap::new();

        for (key, value) in map {
            res.insert(key, value);
        }

        Self(res)
    }
}

pub fn match_call(patterns: &[ASTNode], args: &[Object]) -> Option<Match> {
    match_list(patterns, args)
}

fn join(map1: Option<Match>, map2: Option<Match>) -> Option<Match> {
    let m1 = map1?;
    let m2 = map2?;
    for key in key_intersection(&m1, &m2) {
        if m1.0.get(&key) != m2.0.get(&key) {
            return None;
        }
    }

    let mut map = BTreeMap::new();
    map.extend(m1.0);
    map.extend(m2.0);

    Some(Match(map))
}

fn match_list(patterns: &[ASTNode], vals: &[Object]) -> Option<Match> {
    if patterns.len() != vals.len() {
        None
    } else {
        zip(patterns, vals)
            .map(|(pattern, val)| match_(pattern, val))
            .fold(empty_match(), join)
    }
}

fn match_(pattern: &ASTNode, val: &Object) -> Option<Match> {
    match &pattern.kind {
        ASTNodeType::Wildcard => empty_match(),
        ASTNodeType::Symbol { name } => single_match(name, val),
        ASTNodeType::ExtensionList { list } => match_extension_list(list, val),
        ASTNodeType::Cons { first, tail } => match_prefix_crop(first, tail, val),
        _ => match_constant(pattern, val),
    }
}

fn single_match(name: &str, val: &Object) -> Option<Match> {
    Some(Match::from(vec![(name.to_string(), val.clone())]))
}

fn empty_match() -> Option<Match> {
    Some(Match::from(vec![]))
}

fn match_extension_list(pattern: &[ASTNode], val: &Object) -> Option<Match> {
    match val {
        Object::ExtensionList(ExtensionList { list: al }) => match_list(pattern, al),
        _ => None,
    }
}

fn match_prefix_crop(first: &ASTNode, most: &ASTNode, val: &Object) -> Option<Match> {
    match val {
        Object::ExtensionList(ExtensionList { list }) if !list.is_empty() => {
            let first_match = match_(first, &list[0]);

            let last_list = Object::ExtensionList(ExtensionList::from(list[1..].to_owned()));
            let last_match = match_(most, &last_list);

            join(first_match, last_match)
        }
        _ => None,
    }
}

fn match_constant(pattern: &ASTNode, val: &Object) -> Option<Match> {
    if exec(pattern, &mut Environment::default()).unwrap() == *val {
        empty_match()
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::tests::{cons, extension_list, symbol},
        cst::tests::dummy_pos,
        object::Integer,
    };

    use super::*;

    #[test]
    fn two_args() {
        let patterns = [
            extension_list(vec![symbol("a", dummy_pos())], dummy_pos()),
            extension_list(vec![symbol("b", dummy_pos())], dummy_pos()),
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
            Some(Match::from(vec![
                (String::from("a"), Object::Integer(Integer::from(1))),
                (String::from("b"), Object::Integer(Integer::from(2)))
            ]))
        );
    }

    #[test]
    fn list_prefix() {
        let pattern = cons(
            symbol("first", dummy_pos()),
            symbol("most", dummy_pos()),
            dummy_pos(),
        );

        let value =
            Object::ExtensionList(ExtensionList::from(vec![Object::Integer(Integer::from(4))]));

        assert_eq!(
            match_(&pattern, &value),
            Some(Match::from(vec![
                (String::from("first"), Object::Integer(Integer::from(4))),
                (
                    String::from("most"),
                    Object::ExtensionList(ExtensionList::from(vec![]))
                ),
            ])),
        );
    }

    #[test]
    fn unmatch_different_value() {
        let patterns = [symbol("a", dummy_pos()), symbol("a", dummy_pos())];

        let values = [
            Object::Integer(Integer::from(1)),
            Object::Integer(Integer::from(2)),
        ];

        assert_eq!(match_call(&patterns, &values), None);
    }
}
