use std::{collections::BTreeMap, iter::zip};

use crate::{
    ast::{ASTNode, ASTNodeType},
    env::Environment,
    exec::exec,
    object::{Dictionary, ExtensionList, Object},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Match(pub BTreeMap<String, Object>);

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

fn join(match1: Option<Match>, match2: Option<Match>) -> Option<Match> {
    let Match(map1) = match1?;
    let Match(mut map2) = match2?;

    for (key, val) in map1 {
        match map2.get(&key) {
            None => {
                map2.insert(key, val);
            }
            Some(other_val) if *other_val != val => return None,
            Some(_) => continue,
        }
    }

    Some(Match(map2))
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
        ASTNodeType::Dictionary(pairs) => match_dictionary(pairs, val),
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

fn match_dictionary(pairs: &Vec<(ASTNode, ASTNode)>, val: &Object) -> Option<Match> {
    match val {
        Object::Dictionary(dict) => match_dict(pairs, dict),
        _ => None,
    }
}

fn match_dict(pairs: &Vec<(ASTNode, ASTNode)>, dict: &Dictionary) -> Option<Match> {
    let mut res = empty_match();
    for (key, value) in pairs {
        let actual_value = dict.dict.get(&isolated_unchecked_exec(key))?;
        res = join(res, match_(value, actual_value));
        res.as_ref()?; // return None if res is None
    }

    res
}

fn match_constant(pattern: &ASTNode, val: &Object) -> Option<Match> {
    if isolated_unchecked_exec(pattern) == *val {
        empty_match()
    } else {
        None
    }
}

fn isolated_unchecked_exec(node: &ASTNode) -> Object {
    exec(node, &mut Environment::default()).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::tests::{cons, dictionary, extension_list, string, symbol},
        cst::tests::dummy_pos,
        object::{Dictionary, Integer},
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

    #[test]
    fn match_empty_dict() {
        let pattern = dictionary(vec![], dummy_pos());
        let value = Object::Dictionary(Dictionary::from(vec![]));

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn match_single_pair_dict() {
        let pattern = dictionary(
            vec![(string("foo", dummy_pos()), symbol("bar", dummy_pos()))],
            dummy_pos(),
        );

        let value = Object::Dictionary(Dictionary::from(vec![(
            Object::String("foo".into()),
            Object::Integer(5.into()),
        )]));

        assert_eq!(
            match_(&pattern, &value),
            single_match("bar", &Object::Integer(5.into())),
        );
    }
}
