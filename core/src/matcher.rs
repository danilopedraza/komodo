use std::iter::zip;

use crate::{
    cst::{CSTNode, CSTNodeType},
    env::Environment,
    exec::exec,
    object::{ExtensionList, Object},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Match(pub Vec<(String, Object)>);

pub fn match_call(patterns: &[CSTNode], args: &[Object]) -> Option<Match> {
    match_list(patterns, args)
}

fn join(map1: Option<Match>, map2: Option<Match>) -> Option<Match> {
    let Match(v1) = map1?;
    let Match(v2) = map2?;
    for (key1, val1) in &v1 {
        for (key2, val2) in &v2 {
            if key1 == key2 && val1 != val2 {
                return None;
            }
        }
    }

    let mut map = vec![];
    map.extend(v1);
    map.extend(v2);

    Some(Match(map))
}

fn match_list(patterns: &[CSTNode], vals: &[Object]) -> Option<Match> {
    if patterns.len() != vals.len() {
        None
    } else {
        zip(patterns, vals)
            .map(|(pattern, val)| match_(pattern, val))
            .fold(empty_match(), join)
    }
}

fn match_(pattern: &CSTNode, val: &Object) -> Option<Match> {
    match &pattern._type {
        CSTNodeType::Wildcard => empty_match(),
        CSTNodeType::Symbol(s) => single_match(s, val),
        CSTNodeType::ExtensionList(l) => match_extension_list(l, val),
        CSTNodeType::Cons(first, most) => match_prefix_crop(first, most, val),
        _ => match_constant(pattern, val),
    }
}

fn single_match(name: &str, val: &Object) -> Option<Match> {
    Some(Match(vec![(name.to_string(), val.clone())]))
}

fn empty_match() -> Option<Match> {
    Some(Match(vec![]))
}

fn match_extension_list(pattern: &[CSTNode], val: &Object) -> Option<Match> {
    match val {
        Object::ExtensionList(ExtensionList { list: al }) => match_list(pattern, al),
        _ => None,
    }
}

fn match_prefix_crop(first: &CSTNode, most: &CSTNode, val: &Object) -> Option<Match> {
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

fn match_constant(pattern: &CSTNode, val: &Object) -> Option<Match> {
    if exec(pattern, &mut Environment::default()).unwrap() == *val {
        empty_match()
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cst::{cons, extension_list, symbol, tests::dummy_pos},
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
            Some(Match(vec![
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
            Some(Match(vec![
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
