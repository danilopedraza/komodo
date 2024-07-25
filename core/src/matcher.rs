use std::{collections::BTreeMap, iter::zip};

use crate::{
    ast::{ASTNode, ASTNodeType, InfixOperator},
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
    match_sequence(patterns, args)
}

fn join(match1: Option<Match>, match2: Option<Match>) -> Option<Match> {
    let Match(mut map1) = match1?;
    let Match(map2) = match2?;

    for (key, val) in map2 {
        match map1.get(&key) {
            None => {
                map1.insert(key, val);
            }
            Some(other_val) if *other_val != val => return None,
            Some(_) => continue,
        }
    }

    Some(Match(map1))
}

fn match_sequence(patterns: &[ASTNode], vals: &[Object]) -> Option<Match> {
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
        ASTNodeType::Dictionary { pairs, complete } => match_dictionary(pairs, *complete, val),
        ASTNodeType::Infix {
            op: InfixOperator::Range,
            lhs,
            rhs,
        } => match_range(lhs, rhs, val),
        ASTNodeType::SetCons { some, most } => set_cons(some, most, val),
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
        Object::ExtensionList(ExtensionList { list }) => match_list(pattern, list),
        _ => None,
    }
}

fn match_list(pattern: &[ASTNode], list: &[Object]) -> Option<Match> {
    match pattern.last() {
        Some(ASTNode {
            kind: ASTNodeType::AdInfinitum,
            position: _,
        }) => zip(&pattern[..pattern.len() - 1], list)
            .map(|(pattern, val)| match_(pattern, val))
            .fold(empty_match(), join),
        _ => match_sequence(pattern, list),
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

fn match_dictionary(
    pairs: &Vec<(ASTNode, ASTNode)>,
    must_match_all: bool,
    val: &Object,
) -> Option<Match> {
    match val {
        Object::Dictionary(dict) => match_dict(pairs, dict, must_match_all),
        _ => None,
    }
}

fn match_dict(
    pairs: &Vec<(ASTNode, ASTNode)>,
    dict: &Dictionary,
    must_match_all: bool,
) -> Option<Match> {
    if pairs.len() != dict.dict.len() && must_match_all {
        return None;
    }

    let mut res = empty_match();

    for (key, value) in pairs {
        let mut match_found = false;

        for (some_key, some_value) in &dict.dict {
            let left_match = match_(key, some_key);
            let right_match = match_(value, some_value);
            let whole_match = join(left_match, right_match);

            if whole_match.is_some() {
                match_found = true;
                res = join(res, whole_match);
                break;
            }
        }

        if !match_found {
            return None;
        }
    }

    res
}

fn match_range(lhs: &ASTNode, rhs: &ASTNode, val: &Object) -> Option<Match> {
    match val {
        Object::Range(range) => join(
            match_(lhs, &Object::Integer(range.start.to_owned())),
            match_(rhs, &Object::Integer(range.end.to_owned())),
        ),
        _ => None,
    }
}

fn set_cons(some: &ASTNode, most: &ASTNode, val: &Object) -> Option<Match> {
    match val {
        Object::ExtensionSet(set) => {
            for val in &set.set {
                let mut new_set = set.set.clone();
                new_set.remove(val);
                let res = join(
                    match_(some, val),
                    match_(most, &Object::ExtensionSet(new_set.into())),
                );

                if res.is_some() {
                    return res;
                }
            }

            None
        }
        _ => None,
    }
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
        ast::tests::{
            ad_infinitum, cons, dictionary, extension_list, integer, range, set_cons, string,
            symbol, wildcard,
        },
        cst::tests::dummy_pos,
        object::{Dictionary, Integer, Range},
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
            match_sequence(&patterns, &args),
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
        let pattern = dictionary(vec![], true, dummy_pos());
        let value = Object::Dictionary(Dictionary::from(vec![]));

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn match_single_pair_dict() {
        let pattern = dictionary(
            vec![(string("foo", dummy_pos()), symbol("bar", dummy_pos()))],
            true,
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

    #[test]
    fn match_dict_key() {
        let pattern = dictionary(
            vec![(wildcard(dummy_pos()), symbol("a", dummy_pos()))],
            true,
            dummy_pos(),
        );

        let value = Object::Dictionary(Dictionary::from(vec![(
            Object::String("foo".into()),
            Object::Integer(5.into()),
        )]));

        assert_eq!(
            match_(&pattern, &value),
            single_match("a", &Object::Integer(5.into()))
        );
    }

    #[test]
    fn match_dict_of_same_length() {
        let pattern = dictionary(
            vec![(string("foo", dummy_pos()), symbol("a", dummy_pos()))],
            true,
            dummy_pos(),
        );

        let value = Object::Dictionary(Dictionary::from(vec![
            (Object::String("foo".into()), Object::Integer(5.into())),
            (Object::String("bar".into()), Object::Integer(6.into())),
        ]));

        assert_eq!(match_(&pattern, &value), None,);
    }

    #[test]
    fn dict_composite_key() {
        let pattern = dictionary(
            vec![(
                extension_list(vec![wildcard(dummy_pos())], dummy_pos()),
                integer("10", dummy_pos()),
            )],
            true,
            dummy_pos(),
        );

        let value = Object::Dictionary(Dictionary::from(vec![(
            Object::ExtensionList(ExtensionList::from(vec![Object::Integer(1.into())])),
            Object::Integer(10.into()),
        )]));

        assert_eq!(match_(&pattern, &value), empty_match());
    }

    #[test]
    fn incomplete_dict() {
        let pattern = dictionary(
            vec![(integer("5", dummy_pos()), integer("10", dummy_pos()))],
            false,
            dummy_pos(),
        );

        let value = Object::Dictionary(Dictionary::from(vec![
            (Object::Integer(5.into()), Object::Integer(10.into())),
            (Object::String("foo".into()), Object::Integer(11.into())),
        ]));

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn incomplete_list() {
        let pattern = extension_list(
            vec![wildcard(dummy_pos()), ad_infinitum(dummy_pos())],
            dummy_pos(),
        );

        let value = Object::ExtensionList(
            vec![Object::Integer(1.into()), Object::Integer(2.into())].into(),
        );

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn range_() {
        let pattern = range(
            symbol("a", dummy_pos()),
            symbol("b", dummy_pos()),
            dummy_pos(),
        );

        let value = Object::Range(Range::new(&Integer::from(0), &Integer::from(1)));

        assert_eq!(
            match_(&pattern, &value),
            join(
                single_match("a", &Object::Integer(0.into())),
                single_match("b", &Object::Integer(1.into()))
            ),
        );
    }

    #[test]
    fn set_cons_() {
        let pattern = set_cons(
            symbol("val", dummy_pos()),
            wildcard(dummy_pos()),
            dummy_pos(),
        );

        let value = Object::ExtensionSet(vec![Object::Integer(0.into())].into());

        assert_eq!(
            match_(&pattern, &value),
            single_match("val", &Object::Integer(0.into())),
        );
    }
}
