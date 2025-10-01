use std::{collections::BTreeMap, iter::zip};

use crate::{
    ast::{Constant, Pattern},
    env::Address,
    exec::eval_constant,
    object::{Dictionary, List, Object, Tuple},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Match(pub BTreeMap<String, (Object, Address)>);

impl From<Vec<(String, Object)>> for Match {
    fn from(map: Vec<(String, Object)>) -> Self {
        let mut res = BTreeMap::new();

        for (key, value) in map {
            res.insert(key, (value, Address::default()));
        }

        Self(res)
    }
}

impl From<(String, Object)> for Match {
    fn from((key, value): (String, Object)) -> Self {
        let mut res = BTreeMap::new();
        res.insert(key, (value, Address::default()));
        Self(res)
    }
}

pub fn match_call(patterns: &[Pattern], args: &[Object]) -> Option<Match> {
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

fn match_sequence(patterns: &[Pattern], vals: &[Object]) -> Option<Match> {
    if patterns.len() != vals.len() {
        None
    } else {
        zip(patterns, vals)
            .map(|(pattern, val)| match_(pattern, val))
            .fold(empty_match(), join)
    }
}

fn satisfies(obj: &Object, constraint: &Pattern) -> bool {
    match constraint {
        Pattern::Symbol { name } => obj.has_property(name),
        Pattern::Either { lhs, rhs } => satisfies(obj, lhs) || satisfies(obj, rhs),
        _ => false,
    }
}

pub fn match_(pattern: &Pattern, val: &Object) -> Option<Match> {
    match pattern {
        Pattern::Constant(constant) => match_constant(constant, val),
        Pattern::Symbol { name } => single_match(name, val),
        Pattern::List { list } => match_extension_list(list, val),
        Pattern::Tuple { list } => match_tuple(list, val),
        Pattern::Set { list } => match_extension_set(list, val),
        Pattern::ListCons { first, tail } => match_prefix_crop(first, tail, val),
        Pattern::Dictionary { pairs, complete } => match_dictionary(pairs, *complete, val),
        Pattern::SetCons { some, most } => set_cons(some, most, val),
        Pattern::Wildcard => empty_match(),
        Pattern::Range { start, end } => match_range(start, end, val),
        Pattern::Fraction { numer, denom } => fraction(numer, denom, val),
        Pattern::Signature {
            pattern,
            constraint,
        } => match_signature(pattern, constraint, val),
        Pattern::Either { lhs, rhs } => match_either(lhs, rhs, val),
        Pattern::AdInfinitum => None,
    }
}

fn match_either(lhs: &Pattern, rhs: &Pattern, val: &Object) -> Option<Match> {
    match_(lhs, val).or_else(|| match_(rhs, val))
}

fn match_signature(pattern: &Pattern, constraint: &Pattern, val: &Object) -> Option<Match> {
    if satisfies(val, constraint) {
        match_(pattern, val)
    } else {
        None
    }
}

fn single_match(name: &str, val: &Object) -> Option<Match> {
    Some(Match::from((name.to_string(), val.clone())))
}

fn empty_match() -> Option<Match> {
    Some(Match::from(vec![]))
}

fn match_extension_list(pattern: &[Pattern], val: &Object) -> Option<Match> {
    match pattern.last() {
        Some(Pattern::AdInfinitum) => {
            let pattern = &pattern[..pattern.len() - 1];

            match val {
                Object::List(List { list }) => {
                    let list = &list[..pattern.len()];

                    match_list(
                        pattern,
                        &list.iter().map(|(obj, _)| obj.clone()).collect::<Vec<_>>(),
                    )
                }
                _ => None,
            }
        }
        _ => match val {
            Object::List(List { list }) => match_list(
                pattern,
                &list.iter().map(|(obj, _)| obj.clone()).collect::<Vec<_>>(),
            ),
            _ => None,
        },
    }
}

fn match_tuple(pattern: &[Pattern], val: &Object) -> Option<Match> {
    match val {
        Object::Tuple(Tuple { list }) => match_list(
            pattern,
            &list.iter().map(|(obj, _)| obj.clone()).collect::<Vec<_>>(),
        ),
        _ => None,
    }
}

fn match_extension_set(patterns: &[Pattern], val: &Object) -> Option<Match> {
    match val {
        Object::Set(set) if patterns.len() == set.len() => {
            let mut res = empty_match();

            for (pattern, val) in zip(patterns, set.iter()) {
                res = join(res, match_(pattern, val));
            }

            res
        }
        _ => None,
    }
}

fn match_list(pattern: &[Pattern], list: &[Object]) -> Option<Match> {
    match_sequence(pattern, list)
}

fn match_prefix_crop(first: &Pattern, most: &Pattern, val: &Object) -> Option<Match> {
    match val {
        Object::List(List { list }) if !list.is_empty() => {
            let first_match = match_(first, &list[0].0);

            let last_list = Object::List(List::from(list[1..].to_owned()));
            let last_match = match_(most, &last_list);

            join(first_match, last_match)
        }
        Object::String(str) => {
            let (first_val, tail_val) = str.cons_format()?;
            let first_match = match_(first, &Object::Char(first_val.into()));
            let tail_match = match_(most, &Object::String(tail_val.into()));

            join(first_match, tail_match)
        }
        _ => None,
    }
}

fn match_dictionary(
    pairs: &Vec<(Pattern, Pattern)>,
    must_match_all: bool,
    val: &Object,
) -> Option<Match> {
    match val {
        Object::Dictionary(dict) => match_dict(pairs, dict, must_match_all),
        _ => None,
    }
}

fn match_dict(
    pairs: &Vec<(Pattern, Pattern)>,
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

fn match_range(lhs: &Pattern, rhs: &Pattern, val: &Object) -> Option<Match> {
    match val {
        Object::Range(range) => join(
            match_(lhs, &Object::Integer(range.start.to_owned())),
            match_(rhs, &Object::Integer(range.end.to_owned())),
        ),
        _ => None,
    }
}

fn set_cons(some: &Pattern, most: &Pattern, val: &Object) -> Option<Match> {
    match val {
        Object::Set(set) => {
            for val in set.iter() {
                let mut new_set = set.clone();
                new_set.remove(val);
                let res = join(match_(some, val), match_(most, &new_set.into()));

                if res.is_some() {
                    return res;
                }
            }

            None
        }
        _ => None,
    }
}

fn fraction(numer: &Pattern, denom: &Pattern, val: &Object) -> Option<Match> {
    match val {
        Object::Fraction(frac) => {
            let numer_val = Object::Integer(frac.numer());
            let denom_val = Object::Integer(frac.denom());

            join(match_(numer, &numer_val), match_(denom, &denom_val))
        }
        _ => None,
    }
}

fn match_constant(constant: &Constant, val: &Object) -> Option<Match> {
    if eval_constant(constant) == *val {
        empty_match()
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::tests::{
            ad_infinitum, cons_pattern, dec_integer_pattern, dictionary_pattern, either_pattern,
            fraction_pattern, list_pattern, range_pattern, set_cons_pattern, set_pattern,
            signature_pattern, string_pattern, symbol_pattern, wildcard,
        },
        env::Address,
        object::{fraction::Fraction, integer::Integer, Dictionary, Range, Set, Symbol},
    };

    use super::*;

    #[test]
    fn two_args() {
        let patterns = [
            list_pattern(vec![symbol_pattern("a")]),
            list_pattern(vec![symbol_pattern("b")]),
        ];

        let args = [
            Object::List(List {
                list: vec![(Object::Integer(Integer::from(1)), Address::default())],
            }),
            Object::List(List {
                list: vec![(Object::Integer(Integer::from(2)), Address::default())],
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
        let pattern = cons_pattern(symbol_pattern("first"), symbol_pattern("most"));

        let value = Object::List(List::from(vec![Object::Integer(Integer::from(4))]));

        assert_eq!(
            match_(&pattern, &value),
            Some(Match::from(vec![
                (String::from("first"), Object::Integer(Integer::from(4))),
                (String::from("most"), Object::empty_list()),
            ])),
        );
    }

    #[test]
    fn unmatch_different_value() {
        let patterns = [symbol_pattern("a"), symbol_pattern("a")];

        let values = [
            Object::Integer(Integer::from(1)),
            Object::Integer(Integer::from(2)),
        ];

        assert_eq!(match_call(&patterns, &values), None);
    }

    #[test]
    fn match_empty_dict() {
        let pattern = dictionary_pattern(vec![], true);
        let value = Object::Dictionary(Dictionary::from(vec![]));

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn match_single_pair_dict() {
        let pattern =
            dictionary_pattern(vec![(string_pattern("foo"), symbol_pattern("bar"))], true);

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
        let pattern = dictionary_pattern(vec![(wildcard(), symbol_pattern("a"))], true);

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
        let pattern = dictionary_pattern(vec![(string_pattern("foo"), symbol_pattern("a"))], true);

        let value = Object::Dictionary(Dictionary::from(vec![
            (Object::String("foo".into()), Object::Integer(5.into())),
            (Object::String("bar".into()), Object::Integer(6.into())),
        ]));

        assert_eq!(match_(&pattern, &value), None,);
    }

    #[test]
    fn dict_composite_key() {
        let pattern = dictionary_pattern(
            vec![(list_pattern(vec![wildcard()]), dec_integer_pattern("10"))],
            true,
        );

        let value = Object::Dictionary(Dictionary::from(vec![(
            Object::List(List::from(vec![Object::Integer(1.into())])),
            Object::Integer(10.into()),
        )]));

        assert_eq!(match_(&pattern, &value), empty_match());
    }

    #[test]
    fn incomplete_dict() {
        let pattern = dictionary_pattern(
            vec![(dec_integer_pattern("5"), dec_integer_pattern("10"))],
            false,
        );

        let value = Object::Dictionary(Dictionary::from(vec![
            (Object::Integer(5.into()), Object::Integer(10.into())),
            (Object::String("foo".into()), Object::Integer(11.into())),
        ]));

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn incomplete_list() {
        let pattern = list_pattern(vec![wildcard(), ad_infinitum()]);

        let value = Object::List(vec![Object::Integer(1.into()), Object::Integer(2.into())].into());

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn range_() {
        let pattern = range_pattern(symbol_pattern("a"), symbol_pattern("b"));

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
        let pattern = set_cons_pattern(symbol_pattern("some"), symbol_pattern("most"));

        let value = Object::Set(vec![Object::Integer(0.into()), Object::Integer(1.into())].into());

        assert_eq!(
            match_(&pattern, &value),
            join(
                single_match("some", &Object::Integer(0.into())),
                single_match(
                    "most",
                    &Object::Set(vec![Object::Integer(1.into()),].into())
                ),
            ),
        );
    }

    #[test]
    fn fraction_() {
        let pattern = fraction_pattern(dec_integer_pattern("1"), symbol_pattern("denom"));

        let value = Object::Fraction(Fraction::new(1.into(), 2.into()));

        assert_eq!(
            match_(&pattern, &value),
            single_match("denom", &Object::Integer(2.into()))
        );
    }

    #[test]
    fn full_set() {
        let pattern = set_pattern(vec![wildcard(), wildcard()]);

        let value = Object::Set(Set::from(vec![
            Object::Integer(0.into()),
            Object::Integer(1.into()),
        ]));

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn match_property() {
        let pattern = signature_pattern(wildcard(), symbol_pattern("String"));

        let value = Object::String("".into());

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn unsatisfied_constraint() {
        let pattern = signature_pattern(wildcard(), symbol_pattern("String"));

        let value = Object::Integer(0.into());

        assert_eq!(match_(&pattern, &value), None);
    }

    #[test]
    fn symbol_with_property() {
        let pattern = signature_pattern(symbol_pattern("a"), symbol_pattern("Real"));

        let value = Object::Symbol(Symbol::new("a".into(), "Real".into()));

        assert_eq!(
            match_(&pattern, &value),
            single_match("a", &Object::Symbol(Symbol::new("a".into(), "Real".into())),)
        );
    }

    #[test]
    fn string_cons() {
        let pattern = cons_pattern(symbol_pattern("first"), symbol_pattern("tail"));

        let value = Object::String("foo".into());

        assert_eq!(
            match_(&pattern, &value),
            join(
                single_match("first", &Object::Char('f'.into())),
                single_match("tail", &Object::String("oo".into())),
            ),
        );
    }

    #[test]
    fn signature_conjunction() {
        let pattern = signature_pattern(
            symbol_pattern("n"),
            either_pattern(symbol_pattern("Float"), symbol_pattern("Integer")),
        );

        let value = Object::Integer(0.into());

        assert_eq!(match_(&pattern, &value), single_match("n", &value),);
    }

    #[test]
    fn pattern_conjunction() {
        let pattern = either_pattern(
            list_pattern(vec![symbol_pattern("val")]),
            set_pattern(vec![symbol_pattern("val")]),
        );

        let value = Object::Set(Set::from(vec![Object::Integer(5.into())]));

        assert_eq!(
            match_(&pattern, &value),
            single_match("val", &Object::Integer(5.into())),
        );
    }

    #[test]
    fn unmatch_set_cons() {
        let pattern = set_pattern(vec![]);

        let value = Object::Set(Set::from(vec![Object::Integer(0.into())]));

        assert_eq!(match_(&pattern, &value), None,);
    }
}
