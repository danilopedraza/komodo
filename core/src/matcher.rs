use crate::{
    ast::{Pattern, TypeHint},
    exec::eval_constant,
    object::{List, Object},
};
use std::collections::BTreeMap;

/// Source shape remains available for binding and capture analysis.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RuntimePattern(std::rc::Rc<RuntimePatternData>);
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct RuntimePatternData {
    source: Pattern,
    compiled: CompiledPattern,
}
impl From<&Pattern> for RuntimePattern {
    fn from(source: &Pattern) -> Self {
        Self(std::rc::Rc::new(RuntimePatternData {
            source: source.clone(),
            compiled: source.into(),
        }))
    }
}
impl std::ops::Deref for RuntimePattern {
    type Target = Pattern;
    fn deref(&self) -> &Pattern {
        &self.0.source
    }
}
impl RuntimePattern {
    pub fn matches(&self, value: &Object) -> Option<Match> {
        self.0.compiled.matches(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Match(pub BTreeMap<String, Object>);
impl From<Vec<(String, Object)>> for Match {
    fn from(values: Vec<(String, Object)>) -> Self {
        Self(values.into_iter().collect())
    }
}
impl From<(String, Object)> for Match {
    fn from(value: (String, Object)) -> Self {
        Self::from(vec![value])
    }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CompiledPattern {
    Constant(Object),
    Dictionary {
        pairs: Vec<(CompiledPattern, CompiledPattern)>,
        complete: bool,
    },
    List {
        list: Vec<CompiledPattern>,
    },
    Set {
        list: Vec<CompiledPattern>,
    },
    ListCons {
        first: Box<CompiledPattern>,
        tail: Box<CompiledPattern>,
    },
    SetCons {
        some: Box<CompiledPattern>,
        most: Box<CompiledPattern>,
    },
    Symbol {
        name: String,
    },
    Tuple {
        list: Vec<CompiledPattern>,
    },
    Range {
        start: Box<CompiledPattern>,
        end: Box<CompiledPattern>,
    },
    Fraction {
        numer: Box<CompiledPattern>,
        denom: Box<CompiledPattern>,
    },
    Signature {
        pattern: Box<CompiledPattern>,
        constraint: TypeHint,
    },
    Either {
        lhs: Box<CompiledPattern>,
        rhs: Box<CompiledPattern>,
    },
    Wildcard,
    AdInfinitum,
}

impl From<&Pattern> for CompiledPattern {
    fn from(pattern: &Pattern) -> Self {
        match pattern {
            Pattern::Dictionary { pairs, complete } => Self::Dictionary {
                pairs: pairs.iter().map(|(k, v)| (k.into(), v.into())).collect(),
                complete: *complete,
            },
            Pattern::List { list } => Self::List {
                list: list.iter().map(Into::into).collect(),
            },
            Pattern::Set { list } => Self::Set {
                list: list.iter().map(Into::into).collect(),
            },
            Pattern::Tuple { list } => Self::Tuple {
                list: list.iter().map(Into::into).collect(),
            },
            Pattern::ListCons { first, tail } => Self::ListCons {
                first: Box::new(first.as_ref().into()),
                tail: Box::new(tail.as_ref().into()),
            },
            Pattern::SetCons { some, most } => Self::SetCons {
                some: Box::new(some.as_ref().into()),
                most: Box::new(most.as_ref().into()),
            },
            Pattern::Range { start, end } => Self::Range {
                start: Box::new(start.as_ref().into()),
                end: Box::new(end.as_ref().into()),
            },
            Pattern::Fraction { numer, denom } => Self::Fraction {
                numer: Box::new(numer.as_ref().into()),
                denom: Box::new(denom.as_ref().into()),
            },
            Pattern::Signature {
                pattern,
                constraint,
            } => Self::Signature {
                pattern: Box::new(pattern.as_ref().into()),
                constraint: constraint.clone(),
            },
            Pattern::Either { lhs, rhs } => Self::Either {
                lhs: Box::new(lhs.as_ref().into()),
                rhs: Box::new(rhs.as_ref().into()),
            },
            Pattern::Symbol { name } => Self::Symbol { name: name.clone() },
            Pattern::Constant(c) => Self::Constant(eval_constant(c)),
            Pattern::Wildcard => Self::Wildcard,
            Pattern::AdInfinitum => Self::AdInfinitum,
        }
    }
}
fn satisfies(obj: &Object, constraint: &TypeHint) -> bool {
    match constraint {
        TypeHint::Simple(name) => obj.has_property(name),
        TypeHint::Either { lhs, rhs } => satisfies(obj, lhs) || satisfies(obj, rhs),
    }
}
#[cfg(test)]
pub fn match_(pattern: &Pattern, val: &Object) -> Option<Match> {
    CompiledPattern::from(pattern).matches(val)
}
fn merge(into: &mut BTreeMap<String, Object>, values: Match) -> Option<()> {
    for (name, value) in values.0 {
        bind(into, &name, &value)?;
    }
    Some(())
}
fn bind(into: &mut BTreeMap<String, Object>, name: &str, value: &Object) -> Option<()> {
    if let Some(previous) = into.get(name) {
        return (previous == value).then_some(());
    }
    into.insert(name.to_owned(), value.clone());
    Some(())
}
impl CompiledPattern {
    pub fn matches(&self, value: &Object) -> Option<Match> {
        let mut bindings = BTreeMap::new();
        self.accumulate(value, &mut bindings)?;
        Some(Match(bindings))
    }
    fn sequence(
        patterns: &[Self],
        values: &[Object],
        out: &mut BTreeMap<String, Object>,
    ) -> Option<()> {
        if patterns.len() != values.len() {
            return None;
        }
        for (pattern, value) in patterns.iter().zip(values) {
            pattern.accumulate(value, out)?;
        }
        Some(())
    }
    fn accumulate(&self, value: &Object, out: &mut BTreeMap<String, Object>) -> Option<()> {
        match (self, value) {
            (Self::Constant(c), value) => (c == value).then_some(()),
            (Self::Wildcard, _) => Some(()),
            (Self::Symbol { name }, value) => bind(out, name, value),
            (
                Self::Signature {
                    pattern,
                    constraint,
                },
                value,
            ) => {
                if !satisfies(value, constraint) {
                    return None;
                }
                pattern.accumulate(value, out)
            }
            // Alternatives choose locally before merging with enclosing bindings.
            (Self::Either { lhs, rhs }, value) => {
                merge(out, lhs.matches(value).or_else(|| rhs.matches(value))?)
            }
            (Self::List { list: patterns }, Object::List(list)) => {
                if matches!(patterns.last(), Some(Self::AdInfinitum)) {
                    let patterns = &patterns[..patterns.len() - 1];
                    Self::sequence(patterns, list.list.get(..patterns.len())?, out)
                } else {
                    Self::sequence(patterns, &list.list, out)
                }
            }
            (Self::Tuple { list }, Object::Tuple(tuple)) => Self::sequence(list, &tuple.list, out),
            (Self::Set { list }, Object::Set(set)) => {
                if list.len() != set.len() {
                    return None;
                }
                for (p, v) in list.iter().zip(set.iter()) {
                    p.accumulate(v, out)?;
                }
                Some(())
            }
            (Self::ListCons { first, tail }, Object::List(list)) => {
                first.accumulate(list.list.first()?, out)?;
                tail.accumulate(
                    &Object::List(List {
                        list: list.list.suffix(1),
                    }),
                    out,
                )
            }
            (Self::ListCons { first, tail }, Object::String(string)) => {
                let (head, rest) = string.cons_format()?;
                first.accumulate(&Object::Char(head.into()), out)?;
                tail.accumulate(&Object::String(rest), out)
            }
            (Self::Dictionary { pairs, complete }, Object::Dictionary(dict)) => {
                if *complete && pairs.len() != dict.dict.len() {
                    return None;
                }
                for (key, value) in pairs {
                    let matched = if let Self::Constant(key) = key {
                        value.matches(dict.dict.get(key)?)?
                    } else {
                        dict.dict.iter().find_map(|(k, v)| {
                            let mut local = key.matches(k)?;
                            value.accumulate(v, &mut local.0)?;
                            Some(local)
                        })?
                    };
                    merge(out, matched)?;
                }
                Some(())
            }
            (Self::SetCons { some, most }, Object::Set(set)) => {
                for candidate in set.iter() {
                    let Some(mut local) = some.matches(candidate) else {
                        continue;
                    };
                    let mut rest = set.clone();
                    rest.remove(candidate);
                    if most.accumulate(&rest.into(), &mut local.0).is_some() {
                        return merge(out, local);
                    }
                }
                None
            }
            (Self::Range { start, end }, Object::Range(range)) => {
                start.accumulate(&Object::Integer(range.start.clone()), out)?;
                end.accumulate(&Object::Integer(range.end.clone()), out)
            }
            (Self::Fraction { numer, denom }, Object::Fraction(frac)) => {
                numer.accumulate(&Object::Integer(frac.numer()), out)?;
                denom.accumulate(&Object::Integer(frac.denom()), out)
            }
            _ => None,
        }
    }
}
#[cfg(test)]
fn empty_match() -> Option<Match> {
    Some(Match(BTreeMap::new()))
}
#[cfg(test)]
fn single_match(name: &str, value: &Object) -> Option<Match> {
    Some(Match::from((name.into(), value.clone())))
}
#[cfg(test)]
fn join(a: Option<Match>, b: Option<Match>) -> Option<Match> {
    let mut a = a?;
    merge(&mut a.0, b?)?;
    Some(a)
}
#[cfg(test)]
fn match_sequence(patterns: &[Pattern], values: &[Object]) -> Option<Match> {
    let mut out = BTreeMap::new();
    CompiledPattern::sequence(
        &patterns.iter().map(Into::into).collect::<Vec<_>>(),
        values,
        &mut out,
    )?;
    Some(Match(out))
}
#[cfg(test)]
mod tests {
    use crate::{
        ast::tests::{
            ad_infinitum, cons_pattern, dec_integer_pattern, dictionary_pattern, either_pattern,
            either_type_hint, fraction_pattern, list_pattern, range_pattern, set_cons_pattern,
            set_pattern, signature_pattern, simple_type_hint, string_pattern, symbol_pattern,
            wildcard,
        },
        object::{fraction::Fraction, integer::Integer, Dictionary, Range, Set, Symbol},
    };

    use super::*;

    #[test]
    fn short_prefix_fails_without_panicking() {
        assert_eq!(
            match_(
                &list_pattern(vec![wildcard(), wildcard(), ad_infinitum()]),
                &Object::List(vec![Object::true_obj()].into())
            ),
            None
        );
    }

    #[test]
    fn failed_alternative_does_not_leak_bindings() {
        let pattern = either_pattern(
            list_pattern(vec![symbol_pattern("x"), dec_integer_pattern("0")]),
            list_pattern(vec![wildcard(), symbol_pattern("x")]),
        );
        let value = Object::List(vec![Object::Integer(1.into()), Object::Integer(2.into())].into());
        assert_eq!(
            match_(&pattern, &value),
            single_match("x", &Object::Integer(2.into()))
        );
    }

    #[test]
    fn dictionary_keeps_first_local_candidate_on_outer_conflict() {
        let pattern = list_pattern(vec![
            symbol_pattern("x"),
            dictionary_pattern(vec![(wildcard(), symbol_pattern("x"))], false),
        ]);
        let dict = Dictionary::from(vec![
            (Object::Integer(0.into()), Object::Integer(1.into())),
            (Object::Integer(1.into()), Object::Integer(2.into())),
        ]);
        let value = Object::List(vec![Object::Integer(2.into()), Object::Dictionary(dict)].into());
        assert_eq!(match_(&pattern, &value), None);
    }

    #[test]
    fn set_cons_retries_after_tail_failure() {
        let pattern = set_cons_pattern(
            symbol_pattern("x"),
            set_pattern(vec![dec_integer_pattern("1")]),
        );
        let value = Object::Set(vec![Object::Integer(1.into()), Object::Integer(2.into())].into());
        assert_eq!(
            match_(&pattern, &value),
            single_match("x", &Object::Integer(2.into()))
        );
    }

    #[test]
    fn two_args() {
        let patterns = [
            list_pattern(vec![symbol_pattern("a")]),
            list_pattern(vec![symbol_pattern("b")]),
        ];

        let args = [
            Object::List(List {
                list: vec![Object::Integer(Integer::from(1))].into(),
            }),
            Object::List(List {
                list: vec![Object::Integer(Integer::from(2))].into(),
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

        assert_eq!(match_sequence(&patterns, &values), None);
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
        let pattern = signature_pattern(wildcard(), simple_type_hint("String"));

        let value = Object::String("".into());

        assert_eq!(match_(&pattern, &value), empty_match(),);
    }

    #[test]
    fn unsatisfied_constraint() {
        let pattern = signature_pattern(wildcard(), simple_type_hint("String"));

        let value = Object::Integer(0.into());

        assert_eq!(match_(&pattern, &value), None);
    }

    #[test]
    fn symbol_with_property() {
        let pattern = signature_pattern(symbol_pattern("a"), simple_type_hint("Real"));

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
            either_type_hint(simple_type_hint("Float"), simple_type_hint("Integer")),
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
