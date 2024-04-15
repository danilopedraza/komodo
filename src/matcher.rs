use std::iter::zip;

use crate::{
    ast::ASTNode,
    env::Environment,
    exec,
    object::{ExtensionList, Object},
};

#[derive(Debug, PartialEq, Eq)]
pub enum Match {
    Match(Vec<(String, Object)>),
    NotAMatch,
}

pub fn match_call(patterns: &[ASTNode], args: &[Object]) -> Match {
    match_list(patterns, args)
}

fn match_list(patterns: &[ASTNode], vals: &[Object]) -> Match {
    let mut map = vec![];

    for (pattern, arg) in zip(patterns, vals) {
        if let Match::Match(v) = match_(pattern, arg) {
            map.extend(v);
        } else {
            return Match::NotAMatch;
        }
    }

    Match::Match(map)
}

fn match_(pattern: &ASTNode, val: &Object) -> Match {
    match pattern {
        ASTNode::Wildcard => empty_match(),
        ASTNode::Symbol(s) => single_match(s, val),
        ASTNode::ExtensionList(l) => match_extension_list(l, val),
        ASTNode::Prepend(first, most) => match_prefix_crop(first, most, val),
        _ => match_constant(pattern, val),
    }
}

fn single_match(name: &str, val: &Object) -> Match {
    Match::Match(vec![(name.to_string(), val.clone())])
}

fn empty_match() -> Match {
    Match::Match(vec![])
}

fn match_extension_list(pattern: &[ASTNode], val: &Object) -> Match {
    match val {
        Object::ExtensionList(ExtensionList { list: al }) => match_list(pattern, al),
        _ => Match::NotAMatch,
    }
}

fn match_prefix_crop(first: &ASTNode, most: &ASTNode, val: &Object) -> Match {
    match val {
        Object::ExtensionList(ExtensionList { list }) if list.len() > 0 => {
            let first_match = match_(first, &list[0]);
            
            let last_list = Object::ExtensionList(ExtensionList::from(list[1..].to_owned()));
            let last_match = match_(most, &last_list);

            match (first_match, last_match) {
                (Match::Match(m1), Match::Match(m2)) => {
                    let mut m = vec![];
                    m.extend(m1);
                    m.extend(m2);
                    Match::Match(m)
                },
                _ => Match::NotAMatch,
            }
        },
        _ => Match::NotAMatch,
    }
}

fn match_constant(pattern: &ASTNode, val: &Object) -> Match {
    if exec(pattern, &mut Environment::default()).unwrap() == *val {
        empty_match()
    } else {
        Match::NotAMatch
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
            Match::Match(vec![
                (String::from("a"), Object::Integer(Integer::from(1))),
                (String::from("b"), Object::Integer(Integer::from(2)))
            ])
        );
    }

    #[test]
    fn list_prefix() {
        let pattern = ASTNode::Prepend(
            Box::new(ASTNode::Symbol(String::from("first"))),
            Box::new(ASTNode::Symbol(String::from("most"))),
        );

        let value =
            Object::ExtensionList(ExtensionList::from(vec![Object::Integer(Integer::from(4))]));

        assert_eq!(
            match_(&pattern, &value),
            Match::Match(vec![
                (String::from("first"), Object::Integer(Integer::from(4))),
                (String::from("most"), Object::ExtensionList(ExtensionList::from(vec![]))),
            ]),
        );
    }
}
