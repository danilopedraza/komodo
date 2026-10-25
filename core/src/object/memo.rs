//! Persistent AVL cache: snapshots share untouched subtrees on insertion.
use super::Object;
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    rc::Rc,
};
#[derive(Clone, Debug, Default)]
pub(super) struct Cache(Option<Rc<Node>>);
#[derive(Clone, Debug)]
struct Node {
    key: Object,
    value: Object,
    left: Cache,
    right: Cache,
    height: u32,
    len: usize,
}
impl Cache {
    pub fn len(&self) -> usize {
        self.0.as_ref().map_or(0, |n| n.len)
    }
    fn height(&self) -> u32 {
        self.0.as_ref().map_or(0, |n| n.height)
    }
    fn node(key: Object, value: Object, left: Self, right: Self) -> Self {
        let height = 1 + left.height().max(right.height());
        let len = 1 + left.len() + right.len();
        Self(Some(Rc::new(Node {
            key,
            value,
            left,
            right,
            height,
            len,
        })))
    }
    fn balance(key: Object, value: Object, left: Self, right: Self) -> Self {
        if left.height() > right.height() + 1 {
            let l = left.0.as_ref().unwrap();
            if l.left.height() >= l.right.height() {
                return Self::node(
                    l.key.clone(),
                    l.value.clone(),
                    l.left.clone(),
                    Self::node(key, value, l.right.clone(), right),
                );
            }
            let m = l.right.0.as_ref().unwrap();
            return Self::node(
                m.key.clone(),
                m.value.clone(),
                Self::node(
                    l.key.clone(),
                    l.value.clone(),
                    l.left.clone(),
                    m.left.clone(),
                ),
                Self::node(key, value, m.right.clone(), right),
            );
        }
        if right.height() > left.height() + 1 {
            let r = right.0.as_ref().unwrap();
            if r.right.height() >= r.left.height() {
                return Self::node(
                    r.key.clone(),
                    r.value.clone(),
                    Self::node(key, value, left, r.left.clone()),
                    r.right.clone(),
                );
            }
            let m = r.left.0.as_ref().unwrap();
            return Self::node(
                m.key.clone(),
                m.value.clone(),
                Self::node(key, value, left, m.left.clone()),
                Self::node(
                    r.key.clone(),
                    r.value.clone(),
                    m.right.clone(),
                    r.right.clone(),
                ),
            );
        }
        Self::node(key, value, left, right)
    }
    pub fn insert(&mut self, key: Object, value: Object) {
        *self = match &self.0 {
            None => Self::node(key, value, Self::default(), Self::default()),
            Some(n) => match key.cmp(&n.key) {
                Ordering::Equal => Self::node(key, value, n.left.clone(), n.right.clone()),
                Ordering::Less => {
                    let mut left = n.left.clone();
                    left.insert(key, value);
                    Self::balance(n.key.clone(), n.value.clone(), left, n.right.clone())
                }
                Ordering::Greater => {
                    let mut right = n.right.clone();
                    right.insert(key, value);
                    Self::balance(n.key.clone(), n.value.clone(), n.left.clone(), right)
                }
            },
        };
    }
    pub fn get(&self, key: &Object) -> Option<&Object> {
        let mut tree = self;
        while let Some(n) = &tree.0 {
            match key.cmp(&n.key) {
                Ordering::Equal => return Some(&n.value),
                Ordering::Less => tree = &n.left,
                Ordering::Greater => tree = &n.right,
            }
        }
        None
    }
    fn iter(&self) -> Iter<'_> {
        let mut iter = Iter(Vec::new());
        iter.push_left(self);
        iter
    }
}
struct Iter<'a>(Vec<&'a Node>);
impl<'a> Iter<'a> {
    fn push_left(&mut self, mut tree: &'a Cache) {
        while let Some(n) = &tree.0 {
            self.0.push(n);
            tree = &n.left;
        }
    }
}
impl<'a> Iterator for Iter<'a> {
    type Item = (&'a Object, &'a Object);
    fn next(&mut self) -> Option<Self::Item> {
        let n = self.0.pop()?;
        self.push_left(&n.right);
        Some((&n.key, &n.value))
    }
}
impl PartialEq for Cache {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.cmp(other) == Ordering::Equal
    }
}
impl Eq for Cache {}
impl PartialOrd for Cache {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Cache {
    fn cmp(&self, other: &Self) -> Ordering {
        if let (Some(a), Some(b)) = (&self.0, &other.0) {
            if Rc::ptr_eq(a, b) {
                return Ordering::Equal;
            }
        }
        self.iter().cmp(other.iter())
    }
}
impl Hash for Cache {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for pair in self.iter() {
            pair.hash(state);
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn persistent_balancing_and_structural_identity() {
        let mut ascending = Cache::default();
        let mut descending = Cache::default();
        let mut snapshots = Vec::new();
        for i in 0..1000 {
            snapshots.push(ascending.clone());
            ascending.insert(Object::Integer((i).into()), Object::Integer((i + 1).into()));
        }
        for i in (0..1000).rev() {
            descending.insert(Object::Integer((i).into()), Object::Integer((i + 1).into()));
        }
        assert_eq!(ascending, descending);
        assert!(ascending.height() < 15);
        for (i, snapshot) in snapshots.iter().enumerate() {
            assert_eq!(snapshot.get(&Object::Integer((i as i32).into())), None);
            if i > 0 {
                assert_eq!(
                    snapshot.get(&Object::Integer((i as i32 - 1).into())),
                    Some(&Object::Integer((i as i32).into()))
                );
            }
        }
    }
}

#[cfg(test)]
mod persistence_tests {
    use super::*;
    fn value(n: i32) -> Object {
        Object::Integer(n.into())
    }
    fn hash(cache: &Cache) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        cache.hash(&mut hasher);
        hasher.finish()
    }
    #[test]
    fn mixed_insertions_and_updates_match_ordered_map() {
        fn validate(tree: &Cache) {
            if let Some(n) = &tree.0 {
                assert!(n.left.height().abs_diff(n.right.height()) <= 1);
                assert_eq!(n.height, 1 + n.left.height().max(n.right.height()));
                assert_eq!(n.len, 1 + n.left.len() + n.right.len());
                validate(&n.left);
                validate(&n.right);
            }
        }
        let mut tree = Cache::default();
        let mut expected = std::collections::BTreeMap::new();
        for i in 0..1000 {
            let key = (i * 137) % 500;
            expected.insert(key, i);
            tree.insert(value(key), value(i));
            validate(&tree);
            assert!(tree
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .eq(expected.iter().map(|(k, v)| (value(*k), value(*v)))));
        }
    }

    #[test]
    fn replacement_shares_untouched_subtrees_and_keeps_snapshot_values() {
        let mut tree = Cache::default();
        for n in [4, 2, 6, 1, 3, 5, 7] {
            tree.insert(value(n), value(n));
        }
        let old = tree.clone();
        tree.insert(value(7), value(99));
        assert!(Rc::ptr_eq(
            tree.0.as_ref().unwrap().left.0.as_ref().unwrap(),
            old.0.as_ref().unwrap().left.0.as_ref().unwrap()
        ));
        assert_eq!(old.get(&value(7)), Some(&value(7)));
        assert_eq!(tree.get(&value(7)), Some(&value(99)));
        assert_ne!(tree, old);
        let mut different_shape = Cache::default();
        for n in (1..=7).rev() {
            different_shape.insert(value(n), value(n));
        }
        assert_eq!(old, different_shape);
        assert_eq!(hash(&old), hash(&different_shape));
    }
}
