//! Shared suffixes. Reads borrow visible contents; writes detach only that suffix.
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    rc::Rc,
};

pub trait Storage: Clone {
    type Slice: ?Sized + Eq + Ord + Hash;
    fn suffix(&self, start: usize) -> &Self::Slice;
    fn copy_suffix(&self, start: usize) -> Self;
}
impl<T: Clone + Eq + Ord + Hash> Storage for Vec<T> {
    type Slice = [T];
    fn suffix(&self, start: usize) -> &[T] {
        &self[start..]
    }
    fn copy_suffix(&self, start: usize) -> Self {
        self[start..].to_vec()
    }
}
impl Storage for String {
    type Slice = str;
    fn suffix(&self, start: usize) -> &str {
        &self[start..]
    }
    fn copy_suffix(&self, start: usize) -> Self {
        self[start..].to_owned()
    }
}
#[derive(Clone, Debug)]
pub struct View<T: Storage>(pub(super) Rc<T>, usize);
impl<T: Storage> From<T> for View<T> {
    fn from(value: T) -> Self {
        Self(Rc::new(value), 0)
    }
}
impl<T: Storage> View<T> {
    pub fn suffix(&self, start: usize) -> Self {
        let start = self.1 + start;
        self.0.suffix(start); // Validate bounds and UTF-8 boundaries.
        Self(self.0.clone(), start)
    }
    pub fn into_owned(self) -> T {
        if self.1 == 0 {
            Rc::unwrap_or_clone(self.0)
        } else {
            self.0.copy_suffix(self.1)
        }
    }
    fn owned_mut(&mut self) -> &mut T {
        if self.1 != 0 {
            self.0 = Rc::new(self.0.copy_suffix(self.1));
            self.1 = 0;
        }
        Rc::make_mut(&mut self.0)
    }
}
impl<T: Storage> Deref for View<T> {
    type Target = T::Slice;
    fn deref(&self) -> &Self::Target {
        self.0.suffix(self.1)
    }
}
impl<T: Clone + Eq + Ord + Hash> DerefMut for View<Vec<T>> {
    fn deref_mut(&mut self) -> &mut [T] {
        self.owned_mut().as_mut_slice()
    }
}
impl<T: Clone + Eq + Ord + Hash> IntoIterator for View<Vec<T>> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.into_owned().into_iter()
    }
}
impl<'a, T: Clone + Eq + Ord + Hash> IntoIterator for &'a View<Vec<T>> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl<T: Storage> PartialEq for View<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}
impl<T: Storage> Eq for View<T> {}
impl<T: Storage> PartialOrd for View<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: Storage> Ord for View<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        (**self).cmp(&**other)
    }
}
impl<T: Storage> Hash for View<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}
impl fmt::Display for View<String> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}
