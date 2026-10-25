//! Shared runtime identifiers. Weak entries do not keep identifier allocations alive.
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

thread_local! {
    static NAMES: RefCell<(HashMap<String, Weak<str>>, usize)> = RefCell::default();
}

pub fn intern(name: &str) -> Rc<str> {
    NAMES.with(|names| {
        let mut state = names.borrow_mut();
        let (names, insertions) = &mut *state;
        if let Some(value) = names.get(name).and_then(Weak::upgrade) {
            return value;
        }
        *insertions += 1;
        if *insertions == 256 {
            names.retain(|_, value| value.strong_count() != 0);
            *insertions = 0;
        }
        let value: Rc<str> = Rc::from(name);
        names.insert(name.to_owned(), Rc::downgrade(&value));
        value
    })
}
