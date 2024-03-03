pub trait Operable {
    fn sum(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum _Object {
    Boolean(Bool),
    Char(Char),
    ExtensionSet(ExtensionSet),
    ComprehensionSet(ComprehensionSet),
    Integer(Integer),
    String(MyString),
    Symbol(Symbol),
}

impl _Object {
    pub fn sum(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.sum(other),
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Bool {}

#[derive(Debug, PartialEq, Eq)]
struct Char {}

#[derive(Debug, PartialEq, Eq)]
pub struct ExtensionSet {
    list: Vec<_Object>,
}

impl ExtensionSet {
    pub fn new(list: Vec<_Object>) -> _Object {
        _Object::ExtensionSet(Self { list })
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ComprehensionSet {}

#[derive(Debug, PartialEq, Eq)]
pub struct Integer {
    val: i64,
}

impl Integer {
    pub fn new(val: &str) -> _Object {
        _Object::Integer(Self {
            val: val.parse().unwrap(),
        })
    }
}

impl Operable for Integer {
    fn sum(&self, other: _Object) -> Result<_Object, ()> {
        Ok(Self::new("1"))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct MyString {}

#[derive(Debug, PartialEq, Eq)]
pub struct Symbol {
    val: String,
}

impl Symbol {
    pub fn new(val: &str) -> _Object {
        _Object::Symbol(Self {
            val: val.to_string(),
        })
    }
}
