pub trait Operable {
    fn equality(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn product(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn sum(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn substraction(&self, _other: _Object) -> Result<_Object, ()> {
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
    pub fn equality(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Symbol(symbol) => symbol.equality(other),
            _ => todo!(),
        }
    }

    pub fn product(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.product(other),
            _ => todo!(),
        }
    }

    pub fn sum(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.sum(other),
            _ => todo!(),
        }
    }

    pub fn substraction(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.substraction(other),
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Bool {
    val: bool,
}

impl From<bool> for Bool {
    fn from(val: bool) -> Self {
        Self { val }
    }
}

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

impl From<i64> for Integer {
    fn from(val: i64) -> Self {
        Self { val }
    }
}

impl Operable for Integer {
    fn sum(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val + val)))
            }
            _ => Err(()),
        }
    }

    fn substraction(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val - val)))
            }
            _ => Err(()),
        }
    }

    fn product(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val * val)))
            }
            _ => Err(()),
        }
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

impl Operable for Symbol {
    fn equality(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Symbol(symbol) => Ok(_Object::Boolean(Bool::from(self.val == symbol.val))),
            _ => todo!(),
        }
    }
}
