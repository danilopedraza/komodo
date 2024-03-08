use std::fmt;

macro_rules! default_infix_method {
    ($ident:ident) => {
        fn $ident(&self, _other: Object) -> Result<Object, ()> {
            Err(())
        }
    };
}

macro_rules! default_infix_methods {
    () => {};
    ($($ident:ident),*) => {
        pub trait InfixOperable {
            $(
                default_infix_method!($ident);
            )*
        }
    };
}

default_infix_methods!(
    bitwise_and,
    bitwise_or,
    bitwise_xor,
    equality,
    greater,
    greater_equal,
    left_shift,
    less,
    less_equal,
    logic_and,
    logic_or,
    modulo,
    neq,
    over,
    pow,
    product,
    right_shift,
    sum,
    substraction
);

macro_rules! default_prefix_method {
    ($ident:ident) => {
        fn $ident(&self) -> Result<Object, ()> {
            Err(())
        }
    };
}

macro_rules! default_prefix_methods {
    () => {};
    ($($ident:ident),*) => {
        pub trait PrefixOperable {
            $(
                default_prefix_method!($ident);
            )*
        }
    };
}

default_prefix_methods!(bitwise_not, logic_not, inverse);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(Bool),
    Char(Char),
    ExtensionSet(ExtensionSet),
    // ComprehensionSet(ComprehensionSet),
    Integer(Integer),
    String(MyString),
    Symbol(Symbol),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Boolean(boolean) => boolean.fmt(f),
            Object::Char(chr) => chr.fmt(f),
            Object::ExtensionSet(es) => es.fmt(f),
            Object::Integer(int) => int.fmt(f),
            Object::String(str) => str.fmt(f),
            Object::Symbol(s) => s.fmt(f),
        }
    }
}

macro_rules! derived_object_infix_trait {
    ($ident:ident) => {
        pub fn $ident(&self, other: Object) -> Result<Object, ()> {
            match self {
                Self::Boolean(left) => left.$ident(other),
                Self::Char(left) => left.$ident(other),
                Self::ExtensionSet(left) => left.$ident(other),
                Self::Integer(left) => left.$ident(other),
                Self::String(left) => left.$ident(other),
                Self::Symbol(left) => left.$ident(other),
            }
        }
    };
}

macro_rules! derived_object_infix_traits {
    () => {};
    ($($ident:ident),*) => {
        impl Object {
            $(
                derived_object_infix_trait!($ident);
            )*
        }
    };
}

derived_object_infix_traits!(
    bitwise_and,
    bitwise_or,
    bitwise_xor,
    equality,
    greater,
    greater_equal,
    left_shift,
    less,
    less_equal,
    logic_and,
    logic_or,
    modulo,
    neq,
    over,
    pow,
    product,
    right_shift,
    sum,
    substraction
);

macro_rules! derived_object_prefix_trait {
    ($ident:ident) => {
        pub fn $ident(&self) -> Result<Object, ()> {
            match self {
                Self::Boolean(left) => left.$ident(),
                Self::Char(left) => left.$ident(),
                Self::ExtensionSet(left) => left.$ident(),
                Self::Integer(left) => left.$ident(),
                Self::String(left) => left.$ident(),
                Self::Symbol(left) => left.$ident(),
            }
        }
    };
}

macro_rules! derived_object_prefix_traits {
    () => {};
    ($($ident:ident),*) => {
        impl Object {
            $(
                derived_object_prefix_trait!($ident);
            )*
        }
    };
}

derived_object_prefix_traits!(bitwise_not, logic_not, inverse);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bool {
    pub val: bool,
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = if self.val { "true" } else { "false" };

        write!(f, "{}", text)
    }
}

impl InfixOperable for Bool {
    fn logic_and(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Boolean(boolean) => Ok(Object::Boolean(Bool::from(self.val && boolean.val))),
            _ => todo!(),
        }
    }

    fn logic_or(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Boolean(boolean) => Ok(Object::Boolean(Bool::from(self.val || boolean.val))),
            _ => todo!(),
        }
    }
}

impl PrefixOperable for Bool {
    fn logic_not(&self) -> Result<Object, ()> {
        Ok(Object::Boolean(Bool::from(!self.val)))
    }
}

impl From<bool> for Bool {
    fn from(val: bool) -> Self {
        Self { val }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Char {
    val: char,
}

impl InfixOperable for Char {}
impl PrefixOperable for Char {}

impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl From<char> for Char {
    fn from(val: char) -> Self {
        Char { val }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExtensionSet {
    list: Vec<Object>,
}

impl InfixOperable for ExtensionSet {}
impl PrefixOperable for ExtensionSet {}

impl From<Vec<Object>> for ExtensionSet {
    fn from(list: Vec<Object>) -> Self {
        Self { list }
    }
}

impl fmt::Display for ExtensionSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = self
            .list
            .iter()
            .map(|obj| obj.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "[{}]", list)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ComprehensionSet {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Integer {
    val: i64,
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl From<i64> for Integer {
    fn from(val: i64) -> Self {
        Self { val }
    }
}

impl From<&str> for Integer {
    fn from(val: &str) -> Self {
        Self {
            val: val.parse().unwrap(),
        }
    }
}

impl InfixOperable for Integer {
    fn bitwise_and(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val & val))),
            _ => Err(()),
        }
    }

    fn bitwise_or(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val | val))),
            _ => Err(()),
        }
    }

    fn bitwise_xor(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val ^ val))),
            _ => Err(()),
        }
    }

    fn greater(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val > val))),
            _ => Err(()),
        }
    }

    fn greater_equal(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val >= val))),
            _ => Err(()),
        }
    }

    fn left_shift(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val << val))),
            _ => Err(()),
        }
    }

    fn less(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val < val))),
            _ => Err(()),
        }
    }

    fn less_equal(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val <= val))),
            _ => Err(()),
        }
    }

    fn modulo(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val % val))),
            _ => Err(()),
        }
    }

    fn neq(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val != val))),
            _ => Ok(Object::Boolean(Bool::from(true))),
        }
    }

    fn over(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val / val))),
            _ => Err(()),
        }
    }

    fn pow(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(
                self.val.pow(val.try_into().unwrap()),
            ))),
            _ => Err(()),
        }
    }

    fn right_shift(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val >> val))),
            _ => Err(()),
        }
    }

    fn sum(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val + val))),
            _ => Err(()),
        }
    }

    fn substraction(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val - val))),
            _ => Err(()),
        }
    }

    fn product(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(self.val * val))),
            _ => Err(()),
        }
    }
}

impl PrefixOperable for Integer {
    fn bitwise_not(&self) -> Result<Object, ()> {
        Ok(Object::Integer(Integer::from(!self.val)))
    }

    fn inverse(&self) -> Result<Object, ()> {
        Ok(Object::Integer(Integer::from(-self.val)))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MyString {
    val: String,
}

impl InfixOperable for MyString {}
impl PrefixOperable for MyString {}

impl fmt::Display for MyString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl From<&str> for MyString {
    fn from(val: &str) -> Self {
        MyString {
            val: val.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    val: String,
}

impl From<&str> for Symbol {
    fn from(val: &str) -> Self {
        Self { val: val.into() }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl InfixOperable for Symbol {
    fn equality(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Symbol(symbol) => Ok(Object::Boolean(Bool::from(self.val == symbol.val))),
            _ => todo!(),
        }
    }
}

impl PrefixOperable for Symbol {}
