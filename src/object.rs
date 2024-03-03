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

#[derive(Debug, PartialEq, Eq)]
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

impl Object {
    pub fn bitwise_and(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Boolean(left) => left.bitwise_and(other),
            Self::Char(left) => left.bitwise_and(other),
            Self::ExtensionSet(left) => left.bitwise_and(other),
            Self::Integer(left) => left.bitwise_and(other),
            Self::String(left) => left.bitwise_and(other),
            Self::Symbol(left) => left.bitwise_and(other),
        }
    }

    pub fn bitwise_or(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.bitwise_or(other),
            _ => todo!(),
        }
    }

    pub fn bitwise_xor(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.bitwise_xor(other),
            _ => todo!(),
        }
    }

    pub fn equality(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Symbol(symbol) => symbol.equality(other),
            _ => todo!(),
        }
    }

    pub fn greater(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.greater(other),
            _ => todo!(),
        }
    }

    pub fn greater_equal(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.greater_equal(other),
            _ => todo!(),
        }
    }

    pub fn left_shift(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.left_shift(other),
            _ => todo!(),
        }
    }

    pub fn less(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.less(other),
            _ => todo!(),
        }
    }

    pub fn less_equal(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.less_equal(other),
            _ => todo!(),
        }
    }

    pub fn logic_and(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Boolean(boolean) => boolean.logic_and(other),
            _ => todo!(),
        }
    }

    pub fn logic_or(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Boolean(boolean) => boolean.logic_or(other),
            _ => todo!(),
        }
    }

    pub fn modulo(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.modulo(other),
            _ => todo!(),
        }
    }

    pub fn neq(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.neq(other),
            Self::Boolean(boolean) => boolean.neq(other),
            _ => todo!(),
        }
    }

    pub fn over(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.over(other),
            _ => todo!(),
        }
    }

    pub fn pow(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.pow(other),
            _ => todo!(),
        }
    }

    pub fn product(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.product(other),
            _ => todo!(),
        }
    }

    pub fn right_shift(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.right_shift(other),
            _ => todo!(),
        }
    }

    pub fn sum(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.sum(other),
            _ => todo!(),
        }
    }

    pub fn substraction(&self, other: Object) -> Result<Object, ()> {
        match self {
            Self::Integer(int) => int.substraction(other),
            _ => todo!(),
        }
    }
}

impl Object {
    pub fn bitwise_not(&self) -> Result<Object, ()> {
        match self {
            Object::Integer(int) => int.bitwise_not(),
            _ => todo!(),
        }
    }

    pub fn logic_not(&self) -> Result<Object, ()> {
        match self {
            Object::Boolean(boolean) => boolean.logic_not(),
            _ => todo!(),
        }
    }

    pub fn inverse(&self) -> Result<Object, ()> {
        match self {
            Object::Integer(int) => int.inverse(),
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct Char {
    val: char,
}

impl InfixOperable for Char {}

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

#[derive(Debug, PartialEq, Eq)]
pub struct ExtensionSet {
    list: Vec<Object>,
}

impl InfixOperable for ExtensionSet {}

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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct MyString {
    val: String,
}

impl InfixOperable for MyString {}

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

#[derive(Debug, PartialEq, Eq)]
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
