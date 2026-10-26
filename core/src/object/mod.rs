pub use crate::exec::generator::Generator;
pub mod float;
pub mod fraction;
pub mod integer;
mod memo;
mod view;
use view::View;

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{
        btree_set::{IntoIter, Iter},
        BTreeMap, BTreeSet,
    },
    fmt,
    hash::Hash,
    rc::{Rc, Weak},
    vec,
};

use num_bigint::BigInt;
use num_rational::BigRational;

use float::Float;
use fraction::Fraction;
use integer::Integer;

use crate::{
    ast::Pattern,
    env::{Environment, ScopeKind},
    error::{Error, Position},
    exec::{exec, ExecError},
    ir::Block,
    matcher::{CompiledPattern, Match, RuntimePattern},
};

/// Value storage shared by snapshots, detached only through mutable access.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Shared<T>(Rc<T>);
impl<T> From<T> for Shared<T> {
    fn from(value: T) -> Self {
        Self(Rc::new(value))
    }
}
impl<T> std::ops::Deref for Shared<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}
impl<T: Clone> std::ops::DerefMut for Shared<T> {
    fn deref_mut(&mut self) -> &mut T {
        Rc::make_mut(&mut self.0)
    }
}
impl<T: Clone> Shared<T> {
    pub(crate) fn into_owned(self) -> T {
        Rc::unwrap_or_clone(self.0)
    }
}
impl<T: Clone + IntoIterator> IntoIterator for Shared<T> {
    type Item = T::Item;
    type IntoIter = T::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.into_owned().into_iter()
    }
}
impl<'a, T> IntoIterator for &'a Shared<T>
where
    &'a T: IntoIterator,
{
    type Item = <&'a T as IntoIterator>::Item;
    type IntoIter = <&'a T as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&*self.0).into_iter()
    }
}
impl<T: fmt::Display> fmt::Display for Shared<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

macro_rules! default_infix_method {
    ($ident:ident) => {
        fn $ident(&self, _other: &Object) -> Option<Object> {
            None
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
    bitwise_xor,
    contains,
    left_shift,
    logic_and,
    or,
    rem,
    over,
    pow,
    product,
    right_shift,
    sum,
    substraction
);

macro_rules! default_prefix_method {
    ($ident:ident) => {
        fn $ident(&self) -> Option<Object> {
            None
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

#[derive(Clone, Debug)]
pub enum Object {
    Boolean(Bool),
    Char(Char),
    Integer(Integer),
    Float(Float),
    Fraction(Fraction),
    Symbol(Symbol),
    String(MyString),
    Tuple(Tuple),
    List(List),
    Set(Set),
    Dictionary(Dictionary),
    Function(Function),
    Range(Range),
    Generator(Generator),
    Error(ObjectError),
}

// This key defines the language's total order. Deriving the traits on a
// borrowed key keeps ordering, equality, and hashing consistent, including
// recursively inside collections, without cloning nonnumeric values.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum OrderKey<'a> {
    Boolean(&'a Bool),
    Char(&'a Char),
    Number(NumberKey),
    Symbol(&'a Symbol),
    String(&'a MyString),
    Tuple(&'a Tuple),
    List(&'a List),
    Set(&'a Set),
    Dictionary(&'a Dictionary),
    Function(&'a Function),
    Range(&'a Integer, &'a Integer),
    Generator(&'a Generator),
    Error(&'a ObjectError),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
enum NumberKey {
    NegativeNonFinite(Float),
    Finite(BigRational),
    PositiveNonFinite(Float),
}

impl Object {
    fn order_key(&self) -> OrderKey<'_> {
        match self {
            Self::Boolean(value) => OrderKey::Boolean(value),
            Self::Char(value) => OrderKey::Char(value),
            Self::Integer(value) => {
                OrderKey::Number(NumberKey::Finite(BigRational::from_integer(value.into())))
            }
            Self::Fraction(value) => OrderKey::Number(NumberKey::Finite(value.clone().into())),
            Self::Float(value) => {
                let float: f64 = value.clone().into();
                OrderKey::Number(match BigRational::from_float(float) {
                    Some(value) => NumberKey::Finite(value),
                    None if float.is_sign_negative() => NumberKey::NegativeNonFinite(value.clone()),
                    None => NumberKey::PositiveNonFinite(value.clone()),
                })
            }
            Self::Symbol(value) => OrderKey::Symbol(value),
            Self::String(value) => OrderKey::String(value),
            Self::Tuple(value) => OrderKey::Tuple(value),
            Self::List(value) => OrderKey::List(value),
            Self::Set(value) => OrderKey::Set(value),
            Self::Dictionary(value) => OrderKey::Dictionary(value),
            Self::Function(value) => OrderKey::Function(value),
            Self::Range(value) => OrderKey::Range(&value.start, &value.end),
            Self::Generator(value) => OrderKey::Generator(value),
            Self::Error(value) => OrderKey::Error(value),
        }
    }
}

impl Ord for Object {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            // Preserve the allocation-free path for the most common comparison.
            (Self::Integer(left), Self::Integer(right)) => left.cmp(right),
            _ => self.order_key().cmp(&other.order_key()),
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.order_key().hash(state);
    }
}

macro_rules! object_comparison {
    ($name:ident, $op:tt) => {
        pub fn $name(&self, other: &Object) -> Option<Object> {
            Some((self $op other).into())
        }
    };
}

impl Object {
    object_comparison!(less, <);
    object_comparison!(less_equal, <=);
    object_comparison!(greater, >);
    object_comparison!(greater_equal, >=);
    object_comparison!(equality, ==);
    object_comparison!(neq, !=);

    pub(crate) fn snapshot(&self) -> Self {
        match self {
            Self::Function(Function::Pattern(function)) => {
                Self::Function(Function::Pattern(function.strong_clone()))
            }
            object => object.clone(),
        }
    }

    pub(crate) fn is_recursive_reference(&self) -> bool {
        matches!(
            self,
            Self::Function(Function::Pattern(PatternFunction {
                env: CapturedEnvironment::Recursive(_),
                ..
            }))
        )
    }

    pub fn empty_tuple() -> Self {
        Self::Tuple(Tuple::empty_tuple())
    }

    pub fn empty_list() -> Self {
        Self::List(List::empty_list())
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Object::Integer(val) => val.is_zero(),
            Object::Float(val) => val.is_zero(),
            Object::Fraction(val) => val.is_zero(),
            _ => false,
        }
    }

    pub fn has_property(&self, prop: &str) -> bool {
        match self {
            Object::Symbol(symbol) => symbol.property == prop,
            _ => prop == self.kind(),
        }
    }

    pub fn true_obj() -> Self {
        Self::Boolean(Bool::from(true))
    }

    pub fn false_obj() -> Self {
        Self::Boolean(Bool::from(false))
    }

    pub fn from_fn(func: fn(&Object) -> Object, name: String) -> Self {
        Self::Function(Function::Extern(ExternFunction::new(func, name)))
    }

    pub fn to_float(&self) -> Result<Float, ObjectError> {
        match self {
            Object::Float(num) => Ok(num.to_owned()),
            Object::Integer(num) => num.try_into(),
            Object::Fraction(num) => Ok(num.into()),
            obj => Err(ObjectError::UnexpectedType(
                vec![
                    String::from("Float"),
                    String::from("Integer"),
                    String::from("Fraction"),
                ],
                obj.kind(),
            )),
        }
    }

    pub fn to_int(&self) -> Result<Integer, ObjectError> {
        match self {
            Object::Integer(num) => Ok(num.to_owned()),
            Object::Float(num) => num.floor(),
            Object::Fraction(num) => Ok(num.floor()),
            Object::String(MyString { val }) => match BigInt::parse_bytes(val.as_bytes(), 10) {
                Some(parsed) => Ok(Integer::from(parsed)),
                None => Err(ObjectError::ParseError(format!(
                    "Cannot parse string \"{}\" as an integer",
                    val
                ))),
            },
            obj => Err(ObjectError::UnexpectedType(
                vec![
                    String::from("Float"),
                    String::from("Integer"),
                    String::from("Fraction"),
                    String::from("String"),
                ],
                obj.kind(),
            )),
        }
    }

    pub fn to_list(&self) -> Result<List, ObjectError> {
        match self {
            Object::List(list) => Ok(list.to_owned()),
            Object::Set(set) => {
                let list: Vec<_> = set.iter().map(|obj| obj.to_owned()).collect();
                Ok(list.into())
            }
            Object::Range(rng) => {
                let list: Vec<_> = rng.to_owned().collect();
                Ok(list.into())
            }
            obj => Err(ObjectError::UnexpectedType(
                vec![
                    String::from("List"),
                    String::from("Set"),
                    String::from("Range"),
                ],
                obj.kind(),
            )),
        }
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "Object ordering uses stable captured-environment identity; see PatternFunction::cmp"
    )]
    pub fn to_set(&self) -> Result<Set, ObjectError> {
        match self {
            Object::Set(set) => Ok(set.to_owned()),
            Object::List(list) => {
                let set: BTreeSet<_> = list.list.iter().map(|obj| obj.to_owned()).collect();
                Ok(set.into())
            }
            Object::Range(rng) => {
                let set: BTreeSet<_> = rng.to_owned().collect();
                Ok(set.into())
            }
            obj => Err(ObjectError::UnexpectedType(
                vec![
                    String::from("List"),
                    String::from("Set"),
                    String::from("Range"),
                ],
                obj.kind(),
            )),
        }
    }
}

impl From<bool> for Object {
    fn from(val: bool) -> Self {
        Object::Boolean(val.into())
    }
}

impl From<Set> for Object {
    fn from(set: Set) -> Self {
        Self::Set(set)
    }
}

impl From<ObjectError> for Object {
    fn from(err: ObjectError) -> Self {
        Self::Error(err)
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Boolean(boolean) => boolean.fmt(f),
            Object::Char(chr) => chr.fmt(f),
            Object::Float(dec) => dec.fmt(f),
            Object::Dictionary(dict) => dict.fmt(f),
            Object::Error(err) => err.fmt(f),
            Object::List(list) => list.fmt(f),
            Object::Set(es) => es.fmt(f),
            Object::Fraction(frac) => frac.fmt(f),
            Object::Function(func) => func.fmt(f),
            Object::Integer(int) => int.fmt(f),
            Object::Range(range) => range.fmt(f),
            Object::Generator(generator) => generator.fmt(f),
            Object::String(str) => str.fmt(f),
            Object::Symbol(s) => s.fmt(f),
            Object::Tuple(s) => s.fmt(f),
        }
    }
}

pub trait Kind {
    fn kind(&self) -> String;
}

impl Kind for Object {
    fn kind(&self) -> String {
        match self {
            Object::Boolean(_) => "Bool",
            Object::Char(_) => "Char",
            Object::Float(_) => "Float",
            Object::Dictionary(_) => "Dictionary",
            Object::Error(_) => "Error",
            Object::List(_) => "List",
            Object::Set(_) => "Set",
            Object::Fraction(_) => "Fraction",
            Object::Function(_) => "Function",
            Object::Integer(_) => "Integer",
            Object::Range(_) => "Range",
            Object::Generator(_) => "Generator",
            Object::String(_) => "String",
            Object::Symbol(_) => "Symbol",
            Object::Tuple(_) => "Tuple",
        }
        .into()
    }
}

macro_rules! derived_object_infix_trait {
    ($ident:ident) => {
        pub fn $ident(&self, other: &Object) -> Option<Object> {
            match self {
                Self::Boolean(left) => left.$ident(other),
                Self::Char(left) => left.$ident(other),
                Self::Float(left) => left.$ident(other),
                Self::Dictionary(left) => left.$ident(other),
                Self::Error(left) => left.$ident(other),
                Self::List(left) => left.$ident(other),
                Self::Set(left) => left.$ident(other),
                Self::Fraction(left) => left.$ident(other),
                Self::Function(left) => left.$ident(other),
                Self::Integer(left) => left.$ident(other),
                Self::Range(left) => left.$ident(other),
                Self::Generator(left) => left.$ident(other),
                Self::String(left) => left.$ident(other),
                Self::Symbol(left) => left.$ident(other),
                Self::Tuple(left) => left.$ident(other),
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
    bitwise_xor,
    contains,
    left_shift,
    logic_and,
    or,
    rem,
    over,
    pow,
    product,
    right_shift,
    sum,
    substraction
);

macro_rules! derived_object_prefix_trait {
    ($ident:ident) => {
        pub fn $ident(&self) -> Option<Object> {
            match self {
                Self::Boolean(left) => left.$ident(),
                Self::Char(left) => left.$ident(),
                Self::Float(left) => left.$ident(),
                Self::Dictionary(left) => left.$ident(),
                Self::Error(left) => left.$ident(),
                Self::List(left) => left.$ident(),
                Self::Set(left) => left.$ident(),
                Self::Fraction(left) => left.$ident(),
                Self::Function(left) => left.$ident(),
                Self::Integer(left) => left.$ident(),
                Self::Range(left) => left.$ident(),
                Self::Generator(left) => left.$ident(),
                Self::String(left) => left.$ident(),
                Self::Symbol(left) => left.$ident(),
                Self::Tuple(left) => left.$ident(),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Bool {
    val: bool,
}

impl Bool {
    pub fn value(&self) -> bool {
        self.val
    }
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = if self.val { "true" } else { "false" };

        write!(f, "{}", text)
    }
}

impl InfixOperable for Bool {
    fn logic_and(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Boolean(bool) => Some(Object::Boolean(Bool::from(self.val && bool.val))),
            _ => None,
        }
    }

    fn or(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Boolean(bool) => Some(Object::Boolean(Bool::from(self.val || bool.val))),
            _ => None,
        }
    }
}

impl PrefixOperable for Bool {
    fn logic_not(&self) -> Option<Object> {
        Some(Object::Boolean(Bool::from(!self.val)))
    }
}

impl From<bool> for Bool {
    fn from(val: bool) -> Self {
        Self { val }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Char {
    val: char,
}

impl From<&Char> for String {
    fn from(value: &Char) -> Self {
        value.val.to_string()
    }
}

impl Char {
    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let val = self.val.to_string().repeat(times);
        Object::String(MyString { val: val.into() })
    }
}

impl InfixOperable for Char {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::String(MyString { val: str }) => {
                let mut val = String::with_capacity(self.val.len_utf8() + str.len());
                val.push(self.val);
                val.push_str(str);

                Some(Object::String(MyString { val: val.into() }))
            }
            Object::Char(Char { val: other_chr }) => {
                let mut val = String::with_capacity(self.val.len_utf8() + other_chr.len_utf8());
                val.push(self.val);
                val.push(*other_chr);

                Some(Object::String(MyString { val: val.into() }))
            }
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(num) => Some(self.multiply(num)),
            _ => None,
        }
    }
}
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Set {
    set: Shared<BTreeSet<Object>>,
}

#[allow(
    clippy::mutable_key_type,
    reason = "Object ordering uses stable captured-environment identity; see PatternFunction::cmp"
)]
impl Set {
    pub fn empty_set() -> Object {
        Object::Set(Self {
            set: BTreeSet::new().into(),
        })
    }

    fn union(&self, other: &Set) -> Object {
        let set: BTreeSet<Object> = self
            .set
            .union(&other.set)
            .map(|val| val.to_owned())
            .collect();
        Object::Set(Set { set: set.into() })
    }

    fn difference(&self, other: &Set) -> Object {
        let set: BTreeSet<Object> = self
            .set
            .difference(&other.set)
            .map(|val| val.to_owned())
            .collect();
        Object::Set(Set { set: set.into() })
    }

    pub fn iter(&self) -> Iter<'_, Object> {
        self.set.iter()
    }

    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self) -> IntoIter<Object> {
        self.set.into_iter()
    }

    pub fn insert(&mut self, val: Object) {
        self.set.insert(val);
    }

    pub fn remove(&mut self, val: &Object) -> bool {
        self.set.remove(val)
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }
}

impl InfixOperable for Set {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(self.union(set)),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(self.difference(set)),
            _ => None,
        }
    }

    fn contains(&self, val: &Object) -> Option<Object> {
        Some(self.set.contains(val).into())
    }
}
impl PrefixOperable for Set {}

#[allow(
    clippy::mutable_key_type,
    reason = "Object ordering uses stable captured-environment identity; see PatternFunction::cmp"
)]
impl From<Vec<Object>> for Set {
    fn from(list: Vec<Object>) -> Self {
        let mut set = BTreeSet::new();

        for obj in list {
            set.insert(obj);
        }

        Self { set: set.into() }
    }
}

impl From<BTreeSet<Object>> for Set {
    fn from(set: BTreeSet<Object>) -> Self {
        Self { set: set.into() }
    }
}

impl fmt::Display for Set {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = self
            .set
            .iter()
            .map(quote_mystring)
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "{{{}}}", list)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default, PartialOrd, Ord, Hash)]
pub struct Dictionary {
    pub dict: Shared<BTreeMap<Object, Object>>,
}

impl From<BTreeMap<Object, Object>> for Dictionary {
    fn from(dict: BTreeMap<Object, Object>) -> Self {
        Self { dict: dict.into() }
    }
}

#[allow(
    clippy::mutable_key_type,
    reason = "Object ordering uses stable captured-environment identity; see PatternFunction::cmp"
)]
impl From<Vec<(Object, Object)>> for Dictionary {
    fn from(pairs: Vec<(Object, Object)>) -> Self {
        let mut dict = BTreeMap::new();

        for (key, value) in pairs {
            dict.insert(key, value);
        }

        Self { dict: dict.into() }
    }
}

impl Dictionary {
    pub fn get(&self, index: &Object) -> Result<Object, ExecError> {
        match self.dict.get(index) {
            None => Err(ExecError::NonExistentKey {
                key: index.to_string(),
            }),
            Some(val) => Ok(val.to_owned()),
        }
    }

    pub fn get_mut(&mut self, index: &Object) -> Result<&mut Object, ExecError> {
        match self.dict.get_mut(index) {
            None => Err(ExecError::NonExistentKey {
                key: index.to_string(),
            }),
            Some(val) => Ok(val),
        }
    }
}

fn quote_mystring(obj: &Object) -> String {
    match obj {
        Object::String(str) => format!("\"{str}\""),
        Object::Char(chr) => format!("'{chr}'"),
        obj => obj.to_string(),
    }
}

impl fmt::Display for Dictionary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pairs = self
            .dict
            .iter()
            .map(|(key, val)| format!("{} => {}", quote_mystring(key), quote_mystring(val)))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{{{}}}", pairs)
    }
}

impl InfixOperable for Dictionary {}
impl PrefixOperable for Dictionary {}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MyString {
    val: View<String>,
}

impl From<String> for MyString {
    fn from(val: String) -> Self {
        Self { val: val.into() }
    }
}

impl From<MyString> for String {
    fn from(value: MyString) -> Self {
        value.val.into_owned()
    }
}

impl MyString {
    pub(crate) fn has_prefix(&self, prefix: &Object) -> bool {
        match prefix {
            Object::String(prefix) => self.val.starts_with(&*prefix.val),
            Object::Char(prefix) => self.val.starts_with(prefix.val),
            _ => false,
        }
    }

    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let val = self.val.repeat(times);
        Object::String(MyString { val: val.into() })
    }

    pub fn cons_format(&self) -> Option<(char, Self)> {
        let mut iter = self.val.chars();
        let first = iter.next()?;

        Some((
            first,
            Self {
                val: self.val.suffix(first.len_utf8()),
            },
        ))
    }
}

impl InfixOperable for MyString {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::String(MyString { val: other_string }) => {
                let mut val = String::with_capacity(self.val.len() + other_string.len());
                val.push_str(&self.val);
                val.push_str(other_string);
                Some(Object::String(MyString { val: val.into() }))
            }
            Object::Char(Char { val: chr }) => {
                let mut val = String::with_capacity(self.val.len() + chr.len_utf8());
                val.push_str(&self.val);
                val.push(*chr);

                Some(Object::String(MyString { val: val.into() }))
            }
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(num) => Some(self.multiply(num)),
            _ => None,
        }
    }
}

impl PrefixOperable for MyString {}

impl fmt::Display for MyString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl From<&str> for MyString {
    fn from(val: &str) -> Self {
        MyString {
            val: val.to_string().into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol {
    pub name: String,
    pub property: String,
}

impl Symbol {
    pub fn new(name: String, property: String) -> Self {
        Self { name, property }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl InfixOperable for Symbol {}

impl PrefixOperable for Symbol {}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tuple {
    pub list: Shared<Vec<Object>>,
}

impl Tuple {
    pub fn empty_tuple() -> Self {
        Self {
            list: Vec::new().into(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }
}

impl InfixOperable for Tuple {}
impl PrefixOperable for Tuple {}

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = self
            .list
            .iter()
            .map(|obj| obj.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "({})", list)
    }
}

impl From<Vec<Object>> for Tuple {
    fn from(list: Vec<Object>) -> Self {
        Self { list: list.into() }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Function {
    Anonymous(Box<AnonFunction>),
    Pattern(PatternFunction),
    Extern(ExternFunction),
}

impl InfixOperable for Function {}
impl PrefixOperable for Function {}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function")
    }
}

impl Function {
    pub fn call(
        &mut self,
        arg: &Object,
        env: &mut Environment,
        call_pos: Position,
    ) -> Result<Object, Error> {
        let mut outcome = self.call_step(arg, env, call_pos)?;
        loop {
            match outcome {
                crate::exec::Outcome::Value(value) => return Ok(value),
                crate::exec::Outcome::TailCall {
                    mut function,
                    argument,
                    position,
                    context,
                } => {
                    outcome = function.call_step(&argument, env, position)?;
                    if let crate::exec::Outcome::Value(Object::Error(error)) = outcome {
                        return Err(Error::with_position(
                            error.into(),
                            position,
                            context.executed_file_path.clone(),
                        ));
                    }
                }
            }
        }
    }
    fn call_step(
        &mut self,
        arg: &Object,
        env: &mut Environment,
        call_pos: Position,
    ) -> Result<crate::exec::Outcome, Error> {
        match self {
            Self::Pattern(f) => f.call_step(arg, call_pos),
            Self::Anonymous(f) => f.call_step(arg, call_pos),
            Self::Extern(f) => f.call(arg, env, call_pos).map(crate::exec::Outcome::Value),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FunctionPatternKind {
    Memoized,
    NotMemoized,
}

/// Recursive bindings borrow their owning environment. Values that escape a
/// frame promote that back reference to a strong capture before leaving it.
#[derive(Clone, Debug)]
pub enum CapturedEnvironment {
    Strong(Rc<RefCell<Environment>>),
    Recursive(Weak<RefCell<Environment>>),
}
impl CapturedEnvironment {
    pub fn upgrade(&self) -> Rc<RefCell<Environment>> {
        match self {
            Self::Strong(env) => env.clone(),
            Self::Recursive(env) => env
                .upgrade()
                .expect("recursive capture must have a live owner"),
        }
    }
    fn as_ptr(&self) -> *const RefCell<Environment> {
        match self {
            Self::Strong(env) => Rc::as_ptr(env),
            Self::Recursive(env) => env.as_ptr(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PatternFunction {
    pub env: CapturedEnvironment,
    patterns: Rc<Vec<(FunctionPatternKind, Pattern, Block, CompiledPattern)>>,
    cache: memo::Cache,
    clause_index: Rc<BTreeMap<Option<usize>, Vec<usize>>>,
}

// Captured environments are shared and can change when recursive clauses are
// installed. Compare their allocation identity, never their mutable contents:
// functions (including those nested in containers) must remain stable map keys.
impl PartialEq for PatternFunction {
    fn eq(&self, other: &Self) -> bool {
        self.cache.len() == other.cache.len() && self.cmp(other) == Ordering::Equal
    }
}

impl Eq for PatternFunction {}

impl PartialOrd for PatternFunction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PatternFunction {
    fn cmp(&self, other: &Self) -> Ordering {
        self.env
            .as_ptr()
            .cmp(&other.env.as_ptr())
            .then_with(|| {
                if Rc::ptr_eq(&self.patterns, &other.patterns) {
                    Ordering::Equal
                } else {
                    self.patterns.cmp(&other.patterns)
                }
            })
            .then_with(|| self.cache.cmp(&other.cache))
    }
}

impl Hash for PatternFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.env.as_ptr().hash(state);
        self.patterns.hash(state);
        self.cache.hash(state);
    }
}

fn tuple_arity(pattern: &Pattern) -> Option<usize> {
    match pattern {
        Pattern::Tuple { list } => Some(list.len()),
        Pattern::Signature { pattern, .. } => tuple_arity(pattern),
        _ => None,
    }
}

impl PatternFunction {
    pub fn new(env: Rc<RefCell<Environment>>) -> Self {
        Self {
            env: CapturedEnvironment::Strong(env),
            patterns: Rc::default(),
            cache: memo::Cache::default(),
            clause_index: Rc::default(),
        }
    }
    pub(crate) fn recursive_clone(&self) -> Self {
        let mut result = self.clone();
        result.env = CapturedEnvironment::Recursive(Rc::downgrade(&self.env.upgrade()));
        result
    }
    pub(crate) fn strong_clone(&self) -> Self {
        let mut result = self.clone();
        result.env = CapturedEnvironment::Strong(self.env.upgrade());
        result
    }

    pub fn add_pattern(&mut self, param: &Pattern, value: &Block, kind: FunctionPatternKind) {
        let patterns = Rc::make_mut(&mut self.patterns);
        for pattern in patterns.iter_mut() {
            let (_, other_args, _, _) = pattern;
            if param == other_args {
                *pattern = (kind, param.to_owned(), value.to_owned(), param.into());
                return;
            }
        }

        let index = patterns.len();
        patterns.push((kind, param.clone(), value.clone(), param.into()));
        Rc::make_mut(&mut self.clause_index)
            .entry(tuple_arity(param))
            .or_default()
            .push(index);
    }

    fn exec_call(
        matched_values: BTreeMap<String, Object>,
        result_node: &Block,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        env.resolve_captures(result_node);
        env.push_bindings(ScopeKind::Function, matched_values);

        let res = exec(result_node, env);

        env.pop_scope();

        res
    }

    fn call_step(
        &mut self,
        arg: &Object,
        call_pos: Position,
    ) -> Result<crate::exec::Outcome, Error> {
        let captured = self.env.upgrade();
        if let Some(cached) = self.cache.get(arg) {
            Ok(crate::exec::Outcome::Value(cached.to_owned()))
        } else {
            let arity = match arg {
                Object::Tuple(t) => Some(t.list.len()),
                _ => None,
            };
            let mut general = self
                .clause_index
                .get(&None)
                .into_iter()
                .flatten()
                .copied()
                .peekable();
            let mut exact = arity
                .and_then(|arity| self.clause_index.get(&Some(arity)))
                .into_iter()
                .flatten()
                .copied()
                .peekable();
            let candidates = std::iter::from_fn(|| match (general.peek(), exact.peek()) {
                (Some(a), Some(b)) if a < b => general.next(),
                (Some(_), Some(_)) | (None, Some(_)) => exact.next(),
                (Some(_), None) => general.next(),
                (None, None) => None,
            });
            for index in candidates {
                let (kind, _, val, pattern) = &self.patterns[index];
                if let Some(Match(v)) = pattern.matches(arg) {
                    if self.patterns.iter().any(|(_, _, body, _)| body.has_yield) {
                        let mut frame = captured.borrow().to_owned();
                        frame.resolve_captures(val);
                        frame.push_bindings(ScopeKind::Function, v);
                        let value = Object::Generator(Generator::new(val.clone(), &frame));
                        if *kind == FunctionPatternKind::Memoized {
                            self.cache.insert(arg.clone(), value.clone());
                        }
                        return Ok(crate::exec::Outcome::Value(value));
                    }
                    match kind {
                        FunctionPatternKind::NotMemoized => {
                            let mut frame = captured.borrow().to_owned();
                            frame.resolve_captures(val);
                            frame.push_bindings(ScopeKind::Function, v);
                            let result = crate::exec::exec_tail(val, &mut frame);
                            frame.pop_scope();
                            return result;
                        }
                        FunctionPatternKind::Memoized => {
                            let res = Self::exec_call(v, val, &mut captured.borrow().to_owned())?;
                            self.cache.insert(arg.to_owned(), res.clone());
                            return Ok(crate::exec::Outcome::Value(res));
                        }
                    }
                }
            }

            Err(Error::with_position(
                ExecError::UnmatchedCall.into(),
                call_pos,
                captured.borrow().file_path(),
            ))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AnonFunction {
    param: RuntimePattern,
    result: Block,
    env: Environment,
}

impl AnonFunction {
    pub fn new(param: Pattern, result: Block, env: Environment) -> Self {
        let result = crate::ir::resolve_function(&param, result);
        let env = env.capture(&result, &param);
        Self {
            param: (&param).into(),
            result,
            env,
        }
    }

    fn call_step(
        &mut self,
        arg: &Object,
        call_pos: Position,
    ) -> Result<crate::exec::Outcome, Error> {
        let Some(Match(values)) = self.param.matches(arg) else {
            return Err(Error::with_position(
                ExecError::UnmatchedCall.into(),
                call_pos,
                self.env.file_path(),
            ));
        };
        self.env.resolve_captures(&self.result);
        self.env.push_bindings(ScopeKind::Function, values);
        let res = if self.result.has_yield {
            Ok(crate::exec::Outcome::Value(Object::Generator(
                Generator::new(self.result.clone(), &self.env),
            )))
        } else {
            crate::exec::exec_tail(&self.result, &mut self.env)
        };
        self.env.pop_scope();
        res
    }
}

type ContextFunction = fn(&Object, &mut Environment, Position) -> Result<Object, Error>;
#[derive(Clone, Debug)]
enum NativeFunction {
    Simple(fn(&Object) -> Object),
    Context(ContextFunction),
}
#[derive(Clone, Debug)]
pub struct ExternFunction {
    func: NativeFunction,
    name: String,
}

impl ExternFunction {
    pub fn new(func: fn(&Object) -> Object, name: String) -> Self {
        Self {
            func: NativeFunction::Simple(func),
            name,
        }
    }

    pub(crate) fn contextual(func: ContextFunction, name: String) -> Self {
        Self {
            func: NativeFunction::Context(func),
            name,
        }
    }
    fn call(
        &mut self,
        arg: &Object,
        env: &mut Environment,
        pos: Position,
    ) -> Result<Object, Error> {
        match self.func {
            NativeFunction::Simple(f) => Ok(f(arg)),
            NativeFunction::Context(f) => f(arg, env, pos),
        }
    }
}

impl Hash for ExternFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Ord for ExternFunction {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for ExternFunction {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ExternFunction {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for ExternFunction {}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct List {
    pub list: View<Vec<Object>>,
}

impl List {
    pub fn empty_list() -> Self {
        Self {
            list: Vec::new().into(),
        }
    }

    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let mut list = Vec::with_capacity(
            self.len()
                .checked_mul(times)
                .expect("list capacity overflow"),
        );

        for _ in 0..if self.is_empty() { 0 } else { times } {
            list.extend(self.list.iter().cloned());
        }

        Object::List(list.into())
    }

    pub fn get(&self, index: &Object) -> Result<Object, ExecError> {
        match index {
            Object::Integer(int) => {
                let index = int.to_machine_magnitude();
                match self.list.get(index) {
                    Some(val) => Ok(val.to_owned()),
                    None => Err(ExecError::ListIndexOutOfBounds),
                }
            }
            obj => Err(ExecError::InvalidIndex { kind: obj.kind() }),
        }
    }

    pub fn get_mut(&mut self, index: &Object) -> Result<&mut Object, ExecError> {
        match index {
            Object::Integer(int) => {
                let index = int.to_machine_magnitude();
                match self.list.get_mut(index) {
                    Some(val) => Ok(val),
                    None => Err(ExecError::ListIndexOutOfBounds),
                }
            }
            obj => Err(ExecError::InvalidIndex { kind: obj.kind() }),
        }
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn sorted(&self) -> Self {
        let mut list = self.list.to_owned();
        list.sort();

        Self { list }
    }
}

impl From<Vec<Object>> for List {
    fn from(list: Vec<Object>) -> Self {
        Self { list: list.into() }
    }
}

impl InfixOperable for List {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::List(List { list: other_list }) => {
                let list: Vec<Object> = self
                    .list
                    .iter()
                    .chain(other_list)
                    .map(|obj| obj.to_owned())
                    .collect();

                Some(Object::List(list.into()))
            }
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(num) => Some(self.multiply(num)),
            _ => None,
        }
    }

    fn contains(&self, val: &Object) -> Option<Object> {
        Some(self.list.contains(val).into())
    }
}

impl PrefixOperable for List {}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = self
            .list
            .iter()
            .map(quote_mystring)
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "[{}]", list)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Range {
    pub start: Integer,
    pub end: Integer,
    cur: Integer,
}

impl Iterator for Range {
    type Item = Object;
    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (&self.end - &self.cur).to_machine_magnitude();
        (
            remaining.min(4096),
            if remaining == usize::MAX {
                None
            } else {
                Some(remaining)
            },
        )
    }
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.end {
            let val = self.cur.to_owned();
            self.cur += Integer::one();
            Some(Object::Integer(val))
        } else {
            None
        }
    }
}

impl PartialOrd for Range {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Range {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (&self.start, &self.end).cmp(&(&other.start, &other.end))
    }
}

impl Range {
    pub fn _new(start_int: i32, end_int: i32) -> Self {
        let cur: Integer = start_int.into();
        let start: Integer = start_int.into();
        let end: Integer = end_int.into();

        Self { cur, start, end }
    }

    pub fn new(start: &Integer, end: &Integer) -> Self {
        Self {
            cur: start.to_owned(),
            start: start.to_owned(),
            end: end.to_owned(),
        }
    }
}

impl PrefixOperable for Range {}
impl InfixOperable for Range {}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.start.fmt(f)?;
        write!(f, "..")?;
        self.end.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ObjectError {
    BadJSONParse(String),
    CastInfinityToInt,
    FailedAssertion(Option<String>),
    FailedCast { from: String, to: String },
    ParseError(String),
    UnexpectedType(Vec<String>, String),
}

impl fmt::Display for ObjectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime error")
    }
}

impl PrefixOperable for ObjectError {}
impl InfixOperable for ObjectError {}

#[cfg(test)]
mod tests {
    use crate::lexer::Radix;

    use super::*;

    #[test]
    fn numeric_order_equality_and_hash_agree() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hasher;

        let hash = |value: &Object| {
            let mut state = DefaultHasher::new();
            value.hash(&mut state);
            state.finish()
        };
        let values = vec![
            Object::Float((-f64::NAN).into()),
            Object::Float(f64::NEG_INFINITY.into()),
            Object::Integer((-1).into()),
            Object::Float((-0.0).into()),
            Object::Integer(0.into()),
            Object::Fraction(Fraction::_new(0, 1)),
            Object::Float(0.0.into()),
            Object::Fraction(Fraction::_new(1, 10)),
            Object::Float(0.1.into()),
            Object::Integer(1.into()),
            Object::Fraction(Fraction::_new(1, 1)),
            Object::Float(1.0.into()),
            Object::Integer((BigInt::from(1) << 2000usize).into()),
            Object::Float(f64::INFINITY.into()),
            Object::Float(f64::NAN.into()),
            Object::Float(f64::from_bits(f64::NAN.to_bits() + 1).into()),
        ];
        for (i, left) in values.iter().enumerate() {
            for (j, right) in values.iter().enumerate() {
                let order = left.cmp(right);
                assert_eq!(order, right.cmp(left).reverse());
                assert_eq!(left == right, order.is_eq());
                assert_eq!(left.equality(right), Some(order.is_eq().into()));
                assert_eq!(left.less(right), Some(order.is_lt().into()));
                if i <= j {
                    assert!(order.is_le());
                }
                if order.is_eq() {
                    assert_eq!(hash(left), hash(right));
                    let left = Object::List(vec![left.clone()].into());
                    let right = Object::List(vec![right.clone()].into());
                    assert_eq!(left, right);
                    assert_eq!(hash(&left), hash(&right));
                }
            }
        }
        let mut reversed = values.clone();
        reversed.reverse();
        assert_eq!(List::from(reversed).sorted(), List::from(values));
    }

    #[test]
    fn range_order_and_hash_ignore_iterator_position() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hasher;

        let range = Range::_new(0, 3);
        let mut advanced = range.clone();
        advanced.next();
        let left = Object::Range(range);
        let right = Object::Range(advanced);
        assert_eq!(left, right);
        let mut left_hash = DefaultHasher::new();
        let mut right_hash = DefaultHasher::new();
        left.hash(&mut left_hash);
        right.hash(&mut right_hash);
        assert_eq!(left_hash.finish(), right_hash.finish());
    }

    #[test]
    #[allow(
        clippy::mutable_key_type,
        reason = "Regression test for stable Object keys containing shared captured environments"
    )]
    fn recursive_function_keys_remain_stable_when_captures_change() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hasher;

        fn hash(value: &Object) -> u64 {
            let mut hasher = DefaultHasher::new();
            value.hash(&mut hasher);
            hasher.finish()
        }

        let captured = Rc::new(RefCell::new(Environment::default()));
        let function = Object::Function(Function::Pattern(PatternFunction::new(captured.clone())));
        // Real named functions capture an environment that refers back to them.
        captured.borrow_mut().set_mutable("self", function.clone());
        let nested = Object::Tuple(vec![function.clone()].into());
        let keys = [function, nested];
        let mut set = BTreeSet::new();
        let mut map = BTreeMap::new();
        for key in &keys {
            set.insert(key.clone());
            map.insert(key.clone(), Object::true_obj());
        }
        let hashes = keys.each_ref().map(hash);

        // Comparisons and hashing must not borrow the captured environment.
        let mut captured = captured.borrow_mut();
        captured.set_mutable("new_binding", Object::false_obj());
        for (key, before) in keys.iter().zip(hashes) {
            assert!(set.contains(key));
            assert_eq!(map.get(key), Some(&Object::true_obj()));
            assert_eq!(hash(key), before);
            assert_eq!(key.cmp(&key.clone()), Ordering::Equal);
        }
    }

    #[test]
    fn separate_function_captures_have_distinct_identities() {
        let first = PatternFunction::new(Rc::new(RefCell::new(Environment::default())));
        let second = PatternFunction::new(Rc::new(RefCell::new(Environment::default())));
        assert_eq!(first, first.clone());
        assert_ne!(first, second);
        assert_ne!(first.cmp(&second), Ordering::Equal);
    }

    #[test]
    fn suffixes_share_storage_and_detach_visible_contents() {
        let original = List::from(vec![
            Object::Integer(1.into()),
            Object::Integer(2.into()),
            Object::Integer(3.into()),
        ]);
        let mut tail = List {
            list: original.list.suffix(1),
        };
        assert!(Rc::ptr_eq(&original.list.0, &tail.list.0));
        assert_eq!(
            tail,
            List::from(vec![Object::Integer(2.into()), Object::Integer(3.into())])
        );
        tail.list[0] = Object::Integer(9.into());
        assert_eq!(original.list[1], Object::Integer(2.into()));
        assert_eq!(
            tail.list.into_owned(),
            vec![Object::Integer(9.into()), Object::Integer(3.into())]
        );
        let text = MyString::from("é🦎z");
        let (head, tail) = text.cons_format().unwrap();
        assert_eq!(head, 'é');
        assert!(Rc::ptr_eq(&text.val.0, &tail.val.0));
        let (head, tail) = tail.cons_format().unwrap();
        assert_eq!(head, '🦎');
        assert_eq!(tail, MyString::from("z"));
        assert_eq!(String::from(tail), "z");
    }

    #[test]
    fn clause_replacement_keeps_position_and_snapshot() {
        let mut env = crate::builtin::standard_env(Default::default());
        crate::run::run("let f(0) := 1\nlet f(x) := 2\nlet old := f\nlet f(0) := 3\nassert(f(0) = 3)\nassert(old(0) = 1)", &mut env).unwrap();
        let Object::Function(Function::Pattern(f)) = env.lookup("f").unwrap() else {
            panic!()
        };
        assert_eq!(f.patterns.len(), 2);
        assert_eq!(f.clause_index.values().map(Vec::len).sum::<usize>(), 2);
    }

    #[test]
    fn arity_index_merges_general_clauses_in_source_order() {
        let mut env = crate::builtin::standard_env(Default::default());
        crate::run::run(
            "let f(0, 0) := 1\nlet fallback(x) := 2\nlet triple(a, b, c) := 3",
            &mut env,
        )
        .unwrap();
        let Object::Function(Function::Pattern(mut f)) = env.lookup("f").unwrap().clone() else {
            panic!()
        };
        let Object::Function(Function::Pattern(fallback)) = env.lookup("fallback").unwrap() else {
            panic!()
        };
        let Object::Function(Function::Pattern(triple)) = env.lookup("triple").unwrap() else {
            panic!()
        };
        f.add_pattern(
            &Pattern::Symbol {
                name: "anything".into(),
            },
            &fallback.patterns[0].2,
            FunctionPatternKind::NotMemoized,
        );
        let (kind, param, body, _) = &triple.patterns[0];
        f.add_pattern(param, body, *kind);
        for (arity, expected) in [(2, 1), (3, 2), (0, 2)] {
            let arg = Object::Tuple(Tuple::from(vec![Object::Integer(0.into()); arity]));
            let crate::exec::Outcome::Value(result) =
                f.call_step(&arg, Position::new(0, 0)).unwrap()
            else {
                panic!()
            };
            assert_eq!(result, Object::Integer(expected.into()));
        }
    }

    #[test]
    fn collection_snapshots_share_storage_and_detach_on_mutation() {
        let mut list = List::from(vec![Object::List(vec![Object::Integer(1.into())].into())]);
        let snapshot = list.clone();
        assert!(Rc::ptr_eq(&list.list.0, &snapshot.list.0));
        let Object::List(nested) = &mut list.list[0] else {
            panic!("nested list")
        };
        nested.list[0] = Object::Integer(2.into());
        assert_eq!(
            snapshot.list[0],
            Object::List(vec![Object::Integer(1.into())].into())
        );
        assert!(!Rc::ptr_eq(&list.list.0, &snapshot.list.0));

        let mut dictionary =
            Dictionary::from(vec![(Object::Integer(1.into()), Object::true_obj())]);
        let snapshot = dictionary.clone();
        assert!(Rc::ptr_eq(&dictionary.dict.0, &snapshot.dict.0));
        *dictionary.get_mut(&Object::Integer(1.into())).unwrap() = Object::false_obj();
        assert_eq!(
            snapshot.get(&Object::Integer(1.into())).unwrap(),
            Object::true_obj()
        );

        let mut set = Set::from(vec![Object::Integer(1.into())]);
        let snapshot = set.clone();
        assert!(Rc::ptr_eq(&set.set.0, &snapshot.set.0));
        assert!(set.remove(&Object::Integer(1.into())));
        assert_eq!(snapshot.len(), 1);

        let tuple = Tuple::from(vec![Object::true_obj()]);
        assert!(Rc::ptr_eq(&tuple.list.0, &tuple.clone().list.0));
        let text = MyString::from("shared");
        assert!(Rc::ptr_eq(&text.val.0, &text.clone().val.0));
    }

    #[test]
    fn concat_strings() {
        let str1 = Object::String(MyString::from("foo"));
        let str2 = Object::String(MyString::from("bar"));

        assert_eq!(
            str1.sum(&str2),
            Some(Object::String(MyString::from("foobar")))
        );
    }

    #[test]
    fn concat_chars() {
        let chr1 = Object::Char('f'.into());
        let chr2 = Object::Char('u'.into());

        assert_eq!(chr1.sum(&chr2), Some(Object::String("fu".into())))
    }

    #[test]
    fn concat_str_and_char() {
        let str = Object::String("bo".into());
        let chr = Object::Char('o'.into());

        assert_eq!(str.sum(&chr), Some(Object::String("boo".into())));
        assert_eq!(chr.sum(&str), Some(Object::String("obo".into())));
    }

    #[test]
    fn multiply_char() {
        let chr = Object::Char('k'.into());
        let num = Object::Integer(7.into());

        assert_eq!(chr.product(&num), Some(Object::String("kkkkkkk".into())));
        assert_eq!(chr.product(&num), num.product(&chr));
    }

    #[test]
    fn multiply_string() {
        let str = Object::String("humongous ".into());
        let num = Object::Integer(2.into());

        assert_eq!(
            str.product(&num),
            Some(Object::String("humongous humongous ".into()))
        );
        assert_eq!(str.product(&num), num.product(&str));
    }

    #[test]
    fn concat_lists() {
        let l1 = Object::List(vec![Object::String("hola".into())].into());
        let l2 = Object::List(vec![Object::Integer(1.into())].into());

        assert_eq!(
            l1.sum(&l2),
            Some(Object::List(
                vec![Object::String("hola".into()), Object::Integer(1.into())].into()
            ))
        );
    }

    #[test]
    fn multiply_lists() {
        let l = Object::List(vec![Object::Integer(0.into())].into());
        let num = Object::Integer(3.into());

        assert_eq!(
            l.product(&num),
            Some(Object::List(
                vec![
                    Object::Integer(0.into()),
                    Object::Integer(0.into()),
                    Object::Integer(0.into())
                ]
                .into()
            ))
        );
        assert_eq!(l.product(&num), num.product(&l));
    }

    #[test]
    fn sum_sets() {
        let s1 = Object::Set(vec![Object::Integer(1.into()), Object::Integer(2.into())].into());
        let s2 = Object::Set(vec![Object::Integer(2.into()), Object::Integer(3.into())].into());

        assert_eq!(
            s1.sum(&s2),
            Some(Object::Set(
                vec![
                    Object::Integer(1.into()),
                    Object::Integer(2.into()),
                    Object::Integer(3.into()),
                ]
                .into()
            )),
        );
    }

    #[test]
    fn exponentiation() {
        let a = Object::Integer(Integer::from(2));
        let b = Object::Integer(Integer::from(3));

        assert_eq!(a.pow(&b), Some(Object::Integer(Integer::from(8))),);
    }

    #[test]
    fn negative_exponentiation() {
        let a = Object::Integer(Integer::from(2));
        let b = Object::Integer(Integer::from(-3));

        let expected = Object::Float(Float::new("0", "125"));

        assert_eq!(a.pow(&b), Some(expected),);
    }

    #[test]
    fn decimal_int_exponentiation() {
        let a = Object::Float(Float::new("0", "5"));
        let b = Object::Integer(Integer::from(2));

        assert_eq!(a.pow(&b), Some(Object::Float(Float::new("0", "25"))),);
    }

    #[test]
    fn int_dec_comparison() {
        let a = Object::Integer(5.into());
        let b = Object::Float(Float::new("1", "0"));

        assert_eq!(a.greater(&b), Some(Object::Boolean(true.into())));
    }

    #[test]
    fn dec_frac_equality() {
        let a = Object::Float(Float::new("5", "5"));
        let b = Object::Fraction(Fraction::new(11.into(), 2.into()));

        assert_eq!(a.equality(&b), Some(Object::Boolean(true.into())));
    }

    #[test]
    fn contain_zeros_pow() {
        let a = Object::Float(Float::new("5", "0"));
        let b = Object::Integer(Integer::new("2", Radix::Decimal));

        let c = a.pow(&b).unwrap().to_string();

        assert_eq!(c, "25");
    }

    #[test]
    fn contain_zeros_integer_negative_pow() {
        let a = Object::Integer(Integer::new("10", Radix::Decimal));
        let b = a.inverse().unwrap();

        println!("{a}, {b}");

        let c = a.pow(&b).unwrap().to_string();

        assert_eq!(c, "0.0000000001");
    }

    #[test]
    fn decimal_remainder() {
        let a = Object::Float(Float::new("3", "5"));
        let b = Object::Float(Float::new("0", "5"));

        assert_eq!(a.rem(&b), Some(Object::Float(Float::new("0", "0"))));
    }

    #[test]
    fn string_to_int() {
        let s = Object::String(MyString::from("42"));
        assert_eq!(s.to_int(), Ok(Integer::from(42)));
    }

    #[test]
    fn negative_string_to_int() {
        let s = Object::String(MyString::from("-7"));
        assert_eq!(s.to_int(), Ok(Integer::from(-7)));
    }

    #[test]
    fn invalid_string_to_int() {
        let s = Object::String(MyString::from("abc"));
        assert!(s.to_int().is_err());
    }
}
