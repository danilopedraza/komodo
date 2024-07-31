use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    hash::Hash,
    iter::zip,
    vec,
};

use bigdecimal::{num_traits::Pow, BigDecimal, One, Signed, Zero};
use num_bigint::{BigInt, BigUint};
use num_rational::BigRational;

use crate::{
    ast::ASTNode,
    env::Environment,
    error::Error,
    exec::{exec, EvalError},
    matcher::{match_call, Match},
};

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
    equality,
    greater,
    greater_equal,
    contains,
    left_shift,
    less,
    less_equal,
    logic_and,
    or,
    rem,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Object {
    Boolean(Bool),
    Char(Char),
    Decimal(Decimal),
    Dictionary(Dictionary),
    Error(FailedAssertion),
    List(List),
    Set(Set),
    Fraction(Fraction),
    Function(Function),
    Integer(Integer),
    Range(Range),
    String(MyString),
    Symbol(Symbol),
    Tuple(Tuple),
}

impl Object {
    pub fn empty_tuple() -> Self {
        Self::Tuple(Tuple::from(vec![]))
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Object::Integer(Integer { val }) => val.is_zero(),
            Object::Decimal(Decimal { val }) => val.is_zero(),
            Object::Fraction(Fraction { val }) => val.is_zero(),
            _ => false,
        }
    }

    pub fn has_property(&self, prop: &str) -> bool {
        prop == self.kind()
    }
}

impl From<bool> for Object {
    fn from(val: bool) -> Self {
        Object::Boolean(val.into())
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Boolean(boolean) => boolean.fmt(f),
            Object::Char(chr) => chr.fmt(f),
            Object::Decimal(dec) => dec.fmt(f),
            Object::Dictionary(dict) => dict.fmt(f),
            Object::Error(err) => err.fmt(f),
            Object::List(list) => list.fmt(f),
            Object::Set(es) => es.fmt(f),
            Object::Fraction(frac) => frac.fmt(f),
            Object::Function(func) => func.fmt(f),
            Object::Integer(int) => int.fmt(f),
            Object::Range(range) => range.fmt(f),
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
            Object::Boolean(_) => "Boolean",
            Object::Char(_) => "Character",
            Object::Decimal(_) => "Decimal",
            Object::Dictionary(_) => "Dictionary",
            Object::Error(_) => "Error",
            Object::List(_) => "List",
            Object::Set(_) => "Set",
            Object::Fraction(_) => "Fraction",
            Object::Function(_) => "Function",
            Object::Integer(_) => "Integer",
            Object::Range(_) => "Range",
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
                Self::Decimal(left) => left.$ident(other),
                Self::Dictionary(left) => left.$ident(other),
                Self::Error(left) => left.$ident(other),
                Self::List(left) => left.$ident(other),
                Self::Set(left) => left.$ident(other),
                Self::Fraction(left) => left.$ident(other),
                Self::Function(left) => left.$ident(other),
                Self::Integer(left) => left.$ident(other),
                Self::Range(left) => left.$ident(other),
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
    equality,
    greater,
    greater_equal,
    contains,
    left_shift,
    less,
    less_equal,
    logic_and,
    or,
    rem,
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
        pub fn $ident(&self) -> Option<Object> {
            match self {
                Self::Boolean(left) => left.$ident(),
                Self::Char(left) => left.$ident(),
                Self::Decimal(left) => left.$ident(),
                Self::Dictionary(left) => left.$ident(),
                Self::Error(left) => left.$ident(),
                Self::List(left) => left.$ident(),
                Self::Set(left) => left.$ident(),
                Self::Fraction(left) => left.$ident(),
                Self::Function(left) => left.$ident(),
                Self::Integer(left) => left.$ident(),
                Self::Range(left) => left.$ident(),
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
            Object::Boolean(boolean) => Some(Object::Boolean(Bool::from(self.val && boolean.val))),
            _ => None,
        }
    }

    fn or(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Boolean(boolean) => Some(Object::Boolean(Bool::from(self.val || boolean.val))),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Boolean(boolean) => Some((self == boolean).into()),
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

impl Char {
    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let val = self.val.to_string().repeat(times);
        Object::String(MyString { val })
    }
}

impl InfixOperable for Char {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::String(MyString { val: str }) => {
                let mut val = String::new();
                val.push(self.val);
                val.push_str(str);

                Some(Object::String(MyString { val }))
            }
            Object::Char(Char { val: other_chr }) => {
                let mut val = String::new();
                val.push(self.val);
                val.push(*other_chr);

                Some(Object::String(MyString { val }))
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

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Char(char) => Some((self == char).into()),
            _ => None,
        }
    }
}
impl PrefixOperable for Char {}

impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}'", self.val)
    }
}

impl From<char> for Char {
    fn from(val: char) -> Self {
        Char { val }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Decimal {
    val: BigDecimal,
}

impl Decimal {
    pub fn new(int: &str, dec: &str) -> Self {
        let val: BigDecimal = format!("{int}.{dec}").parse().unwrap();
        Self { val }
    }

    fn binary_pow(&self, exponent: &Integer) -> Self {
        let zero = BigUint::zero();
        let one = BigUint::one();
        let two = &BigUint::from(2_u32);

        let mut base = self.val.to_owned();
        let mut exp = exponent.val.magnitude().to_owned();
        let mut val = BigDecimal::one();

        while exp != zero {
            if &exp % two == one {
                val *= &base;
            }
            base = &base * &base;

            exp = &exp / two;
        }

        let must_invert = exponent.val.is_negative();
        if must_invert {
            val = val.inverse();
        }

        Decimal { val }
    }
}

impl PrefixOperable for Decimal {
    fn inverse(&self) -> Option<Object> {
        Some(Object::Decimal(Decimal::from(-&self.val)))
    }
}

impl From<&Fraction> for Decimal {
    fn from(frac: &Fraction) -> Self {
        let val = BigDecimal::new(frac.val.numer().to_owned(), 0)
            / BigDecimal::new(frac.val.denom().to_owned(), 0);

        Self { val }
    }
}

impl InfixOperable for Decimal {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(Decimal { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val + val)))
            }
            Object::Integer(Integer { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val + val)))
            }
            Object::Fraction(frac) => Some(Object::Decimal(Decimal::from(
                &self.val + Decimal::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(Decimal { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val - val)))
            }
            Object::Integer(Integer { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val - val)))
            }
            Object::Fraction(frac) => Some(Object::Decimal(Decimal::from(
                &self.val - Decimal::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(Decimal { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val * val)))
            }
            Object::Integer(Integer { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val * val)))
            }
            Object::Fraction(frac) => Some(Object::Decimal(Decimal::from(
                &self.val * Decimal::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(Decimal { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val / val)))
            }
            Object::Integer(Integer { val }) => Some(Object::Decimal(Decimal::from(
                &self.val / BigDecimal::new(val.clone(), 0),
            ))),
            Object::Fraction(frac) => Some(Object::Decimal(Decimal::from(
                &self.val / Decimal::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Decimal(self.binary_pow(int))),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(num) => Some((self == num).into()),
            _ => None,
        }
    }
}

impl fmt::Display for Decimal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.val.fmt(f)
    }
}

impl From<BigDecimal> for Decimal {
    fn from(val: BigDecimal) -> Self {
        Self { val }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Set {
    pub set: BTreeSet<Object>,
}

impl Set {
    fn union(&self, other: &Set) -> Object {
        let set = self
            .set
            .union(&other.set)
            .map(|val| val.to_owned())
            .collect();
        Object::Set(Set { set })
    }

    fn difference(&self, other: &Set) -> Object {
        let set = self
            .set
            .difference(&other.set)
            .map(|val| val.to_owned())
            .collect();
        Object::Set(Set { set })
    }
}

impl InfixOperable for Set {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(self.union(set)),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some((self == set).into()),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(self.difference(set)),
            _ => None,
        }
    }
}
impl PrefixOperable for Set {}

impl From<Vec<Object>> for Set {
    fn from(list: Vec<Object>) -> Self {
        let mut _set = BTreeSet::new();

        for obj in list {
            _set.insert(obj);
        }

        Self { set: _set }
    }
}

impl From<BTreeSet<Object>> for Set {
    fn from(set: BTreeSet<Object>) -> Self {
        Self { set }
    }
}

impl fmt::Display for Set {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = self
            .set
            .iter()
            .map(|obj| obj.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "{{{}}}", list)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default, PartialOrd, Ord, Hash)]
pub struct Dictionary {
    pub dict: BTreeMap<Object, Object>,
}

impl From<Vec<(Object, Object)>> for Dictionary {
    fn from(pairs: Vec<(Object, Object)>) -> Self {
        let mut dict = BTreeMap::new();

        for (key, value) in pairs {
            dict.insert(key, value);
        }

        Self { dict }
    }
}

impl Dictionary {
    pub fn get(&self, index: &Object) -> Result<Object, EvalError> {
        match self.dict.get(index) {
            None => Err(EvalError::NonExistentKey {
                key: index.to_string(),
            }),
            Some(val) => Ok(val.to_owned()),
        }
    }
}

impl fmt::Display for Dictionary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pairs = self
            .dict
            .iter()
            .map(|(key, val)| format!("{key}: {val}"))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{{{}}}", pairs)
    }
}

impl InfixOperable for Dictionary {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Dictionary(dict) => Some((self == dict).into()),
            _ => None,
        }
    }
}
impl PrefixOperable for Dictionary {}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Integer {
    val: BigInt,
}

impl Integer {
    fn to_machine_magnitude(&self) -> usize {
        let max = usize::MAX;
        if self.val <= BigInt::from(0) {
            0
        } else if self.val < BigInt::from(max) {
            self.val.iter_u64_digits().last().unwrap() as usize
        } else {
            max
        }
    }

    pub fn is_zero(&self) -> bool {
        self.val.is_zero()
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl From<i32> for Integer {
    fn from(value: i32) -> Self {
        let val = BigInt::from(value);
        Self { val }
    }
}

impl From<i64> for Integer {
    fn from(value: i64) -> Self {
        let val = BigInt::from(value);
        Self { val }
    }
}

impl From<usize> for Integer {
    fn from(value: usize) -> Self {
        let val = BigInt::from(value);
        Self { val }
    }
}

impl From<BigInt> for Integer {
    fn from(val: BigInt) -> Self {
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
    fn bitwise_and(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val & val)))
            }
            _ => None,
        }
    }

    fn or(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val | val)))
            }
            _ => None,
        }
    }

    fn bitwise_xor(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val ^ val)))
            }
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val > *val))),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val == *val))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val >= *val))),
            _ => None,
        }
    }

    fn left_shift(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Integer(Integer::from(
                &self.val << int.to_machine_magnitude(),
            ))),
            _ => None,
        }
    }

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val < *val))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val <= *val))),
            _ => None,
        }
    }

    fn rem(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val % val)))
            }
            _ => None,
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val != *val))),
            _ => Some(Object::Boolean(Bool::from(true))),
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val / val)))
            }
            Object::Decimal(Decimal { val }) => Some(Object::Decimal(Decimal::from(
                BigDecimal::new(self.val.clone(), 0) / val,
            ))),
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(
                (&BigRational::from_integer(self.val.to_owned()) / val).into(),
            )),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => {
                if int.val.is_negative() {
                    let inverse = self.val.to_owned().pow(int.val.magnitude());
                    let val = BigDecimal::new(inverse, 0).inverse();

                    Some(Object::Decimal(Decimal { val }))
                } else {
                    let val = self.val.to_owned().pow(int.val.magnitude());

                    Some(Object::Integer(Integer { val }))
                }
            }
            _ => None,
        }
    }

    fn right_shift(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Integer(Integer::from(
                &self.val >> int.to_machine_magnitude(),
            ))),
            _ => None,
        }
    }

    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val + val)))
            }
            Object::Decimal(Decimal { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val + val)))
            }
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(
                (&BigRational::from_integer(self.val.to_owned()) + val).into(),
            )),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val - val)))
            }
            Object::Decimal(Decimal { val }) => Some(Object::Decimal(Decimal::from(
                self.val.to_owned() - val.to_owned(),
            ))),
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(
                (&BigRational::from_integer(self.val.to_owned()) - val).into(),
            )),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val * val)))
            }
            Object::Decimal(Decimal { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val * val)))
            }
            Object::Char(chr) => Some(chr.multiply(self)),
            Object::String(str) => Some(str.multiply(self)),
            Object::List(lst) => Some(lst.multiply(self)),
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(
                (&BigRational::from_integer(self.val.to_owned()) * val).into(),
            )),
            _ => None,
        }
    }
}

impl PrefixOperable for Integer {
    fn bitwise_not(&self) -> Option<Object> {
        Some(Object::Integer(Integer::from(!&self.val)))
    }

    fn inverse(&self) -> Option<Object> {
        Some(Object::Integer(Integer::from(-&self.val)))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MyString {
    val: String,
}

impl From<String> for MyString {
    fn from(val: String) -> Self {
        Self { val }
    }
}

impl MyString {
    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let val = self.val.repeat(times);
        Object::String(MyString { val })
    }
}

impl InfixOperable for MyString {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::String(MyString { val: other_string }) => {
                let mut val = String::new();
                val.push_str(&self.val);
                val.push_str(other_string);
                Some(Object::String(MyString { val }))
            }
            Object::Char(Char { val: chr }) => {
                let mut val = self.val.clone();
                val.push(*chr);

                Some(Object::String(MyString { val }))
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

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::String(str) => Some((self == str).into()),
            _ => None,
        }
    }
}

impl PrefixOperable for MyString {}

impl fmt::Display for MyString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.val)
    }
}

impl From<&str> for MyString {
    fn from(val: &str) -> Self {
        MyString {
            val: val.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Symbol(symbol) => Some(Object::Boolean(Bool::from(self.val == symbol.val))),
            _ => None,
        }
    }
}

impl PrefixOperable for Symbol {}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tuple {
    list: Vec<Object>,
}

impl InfixOperable for Tuple {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Tuple(tuple) => Some((self == tuple).into()),
            _ => None,
        }
    }
}
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
        Self { list }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Function {
    DefinedFunction(DefinedFunction),
    Effect(Effect),
}

impl InfixOperable for Function {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Function(func) => Some((self == func).into()),
            _ => None,
        }
    }
}
impl PrefixOperable for Function {}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DefinedFunction(func) => func.fmt(f),
            Self::Effect(ef) => ef.fmt(f),
        }
    }
}

impl Callable for Function {
    fn call(&self, args: &[Object], env: &mut Environment) -> Result<Object, Error> {
        match self {
            Self::DefinedFunction(f) => f.call(args, env),
            Self::Effect(ef) => ef.call(args, env),
        }
    }

    fn param_number(&self) -> usize {
        match self {
            Self::DefinedFunction(f) => f.param_number(),
            Self::Effect(f) => f.param_number(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefinedFunction {
    patterns: Vec<(Vec<ASTNode>, ASTNode)>,
    params: Vec<String>,
    proc: Vec<ASTNode>,
}

impl InfixOperable for DefinedFunction {}
impl PrefixOperable for DefinedFunction {}

impl fmt::Display for DefinedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function")
    }
}

impl DefinedFunction {
    pub fn new(params: Vec<String>, proc: Vec<ASTNode>) -> Self {
        Self {
            patterns: vec![],
            params,
            proc,
        }
    }

    pub fn add_pattern(&mut self, args: &[ASTNode], value: &ASTNode) {
        self.patterns.push((args.to_vec(), value.clone()))
    }

    fn default_call(&self, args: &[Object], env: &mut Environment) -> Result<Object, Error> {
        env.push_scope();

        for (arg, param) in zip(args, self.params.clone()) {
            env.set(&param, arg.clone());
        }

        let mut res = Object::empty_tuple();

        for step in &self.proc {
            res = exec(step, env)?;
        }

        env.pop_scope();

        Ok(res)
    }

    fn pattern_call(
        &self,
        args: &[Object],
        env: &mut Environment,
    ) -> Option<Result<Object, Error>> {
        for (patterns, val) in &self.patterns {
            if let Some(Match(v)) = match_call(patterns, args) {
                env.push_scope();

                for (name, pattern_val) in v {
                    env.set(&name, pattern_val);
                }

                let res = Some(exec(val, env));

                env.pop_scope();

                return res;
            }
        }

        None
    }
}

pub trait Callable {
    fn call(&self, args: &[Object], env: &mut Environment) -> Result<Object, Error>;
    fn param_number(&self) -> usize;
}

impl Callable for DefinedFunction {
    fn call(&self, args: &[Object], env: &mut Environment) -> Result<Object, Error> {
        match self.pattern_call(args, env) {
            Some(res) => res,
            None => self.default_call(args, env),
        }
    }

    fn param_number(&self) -> usize {
        self.params.len()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Effect {
    func: fn(&[Object]) -> Object,
    param_number: usize,
}

impl Effect {
    pub fn new(func: fn(&[Object]) -> Object, param_number: usize) -> Self {
        Self { func, param_number }
    }
}

impl fmt::Display for Effect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "effect")
    }
}

impl Callable for Effect {
    fn call(&self, args: &[Object], _env: &mut Environment) -> Result<Object, Error> {
        Ok((self.func)(args))
    }

    fn param_number(&self) -> usize {
        self.param_number
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct List {
    pub list: Vec<Object>,
}

impl List {
    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let mut list = vec![];

        for _ in 0..times {
            list.extend(self.list.clone());
        }

        Object::List(list.into())
    }

    pub fn get(&self, index: &Object) -> Result<Object, EvalError> {
        match index {
            Object::Integer(int) => {
                let index = int.to_machine_magnitude();
                match self.list.get(index) {
                    Some(val) => Ok(val.to_owned()),
                    None => Err(EvalError::ListIndexOutOfBounds),
                }
            }
            obj => Err(EvalError::InvalidIndex { kind: obj.kind() }),
        }
    }
}

impl Kind for List {
    fn kind(&self) -> String {
        "extension list".into()
    }
}

impl From<Vec<Object>> for List {
    fn from(list: Vec<Object>) -> Self {
        Self { list }
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

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::List(list) => Some(Object::Boolean((self.list == list.list).into())),
            _ => Some(Object::Boolean(false.into())),
        }
    }
}

impl PrefixOperable for List {}

impl fmt::Display for List {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Fraction {
    pub val: BigRational,
}

impl Fraction {
    pub fn _new(numer: i32, denom: i32) -> Self {
        let val = BigRational::new(numer.into(), denom.into());

        Self { val }
    }

    pub fn new(numer: Integer, denom: Integer) -> Self {
        let val = BigRational::new(numer.val, denom.val);

        Self { val }
    }
}

impl From<BigRational> for Fraction {
    fn from(val: BigRational) -> Self {
        Self { val }
    }
}

impl InfixOperable for Fraction {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(Fraction {
                val: &self.val + val,
            })),
            Object::Integer(Integer { val }) => Some(Object::Fraction(Fraction {
                val: &self.val + BigRational::from_integer(val.to_owned()),
            })),
            Object::Decimal(dec) => Some(Object::Decimal(Decimal::from(
                &Decimal::from(self).val + &dec.val,
            ))),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(Fraction {
                val: &self.val - val,
            })),
            Object::Integer(Integer { val }) => Some(Object::Fraction(Fraction {
                val: &self.val - BigRational::from_integer(val.to_owned()),
            })),
            Object::Decimal(dec) => Some(Object::Decimal(Decimal::from(
                &Decimal::from(self).val - &dec.val,
            ))),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(Fraction {
                val: &self.val * val,
            })),
            Object::Integer(Integer { val }) => Some(Object::Fraction(Fraction {
                val: &self.val * BigRational::from_integer(val.to_owned()),
            })),
            Object::Decimal(dec) => Some(Object::Decimal(Decimal::from(
                &Decimal::from(self).val * &dec.val,
            ))),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(Fraction {
                val: &self.val / val,
            })),
            Object::Integer(Integer { val }) => Some(Object::Fraction(Fraction {
                val: &self.val / BigRational::from_integer(val.to_owned()),
            })),
            Object::Decimal(dec) => Some(Object::Decimal(Decimal::from(
                &Decimal::from(self).val / &dec.val,
            ))),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Fraction(Fraction {
                val: Pow::pow(self.val.to_owned(), val),
            })),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(frac) => Some((self == frac).into()),
            _ => None,
        }
    }
}

impl PrefixOperable for Fraction {
    fn inverse(&self) -> Option<Object> {
        let val = -self.val.to_owned();
        Some(Object::Fraction(Fraction { val }))
    }
}

impl fmt::Display for Fraction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.val.numer().fmt(f)?;
        write!(f, " // ")?;
        self.val.denom().fmt(f)
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
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur.val < self.end.val {
            let val = self.cur.val.to_owned();
            self.cur.val += 1;
            Some(Object::Integer(Integer { val }))
        } else {
            None
        }
    }
}

impl PartialOrd for Range {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(vec![&self.start, &self.end].cmp(&vec![&other.start, &other.end]))
    }
}

impl Ord for Range {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        vec![&self.start, &self.end].cmp(&vec![&other.start, &other.end])
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
impl InfixOperable for Range {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Range(range) => Some((self == range).into()),
            _ => None,
        }
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.start.fmt(f)?;
        write!(f, "..")?;
        self.end.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FailedAssertion(pub Option<String>);

impl fmt::Display for FailedAssertion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            None => write!(f, "Failed assertion"),
            Some(msg) => write!(f, "Failed assertion: {msg}"),
        }
    }
}

impl PrefixOperable for FailedAssertion {}
impl InfixOperable for FailedAssertion {
    fn equality(&self, _other: &Object) -> Option<Object> {
        Some(Object::Boolean(false.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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

        let expected = Object::Decimal(Decimal::new("0", "125"));

        assert_eq!(a.pow(&b), Some(expected),);
    }

    #[test]
    fn decimal_int_exponentiation() {
        let a = Object::Decimal(Decimal::new("0", "5"));
        let b = Object::Integer(Integer::from(2));

        assert_eq!(a.pow(&b), Some(Object::Decimal(Decimal::new("0", "25"))),);
    }
}
