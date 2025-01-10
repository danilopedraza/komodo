pub mod float;
pub mod fraction;
pub mod integer;

use std::{
    cell::RefCell,
    cmp::min,
    collections::{
        btree_set::{IntoIter, Iter},
        BTreeMap, BTreeSet,
    },
    fmt,
    hash::Hash,
    iter::zip,
    rc::Rc,
    vec,
};

use float::Float;
use fraction::Fraction;
use integer::Integer;

use crate::{
    ast::ASTNode,
    env::{Address, Environment, ScopeKind},
    error::{Error, Position},
    exec::{exec, ExecError},
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
    Error(ObjectError),
}

impl Object {
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

    pub fn from_fn(func: fn(&[Object]) -> Object, param_number: usize) -> Self {
        Self::Function(Function::Extern(ExternFunction { func, param_number }))
    }

    pub fn to_float(&self) -> Result<Float, ObjectError> {
        match self {
            Object::Float(num) => Ok(num.to_owned()),
            Object::Integer(num) => Ok(num.into()),
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
            Object::Float(_) => "Float",
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
                Self::Float(left) => left.$ident(other),
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
    contains,
    equality,
    greater,
    greater_equal,
    left_shift,
    less,
    less_equal,
    logic_and,
    neq,
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

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Boolean(bool) => Some(Object::Boolean(Bool::from(self == bool))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Boolean(bool) => Some(Object::Boolean(Bool::from(self != bool))),
            _ => Some(Object::true_obj()),
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
            Object::Char(chr) => Some(Object::Boolean(Bool::from(self == chr))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Char(chr) => Some(Object::Boolean(Bool::from(self != chr))),
            _ => Some(Object::true_obj()),
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
    set: BTreeSet<Object>,
}

impl Set {
    pub fn empty_set() -> Object {
        Object::Set(Self {
            set: BTreeSet::new(),
        })
    }

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

    fn subset_strict(&self, other: &Set) -> Object {
        Object::Boolean((self.set.is_subset(&other.set) && self.set.len() < other.set.len()).into())
    }

    fn subset_eq(&self, other: &Set) -> Object {
        Object::Boolean(self.set.is_subset(&other.set).into())
    }

    pub fn iter(&self) -> Iter<'_, Object> {
        self.set.iter()
    }

    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self) -> IntoIter<Object> {
        self.set.into_iter()
    }

    pub fn remove(&mut self, val: &Object) -> bool {
        self.set.remove(val)
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

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(self.subset_strict(set)),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(self.subset_eq(set)),
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(set.subset_strict(self)),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(set.subset_eq(self)),
            _ => None,
        }
    }

    fn contains(&self, val: &Object) -> Option<Object> {
        Some(self.set.contains(val).into())
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(Object::Boolean(Bool::from(self == set))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Set(set) => Some(Object::Boolean(Bool::from(self != set))),
            _ => Some(Object::true_obj()),
        }
    }
}
impl PrefixOperable for Set {}

impl From<Vec<Object>> for Set {
    fn from(list: Vec<Object>) -> Self {
        let mut set = BTreeSet::new();

        for obj in list {
            set.insert(obj);
        }

        let set = set;

        Self { set }
    }
}

impl From<Vec<(Object, Address)>> for Set {
    fn from(list: Vec<(Object, Address)>) -> Self {
        let mut set = BTreeSet::new();

        for obj in list {
            set.insert(obj.0);
        }

        let set = set;

        Self { set }
    }
}

impl From<BTreeSet<Object>> for Set {
    fn from(set: BTreeSet<Object>) -> Self {
        Self { set }
    }
}

impl From<BTreeSet<(Object, Address)>> for Set {
    fn from(set: BTreeSet<(Object, Address)>) -> Self {
        let mut new_set = BTreeSet::new();
        for obj in set {
            new_set.insert(obj.0);
        }

        Self { set: new_set }
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
    pub dict: BTreeMap<Object, Object>,
}

impl From<BTreeMap<Object, Object>> for Dictionary {
    fn from(dict: BTreeMap<Object, Object>) -> Self {
        Self { dict }
    }
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

impl InfixOperable for Dictionary {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Dictionary(dict) => Some(Object::Boolean(Bool::from(self == dict))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Dictionary(dict) => Some(Object::Boolean(Bool::from(self != dict))),
            _ => Some(Object::true_obj()),
        }
    }
}
impl PrefixOperable for Dictionary {}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MyString {
    val: String,
}

impl From<String> for MyString {
    fn from(val: String) -> Self {
        Self { val }
    }
}

impl From<MyString> for String {
    fn from(value: MyString) -> Self {
        value.val
    }
}

impl MyString {
    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let val = self.val.repeat(times);
        Object::String(MyString { val })
    }

    pub fn cons_format(&self) -> Option<(char, &str)> {
        let mut iter = self.val.chars();
        let first = iter.next()?;

        Some((first, iter.as_str()))
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
            Object::String(str) => Some(Object::Boolean(Bool::from(self == str))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::String(str) => Some(Object::Boolean(Bool::from(self != str))),
            _ => Some(Object::true_obj()),
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
            val: val.to_string(),
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

impl InfixOperable for Symbol {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Symbol(sym) => Some(Object::Boolean(Bool::from(self == sym))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Symbol(sym) => Some(Object::Boolean(Bool::from(self != sym))),
            _ => Some(Object::true_obj()),
        }
    }
}

impl PrefixOperable for Symbol {}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tuple {
    pub list: Vec<(Object, Address)>,
}

impl Tuple {
    pub fn empty_tuple() -> Self {
        Self { list: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }
}

impl InfixOperable for Tuple {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Tuple(tup) => Some(Object::Boolean(Bool::from(self == tup))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Tuple(tup) => Some(Object::Boolean(Bool::from(self != tup))),
            _ => Some(Object::true_obj()),
        }
    }
}
impl PrefixOperable for Tuple {}

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = self
            .list
            .iter()
            .map(|obj| obj.0.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "({})", list)
    }
}

impl From<Vec<Object>> for Tuple {
    fn from(list: Vec<Object>) -> Self {
        let list = list
            .into_iter()
            .map(|obj| (obj, Address::default()))
            .collect();
        Self { list }
    }
}

impl From<Vec<(Object, Address)>> for Tuple {
    fn from(list: Vec<(Object, Address)>) -> Self {
        Self { list }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Function {
    Anonymous(AnonFunction),
    Pattern(PatternFunction),
    Extern(ExternFunction),
}

impl InfixOperable for Function {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Function(func) => Some(Object::Boolean(Bool::from(self == func))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Function(func) => Some(Object::Boolean(Bool::from(self != func))),
            _ => Some(Object::true_obj()),
        }
    }
}
impl PrefixOperable for Function {}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function")
    }
}

impl Function {
    pub fn call(
        &mut self,
        args: &[Object],
        env: &mut Environment,
        call_pos: Position,
    ) -> Result<Object, Error> {
        match self {
            Self::Pattern(f) => f.call(args, call_pos),
            Self::Anonymous(f) => f.call(args, call_pos),
            Self::Extern(ef) => ef.call(args, env, call_pos),
        }
    }

    pub fn param_number(&self) -> usize {
        match self {
            Self::Pattern(f) => f.param_number(),
            Self::Anonymous(f) => f.param_number(),
            Self::Extern(f) => f.param_number(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FunctionPatternKind {
    Memoized,
    NotMemoized,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PatternFunction {
    pub env: Rc<RefCell<Environment>>,
    patterns: Vec<(FunctionPatternKind, Vec<ASTNode>, ASTNode)>,
    cache: BTreeMap<Vec<Object>, Object>,
    params: usize,
}

impl Hash for PatternFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.patterns.hash(state);
        self.cache.hash(state);
        self.params.hash(state);
    }
}

impl PatternFunction {
    pub fn new(env: Rc<RefCell<Environment>>) -> Self {
        Self {
            env,
            patterns: Vec::default(),
            cache: BTreeMap::default(),
            params: usize::default(),
        }
    }
    pub fn add_pattern(&mut self, args: &[ASTNode], value: &ASTNode, kind: FunctionPatternKind) {
        for i in 0..self.patterns.len() {
            let (_, other_args, _) = &self.patterns[i];
            if args == other_args {
                self.patterns[i] = (kind, args.to_owned(), value.to_owned());
            }
        }

        if self.patterns.is_empty() {
            self.params = args.len();
        } else {
            self.params = min(self.params, args.len())
        }

        self.patterns.push((kind, args.to_vec(), value.clone()));
    }

    fn exec_call(
        matched_values: BTreeMap<String, (Object, Address)>,
        result_node: &ASTNode,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        env.push_scope(ScopeKind::Function);

        for (name, pattern_val) in matched_values {
            env.set_inmutable(&name, pattern_val);
        }

        let res = exec(result_node, env);

        env.pop_scope();

        res.map(|(obj, _)| obj)
    }

    fn call(&mut self, args: &[Object], call_pos: Position) -> Result<Object, Error> {
        if let Some(cached) = self.cache.get(args) {
            Ok(cached.to_owned())
        } else {
            for (kind, patterns, val) in &self.patterns {
                if let Some(Match(v)) = match_call(patterns, args) {
                    match kind {
                        FunctionPatternKind::NotMemoized => {
                            return Self::exec_call(v, val, &mut self.env.borrow().to_owned());
                        }
                        FunctionPatternKind::Memoized => {
                            let res = Self::exec_call(v, val, &mut self.env.borrow().to_owned())?;
                            self.cache.insert(args.to_owned(), res.clone());
                            return Ok(res);
                        }
                    }
                }
            }

            Err(Error::with_position(
                ExecError::UnmatchedCall.into(),
                call_pos,
                self.env.borrow().file_path(),
            ))
        }
    }

    fn param_number(&self) -> usize {
        self.params
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AnonFunction {
    params: Vec<String>,
    result: ASTNode,
    env: Environment,
}

impl AnonFunction {
    pub fn new(params: Vec<String>, result: ASTNode, env: Environment) -> Self {
        Self {
            params,
            result,
            env,
        }
    }

    fn call(&mut self, args: &[Object], _call_pos: Position) -> Result<Object, Error> {
        self.env.push_scope(ScopeKind::Function);

        for (arg, param) in zip(args, &self.params) {
            self.env
                .set_inmutable(param, (arg.clone(), Address::default()));
        }

        let res = exec(&self.result, &mut self.env)?;

        self.env.pop_scope();

        Ok(res.0)
    }

    fn param_number(&self) -> usize {
        self.params.len()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExternFunction {
    func: fn(&[Object]) -> Object,
    param_number: usize,
}

impl ExternFunction {
    pub fn new(func: fn(&[Object]) -> Object, param_number: usize) -> Self {
        Self { func, param_number }
    }

    fn call(
        &mut self,
        args: &[Object],
        _env: &mut Environment,
        _call_pos: Position,
    ) -> Result<Object, Error> {
        Ok((self.func)(args))
    }

    fn param_number(&self) -> usize {
        self.param_number
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct List {
    pub list: Vec<(Object, Address)>,
}

impl List {
    pub fn empty_list() -> Self {
        Self { list: Vec::new() }
    }

    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let mut list = vec![];

        for _ in 0..times {
            list.extend(self.list.clone());
        }

        Object::List(list.into())
    }

    pub fn get(&self, index: &Object) -> Result<(Object, Address), ExecError> {
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

    pub fn get_mut(&mut self, index: &Object) -> Result<&mut (Object, Address), ExecError> {
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
}

impl From<Vec<Object>> for List {
    fn from(list: Vec<Object>) -> Self {
        let list = list
            .into_iter()
            .map(|obj| (obj, Address::default()))
            .collect();

        Self { list }
    }
}

impl From<Vec<(Object, Address)>> for List {
    fn from(list: Vec<(Object, Address)>) -> Self {
        Self { list }
    }
}

impl InfixOperable for List {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::List(List { list: other_list }) => {
                let list: Vec<(Object, Address)> = self
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
        Some(
            self.list
                .contains(&(val.clone(), Address::default()))
                .into(),
        )
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::List(list) => Some(Object::Boolean(Bool::from(self == list))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::List(list) => Some(Object::Boolean(Bool::from(self != list))),
            _ => Some(Object::true_obj()),
        }
    }
}

impl PrefixOperable for List {}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = self
            .list
            .iter()
            .map(|(obj, _)| quote_mystring(obj))
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
            Object::Range(range) => Some(Object::Boolean(Bool::from(self == range))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Range(range) => Some(Object::Boolean(Bool::from(self != range))),
            _ => Some(Object::true_obj()),
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
pub enum ObjectError {
    BadJSONParse(String),
    CastInfinityToInt,
    FailedAssertion(Option<String>),
    UnexpectedType(Vec<String>, String),
}

impl fmt::Display for ObjectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime error")
    }
}

impl PrefixOperable for ObjectError {}
impl InfixOperable for ObjectError {
    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Error(err) => Some(Object::Boolean(Bool::from(self == err))),
            _ => Some(Object::false_obj()),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Error(err) => Some(Object::Boolean(Bool::from(self != err))),
            _ => Some(Object::true_obj()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Radix;

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

        assert_eq!(c, "25.000000000000000");
    }

    #[test]
    fn contain_zeros_integer_negative_pow() {
        let a = Object::Integer(Integer::new("10", Radix::Decimal));
        let b = a.inverse().unwrap();

        let c = a.pow(&b).unwrap().to_string();

        assert_eq!(c, "1.0000000000000000e-10");
    }

    #[test]
    fn decimal_remainder() {
        let a = Object::Float(Float::new("3", "5"));
        let b = Object::Float(Float::new("0", "5"));

        assert_eq!(a.rem(&b), Some(Object::Float(Float::new("0", "0"))));
    }
}
