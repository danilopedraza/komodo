use std::{
    collections::HashSet,
    fmt,
    hash::{Hash, Hasher},
    iter::zip,
    vec,
};

use bigdecimal::BigDecimal;
use num_bigint::BigInt;

use crate::{
    ast::{ASTNode, ASTNodeType},
    env::Environment,
    error::Error,
    exec::exec,
    matcher::{match_call, Match},
};

macro_rules! default_infix_method {
    ($ident:ident) => {
        fn $ident(&self, _other: &Object) -> Result<Object, ()> {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Object {
    Boolean(Bool),
    Char(Char),
    Decimal(Decimal),
    ExtensionList(ExtensionList),
    ExtensionSet(ExtensionSet),
    Function(Function),
    ComprehensionSet(ComprehensionSet),
    Integer(Integer),
    String(MyString),
    Symbol(Symbol),
    Tuple(Tuple),
}

impl Object {
    pub fn empty_tuple() -> Self {
        Self::Tuple(Tuple::from(vec![]))
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Boolean(boolean) => boolean.fmt(f),
            Object::Char(chr) => chr.fmt(f),
            Object::ComprehensionSet(set) => set.fmt(f),
            Object::Decimal(dec) => dec.fmt(f),
            Object::ExtensionList(list) => list.fmt(f),
            Object::ExtensionSet(es) => es.fmt(f),
            Object::Function(func) => func.fmt(f),
            Object::Integer(int) => int.fmt(f),
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
            Object::Boolean(_) => "boolean",
            Object::Char(_) => "character",
            Object::ComprehensionSet(_) => "comprehension set",
            Object::Decimal(_) => "decimal",
            Object::ExtensionList(_) => "extension list",
            Object::ExtensionSet(_) => "extension set",
            Object::Function(_) => "function",
            Object::Integer(_) => "integer",
            Object::String(_) => "string",
            Object::Symbol(_) => "symbol",
            Object::Tuple(_) => "tuple",
        }
        .into()
    }
}

macro_rules! derived_object_infix_trait {
    ($ident:ident) => {
        pub fn $ident(&self, other: &Object) -> Result<Object, ()> {
            match self {
                Self::Boolean(left) => left.$ident(other),
                Self::Char(left) => left.$ident(other),
                Self::ComprehensionSet(left) => left.$ident(other),
                Self::Decimal(left) => left.$ident(other),
                Self::ExtensionList(left) => left.$ident(other),
                Self::ExtensionSet(left) => left.$ident(other),
                Self::Function(left) => left.$ident(other),
                Self::Integer(left) => left.$ident(other),
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
                Self::ComprehensionSet(left) => left.$ident(),
                Self::Decimal(left) => left.$ident(),
                Self::ExtensionList(left) => left.$ident(),
                Self::ExtensionSet(left) => left.$ident(),
                Self::Function(left) => left.$ident(),
                Self::Integer(left) => left.$ident(),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    fn logic_and(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Boolean(boolean) => Ok(Object::Boolean(Bool::from(self.val && boolean.val))),
            _ => Err(()),
        }
    }

    fn or(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Boolean(boolean) => Ok(Object::Boolean(Bool::from(self.val || boolean.val))),
            _ => Err(()),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    fn sum(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::String(MyString { val: str }) => {
                let mut val = String::new();
                val.push(self.val);
                val.push_str(str);

                Ok(Object::String(MyString { val }))
            }
            Object::Char(Char { val: other_chr }) => {
                let mut val = String::new();
                val.push(self.val);
                val.push(*other_chr);

                Ok(Object::String(MyString { val }))
            }
            _ => Err(()),
        }
    }

    fn product(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(num) => Ok(self.multiply(num)),
            _ => Err(()),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Decimal {
    val: BigDecimal,
}

impl PrefixOperable for Decimal {}
impl InfixOperable for Decimal {}

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

#[derive(Clone, Debug)]
pub struct ExtensionSet {
    pub set: HashSet<Object>,
}

impl ExtensionSet {
    fn union(&self, other: &ExtensionSet) -> Object {
        let set = self
            .set
            .union(&other.set)
            .map(|val| val.to_owned())
            .collect();
        Object::ExtensionSet(ExtensionSet { set })
    }
}

impl Hash for ExtensionSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for obj in &self.set {
            obj.hash(state);
        }
    }
}

impl PartialEq for ExtensionSet {
    fn eq(&self, other: &Self) -> bool {
        self.set == other.set
    }
}
impl Eq for ExtensionSet {}

impl InfixOperable for ExtensionSet {
    fn sum(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::ExtensionSet(set) => Ok(self.union(set)),
            _ => Err(()),
        }
    }
}
impl PrefixOperable for ExtensionSet {}

impl From<Vec<Object>> for ExtensionSet {
    fn from(list: Vec<Object>) -> Self {
        let mut _set = HashSet::new();

        for obj in list {
            _set.insert(obj);
        }

        Self { set: _set }
    }
}

impl fmt::Display for ExtensionSet {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ComprehensionSet {
    value: ASTNode,
    prop: ASTNode,
}

impl Hash for ComprehensionSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value._type.hash(state);
        self.prop._type.hash(state);
    }
}

impl From<(ASTNode, ASTNode)> for ComprehensionSet {
    fn from((value, prop): (ASTNode, ASTNode)) -> Self {
        Self { value, prop }
    }
}

impl PrefixOperable for ComprehensionSet {}

impl InfixOperable for ComprehensionSet {
    fn contains(&self, other: &Object) -> Result<Object, ()> {
        let symbol = match &self.value._type {
            ASTNodeType::Symbol(s) => s,
            _ => unimplemented!(),
        };

        let mut env = Environment::default();
        env.set(symbol, other.to_owned());

        Ok(exec(&self.prop, &mut env).unwrap())
    }
}

impl fmt::Display for ComprehensionSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "set")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Integer {
    val: BigInt,
}

impl Integer {
    fn to_machine_magnitude(&self) -> usize {
        let max = std::usize::MAX;
        if self.val < BigInt::from(0) {
            0
        } else if self.val < BigInt::from(max) {
            self.val.iter_u64_digits().last().unwrap() as usize
        } else {
            max
        }
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
    fn bitwise_and(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(&self.val & val))),
            _ => Err(()),
        }
    }

    fn or(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(&self.val | val))),
            _ => Err(()),
        }
    }

    fn bitwise_xor(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(&self.val ^ val))),
            _ => Err(()),
        }
    }

    fn greater(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val > *val))),
            _ => Err(()),
        }
    }

    fn equality(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val == *val))),
            _ => Err(()),
        }
    }

    fn greater_equal(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val >= *val))),
            _ => Err(()),
        }
    }

    fn left_shift(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(int) => Ok(Object::Integer(Integer::from(
                &self.val << int.to_machine_magnitude(),
            ))),
            _ => Err(()),
        }
    }

    fn less(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val < *val))),
            _ => Err(()),
        }
    }

    fn less_equal(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val <= *val))),
            _ => Err(()),
        }
    }

    fn modulo(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(&self.val % val))),
            _ => Err(()),
        }
    }

    fn neq(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val != *val))),
            _ => Ok(Object::Boolean(Bool::from(true))),
        }
    }

    fn over(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(&self.val / val))),
            _ => Err(()),
        }
    }

    fn pow(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(int) => Ok(Object::Integer(Integer::from(
                self.val.pow(int.to_machine_magnitude() as u32),
            ))),
            _ => Err(()),
        }
    }

    fn right_shift(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(int) => Ok(Object::Integer(Integer::from(
                &self.val >> int.to_machine_magnitude(),
            ))),
            _ => Err(()),
        }
    }

    fn sum(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(&self.val + val))),
            _ => Err(()),
        }
    }

    fn substraction(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(&self.val - val))),
            _ => Err(()),
        }
    }

    fn product(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Integer(Integer::from(&self.val * val))),
            Object::Char(chr) => Ok(chr.multiply(self)),
            Object::String(str) => Ok(str.multiply(self)),
            Object::ExtensionList(lst) => Ok(lst.multiply(self)),
            _ => Err(()),
        }
    }
}

impl PrefixOperable for Integer {
    fn bitwise_not(&self) -> Result<Object, ()> {
        Ok(Object::Integer(Integer::from(!&self.val)))
    }

    fn inverse(&self) -> Result<Object, ()> {
        Ok(Object::Integer(Integer::from(-&self.val)))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MyString {
    val: String,
}

impl MyString {
    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let val = self.val.repeat(times);
        Object::String(MyString { val })
    }
}

impl InfixOperable for MyString {
    fn sum(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::String(MyString { val: other_string }) => {
                let mut val = String::new();
                val.push_str(&self.val);
                val.push_str(other_string);
                Ok(Object::String(MyString { val }))
            }
            Object::Char(Char { val: chr }) => {
                let mut val = self.val.clone();
                val.push(*chr);

                Ok(Object::String(MyString { val }))
            }
            _ => Err(()),
        }
    }

    fn product(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(num) => Ok(self.multiply(num)),
            _ => Err(()),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    fn equality(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Symbol(symbol) => Ok(Object::Boolean(Bool::from(self.val == symbol.val))),
            _ => Err(()),
        }
    }
}

impl PrefixOperable for Symbol {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tuple {
    list: Vec<Object>,
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
        Self { list }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Function {
    DefinedFunction(DefinedFunction),
    Effect(Effect),
}

impl InfixOperable for Function {}
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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DefinedFunction {
    patterns: Vec<(Vec<ASTNode>, ASTNode)>,
    params: Vec<String>,
    proc: Vec<ASTNode>,
}

impl Hash for DefinedFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (rule, res) in &self.patterns {
            for member in rule {
                member._type.hash(state);
            }

            res._type.hash(state);
        }
    }
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
            if let Match::Match(v) = match_call(patterns, args) {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ExtensionList {
    pub list: Vec<Object>,
}

impl ExtensionList {
    fn multiply(&self, num: &Integer) -> Object {
        let times = num.to_machine_magnitude();
        let mut list = vec![];

        for _ in 0..times {
            list.extend(self.list.clone());
        }

        Object::ExtensionList(list.into())
    }
}

impl Kind for ExtensionList {
    fn kind(&self) -> String {
        "extension list".into()
    }
}

impl From<Vec<Object>> for ExtensionList {
    fn from(list: Vec<Object>) -> Self {
        Self { list }
    }
}

impl InfixOperable for ExtensionList {
    fn sum(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::ExtensionList(ExtensionList { list: other_list }) => {
                let list: Vec<Object> = self
                    .list
                    .iter()
                    .chain(other_list)
                    .map(|obj| obj.to_owned())
                    .collect();

                Ok(Object::ExtensionList(list.into()))
            }
            _ => Err(()),
        }
    }

    fn product(&self, other: &Object) -> Result<Object, ()> {
        match other {
            Object::Integer(num) => Ok(self.multiply(num)),
            _ => Err(()),
        }
    }
}

impl PrefixOperable for ExtensionList {}

impl fmt::Display for ExtensionList {
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn concat_strings() {
        let str1 = Object::String(MyString::from("foo"));
        let str2 = Object::String(MyString::from("bar"));

        assert_eq!(
            str1.sum(&str2),
            Ok(Object::String(MyString::from("foobar")))
        );
    }

    #[test]
    fn concat_chars() {
        let chr1 = Object::Char('f'.into());
        let chr2 = Object::Char('u'.into());

        assert_eq!(chr1.sum(&chr2), Ok(Object::String("fu".into())))
    }

    #[test]
    fn concat_str_and_char() {
        let str = Object::String("bo".into());
        let chr = Object::Char('o'.into());

        assert_eq!(str.sum(&chr), Ok(Object::String("boo".into())));
        assert_eq!(chr.sum(&str), Ok(Object::String("obo".into())));
    }

    #[test]
    fn multiply_char() {
        let chr = Object::Char('k'.into());
        let num = Object::Integer(7.into());

        assert_eq!(chr.product(&num), Ok(Object::String("kkkkkkk".into())));
        assert_eq!(chr.product(&num), num.product(&chr));
    }

    #[test]
    fn multiply_string() {
        let str = Object::String("humongous ".into());
        let num = Object::Integer(2.into());

        assert_eq!(
            str.product(&num),
            Ok(Object::String("humongous humongous ".into()))
        );
        assert_eq!(str.product(&num), num.product(&str));
    }

    #[test]
    fn concat_lists() {
        let l1 = Object::ExtensionList(vec![Object::String("hola".into())].into());
        let l2 = Object::ExtensionList(vec![Object::Integer(1.into())].into());

        assert_eq!(
            l1.sum(&l2),
            Ok(Object::ExtensionList(
                vec![Object::String("hola".into()), Object::Integer(1.into())].into()
            ))
        );
    }

    #[test]
    fn multiply_lists() {
        let l = Object::ExtensionList(vec![Object::Integer(0.into())].into());
        let num = Object::Integer(3.into());

        assert_eq!(
            l.product(&num),
            Ok(Object::ExtensionList(
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
        let s1 =
            Object::ExtensionSet(vec![Object::Integer(1.into()), Object::Integer(2.into())].into());
        let s2 =
            Object::ExtensionSet(vec![Object::Integer(2.into()), Object::Integer(3.into())].into());

        assert_eq!(
            s1.sum(&s2),
            Ok(Object::ExtensionSet(
                vec![
                    Object::Integer(1.into()),
                    Object::Integer(2.into()),
                    Object::Integer(3.into()),
                ]
                .into()
            )),
        );
    }
}
