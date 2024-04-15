use std::{fmt, iter::zip, vec};

use crate::{
    ast::ASTNode,
    env::Environment,
    exec::{exec, EvalError},
    matcher::{match_and_map, MatchResult},
};

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
    contains,
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

macro_rules! derived_object_infix_trait {
    ($ident:ident) => {
        pub fn $ident(&self, other: Object) -> Result<Object, ()> {
            match self {
                Self::Boolean(left) => left.$ident(other),
                Self::Char(left) => left.$ident(other),
                Self::ComprehensionSet(left) => left.$ident(other),
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
    bitwise_or,
    bitwise_xor,
    equality,
    greater,
    greater_equal,
    contains,
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
                Self::ComprehensionSet(left) => left.$ident(),
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
    fn logic_and(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Boolean(boolean) => Ok(Object::Boolean(Bool::from(self.val && boolean.val))),
            _ => Err(()),
        }
    }

    fn logic_or(&self, other: Object) -> Result<Object, ()> {
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

impl ExtensionSet {
    pub fn list(&self) -> &[Object] {
        &self.list
    }
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

        write!(f, "{{{}}}", list)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ComprehensionSet {
    value: ASTNode,
    prop: ASTNode,
}

impl From<(ASTNode, ASTNode)> for ComprehensionSet {
    fn from((value, prop): (ASTNode, ASTNode)) -> Self {
        Self { value, prop }
    }
}

impl PrefixOperable for ComprehensionSet {}

impl InfixOperable for ComprehensionSet {
    fn contains(&self, other: Object) -> Result<Object, ()> {
        let symbol = match &self.value {
            ASTNode::Symbol(s) => s,
            _ => todo!(),
        };

        let mut env = Environment::default();
        env.set(symbol, other);

        Ok(exec(&self.prop, &mut env).unwrap())
    }
}

impl fmt::Display for ComprehensionSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "set")
    }
}

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

    fn equality(&self, other: Object) -> Result<Object, ()> {
        match other {
            Object::Integer(Integer { val }) => Ok(Object::Boolean(Bool::from(self.val == val))),
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
            _ => Err(()),
        }
    }
}

impl PrefixOperable for Symbol {}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
    fn call(&self, args: &[Object], env: &mut Environment) -> Result<Object, EvalError> {
        match self {
            Self::DefinedFunction(f) => f.call(args, env),
            Self::Effect(ef) => ef.call(args, env),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
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

    fn default_call(&self, args: &[Object], env: &mut Environment) -> Result<Object, EvalError> {
        if args.len() < self.params.len() {
            return Err(EvalError::MissingFunctionArguments);
        }

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
    ) -> Option<Result<Object, EvalError>> {
        for (patterns, val) in &self.patterns {
            if let MatchResult::Match(v) = match_and_map(&patterns[0], &args[0]) {
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
    fn call(&self, args: &[Object], env: &mut Environment) -> Result<Object, EvalError>;
}

impl Callable for DefinedFunction {
    fn call(&self, args: &[Object], env: &mut Environment) -> Result<Object, EvalError> {
        match self.pattern_call(args, env) {
            Some(res) => res,
            None => self.default_call(args, env),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Effect {
    func: fn(&[Object]) -> Object,
}

impl Effect {
    pub fn new(func: fn(&[Object]) -> Object) -> Self {
        Self { func }
    }
}

impl From<fn(&[Object]) -> Object> for Effect {
    fn from(func: fn(&[Object]) -> Object) -> Self {
        Self { func }
    }
}

impl fmt::Display for Effect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "effect")
    }
}

impl Callable for Effect {
    fn call(&self, args: &[Object], _env: &mut Environment) -> Result<Object, EvalError> {
        Ok((self.func)(args))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExtensionList {
    pub list: Vec<Object>,
}

impl From<Vec<Object>> for ExtensionList {
    fn from(list: Vec<Object>) -> Self {
        Self { list }
    }
}

impl InfixOperable for ExtensionList {}
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
