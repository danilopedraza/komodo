use std::{
    fmt,
    hash::Hash,
    ops::{Add, Div, Mul, Rem, Sub},
};

use rug::ops::{CompleteRound, Pow};

use super::{integer::Integer, Bool, Fraction, InfixOperable, Object, PrefixOperable};

macro_rules! wrapper_fn {
    ($name:ident) => {
        pub fn $name(&self) -> Self {
            Self {
                val: self.val.to_owned().$name(),
            }
        }
    };
}

#[derive(Clone, Debug)]
pub struct Float {
    val: rug::Float,
}

pub static PREC: u32 = 53;
impl Float {
    pub fn new(int: &str, dec: &str) -> Self {
        let str = format!("{int}.{dec}");
        let val = rug::Float::parse(str).unwrap().complete(PREC);

        Self { val }
    }

    pub fn is_zero(&self) -> bool {
        self.val.is_zero()
    }

    pub fn inv(&self) -> Self {
        Self {
            val: self.val.to_owned().recip(),
        }
    }

    wrapper_fn!(sin);
    wrapper_fn!(cos);
    wrapper_fn!(tan);
    wrapper_fn!(asin);
    wrapper_fn!(acos);
    wrapper_fn!(atan);
    wrapper_fn!(exp);
    wrapper_fn!(ln);
}

impl Ord for Float {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.val.as_ord().cmp(other.val.as_ord())
    }
}

impl PartialOrd for Float {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        self.val.as_ord().eq(other.val.as_ord())
    }
}

impl Eq for Float {}

impl Hash for Float {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.val.as_ord().hash(state)
    }
}

impl Add for &Float {
    type Output = Float;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val + &rhs.val).complete(PREC),
        }
    }
}

impl Sub for &Float {
    type Output = Float;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val - &rhs.val).complete(PREC),
        }
    }
}

impl Mul for &Float {
    type Output = Float;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val * &rhs.val).complete(PREC),
        }
    }
}

impl Div for &Float {
    type Output = Float;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val / &rhs.val).complete(PREC),
        }
    }
}

impl Div for Float {
    type Output = Float;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: self.val / rhs.val,
        }
    }
}

impl Rem for &Float {
    type Output = Float;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val % &rhs.val).complete(PREC),
        }
    }
}

impl PrefixOperable for Float {
    fn inverse(&self) -> Option<Object> {
        Some(Object::Float(Float::from((-&self.val).complete(PREC))))
    }
}

impl From<Float> for rug::Float {
    fn from(val: Float) -> Self {
        val.val
    }
}

impl From<&Integer> for Float {
    fn from(value: &Integer) -> Self {
        Self {
            val: rug::Integer::from(value.to_owned()) + rug::Float::new(super::float::PREC),
        }
    }
}

impl From<&Fraction> for Float {
    fn from(frac: &Fraction) -> Self {
        let frac = rug::Rational::from(frac.to_owned());
        let mut val = rug::Float::new(PREC);
        val += frac.numer().to_owned();
        val /= frac.denom().to_owned();

        Self { val }
    }
}

impl InfixOperable for Float {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(Float { val }) => {
                Some(Object::Float(Float::from((&self.val + val).complete(PREC))))
            }
            Object::Integer(val) => Some(Object::Float(self + &Float::from(val))),
            Object::Fraction(frac) => Some(Object::Float(Float::from(
                &self.val + Float::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Float(self - val)),
            Object::Integer(val) => Some(Object::Float(self - &Float::from(val))),
            Object::Fraction(frac) => Some(Object::Float(self - &Float::from(frac))),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Float(self * val)),
            Object::Integer(val) => Some(Object::Float(self * &Float::from(val))),
            Object::Fraction(val) => Some(Object::Float(self * &Float::from(val))),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Float(self / val)),
            Object::Integer(val) => Some(Object::Float(self / &Float::from(val))),
            Object::Fraction(val) => Some(Object::Float(self / &Float::from(val))),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Float(
                (&self.val)
                    .pow(<&rug::Integer>::from(int))
                    .complete(PREC)
                    .into(),
            )),
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self > val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self > &Float::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self > &Float::from(val)))),
            _ => None,
        }
    }

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self < val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self < &Float::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self < &Float::from(val)))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self >= val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self >= &Float::from(val)))),
            Object::Fraction(frac) => Some(Object::Boolean(Bool::from(
                self.val >= Float::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self <= val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self <= &Float::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self <= &Float::from(val)))),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self == val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self == &Float::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self == &Float::from(val)))),
            _ => Some(Object::Boolean(false.into())),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self != val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self != &Float::from(val)))),
            Object::Fraction(frac) => Some(Object::Boolean(Bool::from(
                self.val != Float::from(frac).val,
            ))),
            _ => Some(Object::Boolean(true.into())),
        }
    }

    fn rem(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Float(self % val)),
            Object::Integer(val) => Some(Object::Float(self % &Float::from(val))),
            _ => None,
        }
    }
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.val.fmt(f)
    }
}

impl From<rug::Float> for Float {
    fn from(val: rug::Float) -> Self {
        Self { val }
    }
}
