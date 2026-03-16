use std::{
    fmt,
    ops::{Add, Div, Mul, Sub},
};

use num_bigint::BigInt;
use num_rational::{BigRational, Ratio};
use num_traits::{Pow, Signed, Zero};

use super::{float::Float, integer::Integer, Bool, InfixOperable, Object, PrefixOperable};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Fraction {
    val: Ratio<BigInt>,
}

impl Fraction {
    pub fn _new(numer: i32, denom: i32) -> Self {
        let val = Ratio::from((BigInt::from(numer), BigInt::from(denom)));

        Self { val }
    }

    pub fn new(numer: Integer, denom: Integer) -> Self {
        let val = Ratio::from((numer.into(), denom.into()));

        Self { val }
    }

    pub fn numer(&self) -> Integer {
        self.val.numer().to_owned().into()
    }

    pub fn denom(&self) -> Integer {
        self.val.denom().to_owned().into()
    }

    pub fn is_zero(&self) -> bool {
        self.val.is_zero()
    }

    pub fn abs(&self) -> Self {
        self.val.to_owned().abs().into()
    }

    pub fn floor(&self) -> Integer {
        self.val.clone().floor().numer().to_owned().into()
    }

    pub fn ceil(&self) -> Integer {
        self.val.clone().ceil().numer().to_owned().into()
    }

    pub fn round(&self) -> Integer {
        self.val.clone().round().numer().to_owned().into()
    }
}

impl From<BigRational> for Fraction {
    fn from(val: BigRational) -> Self {
        Self { val }
    }
}

impl From<Fraction> for BigRational {
    fn from(value: Fraction) -> Self {
        value.val
    }
}

impl From<&Integer> for Fraction {
    fn from(value: &Integer) -> Self {
        Self {
            val: BigRational::from(BigInt::from(value.to_owned())),
        }
    }
}

impl Add for &Fraction {
    type Output = Fraction;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val + &rhs.val,
        }
    }
}

impl Sub for &Fraction {
    type Output = Fraction;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val - &rhs.val,
        }
    }
}

impl Mul for &Fraction {
    type Output = Fraction;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val * &rhs.val,
        }
    }
}

impl Div for &Fraction {
    type Output = Fraction;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val / &rhs.val,
        }
    }
}

impl InfixOperable for Fraction {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(Fraction { val }) => Some(Object::Fraction(Fraction {
                val: &self.val + val,
            })),
            Object::Integer(val) => Some(Object::Fraction(self + &Fraction::from(val))),
            Object::Float(val) => Some(Object::Float(&Float::from(self) + val)),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(val) => Some(Object::Fraction(self - val)),
            Object::Integer(val) => Some(Object::Fraction(self - &Fraction::from(val))),
            Object::Float(val) => Some(Object::Float(&Float::from(self) - val)),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(val) => Some(Object::Fraction(self * val)),
            Object::Integer(val) => Some(Object::Fraction(self * &Fraction::from(val))),
            Object::Float(val) => Some(Object::Float(&Float::from(self) * val)),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(val) => Some(Object::Fraction(self / val)),
            Object::Integer(val) => Some(Object::Fraction(self / &Fraction::from(val))),
            Object::Float(val) => Some(Object::Float(&Float::from(self) / val)),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => {
                let exponent: u32 = val.try_into().unwrap_or(u32::MAX);
                Some(Object::Fraction(
                    Pow::pow(self.val.to_owned(), exponent).into(),
                ))
            }
            _ => None,
        }
    }

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self < &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self < val))),
            Object::Float(val) => Some(Object::Boolean(Bool::from(&Float::from(self) < val))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self <= &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self <= val))),
            Object::Float(val) => Some(Object::Boolean(Bool::from(&Float::from(self) <= val))),
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self > &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self > val))),
            Object::Float(val) => Some(Object::Boolean(Bool::from(&Float::from(self) > val))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self >= &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self >= val))),
            Object::Float(val) => Some(Object::Boolean(Bool::from(&Float::from(self) >= val))),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self == &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self == val))),
            Object::Float(val) => Some(Object::Boolean(Bool::from(&Float::from(self) == val))),
            _ => Some(Object::Boolean(false.into())),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self != &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self != val))),
            Object::Float(val) => Some(Object::Boolean(Bool::from(&Float::from(self) != val))),
            _ => Some(Object::Boolean(true.into())),
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
