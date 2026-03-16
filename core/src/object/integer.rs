use std::{
    fmt,
    ops::{Add, AddAssign, Div, Mul, Rem, Sub},
};

use num_bigint::BigInt;
use num_traits::{Pow, Signed, ToPrimitive, Zero};

use crate::lexer::Radix;

use super::{float::Float, Bool, Fraction, InfixOperable, Object, PrefixOperable};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Integer {
    val: BigInt,
}

impl Integer {
    pub fn new(literal: &str, radix: Radix) -> Self {
        let val = BigInt::parse_bytes(literal.as_bytes(), u32::from(radix)).unwrap();

        Self { val }
    }

    pub fn to_machine_magnitude(&self) -> usize {
        if self.val <= BigInt::ZERO {
            0
        } else {
            self.val.to_usize().unwrap_or(usize::MAX)
        }
    }

    pub fn is_zero(&self) -> bool {
        self.val.is_zero()
    }

    pub fn is_negative(&self) -> bool {
        self.val.is_negative()
    }

    pub fn magnitude(&self) -> Self {
        Self {
            val: self.val.to_owned().abs(),
        }
    }

    pub fn one() -> Self {
        Self {
            val: BigInt::from(1).to_owned(),
        }
    }

    pub fn zero() -> Self {
        Self { val: BigInt::ZERO }
    }

    pub fn abs(&self) -> Self {
        self.val.to_owned().abs().into()
    }
}

impl Add for &Integer {
    type Output = Integer;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val + &rhs.val,
        }
    }
}

impl AddAssign for Integer {
    fn add_assign(&mut self, rhs: Self) {
        self.val += rhs.val;
    }
}

impl Sub for &Integer {
    type Output = Integer;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val - &rhs.val,
        }
    }
}

impl Mul for &Integer {
    type Output = Integer;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val * &rhs.val,
        }
    }
}

impl Div for &Integer {
    type Output = Integer;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val / &rhs.val,
        }
    }
}

impl Rem for &Integer {
    type Output = Integer;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val % &rhs.val,
        }
    }
}

impl TryFrom<&Integer> for u32 {
    type Error = ();
    fn try_from(value: &Integer) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl TryFrom<&Integer> for f64 {
    type Error = ();
    fn try_from(value: &Integer) -> Result<Self, Self::Error> {
        value.val.to_f64().map(Ok).unwrap_or(Err(()))
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

impl From<Integer> for BigInt {
    fn from(value: Integer) -> Self {
        value.val
    }
}

impl<'a> From<&'a Integer> for &'a BigInt {
    fn from(value: &'a Integer) -> Self {
        &value.val
    }
}

impl InfixOperable for Integer {
    fn bitwise_and(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Integer((&self.val & val).into())),
            _ => None,
        }
    }

    fn or(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Integer((&self.val | val).into())),
            _ => None,
        }
    }

    fn bitwise_xor(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Integer((&self.val ^ val).into())),
            _ => None,
        }
    }

    fn left_shift(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Integer(
                (&self.val << int.to_machine_magnitude(),).0.into(),
            )),
            _ => None,
        }
    }

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self < val))),
            Object::Float(val) => {
                Some(Object::Boolean((&Float::try_from(self).ok()? < val).into()))
            }
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(&Fraction::from(self) < val))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val <= *val))),
            Object::Float(val) => Some(Object::Boolean(
                (&Float::try_from(self).ok()? <= val).into(),
            )),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) <= val)))
            }
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self > val).into()),
            Object::Float(val) => {
                Some(Object::Boolean((&Float::try_from(self).ok()? > val).into()))
            }
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(&Fraction::from(self) > val))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self >= val).into()),
            Object::Float(val) => Some(Object::Boolean(
                (&Float::try_from(self).ok()? >= val).into(),
            )),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) >= val)))
            }
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self == val).into()),
            Object::Float(val) => Some(Object::Boolean(
                (&Float::try_from(self).ok()? == val).into(),
            )),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) == val)))
            }
            _ => Some(Object::Boolean(false.into())),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some((self.val != *val).into()),
            Object::Float(val) => Some(Object::Boolean(
                (&Float::try_from(self).ok()? != val).into(),
            )),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) != val)))
            }
            _ => Some(Object::Boolean(true.into())),
        }
    }

    fn rem(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Integer((&self.val % val).into())),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Integer((&self.val / val).into())),
            Object::Float(val) => Some(Object::Float(&Float::try_from(self).ok()? / val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) / val)),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => {
                let exponent = int.val.abs().to_biguint().unwrap();
                if int.val.is_negative() {
                    let inverse = Pow::pow(self.val.clone(), exponent);
                    let val = Float::try_from(&Integer::from(inverse)).ok()?.inv();

                    Some(Object::Float(val))
                } else {
                    let val = Pow::pow(self.val.to_owned(), exponent);

                    Some(Object::Integer(Integer { val }))
                }
            }
            _ => None,
        }
    }

    fn right_shift(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Integer(
                (&self.val >> int.to_machine_magnitude(),).0.into(),
            )),
            _ => None,
        }
    }

    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self + val)),
            Object::Float(val) => Some(Object::Float(&Float::try_from(self).ok()? + val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) + val)),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self - val)),
            Object::Float(val) => Some(Object::Float(&Float::try_from(self).ok()? - val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) - val)),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self * val)),
            Object::Float(val) => Some(Object::Float(&Float::try_from(self).ok()? * val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) * val)),
            Object::Char(chr) => Some(chr.multiply(self)),
            Object::String(str) => Some(str.multiply(self)),
            Object::List(lst) => Some(lst.multiply(self)),
            _ => None,
        }
    }
}

impl PrefixOperable for Integer {
    fn bitwise_not(&self) -> Option<Object> {
        Some(Object::Integer(Integer::from(!self.val.to_owned())))
    }

    fn inverse(&self) -> Option<Object> {
        Some(Object::Integer(Integer::from(-self.val.to_owned())))
    }
}
