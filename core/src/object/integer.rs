use std::{
    fmt,
    ops::{Add, AddAssign, Div, Mul, Rem, Sub},
};

use rug::{ops::Pow, Complete};

use crate::lexer::Radix;

use super::{decimal::Float, Bool, Fraction, InfixOperable, Object, PrefixOperable};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Integer {
    val: rug::Integer,
}

impl Integer {
    pub fn new(literal: &str, radix: Radix) -> Self {
        let val = rug::Integer::parse_radix(literal.as_bytes(), u32::from(radix) as i32)
            .unwrap()
            .complete();

        Self { val }
    }

    pub fn to_machine_magnitude(&self) -> usize {
        if self.val <= rug::Integer::ZERO {
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
            val: rug::Integer::ONE.to_owned(),
        }
    }

    pub fn zero() -> Self {
        Self {
            val: rug::Integer::ZERO,
        }
    }
}

impl Add for &Integer {
    type Output = Integer;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val + &rhs.val).into(),
        }
    }
}

impl AddAssign for Integer {
    fn add_assign(&mut self, rhs: Self) {
        self.val += rug::Integer::from(rhs);
    }
}

impl Sub for &Integer {
    type Output = Integer;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val - &rhs.val).into(),
        }
    }
}

impl Mul for &Integer {
    type Output = Integer;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val * &rhs.val).into(),
        }
    }
}

impl Div for &Integer {
    type Output = Integer;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val / &rhs.val).into(),
        }
    }
}

impl Rem for &Integer {
    type Output = Integer;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: (&self.val % &rhs.val).into(),
        }
    }
}

impl TryFrom<&Integer> for u32 {
    type Error = ();
    fn try_from(value: &Integer) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl From<i32> for Integer {
    fn from(value: i32) -> Self {
        let val = rug::Integer::from(value);
        Self { val }
    }
}

impl From<i64> for Integer {
    fn from(value: i64) -> Self {
        let val = rug::Integer::from(value);
        Self { val }
    }
}

impl From<usize> for Integer {
    fn from(value: usize) -> Self {
        let val = rug::Integer::from(value);
        Self { val }
    }
}

impl From<rug::Integer> for Integer {
    fn from(val: rug::Integer) -> Self {
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

impl From<Integer> for rug::Integer {
    fn from(value: Integer) -> Self {
        value.val
    }
}

impl<'a> From<&'a Integer> for &'a rug::Integer {
    fn from(value: &'a Integer) -> Self {
        &value.val
    }
}

impl InfixOperable for Integer {
    fn bitwise_and(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer((&self.val & val).complete().into()))
            }
            _ => None,
        }
    }

    fn or(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer((&self.val | val).complete().into()))
            }
            _ => None,
        }
    }

    fn bitwise_xor(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer((&self.val ^ val).complete().into()))
            }
            _ => None,
        }
    }

    fn left_shift(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Integer(
                (&self.val << int.to_machine_magnitude(),)
                    .0
                    .complete()
                    .into(),
            )),
            _ => None,
        }
    }

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self < val))),
            Object::Float(val) => Some(Object::Boolean((&Float::from(self) < val).into())),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(&Fraction::from(self) < val))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val <= *val))),
            Object::Float(val) => Some(Object::Boolean((&Float::from(self) <= val).into())),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) <= val)))
            }
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self > val).into()),
            Object::Float(val) => Some(Object::Boolean((&Float::from(self) > val).into())),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(&Fraction::from(self) > val))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self >= val).into()),
            Object::Float(val) => Some(Object::Boolean((&Float::from(self) >= val).into())),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) >= val)))
            }
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self == val).into()),
            Object::Float(val) => Some(Object::Boolean((&Float::from(self) == val).into())),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) == val)))
            }
            _ => Some(Object::Boolean(false.into())),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some((self.val != *val).into()),
            Object::Float(val) => Some(Object::Boolean((&Float::from(self) != val).into())),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) != val)))
            }
            _ => Some(Object::Boolean(true.into())),
        }
    }

    fn rem(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer((&self.val % val).complete().into()))
            }
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer((&self.val / val).complete().into()))
            }
            Object::Float(val) => Some(Object::Float(&Float::from(self) / val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) / val)),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => {
                let exponent: u32 = int.val.to_owned().abs().try_into().unwrap_or(u32::MAX);
                if int.val.is_negative() {
                    let inverse = Pow::pow(self.val.to_owned(), exponent);
                    let val = Float::from(&Integer::from(inverse)).inv();

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
                (&self.val >> int.to_machine_magnitude(),)
                    .0
                    .complete()
                    .into(),
            )),
            _ => None,
        }
    }

    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self + val)),
            Object::Float(val) => Some(Object::Float(&Float::from(self) + val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) + val)),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self - val)),
            Object::Float(val) => Some(Object::Float(&Float::from(self) - val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) - val)),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self * val)),
            Object::Float(val) => Some(Object::Float(&Float::from(self) * val)),
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
