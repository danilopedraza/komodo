use std::{
    fmt,
    ops::{Add, AddAssign, Div, Mul, Rem, Sub},
};

use bigdecimal::{One, Pow, Signed, Zero};
use num_bigint::BigInt;

use crate::lexer::Radix;

use super::{decimal::Decimal, Bool, Fraction, InfixOperable, Object, PrefixOperable};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Integer {
    val: BigInt,
}

impl Integer {
    pub fn new(literal: &str, radix: Radix) -> Self {
        let val = BigInt::parse_bytes(literal.as_bytes(), radix.into()).unwrap();

        Self { val }
    }

    pub fn to_machine_magnitude(&self) -> usize {
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

    pub fn is_negative(&self) -> bool {
        self.val.is_negative()
    }

    pub fn magnitude(&self) -> Self {
        Self {
            val: BigInt::from_biguint(num_bigint::Sign::Plus, self.val.magnitude().to_owned()),
        }
    }

    pub fn one() -> Self {
        Self { val: BigInt::one() }
    }

    pub fn zero() -> Self {
        Self {
            val: BigInt::zero(),
        }
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
        self.val += BigInt::from(rhs);
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
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self < val))),
            Object::Decimal(val) => Some(Object::Boolean((&Decimal::from(self) < val).into())),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(&Fraction::from(self) < val))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some(Object::Boolean(Bool::from(self.val <= *val))),
            Object::Decimal(val) => Some(Object::Boolean((&Decimal::from(self) <= val).into())),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) <= val)))
            }
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self > val).into()),
            Object::Decimal(val) => Some(Object::Boolean((&Decimal::from(self) > val).into())),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(&Fraction::from(self) > val))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self >= val).into()),
            Object::Decimal(val) => Some(Object::Boolean((&Decimal::from(self) >= val).into())),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) >= val)))
            }
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some((self == val).into()),
            Object::Decimal(val) => Some(Object::Boolean((&Decimal::from(self) == val).into())),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) == val)))
            }
            _ => Some(Object::Boolean(false.into())),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => Some((self.val != *val).into()),
            Object::Decimal(val) => Some(Object::Boolean((&Decimal::from(self) != val).into())),
            Object::Fraction(val) => {
                Some(Object::Boolean(Bool::from(&Fraction::from(self) != val)))
            }
            _ => Some(Object::Boolean(true.into())),
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

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(Integer { val }) => {
                Some(Object::Integer(Integer::from(&self.val / val)))
            }
            Object::Decimal(val) => Some(Object::Decimal(&Decimal::from(self) / val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) / val)),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => {
                if int.val.is_negative() {
                    let inverse = Pow::pow(self.val.to_owned(), int.val.magnitude());
                    let val = Decimal::from(&Integer::from(inverse)).inv();

                    Some(Object::Decimal(val))
                } else {
                    let val = Pow::pow(self.val.to_owned(), int.val.magnitude());

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
            Object::Integer(val) => Some(Object::Integer(self + val)),
            Object::Decimal(val) => Some(Object::Decimal(&Decimal::from(self) + val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) + val)),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self - val)),
            Object::Decimal(val) => Some(Object::Decimal(&Decimal::from(self) - val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) - val)),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self * val)),
            Object::Decimal(val) => Some(Object::Decimal(&Decimal::from(self) * val)),
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
        Some(Object::Integer(Integer::from(!&self.val)))
    }

    fn inverse(&self) -> Option<Object> {
        Some(Object::Integer(Integer::from(-&self.val)))
    }
}
