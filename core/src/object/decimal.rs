use std::{
    fmt,
    ops::{Add, Div, Mul, Rem, Sub},
};

use bigdecimal::{BigDecimal, One, Zero};
use num_rational::BigRational;

use super::{integer::Integer, Bool, Fraction, InfixOperable, Object, PrefixOperable};

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
        let zero = Integer::zero();
        let one = Integer::one();
        let two = Integer::from(2);

        let mut base = self.val.to_owned();
        let mut exp = exponent.magnitude();
        let mut val = BigDecimal::one();

        while exp != zero {
            if &exp % &two == one {
                val *= &base;
                val = val.normalized();
            }

            base = base.square().normalized();

            exp = &exp / &two;
        }

        let must_invert = exponent.is_negative();
        if must_invert {
            val = val.inverse().normalized();
        }

        Decimal { val }
    }

    pub fn is_zero(&self) -> bool {
        self.val.is_zero()
    }

    pub fn inv(&self) -> Self {
        Self {
            val: self.val.inverse().normalized(),
        }
    }
}

impl Add for &Decimal {
    type Output = Decimal;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val + &rhs.val,
        }
    }
}

impl Sub for &Decimal {
    type Output = Decimal;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val - &rhs.val,
        }
    }
}

impl Mul for &Decimal {
    type Output = Decimal;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val * &rhs.val,
        }
    }
}

impl Div for &Decimal {
    type Output = Decimal;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val / &rhs.val,
        }
    }
}

impl Rem for &Decimal {
    type Output = Decimal;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: &self.val % &rhs.val,
        }
    }
}

impl PrefixOperable for Decimal {
    fn inverse(&self) -> Option<Object> {
        Some(Object::Decimal(Decimal::from(-&self.val)))
    }
}

impl From<Decimal> for BigDecimal {
    fn from(val: Decimal) -> Self {
        val.val
    }
}

impl From<&Integer> for Decimal {
    fn from(value: &Integer) -> Self {
        Self {
            val: BigDecimal::from_bigint(value.to_owned().into(), 0),
        }
    }
}

impl From<&Fraction> for Decimal {
    fn from(frac: &Fraction) -> Self {
        let frac = BigRational::from(frac.to_owned());
        let val = BigDecimal::new(frac.numer().to_owned(), 0)
            / BigDecimal::new(frac.denom().to_owned(), 0);

        Self { val }
    }
}

impl InfixOperable for Decimal {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(Decimal { val }) => {
                Some(Object::Decimal(Decimal::from(&self.val + val)))
            }
            Object::Integer(val) => Some(Object::Decimal(self + &Decimal::from(val))),
            Object::Fraction(frac) => Some(Object::Decimal(Decimal::from(
                &self.val + Decimal::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Decimal(self - val)),
            Object::Integer(val) => Some(Object::Decimal(self - &Decimal::from(val))),
            Object::Fraction(frac) => Some(Object::Decimal(self - &Decimal::from(frac))),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Decimal(self * val)),
            Object::Integer(val) => Some(Object::Decimal(self * &Decimal::from(val))),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) * val)),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Decimal(self / val)),
            Object::Integer(val) => Some(Object::Decimal(self / &Decimal::from(val))),
            Object::Fraction(val) => Some(Object::Decimal(self / &Decimal::from(val))),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Decimal(self.binary_pow(int))),
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(self > val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self > &Decimal::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self > &Decimal::from(val)))),
            _ => None,
        }
    }

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(self < val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self < &Decimal::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self < &Decimal::from(val)))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(self >= val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self >= &Decimal::from(val)))),
            Object::Fraction(frac) => Some(Object::Boolean(Bool::from(
                self.val >= Decimal::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(self <= val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self <= &Decimal::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self <= &Decimal::from(val)))),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(self == val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self == &Decimal::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self == &Decimal::from(val)))),
            _ => Some(Object::Boolean(false.into())),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(self != val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self != &Decimal::from(val)))),
            Object::Fraction(frac) => Some(Object::Boolean(Bool::from(
                self.val != Decimal::from(frac).val,
            ))),
            _ => Some(Object::Boolean(true.into())),
        }
    }

    fn rem(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Decimal(val) => Some(Object::Decimal(self % val)),
            Object::Integer(val) => Some(Object::Decimal(self % &Decimal::from(val))),
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
