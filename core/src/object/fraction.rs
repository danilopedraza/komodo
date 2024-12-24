use std::{
    fmt,
    ops::{Add, Div, Mul, Sub},
};

use bigdecimal::{BigDecimal, Pow, Zero};
use num_bigint::BigInt;
use num_rational::BigRational;

use super::{decimal::Decimal, integer::Integer, Bool, InfixOperable, Object, PrefixOperable};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Fraction {
    val: BigRational,
}

impl Fraction {
    pub fn _new(numer: i32, denom: i32) -> Self {
        let val = BigRational::new(numer.into(), denom.into());

        Self { val }
    }

    pub fn new(numer: Integer, denom: Integer) -> Self {
        let val = BigRational::new(numer.into(), denom.into());

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
            val: BigRational::from_integer(value.to_owned().into()),
        }
    }
}

impl From<&Decimal> for Fraction {
    fn from(value: &Decimal) -> Self {
        let value = BigDecimal::from(value.to_owned());
        let (numer, exponent) = value.as_bigint_and_exponent();

        if exponent > 0 {
            Self {
                val: BigRational::new(numer, BigInt::from(10).pow(exponent as u64)),
            }
        } else {
            Self {
                val: BigRational::new(
                    numer * BigInt::from(10).pow(exponent.unsigned_abs()),
                    1.into(),
                ),
            }
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
            Object::Decimal(val) => Some(Object::Decimal(&Decimal::from(self) + val)),
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(val) => Some(Object::Fraction(self - val)),
            Object::Integer(val) => Some(Object::Fraction(self - &Fraction::from(val))),
            Object::Decimal(val) => Some(Object::Decimal(&Decimal::from(self) - val)),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(val) => Some(Object::Fraction(self * val)),
            Object::Integer(val) => Some(Object::Fraction(self * &Fraction::from(val))),
            Object::Decimal(val) => Some(Object::Decimal(&Decimal::from(self) * val)),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Fraction(val) => Some(Object::Fraction(self / val)),
            Object::Integer(val) => Some(Object::Fraction(self / &Fraction::from(val))),
            Object::Decimal(val) => Some(Object::Decimal(&Decimal::from(self) / val)),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Fraction(
                Pow::pow(self.val.to_owned(), BigInt::from(val.to_owned())).into(),
            )),
            _ => None,
        }
    }

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self < &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self < val))),
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(self < &Fraction::from(val)))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self <= &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self <= val))),
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(&Decimal::from(self) <= val))),
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self > &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self > val))),
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(&Decimal::from(self) > val))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self >= &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self >= val))),
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(&Decimal::from(self) >= val))),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self == &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self == val))),
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(&Decimal::from(self) == val))),
            _ => Some(Object::Boolean(false.into())),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self != &Fraction::from(val)))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self != val))),
            Object::Decimal(val) => Some(Object::Boolean(Bool::from(&Decimal::from(self) != val))),
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
