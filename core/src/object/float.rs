use std::{
    fmt,
    hash::Hash,
    ops::{Add, Div, Mul, Rem, Sub},
    time::Duration,
};

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{Pow, ToPrimitive, Zero};

use super::{integer::Integer, Bool, Fraction, InfixOperable, Object, ObjectError, PrefixOperable};

macro_rules! wrapper_fn {
    ($name:ident) => {
        pub fn $name(self) -> Self {
            self.val.$name().into()
        }
    };
}

#[derive(Clone, Debug)]
pub struct Float {
    val: f64,
}

pub static PREC: u32 = 53;
impl Float {
    pub fn new(int: &str, dec: &str) -> Self {
        Self::from_num_str(&format!("{int}.{dec}"))
    }

    pub fn from_num_str(str: &str) -> Self {
        str.parse::<f64>().unwrap().into()
    }

    pub fn is_zero(&self) -> bool {
        self.val.is_zero()
    }

    pub fn inv(&self) -> Self {
        Self {
            val: self.val.to_owned().recip(),
        }
    }

    pub fn abs(&self) -> Self {
        self.val.to_owned().abs().into()
    }

    pub fn hypot(self, other: Self) -> Self {
        self.val.hypot(other.into()).into()
    }

    pub fn floor(&self) -> Result<Integer, ObjectError> {
        Ok(self.val.floor().to_i64().unwrap().into())
    }

    pub fn ceil(&self) -> Result<Integer, ObjectError> {
        Ok(self.val.ceil().to_i64().unwrap().into())
    }

    pub fn round(&self) -> Result<Integer, ObjectError> {
        Ok(self.val.round().to_i64().unwrap().into())
    }

    wrapper_fn!(sin);
    wrapper_fn!(cos);
    wrapper_fn!(tan);
    wrapper_fn!(asin);
    wrapper_fn!(acos);
    wrapper_fn!(atan);
    wrapper_fn!(exp);
    wrapper_fn!(ln);
    wrapper_fn!(cbrt);
    wrapper_fn!(sqrt);
}

impl Ord for Float {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.val.total_cmp(&other.val)
    }
}

impl PartialOrd for Float {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        self.val.to_bits() == other.val.to_bits()
    }
}

impl Eq for Float {}

impl Hash for Float {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.val.to_bits().hash(state);
    }
}

impl Add for &Float {
    type Output = Float;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: self.val + rhs.val,
        }
    }
}

impl Sub for &Float {
    type Output = Float;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: self.val - rhs.val,
        }
    }
}

impl Mul for &Float {
    type Output = Float;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: self.val * rhs.val,
        }
    }
}

impl Div for &Float {
    type Output = Float;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Output {
            val: self.val / rhs.val,
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
            val: self.val % rhs.val,
        }
    }
}

impl PrefixOperable for Float {
    fn inverse(&self) -> Option<Object> {
        Some(Object::Float(Float::from(-&self.val)))
    }
}

impl From<Float> for f64 {
    fn from(val: Float) -> Self {
        val.val
    }
}

impl TryFrom<&Integer> for Float {
    type Error = ObjectError;
    fn try_from(value: &Integer) -> Result<Self, ObjectError> {
        value
            .try_into()
            .map(|val| Self { val })
            .map_err(|_| ObjectError::FailedCast {
                from: "Integer".into(),
                to: "Float".into(),
            })
    }
}

impl From<f64> for Float {
    fn from(val: f64) -> Self {
        Self { val }
    }
}

impl From<&Float> for Duration {
    fn from(value: &Float) -> Self {
        Duration::from_secs_f64(value.val)
    }
}

impl From<&Fraction> for Float {
    fn from(frac: &Fraction) -> Self {
        let frac = BigRational::from(frac.to_owned());
        let mut val = 0.0;
        val += frac.numer().to_f64().unwrap();
        val /= frac.denom().to_f64().unwrap();

        Self { val }
    }
}

impl InfixOperable for Float {
    fn sum(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(Float { val }) => Some(Object::Float(Float::from(self.val + val))),
            Object::Integer(val) => Some(Object::Float(self + &Float::try_from(val).ok()?)),
            Object::Fraction(frac) => {
                Some(Object::Float(Float::from(self.val + Float::from(frac).val)))
            }
            _ => None,
        }
    }

    fn substraction(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Float(self - val)),
            Object::Integer(val) => Some(Object::Float(self - &Float::try_from(val).ok()?)),
            Object::Fraction(frac) => Some(Object::Float(self - &Float::from(frac))),
            _ => None,
        }
    }

    fn product(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Float(self * val)),
            Object::Integer(val) => Some(Object::Float(self * &Float::try_from(val).ok()?)),
            Object::Fraction(val) => Some(Object::Float(self * &Float::from(val))),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Float(self / val)),
            Object::Integer(val) => Some(Object::Float(self / &Float::try_from(val).ok()?)),
            Object::Fraction(val) => Some(Object::Float(self / &Float::from(val))),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Float(
                (&self.val)
                    .pow(i32::try_from(<&BigInt>::from(int)).unwrap())
                    .into(),
            )),
            _ => None,
        }
    }

    fn greater(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self > val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(
                self > &Float::try_from(val).ok()?,
            ))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self > &Float::from(val)))),
            _ => None,
        }
    }

    fn less(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self < val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(
                self < &Float::try_from(val).ok()?,
            ))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self < &Float::from(val)))),
            _ => None,
        }
    }

    fn greater_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self >= val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(
                self >= &Float::try_from(val).ok()?,
            ))),
            Object::Fraction(frac) => Some(Object::Boolean(Bool::from(
                self.val >= Float::from(frac).val,
            ))),
            _ => None,
        }
    }

    fn less_equal(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self <= val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(
                self <= &Float::try_from(val).ok()?,
            ))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self <= &Float::from(val)))),
            _ => None,
        }
    }

    fn equality(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self == val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(
                self == &Float::try_from(val).ok()?,
            ))),
            Object::Fraction(val) => Some(Object::Boolean(Bool::from(self == &Float::from(val)))),
            _ => Some(Object::Boolean(false.into())),
        }
    }

    fn neq(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Boolean(Bool::from(self != val))),
            Object::Integer(val) => Some(Object::Boolean(Bool::from(
                self != &Float::try_from(val).ok()?,
            ))),
            Object::Fraction(frac) => Some(Object::Boolean(Bool::from(
                self.val != Float::from(frac).val,
            ))),
            _ => Some(Object::Boolean(true.into())),
        }
    }

    fn rem(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Float(val) => Some(Object::Float(self % val)),
            Object::Integer(val) => Some(Object::Float(self % &Float::try_from(val).ok()?)),
            _ => None,
        }
    }
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.val.fmt(f)
    }
}
