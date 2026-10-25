use super::{float::Float, Bool, Fraction, InfixOperable, Object, PrefixOperable};
use crate::lexer::Radix;
use num_bigint::BigInt;
use num_traits::{Pow, Signed, ToPrimitive};
use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Add, AddAssign, Div, Mul, Rem, Sub},
};

/// Canonical representation: Big is used only outside the i64 range.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Integer {
    val: Repr,
}
#[derive(Clone, Debug, PartialEq, Eq)]
enum Repr {
    Small(i64),
    Big(BigInt),
}
impl Integer {
    fn big(&self) -> Cow<'_, BigInt> {
        match &self.val {
            Repr::Small(n) => Cow::Owned((*n).into()),
            Repr::Big(n) => Cow::Borrowed(n),
        }
    }
    pub fn new(literal: &str, radix: Radix) -> Self {
        if let Ok(n) = i64::from_str_radix(literal, u32::from(radix)) {
            n.into()
        } else {
            BigInt::parse_bytes(literal.as_bytes(), u32::from(radix))
                .unwrap()
                .into()
        }
    }
    pub fn to_machine_magnitude(&self) -> usize {
        match &self.val {
            Repr::Small(n) => usize::try_from(*n).unwrap_or(if *n < 0 { 0 } else { usize::MAX }),
            Repr::Big(n) => {
                if n.is_negative() {
                    0
                } else {
                    n.to_usize().unwrap_or(usize::MAX)
                }
            }
        }
    }
    pub fn is_zero(&self) -> bool {
        matches!(self.val, Repr::Small(0))
    }
    pub fn is_negative(&self) -> bool {
        match &self.val {
            Repr::Small(n) => *n < 0,
            Repr::Big(n) => n.is_negative(),
        }
    }
    pub fn magnitude(&self) -> Self {
        self.abs()
    }
    pub fn one() -> Self {
        1i64.into()
    }
    pub fn zero() -> Self {
        0i64.into()
    }
    pub fn abs(&self) -> Self {
        if let Repr::Small(n) = self.val {
            if let Some(n) = n.checked_abs() {
                return n.into();
            }
        }
        self.big().abs().into()
    }
}
impl Ord for Integer {
    fn cmp(&self, rhs: &Self) -> Ordering {
        match (&self.val, &rhs.val) {
            (Repr::Small(a), Repr::Small(b)) => a.cmp(b),
            (Repr::Big(a), Repr::Big(b)) => a.cmp(b),
            (Repr::Big(a), Repr::Small(_)) => {
                if a.is_negative() {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            }
            (Repr::Small(_), Repr::Big(b)) => {
                if b.is_negative() {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            }
        }
    }
}
impl PartialOrd for Integer {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}
impl Hash for Integer {
    fn hash<H: Hasher>(&self, h: &mut H) {
        match &self.val {
            Repr::Small(n) => {
                0u8.hash(h);
                n.hash(h);
            }
            Repr::Big(n) => {
                1u8.hash(h);
                n.hash(h);
            }
        }
    }
}
macro_rules! arithmetic {
    ($trait:ident, $method:ident, $checked:ident, $op:tt) => {
        impl $trait for &Integer {
            type Output = Integer;
            fn $method(self, rhs: Self) -> Integer {
                if let (Repr::Small(a), Repr::Small(b)) = (&self.val, &rhs.val) {
                    if let Some(n) = a.$checked(*b) { return n.into(); }
                }
                (self.big().as_ref() $op rhs.big().as_ref()).into()
            }
        }
    }
}
arithmetic!(Add, add, checked_add, +);
arithmetic!(Sub, sub, checked_sub, -);
arithmetic!(Mul, mul, checked_mul, *);
arithmetic!(Div, div, checked_div, /);
arithmetic!(Rem, rem, checked_rem, %);
impl AddAssign for Integer {
    fn add_assign(&mut self, rhs: Self) {
        *self = &*self + &rhs;
    }
}
impl TryFrom<&Integer> for u32 {
    type Error = ();
    fn try_from(n: &Integer) -> Result<Self, ()> {
        match &n.val {
            Repr::Small(n) => (*n).try_into().map_err(|_| ()),
            Repr::Big(n) => n.to_u32().ok_or(()),
        }
    }
}
impl TryFrom<&Integer> for f64 {
    type Error = ();
    fn try_from(n: &Integer) -> Result<Self, ()> {
        match &n.val {
            Repr::Small(n) => Ok(*n as f64),
            Repr::Big(n) => n.to_f64().ok_or(()),
        }
    }
}
impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.val {
            Repr::Small(n) => n.fmt(f),
            Repr::Big(n) => n.fmt(f),
        }
    }
}
impl From<i64> for Integer {
    fn from(n: i64) -> Self {
        Self {
            val: Repr::Small(n),
        }
    }
}
impl From<i32> for Integer {
    fn from(n: i32) -> Self {
        (n as i64).into()
    }
}
impl From<usize> for Integer {
    fn from(n: usize) -> Self {
        match i64::try_from(n) {
            Ok(n) => n.into(),
            Err(_) => BigInt::from(n).into(),
        }
    }
}
impl From<BigInt> for Integer {
    fn from(n: BigInt) -> Self {
        match n.to_i64() {
            Some(n) => n.into(),
            None => Self { val: Repr::Big(n) },
        }
    }
}
impl From<&str> for Integer {
    fn from(n: &str) -> Self {
        n.parse::<i64>()
            .map(Self::from)
            .unwrap_or_else(|_| n.parse::<BigInt>().unwrap().into())
    }
}
impl From<Integer> for BigInt {
    fn from(n: Integer) -> Self {
        match n.val {
            Repr::Small(n) => n.into(),
            Repr::Big(n) => n,
        }
    }
}
impl From<&Integer> for BigInt {
    fn from(n: &Integer) -> Self {
        n.big().into_owned()
    }
}
impl InfixOperable for Integer {
    fn bitwise_and(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(
                (self.big().as_ref() & val.big().as_ref()).into(),
            )),
            _ => None,
        }
    }

    fn or(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(
                (self.big().as_ref() | val.big().as_ref()).into(),
            )),
            _ => None,
        }
    }

    fn bitwise_xor(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(
                (self.big().as_ref() ^ val.big().as_ref()).into(),
            )),
            _ => None,
        }
    }

    fn left_shift(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Integer(
                (self.big().as_ref() << int.to_machine_magnitude(),)
                    .0
                    .into(),
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
            Object::Integer(val) => Some(Object::Boolean(Bool::from(self <= val))),
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
            Object::Integer(val) => Some((self != val).into()),
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
            Object::Integer(val) => Some(Object::Integer(self % val)),
            _ => None,
        }
    }

    fn over(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(val) => Some(Object::Integer(self / val)),
            Object::Float(val) => Some(Object::Float(&Float::try_from(self).ok()? / val)),
            Object::Fraction(val) => Some(Object::Fraction(&Fraction::from(self) / val)),
            _ => None,
        }
    }

    fn pow(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => {
                let (_, exponent) = int.big().into_owned().into_parts();
                if int.is_negative() {
                    let inverse = Pow::pow(self.big().into_owned(), exponent);
                    let val = Float::try_from(&Integer::from(inverse)).ok()?.inv();

                    Some(Object::Float(val))
                } else {
                    let val = Pow::pow(self.big().into_owned(), exponent);

                    Some(Object::Integer(val.into()))
                }
            }
            _ => None,
        }
    }

    fn right_shift(&self, other: &Object) -> Option<Object> {
        match other {
            Object::Integer(int) => Some(Object::Integer(
                (self.big().as_ref() >> int.to_machine_magnitude(),)
                    .0
                    .into(),
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
        if let Repr::Small(n) = self.val {
            return Some(Object::Integer((!n).into()));
        }
        Some(Object::Integer(Integer::from(!self.big().into_owned())))
    }

    fn inverse(&self) -> Option<Object> {
        if let Repr::Small(n) = self.val {
            if let Some(n) = n.checked_neg() {
                return Some(Object::Integer(n.into()));
            }
        }
        Some(Object::Integer(Integer::from(-self.big().into_owned())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;
    #[test]
    fn arithmetic_and_order_match_bigint_at_representation_boundaries() {
        let inputs = [
            "-9223372036854775809",
            "-9223372036854775808",
            "-17",
            "-1",
            "0",
            "1",
            "17",
            "9223372036854775807",
            "9223372036854775808",
        ];
        for a in inputs {
            let x = Integer::from(a);
            let big_x: BigInt = a.parse().unwrap();
            for b in inputs {
                let y = Integer::from(b);
                let big_y: BigInt = b.parse().unwrap();
                assert_eq!(BigInt::from(&x + &y), &big_x + &big_y);
                assert_eq!(BigInt::from(&x - &y), &big_x - &big_y);
                assert_eq!(BigInt::from(&x * &y), &big_x * &big_y);
                assert_eq!(x.cmp(&y), big_x.cmp(&big_y));
                if !y.is_zero() {
                    assert_eq!(BigInt::from(&x / &y), &big_x / &big_y);
                    assert_eq!(BigInt::from(&x % &y), &big_x % &big_y);
                }
            }
        }
    }
    #[test]
    fn normalization_preserves_equality_and_hashing() {
        let promoted = &Integer::from(i64::MAX) + &Integer::one();
        let normalized = &promoted - &Integer::one();
        let small = Integer::from(i64::MAX);
        assert!(matches!(promoted.val, Repr::Big(_)));
        assert!(matches!(normalized.val, Repr::Small(_)));
        assert_eq!(normalized, small);
        let hash = |n: Integer| {
            let mut h = DefaultHasher::new();
            n.hash(&mut h);
            h.finish()
        };
        assert_eq!(hash(normalized), hash(small));
        assert_eq!(u32::try_from(&Integer::from(42)), Ok(42));
        assert!(u32::try_from(&Integer::from(-1)).is_err());
        assert!(u32::try_from(&promoted).is_err());
    }
}
