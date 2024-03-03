pub trait InfixOperable {
    fn bitwise_and(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn bitwise_or(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn bitwise_xor(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn equality(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn greater(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn greater_equal(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn left_shift(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn less(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn less_equal(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn logic_and(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn logic_or(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn modulo(&self, _other: _Object) -> Result<_Object, ()> {
      Err(())
  }

    fn neq(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn over(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn pow(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn product(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn right_shift(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn sum(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }

    fn substraction(&self, _other: _Object) -> Result<_Object, ()> {
        Err(())
    }
}

pub trait PrefixOperable {
    fn bitwise_not(&self) -> Result<_Object, ()> {
      Err(())
    }

    fn logic_not(&self) -> Result<_Object, ()> {
      Err(())
    }

    fn inverse(&self) -> Result<_Object, ()> {
      Err(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum _Object {
    Boolean(Bool),
    Char(Char),
    ExtensionSet(ExtensionSet),
    ComprehensionSet(ComprehensionSet),
    Integer(Integer),
    String(MyString),
    Symbol(Symbol),
}

impl _Object {
    pub fn bitwise_and(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.bitwise_and(other),
            _ => todo!(),
        }
    }

    pub fn bitwise_or(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.bitwise_or(other),
            _ => todo!(),
        }
    }

    pub fn bitwise_xor(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.bitwise_xor(other),
            _ => todo!(),
        }
    }

    pub fn equality(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Symbol(symbol) => symbol.equality(other),
            _ => todo!(),
        }
    }

    pub fn greater(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.greater(other),
            _ => todo!(),
        }
    }

    pub fn greater_equal(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.greater_equal(other),
            _ => todo!(),
        }
    }

    pub fn left_shift(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.left_shift(other),
            _ => todo!(),
        }
    }

    pub fn less(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.less(other),
            _ => todo!(),
        }
    }

    pub fn less_equal(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.less_equal(other),
            _ => todo!(),
        }
    }

    pub fn logic_and(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Boolean(boolean) => boolean.logic_and(other),
            _ => todo!(),
        }
    }

    pub fn logic_or(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Boolean(boolean) => boolean.logic_or(other),
            _ => todo!(),
        }
    }

    pub fn modulo(&self, other: _Object) -> Result<_Object, ()> {
      match self {
          Self::Integer(int) => int.modulo(other),
          _ => todo!(),
      }
  }

    pub fn neq(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.neq(other),
            Self::Boolean(boolean) => boolean.neq(other),
            _ => todo!(),
        }
    }

    pub fn over(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.over(other),
            _ => todo!(),
        }
    }

    pub fn pow(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.pow(other),
            _ => todo!(),
        }
    }

    pub fn product(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.product(other),
            _ => todo!(),
        }
    }

    pub fn right_shift(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.right_shift(other),
            _ => todo!(),
        }
    }

    pub fn sum(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.sum(other),
            _ => todo!(),
        }
    }

    pub fn substraction(&self, other: _Object) -> Result<_Object, ()> {
        match self {
            Self::Integer(int) => int.substraction(other),
            _ => todo!(),
        }
    }
}

impl _Object {
  pub fn bitwise_not(&self) -> Result<_Object, ()> {
    match self {
      _Object::Integer(int) => int.bitwise_not(),
      _ => todo!(),
    }
  }

  pub fn logic_not(&self) -> Result<_Object, ()> {
    match self {
      _Object::Boolean(boolean) => boolean.logic_not(),
      _ => todo!(),
    }
  }

  pub fn inverse(&self) -> Result<_Object, ()> {
    match self {
      _Object::Integer(int) => int.inverse(),
      _ => todo!(),
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Bool {
    val: bool,
}

impl InfixOperable for Bool {
    fn logic_and(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Boolean(boolean) => Ok(_Object::Boolean(Bool::from(self.val && boolean.val))),
            _ => todo!(),
        }
    }

    fn logic_or(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Boolean(boolean) => Ok(_Object::Boolean(Bool::from(self.val || boolean.val))),
            _ => todo!(),
        }
    }
}

impl PrefixOperable for Bool {
  fn logic_not(&self) -> Result<_Object, ()> {
      Ok(_Object::Boolean(Bool::from(!self.val)))
  }
}

impl From<bool> for Bool {
    fn from(val: bool) -> Self {
        Self { val }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Char {}

#[derive(Debug, PartialEq, Eq)]
pub struct ExtensionSet {
    list: Vec<_Object>,
}

impl ExtensionSet {
    pub fn new(list: Vec<_Object>) -> _Object {
        _Object::ExtensionSet(Self { list })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ComprehensionSet {}

#[derive(Debug, PartialEq, Eq)]
pub struct Integer {
    val: i64,
}

impl Integer {
    pub fn new(val: &str) -> _Object {
        _Object::Integer(Self {
            val: val.parse().unwrap(),
        })
    }
}

impl From<i64> for Integer {
    fn from(val: i64) -> Self {
        Self { val }
    }
}

impl InfixOperable for Integer {
    fn bitwise_and(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val & val)))
            }
            _ => Err(()),
        }
    }

    fn bitwise_or(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val | val)))
            }
            _ => Err(()),
        }
    }

    fn bitwise_xor(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val ^ val)))
            }
            _ => Err(()),
        }
    }

    fn greater(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => Ok(_Object::Boolean(Bool::from(self.val > val))),
            _ => Err(()),
        }
    }

    fn greater_equal(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => Ok(_Object::Boolean(Bool::from(self.val >= val))),
            _ => Err(()),
        }
    }

    fn left_shift(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val << val)))
            }
            _ => Err(()),
        }
    }

    fn less(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => Ok(_Object::Boolean(Bool::from(self.val < val))),
            _ => Err(()),
        }
    }

    fn less_equal(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => Ok(_Object::Boolean(Bool::from(self.val <= val))),
            _ => Err(()),
        }
    }

    fn modulo(&self, other: _Object) -> Result<_Object, ()> {
      match other {
          _Object::Integer(Integer { val }) => Ok(_Object::Integer(Integer::from(self.val % val))),
          _ => Err(()),
      }
  }

    fn neq(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => Ok(_Object::Boolean(Bool::from(self.val != val))),
            _ => Ok(_Object::Boolean(Bool::from(true))),
        }
    }

    fn over(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val / val)))
            }
            _ => Err(()),
        }
    }

    fn pow(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => Ok(_Object::Integer(Integer::from(
                self.val.pow(val.try_into().unwrap()),
            ))),
            _ => Err(()),
        }
    }

    fn right_shift(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val >> val)))
            }
            _ => Err(()),
        }
    }

    fn sum(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val + val)))
            }
            _ => Err(()),
        }
    }

    fn substraction(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val - val)))
            }
            _ => Err(()),
        }
    }

    fn product(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Integer(Integer { val }) => {
                Ok(_Object::Integer(Integer::from(self.val * val)))
            }
            _ => Err(()),
        }
    }
}

impl PrefixOperable for Integer {
  fn bitwise_not(&self) -> Result<_Object, ()> {
      Ok(_Object::Integer(Integer::from(!self.val)))
  }

  fn inverse(&self) -> Result<_Object, ()> {
    Ok(_Object::Integer(Integer::from(-self.val)))
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MyString {}

#[derive(Debug, PartialEq, Eq)]
pub struct Symbol {
    val: String,
}

impl Symbol {
    pub fn new(val: &str) -> _Object {
        _Object::Symbol(Self {
            val: val.to_string(),
        })
    }
}

impl InfixOperable for Symbol {
    fn equality(&self, other: _Object) -> Result<_Object, ()> {
        match other {
            _Object::Symbol(symbol) => Ok(_Object::Boolean(Bool::from(self.val == symbol.val))),
            _ => todo!(),
        }
    }
}
