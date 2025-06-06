use std::{
    any::Any,
    fmt::{Debug, Display},
};

use crate::{distribution::Distribution, parser::Op};

pub mod num;
pub use num::*;

pub mod bool;
pub use bool::*;

pub mod string;
pub use string::*;

pub mod dice;
pub use dice::*;

pub mod arr;
pub use arr::*;

trait BaseType {
    fn name(&self) -> String;
    fn dup(&self) -> Datatype;
}

#[allow(private_bounds)]
pub trait Type: Send + Sync + Debug + Display + BaseType {
    fn prop_type(&self, _name: &str) -> Option<Datatype> {
        None
    }
    fn bin_op_result(&self, _other: Datatype, _op: Op) -> Option<Datatype> {
        None
    }
    fn pre_op_result(&self, _op: Op) -> Option<Datatype> {
        None
    }
    fn post_op_result(&self, _op: Op) -> Option<Datatype> {
        None
    }
}

trait BaseVal {
    fn base_dup(&self) -> Value;
    fn base_get_type(&self) -> Datatype;
    fn get_name(&self) -> String {
        self.base_get_type().name()
    }
    fn eq(&self, other: &Value) -> bool;
}

#[allow(private_bounds)]
pub trait Val: Display + Debug + Send + Sync + Any + BaseVal {
    fn get_prop(&self, _name: &str) -> Value {
        unreachable!("Type '{}' has no properties.", self.get_name())
    }
    fn set_prop(&mut self, _prop: &str, _value: Value) {
        unreachable!("Type '{}' has no properties.", self.get_name())
    }
    fn bin_op(&self, _other: &Value, _op: Op) -> Value {
        unreachable!("Type '{}' has no binary operations.", self.get_name())
    }
    fn pre_op(&self, _op: Op) -> Value {
        unreachable!("Type '{}' has no prefix operations.", self.get_name())
    }
    fn post_op(&self, _op: Op) -> Value {
        unreachable!("Type '{}' has no postfix operations.", self.get_name())
    }
    fn get_type(&self) -> Datatype {
        self.base_get_type()
    }
    fn dup(&self) -> Value {
        self.base_dup()
    }
}

#[macro_export]
macro_rules! invalid {
    ($op:expr, $lhs:expr, $rhs:expr) => {{
        let op = $op;
        let lhs = $lhs;
        let rhs = $rhs;
        unreachable!("Invalid operation {op:?} on {lhs:?}, {rhs:?}")
    }};
}

#[macro_export]
macro_rules! _make_type {
    ($ty: ident) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $ty;
    };
    ($ty: ident, [$($tvar: ident, $tty: ident),*]) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $ty {
            $(pub $tvar: $tty),*
        }
    };
}

#[macro_export]
macro_rules! type_init {
    ($ty: ident, $val: ident, $repr: expr $(, $($tvar: ident : $tty: ident),*)?) => {
        crate::_make_type!($ty $(, [$($tvar, $tty),*])?);
        impl BaseType for $ty {
            fn name(&self) -> String {
                $repr.to_string()$(+format!("<{}>", vec![$(&self.$tvar),*].into_iter().map(|v|format!("{v}")).collect::<Vec<_>>().join(", ")).as_str())?
            }
            fn dup(&self) -> Datatype {
                Box::new(self.clone())
            }
        }
        impl BaseVal for $val {
            fn base_dup(&self) -> Value {
                Box::new(self.clone())
            }
            fn base_get_type(&self) -> Datatype {
                Box::new($ty $({
                    $($tvar: self.$tvar.clone()),*
                })?)
            }
            fn eq(&self, other: &Value) -> bool {
                if let Some(rhs) = other.try_downcast_ref::<$val>() {
                    rhs == self
                } else {
                    false
                }
            }
        }
        impl Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.name())
            }
        }
    };
}

pub type Datatype = Box<dyn Type>;
pub type Value = Box<dyn Val>;

impl PartialEq<dyn Type> for Datatype {
    fn eq(&self, other: &dyn Type) -> bool {
        self.name() == other.name()
    }
}

impl PartialEq for Datatype {
    fn eq(&self, other: &Self) -> bool {
        self == other.as_ref()
    }
}

impl<T: Type + 'static> PartialEq<T> for Datatype {
    fn eq(&self, other: &T) -> bool {
        self == other as &dyn Type
    }
}

impl Clone for Datatype {
    fn clone(&self) -> Self {
        BaseType::dup(self.as_ref())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other)
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        self.dup()
    }
}

#[allow(unused)]
pub trait TryDowncast {
    fn try_downcast_ref<T: Val + 'static>(&self) -> Option<&T>;
    fn try_downcast_mut<T: Val + 'static>(&mut self) -> Option<&mut T>;
}

impl TryDowncast for Value {
    fn try_downcast_ref<T: 'static>(&self) -> Option<&T> {
        (self.as_ref() as &dyn Any).downcast_ref()
    }

    fn try_downcast_mut<T: 'static>(&mut self) -> Option<&mut T> {
        (self.as_mut() as &mut dyn Any).downcast_mut()
    }
}

type_init!(Void, Void, "()");
impl Type for Void {}
impl Val for Void {}

const NUM_OPS: [Op; 4] = [Op::Plus, Op::Minus, Op::Times, Op::Divided];
const ORD_OPS: [Op; 6] = [
    Op::Equal,
    Op::NotEqual,
    Op::Greater,
    Op::Less,
    Op::Geq,
    Op::Leq,
];
