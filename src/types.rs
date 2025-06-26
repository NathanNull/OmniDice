use std::{
    any::Any,
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::{distribution::Distribution, interpreter::Interpreter, parser::Op};

mod num;
pub use num::{FloatT, IntT};

mod bool;
pub use bool::BoolT;

mod string;
pub use string::StringT;

mod dice;
pub use dice::DiceT;

mod arr;
pub use arr::{Arr, ArrT};

mod tup;
pub use tup::{TupT, Tuple};

mod ref_t;
pub use ref_t::{Ref, RefT};

mod function;
pub use function::{Func, FuncT, InnerFunc};

mod func_sum;
pub use func_sum::{FuncSum, FuncSumT};

mod iter;
pub use iter::{Iter, IterT};

mod maybe;
pub use maybe::{Maybe, MaybeT};

mod range;
pub use range::{Range, RangeT};

mod typevar;
pub use typevar::TypeVar;

mod map;
pub use map::{Map, MapT};

trait GetRef<'a, T> {
    fn get_ref(&'a self) -> T;
}

impl<'a, T> GetRef<'a, &'a T> for T {
    fn get_ref(&'a self) -> &'a T {
        self
    }
}

trait BaseType {
    fn name(&self) -> String;
    fn dup(&self) -> Datatype;
}

#[allow(private_bounds)]
pub trait Type: Send + Sync + Debug + Display + Any + BaseType {
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        if !self.get_generics().is_empty() {
            Some(Box::new(TypeVar::Prop(self.dup(), name.to_string())))
        } else {
            self.real_prop_type(name)
        }
    }
    fn index_type(&self, index: &Datatype) -> Option<Datatype> {
        if !self.get_generics().is_empty() {
            Some(Box::new(TypeVar::Index(self.dup(), index.clone())))
        } else {
            self.real_index_type(index)
        }
    }
    fn bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if !self.get_generics().is_empty() || !other.get_generics().is_empty() {
            Some(Box::new(TypeVar::BinOp(self.dup(), other.dup(), op)))
        } else {
            self.real_bin_op_result(other, op)
        }
    }
    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        if !self.get_generics().is_empty() {
            Some(Box::new(TypeVar::UnaryOp(self.dup(), op, true)))
        } else {
            self.real_pre_op_result(op)
        }
    }
    fn post_op_result(&self, op: Op) -> Option<Datatype> {
        if !self.get_generics().is_empty() {
            Some(Box::new(TypeVar::UnaryOp(self.dup(), op, false)))
        } else {
            self.real_post_op_result(op)
        }
    }
    fn call_result(
        &self,
        params: Vec<Datatype>,
        expected_output: Option<Datatype>,
    ) -> Option<Datatype> {
        if !self.get_generics().is_empty() || params.iter().any(|p| !p.get_generics().is_empty()) {
            Some(Box::new(TypeVar::Call(self.dup(), params, expected_output)))
        } else {
            self.real_call_result(params, expected_output)
        }
    }
    fn real_prop_type(&self, _name: &str) -> Option<Datatype> {
        None
    }
    fn real_index_type(&self, _index: &Datatype) -> Option<Datatype> {
        None
    }
    fn real_bin_op_result(&self, _other: &Datatype, _op: Op) -> Option<Datatype> {
        None
    }
    fn real_pre_op_result(&self, _op: Op) -> Option<Datatype> {
        None
    }
    fn real_post_op_result(&self, _op: Op) -> Option<Datatype> {
        None
    }
    fn real_call_result(
        &self,
        _params: Vec<Datatype>,
        _expected_output: Option<Datatype>,
    ) -> Option<Datatype> {
        None
    }
    fn possible_call(&self) -> bool {
        false
    }
    fn insert_generics(&self, _generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(self.dup())
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        if self.name() == other.name() {
            Some(HashMap::new())
        } else {
            None
        }
    }
    fn try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        if let Some(typevar) = other.downcast::<TypeVar>() {
            typevar.real_try_match(&self.dup())
        } else {
            self.real_try_match(other)
        }
    }
    fn assert_same(&self, other: &Datatype) -> Datatype {
        let dt = self.dup();
        if let Some(o) = other.downcast::<TypeVar>() {
            return o.assert_same(&dt);
        }
        assert_eq!(&dt, other, "Expected type {self} but saw {other}");
        dt
    }
    fn get_generics(&self) -> Vec<String> {
        vec![]
    }
    fn specify_generics(&self, _generics: &Vec<Datatype>) -> Option<Datatype> {
        None
    }
    fn is_hashable(&self) -> bool {
        false
    }
}

trait BaseVal {
    fn base_dup(&self) -> Value;
    fn base_get_type(&self) -> Datatype;
    fn eq(&self, other: &Value) -> bool;
}

#[allow(private_bounds)]
pub trait Val: Debug + Display + Send + Sync + Any + BaseVal {
    fn get_prop(&self, _name: &str) -> Value {
        unreachable!("Type '{}' has no properties.", self.get_type())
    }
    fn set_prop(&self, _prop: &str, _value: Value) {
        unreachable!("Type '{}' has no properties.", self.get_type())
    }
    fn get_index(&self, _index: Value) -> Value {
        unreachable!("Type '{}' can't be indexed.", self.get_type())
    }
    fn set_index(&self, _index: Value, _value: Value) {
        unreachable!("Type '{}' can't be indexed.", self.get_type())
    }
    fn bin_op(&self, _other: &Value, _op: Op) -> Value {
        unreachable!("Type '{}' has no binary operations.", self.get_type())
    }
    fn pre_op(&self, _op: Op) -> Value {
        unreachable!("Type '{}' has no prefix operations.", self.get_type())
    }
    fn post_op(&self, _op: Op) -> Value {
        unreachable!("Type '{}' has no postfix operations.", self.get_type())
    }
    fn call(
        &self,
        _params: Vec<Value>,
        _interpreter: &mut Interpreter,
        _expected_output: Option<Datatype>,
    ) -> Value {
        unreachable!("Type '{}' cannot be called.", self.get_type())
    }
    fn get_type(&self) -> Datatype {
        self.base_get_type()
    }
    fn dup(&self) -> Value {
        self.base_dup()
    }
    fn insert_generics(&self, _generics: &Vec<Datatype>) -> Value {
        unreachable!("Type '{}' cannot have generics inserted", self.get_type())
    }
    fn hash(&self, h: &mut dyn Hasher) {
        unreachable!("Type '{}' cannot be hashed", self.get_type())
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
macro_rules! mut_type_init {
    ($name: ident, $inner: ident) => {
        #[derive(Clone)]
        pub struct $name(Arc<::std::sync::RwLock<$inner>>);

        impl $name {
            fn make(inner: $inner) -> Self {
                Self(Arc::new(::std::sync::RwLock::new(inner)))
            }
        }
        impl Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match self.0.try_read() {
                    Ok(guard) => write!(f, "{}", *guard),
                    Err(poisoned) => write!(f, "Poisoned: {}", poisoned),
                }
            }
        }
        impl Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match self.0.try_read() {
                    Ok(guard) => write!(f, "{:?}", *guard),
                    Err(poisoned) => write!(f, "Poisoned: {}", poisoned),
                }
            }
        }
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                self.0.try_read().unwrap().eq(&other.0.try_read().unwrap())
            }
        }

        impl $name {
            pub fn inner(&self) -> ::std::sync::RwLockReadGuard<$inner> {
                self.0.try_read().unwrap()
            }
            pub fn inner_mut(&self) -> ::std::sync::RwLockWriteGuard<$inner> {
                self.0.try_write().unwrap()
            }
        }

        impl<'a> GetRef<'a, ::std::sync::RwLockReadGuard<'a, $inner>> for $name {
            fn get_ref(&self) -> ::std::sync::RwLockReadGuard<$inner> {
                self.inner()
            }
        }
    };
}

#[macro_export]
macro_rules! _make_type {
    ($ty: ident, $repr: literal) => {
        #[derive(Debug, Clone)]
        pub struct $ty;
        impl Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", $repr.to_string())
            }
        }
    };
    ($ty: ident, $repr: literal, [$($tvar: ident, $tty: ty),*]) => {
        #[derive(Debug, Clone)]
        pub struct $ty {
            $(pub $tvar: $tty),*
        }
        impl Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", $repr.to_string()+format!(
                    "<{}>",
                    vec![$(format!("{}", &self.$tvar)),*]
                        .into_iter()
                        .collect::<Vec<_>>()
                        .join(", ")
                ).as_str())
            }
        }
    };
    ($ty: ident nodisplay, $repr: literal) => {
        #[derive(Debug, Clone)]
        pub struct $ty;
    };
    ($ty: ident nodisplay, $repr: literal, [$($tvar: ident, $tty: ty),*]) => {
        #[derive(Debug, Clone)]
        pub struct $ty {
            $(pub $tvar: $tty),*
        }
    };
}

#[macro_export]
macro_rules! type_init {
    ($ty: ident $({$nodisplay: ident})?, $val: ty, $repr: expr $(, $(($ref_t: ty), )? $($tvar: ident : $tty: ty),*)?) => {
        crate::_make_type!($ty $($nodisplay)?, $repr $(, [$($tvar, $tty),*])?);
        impl BaseType for $ty {
            fn name(&self) -> String {
                format!("{:?}", self)
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
                $(let r = GetRef$(::<$ref_t>)?::get_ref(self);)?
                Box::new($ty $({
                    $($tvar: r.$tvar.clone()),*
                })?)
            }
            fn eq(&self, other: &Value) -> bool {
                if let Some(rhs) = other.downcast::<$val>() {
                    self == &rhs
                } else {
                    false
                }
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

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        self.dup()
    }
}

#[allow(private_bounds)]
pub trait Downcast {
    fn downcast<T: Clone + 'static>(&self) -> Option<T>;
    fn downcast_mut<T: Clone + 'static>(&mut self) -> Option<&mut T>;
}

#[allow(private_bounds)]
impl Downcast for Value {
    fn downcast<T: Clone + 'static>(&self) -> Option<T> {
        (self.dup() as Box<dyn Any>).downcast().ok().map(|b| *b)
    }
    fn downcast_mut<T: Clone + 'static>(&mut self) -> Option<&mut T> {
        (self.as_mut() as &mut dyn Any).downcast_mut::<T>()
    }
}

impl Downcast for Datatype {
    fn downcast<T: Clone + 'static>(&self) -> Option<T> {
        (self.dup() as Box<dyn Any>).downcast().ok().map(|b| *b)
    }
    fn downcast_mut<T: Clone + 'static>(&mut self) -> Option<&mut T> {
        (self.as_mut() as &mut dyn Any).downcast_mut::<T>()
    }
}

type_init!(Void, Void, "()");
impl Type for Void {}
impl Val for Void {
    fn hash(&self, _: &mut dyn Hasher) {}
}

impl PartialEq for Void {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

const NUM_OPS: [Op; 4] = [Op::Plus, Op::Minus, Op::Times, Op::Divided];
const ORD_OPS: [Op; 6] = [
    Op::Equal,
    Op::NotEqual,
    Op::Greater,
    Op::Less,
    Op::Geq,
    Op::Leq,
];

#[macro_export]
macro_rules! gen_fn_map {
    ($name: ident, $(($fname: literal, $fsig: ident, $ffn: ident)),*) => {
        static $name: ::std::sync::LazyLock<
            ::std::collections::HashMap<
                &'static str,
                (
                    FuncT,
                    fn(Vec<Value>, &mut Interpreter, Option<Datatype>) -> Value,
                ),
            >,
        > = ::std::sync::LazyLock::new(|| {
            ::std::collections::HashMap::from_iter([
                $((
                    $fname,
                    (
                        (&$fsig as &FuncT).clone(),
                        $ffn as fn(Vec<Value>, &mut Interpreter, Option<Datatype>) -> Value,
                    ),
                ),)*
            ])
        });
    };
}

pub trait BoxIterUtils {
    fn next_as<T: Clone + 'static>(&mut self) -> Option<T>;
}

impl<Iter: Iterator<Item = Item>, Item: Downcast> BoxIterUtils for Iter {
    fn next_as<T: Clone + 'static>(&mut self) -> Option<T> {
        self.next().and_then(|v| v.downcast::<T>())
    }
}
