use std::{
    any::Any,
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::{
    distribution::Distribution,
    error::RuntimeError,
    interpreter::Interpreter,
    parser::{Expr, Op},
};
use serde::{Deserialize, Serialize};

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

type OpResult = Result<Value, RuntimeError>;
type VoidResult = Result<(), RuntimeError>;
pub type BinOpFn = fn(&Expr, &Expr, &mut Interpreter) -> OpResult;
pub type UnOpFn = fn(&Expr, &mut Interpreter) -> OpResult;
pub type SetFn = fn(&Expr, &Expr, &mut Interpreter) -> VoidResult;
pub type CallFn = fn(&Expr, &Vec<Expr>, &mut Interpreter, Option<Datatype>) -> OpResult;
pub type SetAtFn = fn(&Expr, &Expr, &Expr, &mut Interpreter) -> VoidResult;

#[allow(private_bounds)]
#[typetag::serde]
pub trait Type: Send + Sync + Debug + Display + Any + BaseType {
    fn prop_type(&self, name: &str) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        if !self.get_generics().is_empty() {
            fn get_err(_: &Expr, _: &mut Interpreter) -> OpResult {
                Err(RuntimeError::partial(
                    "Can't operate on generic types directly",
                ))
            }
            fn set_err(_: &Expr, _: &Expr, _: &mut Interpreter) -> VoidResult {
                Err(RuntimeError::partial(
                    "Can't operate on generic types directly",
                ))
            }
            Ok((
                Box::new(TypeVar::Prop(self.dup(), name.to_string())),
                Some(get_err),
                Some(set_err),
            ))
        } else {
            self.real_prop_type(name)
        }
    }
    fn index_type(&self, index: &Datatype) -> Result<(Datatype, Option<BinOpFn>, Option<SetAtFn>), String> {
        if !self.get_generics().is_empty() {
            fn get_err(_: &Expr, _: &Expr, _: &mut Interpreter) -> OpResult {
                Err(RuntimeError::partial(
                    "Can't operate on generic types directly",
                ))
            }
            fn set_err(_: &Expr, _: &Expr, _: &Expr, _: &mut Interpreter) -> VoidResult {
                Err(RuntimeError::partial(
                    "Can't operate on generic types directly",
                ))
            }
            Ok((
                Box::new(TypeVar::Index(self.dup(), index.clone())),
                Some(get_err),
                Some(set_err),
            ))
        } else {
            self.real_index_type(index)
        }
    }
    fn bin_op_result(&self, other: &Datatype, op: Op) -> Result<(Datatype, BinOpFn), String> {
        if !self.get_generics().is_empty() || !other.get_generics().is_empty() {
            fn err(_: &Expr, _: &Expr, _: &mut Interpreter) -> OpResult {
                Err(RuntimeError::partial(
                    "Can't operate on generic types directly",
                ))
            }
            Ok((Box::new(TypeVar::BinOp(self.dup(), other.dup(), op)), err))
        } else {
            self.real_bin_op_result(other, op)
        }
    }
    fn pre_op_result(&self, op: Op) -> Result<(Datatype, UnOpFn), String> {
        if !self.get_generics().is_empty() {
            fn err(_: &Expr, _: &mut Interpreter) -> OpResult {
                Err(RuntimeError::partial(
                    "Can't operate on generic types directly",
                ))
            }
            Ok((Box::new(TypeVar::UnaryOp(self.dup(), op, true)), err))
        } else {
            self.real_pre_op_result(op)
        }
    }
    fn post_op_result(&self, op: Op) -> Result<(Datatype, UnOpFn), String> {
        if !self.get_generics().is_empty() {
            fn err(_: &Expr, _: &mut Interpreter) -> OpResult {
                Err(RuntimeError::partial(
                    "Can't operate on generic types directly",
                ))
            }
            Ok((Box::new(TypeVar::UnaryOp(self.dup(), op, false)), err))
        } else {
            self.real_post_op_result(op)
        }
    }
    fn call_result(
        &self,
        params: Vec<Datatype>,
        expected_output: Option<Datatype>,
    ) -> Result<(Datatype, CallFn), String> {
        if !self.get_generics().is_empty() || params.iter().any(|p| !p.get_generics().is_empty()) {
            fn err(_: &Expr, _: &Vec<Expr>, _: &mut Interpreter, _: Option<Datatype>) -> OpResult {
                Err(RuntimeError::partial(
                    "Can't operate on generic types directly",
                ))
            }
            Ok((
                Box::new(TypeVar::Call(self.dup(), params, expected_output)),
                err,
            ))
        } else {
            self.real_call_result(params, expected_output)
        }
    }
    fn real_prop_type(
        &self,
        _name: &str,
    ) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        Err(format!("Type {self} has no properties"))
    }
    fn real_index_type(&self, _index: &Datatype) -> Result<(Datatype, Option<BinOpFn>, Option<SetAtFn>), String> {
        Err(format!("Type {self} can't be indexed"))
    }
    fn real_bin_op_result(
        &self,
        _other: &Datatype,
        _op: Op,
    ) -> Result<(Datatype, BinOpFn), String> {
        Err(format!("Type {self} has no binary operations"))
    }
    fn real_pre_op_result(&self, _op: Op) -> Result<(Datatype, UnOpFn), String> {
        Err(format!("Type {self} has no prefix operations"))
    }
    fn real_post_op_result(&self, _op: Op) -> Result<(Datatype, UnOpFn), String> {
        Err(format!("Type {self} has no postfix operations"))
    }
    fn real_call_result(
        &self,
        _params: Vec<Datatype>,
        _expected_output: Option<Datatype>,
    ) -> Result<(Datatype, CallFn), String> {
        Err(format!("Type {self} can't be called"))
    }
    fn possible_call(&self) -> bool {
        false
    }
    fn insert_generics(&self, _generics: &HashMap<String, Datatype>) -> Result<Datatype, String> {
        Ok(self.dup())
    }
    fn real_try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
        if self.name() == other.name() {
            Ok(HashMap::new())
        } else {
            Err(format!("Couldn't match type {self} with type {other}"))
        }
    }
    fn try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
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
    fn specify_generics(&self, _generics: &Vec<Datatype>) -> Result<Datatype, String> {
        Err(format!("Type {self} has no properties"))
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
#[typetag::serde(tag = "type")]
pub trait Val: Debug + Display + Send + Sync + Any + BaseVal {
    fn get_type(&self) -> Datatype {
        self.base_get_type()
    }
    fn dup(&self) -> Value {
        self.base_dup()
    }
    fn insert_generics(&self, _generics: &Vec<Datatype>) -> OpResult {
        Err(RuntimeError::partial(&format!(
            "Type '{}' cannot have generics inserted",
            self.get_type()
        )))
    }
    fn hash(&self, _h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        Err(RuntimeError::partial(&format!(
            "Type '{}' cannot be hashed",
            self.get_type()
        )))
    }
}

#[macro_export]
macro_rules! invalid {
    ($op:expr, $lhs:expr, $rhs:expr) => {{
        let op = $op;
        let lhs = $lhs;
        let rhs = $rhs;
        return Err(RuntimeError::partial(&format!(
            "Invalid operation {op:?} on {lhs:?}, {rhs:?}"
        )));
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

        impl Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                self.inner().serialize(serializer)
            }
        }

        impl<'de> Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                Ok(Self(Arc::new(::std::sync::RwLock::new(
                    $inner::deserialize(deserializer)?,
                ))))
            }
        }
    };
}

#[macro_export]
macro_rules! _make_type {
    ($ty: ident, $repr: literal) => {
        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct $ty;
        impl Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", $repr.to_string())
            }
        }
    };
    ($ty: ident, $repr: literal, [$($tvar: ident, $tty: ty),*]) => {
        #[derive(Debug, Clone, Serialize, Deserialize)]
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
        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct $ty;
    };
    ($ty: ident nodisplay, $repr: literal, [$($tvar: ident, $tty: ty),*]) => {
        #[derive(Debug, Clone, Serialize, Deserialize)]
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

#[macro_export]
macro_rules! op_list {
    ($match_op:expr => { $($op:ident ($($var:ident : $ty:ty),+) -> ($out:expr) $func:expr);+$(;)? }) => {{
        $(
        #[allow(non_snake_case)]
        fn $op($($var: &Expr),+, i: &mut Interpreter) -> OpResult {
            Ok(Box::new(
                ($func)($(i.try_eval_as::<$ty>($var)?),+)?,
            ))
        })+
        match $match_op {
            $(Op::$op => Ok((Box::new($out), $op)),)+
            op => Err(format!("Operation {op:?} is invalid here")),
        }
    }}
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
        let _ = self.as_ref().hash(state).inspect_err(|e| panic!("{e}"));
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
#[typetag::serde]
impl Type for Void {}
#[typetag::serde]
impl Val for Void {
    fn hash(&self, _: &mut dyn Hasher) -> Result<(), RuntimeError> {
        Ok(())
    }
}

impl PartialEq for Void {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

#[macro_export]
macro_rules! gen_fn_map {
    ($name: expr, $self: expr, $ty: literal, $(($fname: literal, $fsig: ident, $ffn: ident, $fpname: ident)),*$(,)?) => {
        match $name {
            $(
                $fname => Ok((
                    Box::new((&$fsig as &FuncT).clone().with_owner($self.dup()).map_err(|e|e.info())?) as Datatype,
                    //$ffn as fn(Vec<Value>, &mut Interpreter, Option<Datatype>) -> Result<Value, crate::error::RuntimeError>,
                    {
                        fn $fpname(me: &crate::parser::expr::Expr, i: &mut Interpreter) -> Result<Value, crate::error::RuntimeError> {
                            let me = i.eval_expr(me)?;
                            Ok(Box::new($fsig.clone().make_rust_member($ffn, format!("{}_{}", $ty, $fname), me)?))
                        }
                        inventory::submit!(crate::types::function::RustFuncEntry($ty, $fname, $ffn));
                        Some($fpname as fn(&crate::parser::expr::Expr, &mut Interpreter) -> Result<Value, crate::error::RuntimeError>)
                    },
                    None,
                )),
            )*
            _ => Err(format!("Unknown property {}", $name))
        }
    };
}

//n if ARR_FNS.contains_key(n) => {
//     let f = &ARR_FNS[n];
//     Ok((
//         Box::new(f.0.clone().with_owner(self.dup()).map_err(|e|e.info())?),
//         Some(f.2),
//         None,
//     ))
// }

pub trait BoxIterUtils {
    fn next_as<T: Clone + 'static>(&mut self) -> Option<T>;
}

impl<Iter: Iterator<Item = Item>, Item: Downcast> BoxIterUtils for Iter {
    fn next_as<T: Clone + 'static>(&mut self) -> Option<T> {
        self.next().and_then(|v| v.downcast::<T>())
    }
}
