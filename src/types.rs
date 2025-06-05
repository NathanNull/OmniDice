use std::{
    any::Any,
    fmt::{Debug, Display},
};

use crate::{distribution::Distribution, parser::Op};

trait BaseType {
    fn name(&self) -> String;
    fn dup(&self) -> Datatype;
}

#[allow(private_bounds)]
pub trait Type: Send + Sync + Debug + BaseType {
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

fn make<T: Type + Default + 'static>() -> Datatype {
    Box::new(T::default())
}

trait BaseVal {
    fn dup(&self) -> Value;
    fn get_type(&self) -> Datatype;
    fn get_name(&self) -> String {
        self.get_type().name()
    }
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
        BaseVal::get_type(self)
    }
    fn dup(&self) -> Value {
        BaseVal::dup(self)
    }
}
macro_rules! invalid {
    ($op:expr, $lhs:expr, $rhs:expr) => {{
        let op = $op;
        let lhs = $lhs;
        let rhs = $rhs;
        unreachable!("Invalid operation {op:?} on {lhs:?}, {rhs:?}")
    }};
}

macro_rules! type_init {
    ($ty: ident, $val: ident, $repr: expr) => {
        #[derive(Debug, Clone, Copy, Default)]
        pub struct $ty;
        impl BaseType for $ty {
            fn name(&self) -> String {
                $repr.to_string()
            }
            fn dup(&self) -> Datatype {
                Box::new($ty)
            }
        }
        impl BaseVal for $val {
            fn dup(&self) -> Value {
                Box::new(self.clone())
            }
            fn get_type(&self) -> Datatype {
                Box::new($ty)
            }
        }
        impl Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, $repr)
            }
        }
    };
}

pub type Datatype = Box<dyn Type>;
pub type Value = Box<dyn Val>;

impl PartialEq<&dyn Type> for Datatype {
    fn eq(&self, other: &&dyn Type) -> bool {
        self.name() == other.name()
    }
}

impl PartialEq for Datatype {
    fn eq(&self, other: &Self) -> bool {
        self == &other.as_ref()
    }
}

impl Clone for Datatype {
    fn clone(&self) -> Self {
        BaseType::dup(self.as_ref())
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        BaseVal::dup(self.as_ref())
    }
}

#[allow(unused)]
pub trait TryDowncast {
    fn try_downcast_ref<T: 'static>(&self) -> Option<&T>;
    fn try_downcast_mut<T: 'static>(&mut self) -> Option<&mut T>;
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

type_init!(Bool, bool, "bool");
impl Type for Bool {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if other == make::<Bool>() {
            match op {
                Op::And | Op::Or | Op::Equal | Op::NotEqual => Some(self.dup()),
                _ => None,
            }
        } else {
            None
        }
    }

    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Not => Some(self.dup()),
            _ => None,
        }
    }
}
impl Val for bool {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.try_downcast_ref::<Self>() {
            Box::new(match op {
                Op::And => *self && *rhs,
                Op::Or => *self || *rhs,
                Op::Equal => self == rhs,
                Op::NotEqual => self != rhs,
                _ => invalid!(op, self, other),
            })
        } else {
            invalid!(op, self, other)
        }
    }

    fn pre_op(&self, op: Op) -> Value {
        match op {
            Op::Not => Box::new(!self),
            _ => invalid!(op, self, ()),
        }
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

type_init!(Float, f32, "float");
impl Type for Float {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if (other == make::<Float>() || other == make::<Int>()) && NUM_OPS.contains(&op) {
            Some(make::<Float>())
        } else if other == make::<Float>() && ORD_OPS.contains(&op) {
            Some(make::<Bool>())
        } else {
            None
        }
    }
    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(make::<Float>()),
            _ => None,
        }
    }
}
impl Val for f32 {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.try_downcast_ref::<f32>() {
            match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(self / rhs),
                Op::Equal => Box::new(self == rhs),
                Op::NotEqual => Box::new(self != rhs),
                Op::Greater => Box::new(self > rhs),
                Op::Less => Box::new(self < rhs),
                Op::Geq => Box::new(self >= rhs),
                Op::Leq => Box::new(self <= rhs),
                _ => invalid!(op, self, other),
            }
        } else if let Some(rhs) = other.try_downcast_ref::<i32>().map(|i| *i as f32) {
            match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(self / rhs),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other)
        }
    }
    fn pre_op(&self, op: Op) -> Value {
        match op {
            Op::Minus => Box::new(-self),
            _ => invalid!(op, self, ()),
        }
    }
}

type_init!(Int, i32, "int");
impl Type for Int {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if other == make::<Int>() {
            if NUM_OPS.contains(&op) || op == Op::Mod {
                Some(make::<Int>())
            } else if ORD_OPS.contains(&op) {
                Some(make::<Bool>())
            } else if other == make::<Int>() && op == Op::D {
                Some(make::<Dice>())
            } else {
                None
            }
        } else {
            None
        }
    }
    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(make::<Float>()),
            _ => None,
        }
    }
}
impl Val for i32 {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.try_downcast_ref::<i32>() {
            match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(self / rhs),
                Op::Equal => Box::new(self == rhs),
                Op::NotEqual => Box::new(self != rhs),
                Op::Greater => Box::new(self > rhs),
                Op::Less => Box::new(self < rhs),
                Op::Geq => Box::new(self >= rhs),
                Op::Leq => Box::new(self <= rhs),
                Op::Mod => Box::new(self % rhs),
                Op::D => Box::new(Distribution::n_die_m(*self as usize, *rhs as usize)),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other)
        }
    }

    fn pre_op(&self, op: Op) -> Value {
        match op {
            Op::Minus => Box::new(-self),
            _ => invalid!(op, self, ()),
        }
    }
}

type_init!(Dice, Distribution, "dice");
impl Type for Dice {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if (other == make::<Int>() || other == make::<Dice>()) && NUM_OPS.contains(&op) {
            Some(make::<Dice>())
        } else {
            None
        }
    }
    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(make::<Dice>()),
            _ => None,
        }
    }
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "mean" => Some(make::<Float>()),
            "max" | "min" => Some(make::<Int>()),
            _ => None,
        }
    }
}
impl Val for Distribution {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.try_downcast_ref::<i32>() {
            match op {
                Op::Plus => Box::new(self.clone() + rhs.into()),
                Op::Minus => Box::new(self.clone() - rhs.into()),
                Op::Times => Box::new(self.clone() * rhs.into()),
                Op::Divided => Box::new(self.clone() / rhs.into()),
                _ => invalid!(op, self, other),
            }
        } else if let Some(rhs) = other.try_downcast_ref::<Distribution>() {
            match op {
                Op::Plus => Box::new(self.clone() + rhs.clone()),
                Op::Minus => Box::new(self.clone() - rhs.clone()),
                Op::Times => Box::new(self.clone() * rhs.clone()),
                Op::Divided => Box::new(self.clone() / rhs.clone()),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other)
        }
    }

    fn pre_op(&self, op: Op) -> Value {
        match op {
            Op::Minus => Box::new(self.clone() * (&-1).into()),
            _ => invalid!(op, self, ()),
        }
    }

    fn get_prop(&self, name: &str) -> Value {
        match name {
            "mean" => Box::new(self.mean()),
            "min" => Box::new(self.min()),
            "max" => Box::new(self.max()),
            _ => invalid!("Prop", self, name),
        }
    }
}

// use std::{
//     cmp::Ordering,
//     collections::HashMap,
//     fmt::{Debug, Display},
//     ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
//     sync::LazyLock,
// };

// use strum::{EnumIter, IntoEnumIterator};

// use crate::{
//     distribution::Distribution,
//     parser::{Op, OpType},
// };

// #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumIter)]
// pub enum Datatype {
//     Float,
//     Int,
//     Bool,
//     Dice,
//     Void,
// }

// pub static PROPERTIES: LazyLock<HashMap<(Datatype, String), Datatype>> = LazyLock::new(|| {
//     let mut map = HashMap::new();
//     for dt in Datatype::iter() {
//         for (prop, out) in dt.get_properties() {
//             map.insert((dt, prop), out);
//         }
//     }
//     map
// });

// pub static OPS: LazyLock<HashMap<(Datatype, Datatype, Op, OpType), Datatype>> =
//     LazyLock::new(|| {
//         let mut map = HashMap::new();
//         for lhs in Datatype::iter() {
//             for rhs in Datatype::iter() {
//                 for (op, out) in lhs.get_bin_ops(rhs) {
//                     map.insert((lhs, rhs, op, OpType::Infix), out);
//                 }
//             }
//         }
//         for ty in Datatype::iter() {
//             for (op, out) in ty.get_prefix_ops() {
//                 map.insert((Datatype::Void, ty, op, OpType::Prefix), out);
//             }
//             for (op, out) in ty.get_postfix_ops() {
//                 map.insert((Datatype::Void, ty, op, OpType::Postfix), out);
//             }
//         }
//         map
//     });

// impl Datatype {
//     fn get_bin_ops(self, rhs: Self) -> Vec<(Op, Self)> {
//         // almost certainly there's a better way of doing this, and I'll probably implement something
//         // later, but while I have relatively few types this is fine.
//         const NUM_OPS: [Op; 4] = [Op::Plus, Op::Minus, Op::Times, Op::Divided];
//         const ORD_OPS: [Op; 6] = [Op::Equal, Op::NotEqual, Op::Greater, Op::Less, Op::Geq, Op::Leq];
//         match (self, rhs) {
//             (Self::Int, Self::Int) => NUM_OPS
//                 .into_iter()
//                 .map(|op| (op, Self::Int))
//                 .chain(ORD_OPS.into_iter().map(|op| (op, Self::Bool)))
//                 .chain([(Op::D, Self::Dice),(Op::Mod, self)])
//                 .collect(),
//             (Self::Float, Self::Float) | (Self::Float, Self::Int) | (Self::Int, Self::Float) => {
//                 NUM_OPS
//                     .into_iter()
//                     .map(|op| (op, Self::Float))
//                     .chain(
//                         ORD_OPS
//                             .into_iter()
//                             .map(|op| (op, Self::Bool))
//                             .filter(|_| self == rhs),
//                     )
//                     .collect()
//             }
//             (Self::Bool, Self::Bool) => [Op::Equal, Op::NotEqual, Op::And, Op::Or]
//                 .into_iter()
//                 .map(|op| (op, self))
//                 .collect(),
//             (_, Self::Bool) | (Self::Bool, _) => vec![],
//             (Self::Dice, Self::Dice) => NUM_OPS.iter().map(|op| (*op, Self::Dice)).collect(),
//             (Self::Float, Self::Dice) | (Self::Dice, Self::Float) => vec![],
//             (Self::Int, Self::Dice) | (Self::Dice, Self::Int) => NUM_OPS
//                 .into_iter()
//                 .filter(|op| *op != Op::D)
//                 .map(|op| (op, Self::Dice))
//                 .collect(),
//             (Self::Void, _) | (_, Self::Void) => vec![],
//         }
//     }

//     fn get_prefix_ops(self) -> Vec<(Op, Self)> {
//         match self {
//             Self::Int | Self::Float | Self::Dice => vec![(Op::Minus, self)],
//             Self::Bool => vec![(Op::Not, self)],
//             _ => vec![],
//         }
//     }

//     fn get_postfix_ops(self) -> Vec<(Op, Self)> {
//         match self {
//             _ => vec![],
//         }
//     }

//     fn get_properties(&self) -> HashMap<String, Datatype> {
//         HashMap::from_iter(
//             match self {
//                 Self::Float | Self::Int | Self::Bool => vec![],
//                 Self::Dice => vec![
//                     ("mean", Self::Float),
//                     ("min", Self::Int),
//                     ("max", Self::Int),
//                 ],
//                 Self::Void => vec![],
//             }
//             .into_iter()
//             .map(|(s, t)| (s.to_string(), t)),
//         )
//     }
// }

// #[derive(Clone)]
// pub enum Value {
//     Int(i32),
//     Float(f32),
//     Bool(bool),
//     Dice(Distribution),
//     Void,
// }

// impl Value {
//     pub fn get_property(&self, prop: &str) -> Self {
//         match self {
//             Self::Float(_) | Self::Int(_) | Self::Bool(_) => {
//                 panic!("Invalid property {prop} for type {self:?}")
//             }
//             Self::Dice(d) => match prop {
//                 "mean" => Self::Float(d.mean()),
//                 "min" => Self::Int(d.min()),
//                 "max" => Self::Int(d.max()),
//                 _ => panic!("Invalid property type {prop} for type {self:?}"),
//             },
//             Self::Void => panic!("Void type has no properties"),
//         }
//     }

//     pub fn set_property(&self, prop: &str, val: Value) {
//         todo!("Currently no good properties to set afaik")
//     }

//     pub fn get_type(&self) -> Datatype {
//         match self {
//             Self::Int(_) => Datatype::Int,
//             Self::Float(_) => Datatype::Float,
//             Self::Bool(_) => Datatype::Bool,
//             Self::Dice(_) => Datatype::Dice,
//             Self::Void => Datatype::Void,
//         }
//     }
// }

// impl Debug for Value {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Int(i) => write!(f, "i_{i}"),
//             Self::Float(fl) => write!(f, "f_{fl}"),
//             Self::Bool(b) => write!(f, "b_{b}"),
//             Self::Dice(dice) => write!(f, "d{}-{}", dice.min(), dice.max()),
//             Self::Void => write!(f, "()"),
//         }
//     }
// }

// impl Display for Value {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Int(v) => write!(f, "{v}"),
//             Self::Float(v) => write!(f, "{v:.2}"),
//             Self::Bool(v) => write!(f, "{v}"),
//             Self::Dice(v) => write!(f, "{v}"),
//             Self::Void => write!(f, "()"),
//         }
//     }
// }

// impl Add for Value {
//     type Output = Self;

//     fn add(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 + d2),
//             (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 + f2),
//             (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 + i2),

//             (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
//                 Self::Dice(d + Distribution::from_vec(vec![i]))
//             }
//             (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
//                 Self::Float(f + i as f32)
//             }

//             (s, r) => panic!("Can't add values {s:?} and {r:?}"),
//         }
//     }
// }

// impl Sub for Value {
//     type Output = Self;

//     fn sub(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 - d2),
//             (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 - f2),
//             (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 - i2),

//             (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
//                 Self::Dice(d - Distribution::from_vec(vec![i]))
//             }
//             (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
//                 Self::Float(f - i as f32)
//             }

//             (s, r) => panic!("Can't subtract values {s:?} and {r:?}"),
//         }
//     }
// }

// impl Mul for Value {
//     type Output = Self;

//     fn mul(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 * d2),
//             (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 * f2),
//             (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 * i2),

//             (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
//                 Self::Dice(d * Distribution::from_vec(vec![i]))
//             }
//             (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
//                 Self::Float(f * i as f32)
//             }

//             (s, r) => panic!("Can't multiply values {s:?} and {r:?}"),
//         }
//     }
// }

// impl Div for Value {
//     type Output = Self;

//     fn div(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 / d2),
//             (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 / f2),
//             (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 / i2),

//             (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
//                 Self::Dice(d / Distribution::from_vec(vec![i]))
//             }
//             (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
//                 Self::Float(f / i as f32)
//             }

//             (s, r) => panic!("Can't divide values {s:?} and {r:?}"),
//         }
//     }
// }

// impl Rem for Value {
//     type Output = Self;

//     fn rem(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Self::Int(li), Self::Int(ri)) => Self::Int(li%ri),
//             (s, r) => panic!("Can't take the remainder between values {s:?} and {r:?}")
//         }
//     }
// }

// impl Neg for Value {
//     type Output = Self;

//     fn neg(self) -> Self::Output {
//         match self {
//             Value::Int(i) => Value::Int(-i),
//             Value::Float(f) => Value::Float(-f),
//             Value::Dice(d) => Value::Dice(Distribution::from_vec(vec![0]) - d),
//             _ => panic!("Can't negate the value '{self}'"),
//         }
//     }
// }

// impl PartialEq for Value {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Int(l0), Self::Int(r0)) => l0 == r0,
//             (Self::Float(l0), Self::Float(r0)) => l0 == r0,
//             (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
//             _ => panic!(
//                 "Equality not implemented between types {:?} and {:?}",
//                 self.get_type(),
//                 other.get_type()
//             ),
//         }
//     }
// }

// impl PartialOrd for Value {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         match (self, other) {
//             (Self::Int(l0), Self::Int(r0)) => l0.partial_cmp(r0),
//             (Self::Float(l0), Self::Float(r0)) => l0.partial_cmp(r0),
//             _ => panic!(
//                 "Ordering not implemented between types {:?} and {:?}",
//                 self.get_type(),
//                 other.get_type()
//             ),
//         }
//     }
// }

// impl Not for Value {
//     type Output = Self;

//     fn not(self) -> Self::Output {
//         match self {
//             Value::Bool(b) => Value::Bool(!b),
//             _ => unreachable!("Invalid prefix ! for type {:?}", self.get_type()),
//         }
//     }
// }
