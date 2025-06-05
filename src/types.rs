use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Neg, Not, Sub},
    sync::LazyLock,
};

use strum::{EnumIter, IntoEnumIterator};

use crate::{
    distribution::Distribution,
    parser::{Op, OpType},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, EnumIter)]
pub enum Datatype {
    Float,
    Int,
    Bool,
    Dice,
    Void,
}

pub static PROPERTIES: LazyLock<HashMap<(Datatype, String), Datatype>> = LazyLock::new(|| {
    let mut map = HashMap::new();
    for dt in Datatype::iter() {
        for (prop, out) in dt.get_properties() {
            map.insert((dt, prop), out);
        }
    }
    map
});

pub static OPS: LazyLock<HashMap<(Datatype, Datatype, Op, OpType), Datatype>> =
    LazyLock::new(|| {
        let mut map = HashMap::new();
        for lhs in Datatype::iter() {
            for rhs in Datatype::iter() {
                for (op, out) in lhs.get_bin_ops(rhs) {
                    map.insert((lhs, rhs, op, OpType::Infix), out);
                }
            }
        }
        for ty in Datatype::iter() {
            for (op, out) in ty.get_prefix_ops() {
                map.insert((Datatype::Void, ty, op, OpType::Prefix), out);
            }
            for (op, out) in ty.get_postfix_ops() {
                map.insert((Datatype::Void, ty, op, OpType::Postfix), out);
            }
        }
        map
    });

impl Datatype {
    fn get_bin_ops(self, rhs: Self) -> Vec<(Op, Self)> {
        // almost certainly there's a better way of doing this, and I'll probably implement something
        // later, but while I have relatively few types this is fine.
        const NUM_OPS: [Op; 4] = [Op::Plus, Op::Minus, Op::Times, Op::Divided];
        const ORD_OPS: [Op; 6] = [Op::Equal, Op::NotEqual, Op::Greater, Op::Less, Op::Geq, Op::Leq];
        match (self, rhs) {
            (Self::Int, Self::Int) => NUM_OPS
                .into_iter()
                .map(|op| (op, Self::Int))
                .chain(ORD_OPS.into_iter().map(|op| (op, Self::Bool)))
                .chain([(Op::D, Self::Dice)])
                .collect(),
            (Self::Float, Self::Float) | (Self::Float, Self::Int) | (Self::Int, Self::Float) => {
                NUM_OPS
                    .into_iter()
                    .map(|op| (op, Self::Float))
                    .chain(
                        ORD_OPS
                            .into_iter()
                            .map(|op| (op, Self::Bool))
                            .filter(|_| self == rhs),
                    )
                    .collect()
            }
            (Self::Bool, Self::Bool) => [Op::Equal, Op::NotEqual, Op::And, Op::Or]
                .into_iter()
                .map(|op| (op, self))
                .collect(),
            (_, Self::Bool) | (Self::Bool, _) => vec![],
            (Self::Dice, Self::Dice) => NUM_OPS.iter().map(|op| (*op, Self::Dice)).collect(),
            (Self::Float, Self::Dice) | (Self::Dice, Self::Float) => vec![],
            (Self::Int, Self::Dice) | (Self::Dice, Self::Int) => NUM_OPS
                .into_iter()
                .filter(|op| *op != Op::D)
                .map(|op| (op, Self::Dice))
                .collect(),
            (Self::Void, _) | (_, Self::Void) => vec![],
        }
    }

    fn get_prefix_ops(self) -> Vec<(Op, Self)> {
        match self {
            Self::Int | Self::Float | Self::Dice => vec![(Op::Minus, self)],
            Self::Bool => vec![(Op::Not, self)],
            _ => vec![],
        }
    }

    fn get_postfix_ops(self) -> Vec<(Op, Self)> {
        match self {
            _ => vec![],
        }
    }

    fn get_properties(&self) -> HashMap<String, Datatype> {
        HashMap::from_iter(
            match self {
                Self::Float | Self::Int | Self::Bool => vec![],
                Self::Dice => vec![
                    ("mean", Self::Float),
                    ("min", Self::Int),
                    ("max", Self::Int),
                ],
                Self::Void => vec![],
            }
            .into_iter()
            .map(|(s, t)| (s.to_string(), t)),
        )
    }
}

#[derive(Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    Dice(Distribution),
    Void,
}

impl Value {
    pub fn get_property(&self, prop: &str) -> Self {
        match self {
            Self::Float(_) | Self::Int(_) | Self::Bool(_) => {
                panic!("Invalid property {prop} for type {self:?}")
            }
            Self::Dice(d) => match prop {
                "mean" => Self::Float(d.mean()),
                "min" => Self::Int(d.min()),
                "max" => Self::Int(d.max()),
                _ => panic!("Invalid property type {prop} for type {self:?}"),
            },
            Self::Void => panic!("Void type has no properties"),
        }
    }

    pub fn set_property(&self, prop: &str, val: Value) {
        todo!("Currently no good properties to set afaik")
    }

    pub fn get_type(&self) -> Datatype {
        match self {
            Self::Int(_) => Datatype::Int,
            Self::Float(_) => Datatype::Float,
            Self::Bool(_) => Datatype::Bool,
            Self::Dice(_) => Datatype::Dice,
            Self::Void => Datatype::Void,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "i_{i}"),
            Self::Float(fl) => write!(f, "f_{fl}"),
            Self::Bool(b) => write!(f, "b_{b}"),
            Self::Dice(dice) => write!(f, "d{}-{}", dice.min(), dice.max()),
            Self::Void => write!(f, "()"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v:.2}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::Dice(v) => write!(f, "{v}"),
            Self::Void => write!(f, "()"),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 + d2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 + f2),
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 + i2),

            (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
                Self::Dice(d + Distribution::from_vec(vec![i]))
            }
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                Self::Float(f + i as f32)
            }

            (s, r) => panic!("Can't add values {s:?} and {r:?}"),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 - d2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 - f2),
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 - i2),

            (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
                Self::Dice(d - Distribution::from_vec(vec![i]))
            }
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                Self::Float(f - i as f32)
            }

            (s, r) => panic!("Can't subtract values {s:?} and {r:?}"),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 * d2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 * f2),
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 * i2),

            (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
                Self::Dice(d * Distribution::from_vec(vec![i]))
            }
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                Self::Float(f * i as f32)
            }

            (s, r) => panic!("Can't multiply values {s:?} and {r:?}"),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 / d2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 / f2),
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 / i2),

            (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
                Self::Dice(d / Distribution::from_vec(vec![i]))
            }
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                Self::Float(f / i as f32)
            }

            (s, r) => panic!("Can't divide values {s:?} and {r:?}"),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Int(i) => Value::Int(-i),
            Value::Float(f) => Value::Float(-f),
            Value::Dice(d) => Value::Dice(Distribution::from_vec(vec![0]) - d),
            _ => panic!("Can't negate the value '{self}'"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            _ => panic!(
                "Equality not implemented between types {:?} and {:?}",
                self.get_type(),
                other.get_type()
            ),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0.partial_cmp(r0),
            (Self::Float(l0), Self::Float(r0)) => l0.partial_cmp(r0),
            _ => panic!(
                "Ordering not implemented between types {:?} and {:?}",
                self.get_type(),
                other.get_type()
            ),
        }
    }
}

impl Not for Value {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Value::Bool(!b),
            _ => unreachable!("Invalid prefix ! for type {:?}", self.get_type()),
        }
    }
}