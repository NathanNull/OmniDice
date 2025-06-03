use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Neg, Sub},
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
        match (self, rhs) {
            (Self::Int, Self::Int) => Op::iter()
                .map(|op| (op, if op == Op::D { Self::Dice } else { Self::Int }))
                .collect(),
            (Self::Float, Self::Float) | (Self::Float, Self::Int) | (Self::Int, Self::Float) => {
                Op::iter()
                    .filter(|op| *op != Op::D)
                    .map(|op| (op, Self::Float))
                    .collect()
            }
            (Self::Dice, Self::Dice) => Op::iter()
                .filter(|op| *op != Op::D)
                .map(|op| (op, Self::Dice))
                .collect(),
            (Self::Float, Self::Dice) | (Self::Dice, Self::Float) => vec![],
            (Self::Int, Self::Dice) | (Self::Dice, Self::Int) => Op::iter()
                .filter(|op| *op != Op::D)
                .map(|op| (op, Self::Dice))
                .collect(),
            // (Self::Void, _), _) if op_type == OpType::Prefix => match (rhs, op) {
            //     (Self::Int | Self::Float | Self::Dice, Op::Minus) => rhs,
            //     _ => panic!("Invalid prefix operator {op:?} on type {rhs:?}"),
            // },
            // ((_, Self::Void), _) if op_type == OpType::Postfix => match (self, op) {
            //     _ => panic!("Currently no postfixes are valid"),
            // },
            (Self::Void, _) | (_, Self::Void) => vec![],
        }
    }

    fn get_prefix_ops(&self) -> Vec<(Op, Self)> {
        match self {
            Self::Int | Self::Float | Self::Dice => vec![(Op::Minus, *self)],
            _ => vec![],
        }
    }

    fn get_postfix_ops(&self) -> Vec<(Op, Self)> {
        match self {
            _ => vec![],
        }
    }

    fn get_properties(&self) -> HashMap<String, Datatype> {
        HashMap::from_iter(
            match self {
                Self::Float | Self::Int => vec![],
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
    Dice(Distribution),
    Void,
}

impl Value {
    pub fn get_property(&self, prop: &str) -> Self {
        match self {
            Self::Float(_) | Self::Int(_) => panic!("Invalid property {prop} for type {self:?}"),
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
            Self::Dice(_) => Datatype::Dice,
            Self::Void => Datatype::Void,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "i{i}"),
            Self::Float(fl) => write!(f, "f{fl}"),
            Self::Dice(dice) => write!(f, "d{}-{}", dice.min(), dice.max()),
            Self::Void => write!(f, "()"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
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
