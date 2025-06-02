use std::{fmt::{Debug, Display}, ops::{Add, Div, Mul, Neg, Sub}};

use crate::{distribution::Distribution, lexer::Op, parser::OpType};

#[derive(Clone, Copy, Debug)]
pub enum Datatype {
    Float,
    Int,
    Dice,
    Void,
}

impl Datatype {
    pub fn decide_op_type(self, rhs: Self, op: Op, op_type: OpType) -> Datatype {
        // almost certainly there's a better way of doing this, and I'll probably implement something
        // later, but while I have relatively few types this is fine.
        match ((self, rhs), op) {
            ((Self::Int, Self::Int), Op::D) => Self::Dice,
            (_, Op::Assign) => unreachable!("Assign shouldn't parse as an operator"),
            (_, Op::D) => panic!("Dice must be made from Ints"),
            (
                (Self::Float, Self::Float)
                | (Self::Float, Self::Int)
                | (Self::Int, Self::Float),
                _,
            ) => Self::Float,
            ((Self::Int, Self::Int), _) => Self::Int,
            ((Self::Dice, Self::Dice), _) => Self::Dice,
            ((Self::Float, Self::Dice) | (Self::Dice, Self::Float), _) => {
                panic!("Can't operate with dice and floats")
            }
            ((Self::Int, Self::Dice) | (Self::Dice, Self::Int), _) => {
                Self::Dice
            }
            ((Self::Void, _), _) if op_type == OpType::Prefix => match (rhs, op) {
                (Self::Int | Self::Float | Self::Dice, Op::Minus) => rhs,
                _ => panic!("Invalid prefix operator {op:?} on type {rhs:?}"),
            },
            ((_, Self::Void), _) if op_type == OpType::Postfix => match (self, op) {
                _ => panic!("Currently no postfixes are valid"),
            },
            ((Self::Void, _) | (_, Self::Void), _) => panic!("Can't operate on Void type"),
        }
    }

    pub fn decide_property_type(&self, prop: &str) -> Datatype {
        match self {
            Self::Float | Self::Int => panic!("Invalid property {prop} for type {self:?}"),
            Self::Dice => match prop {
                "mean" => Self::Float,
                "min" => Self::Int,
                "max" => Self::Int,
                _ => panic!("Invalid property type {prop} for type {self:?}")
            },
            Self::Void => panic!("Void type has no properties"),
        }
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
                _ => panic!("Invalid property type {prop} for type {self:?}")
            },
            Self::Void => panic!("Void type has no properties"),
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