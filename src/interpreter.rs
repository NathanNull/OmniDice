use std::{
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::{
    distribution::Distribution,
    lexer::Op,
    parser::{Binop, Expr, Number, Postfix, Prefix, Program},
};

pub struct Interpreter {
    ast: Program,
}

impl Interpreter {
    pub fn new(ast: Program) -> Self {
        Self { ast }
    }

    pub fn run(&self) -> Value {
        match &self.ast {
            Program::Expr(expr) => self.eval_expr(expr),
        }
    }

    fn eval_expr(&self, expr: &Expr) -> Value {
        match expr {
            Expr::Identifier(_ident) => todo!("No support for variables just yet"),
            Expr::Number(number) => self.eval_number(number),
            Expr::Binop(binop) => self.eval_binop(binop),
            Expr::Prefix(prefix) => self.eval_prefix(prefix),
            Expr::Postfix(postfix) => self.eval_postfix(postfix),
        }
    }

    fn eval_number(&self, number: &Number) -> Value {
        match number {
            Number::Float(f) => Value::Float(*f),
            Number::Int(i) => Value::Int(*i),
        }
    }

    fn eval_binop(&self, binop: &Binop) -> Value {
        let lhs = self.eval_expr(&binop.lhs);
        let rhs = self.eval_expr(&binop.rhs);
        match binop.op {
            Op::Plus => lhs + rhs,
            Op::Minus => lhs - rhs,
            Op::Times => lhs * rhs,
            Op::Divided => lhs / rhs,
            Op::D => match (lhs, rhs) {
                (Value::Int(l), Value::Int(r)) if l >= 0 && r >= 0 => {
                    Value::Dice(Distribution::n_die_m(l as usize, r as usize))
                }
                (l, r) => panic!("Can't build dice from values {l:?}, {r:?}"),
            },
        }
    }

    fn eval_prefix(&self, prefix: &Prefix) -> Value {
        let rhs = self.eval_expr(&prefix.rhs);
        match prefix.prefix {
            Op::Minus => -rhs,
            p => unreachable!("Invalid prefix operator {p:?}"),
        }
    }

    fn eval_postfix(&self, postfix: &Postfix) -> Value {
        let _lhs = self.eval_expr(&postfix.lhs);
        match postfix.postfix {
            p => unreachable!(
                "Currently there are no valid postfix operators, {p:?} parsed as one anyway"
            ),
        }
    }
}

pub enum Value {
    Int(i32),
    Float(f32),
    Dice(Distribution),
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "i{i}"),
            Self::Float(fl) => write!(f, "f{fl}"),
            Self::Dice(dice) => write!(f, "d{}-{}", dice.min(), dice.max()),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Dice(v) => write!(f, "{v}"),
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

            (s, r) => panic!("Can't add values of type {s:?} and {r:?}"),
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

            (s, r) => panic!("Can't add values of type {s:?} and {r:?}"),
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

            (s, r) => panic!("Can't add values of type {s:?} and {r:?}"),
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

            (s, r) => panic!("Can't add values of type {s:?} and {r:?}"),
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
        }
    }
}
