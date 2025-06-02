use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::{
    distribution::Distribution,
    lexer::Op,
    parser::{Assign, Binop, Expr, Literal, Postfix, Prefix, Program},
};

pub struct Interpreter {
    ast: Program,
    variables: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new(ast: Program) -> Self {
        Self {
            ast,
            variables: HashMap::new(),
        }
    }

    pub fn run(&mut self) -> Value {
        match self.ast.clone() {
            Program::Scope(exprs) => {
                let mut last = Value::Void;
                for expr in exprs {
                    last = self.eval_expr(&expr);
                }
                last
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Identifier(ident) => self.get_var(ident),
            Expr::Literal(number) => self.eval_literal(number),
            Expr::Binop(binop) => self.eval_binop(binop),
            Expr::Prefix(prefix) => self.eval_prefix(prefix),
            Expr::Postfix(postfix) => self.eval_postfix(postfix),
            Expr::Assign(assign) => self.eval_assign(assign),
        }
    }

    fn eval_literal(&mut self, number: &Literal) -> Value {
        match number {
            Literal::Float(f) => Value::Float(*f),
            Literal::Int(i) => Value::Int(*i),
            Literal::Void => Value::Void,
        }
    }

    fn eval_binop(&mut self, binop: &Binop) -> Value {
        let lhs = self.eval_expr(&binop.lhs);
        let rhs = self.eval_expr(&binop.rhs);
        match binop.op {
            Op::Plus => lhs.clone() + rhs.clone(),
            Op::Minus => lhs.clone() - rhs.clone(),
            Op::Times => lhs.clone() * rhs.clone(),
            Op::Divided => lhs.clone() / rhs.clone(),
            Op::D => match (&lhs, &rhs) {
                (Value::Int(l), Value::Int(r)) if *l >= 0 && *r >= 0 => {
                    Value::Dice(Distribution::n_die_m(*l as usize, *r as usize))
                }
                (l, r) => panic!("Can't create dice from values {l:?} and {r:?}"),
            },
            Op::Assign => unreachable!("invalid op"),
        }
    }

    fn get_var(&self, var: &String) -> Value {
        self.variables
            .get(var)
            .expect(&format!("Attempted to access nonexistent variable {var}"))
            .clone()
    }

    fn set_var(&mut self, var: String, val: Value) {
        self.variables.insert(var, val);
    }

    fn eval_prefix(&mut self, prefix: &Prefix) -> Value {
        let rhs = self.eval_expr(&prefix.rhs);
        match prefix.prefix {
            Op::Minus => -rhs.clone(),
            p => unreachable!("Invalid prefix operator {p:?}"),
        }
    }

    fn eval_postfix(&mut self, postfix: &Postfix) -> Value {
        let rhs = self.eval_expr(&postfix.lhs);
        self.eval_postfix_core(rhs, postfix.postfix)
    }

    fn eval_postfix_core(&mut self, _lhs: Value, op: Op) -> Value {
        match op {
            Op::Times => Value::Int(0),           // how have you done this
            p => unreachable!("Invalid postfix operator {p:?}"),
        }
    }

    fn eval_assign(&mut self, assign: &Assign) -> Value {
        let val = self.eval_expr(&assign.val);
        let var = self.eval_assignee(&assign.assignee);
        self.set_var(var, val.clone());
        val
    }

    fn eval_assignee(&mut self, assignee: &Expr) -> String {
        match assignee {
            Expr::Identifier(name) => name.clone(),
            val => panic!("Can't assign to value {val:?}")
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
