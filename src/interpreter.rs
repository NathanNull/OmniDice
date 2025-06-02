use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::{
    distribution::Distribution,
    lexer::Op,
    parser::{Binop, Expr, Literal, Postfix, Prefix, Program},
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
                let mut last = Value::Int(0);
                for expr in exprs {
                    last = self.eval_expr(&expr);
                }
                last
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Identifier(ident) => Value::VarName(ident.to_string()),
            Expr::Literal(number) => self.eval_literal(number),
            Expr::Binop(binop) => self.eval_binop(binop),
            Expr::Prefix(prefix) => self.eval_prefix(prefix),
            Expr::Postfix(postfix) => self.eval_postfix(postfix),
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
        self.eval_binop_core(lhs, rhs, binop.op)
    }

    fn eval_binop_core(&mut self, lhs: Value, rhs: Value, op: Op) -> Value {
        let res = match op {
            Op::Plus => lhs.clone() + rhs.clone(),
            Op::Minus => lhs.clone() - rhs.clone(),
            Op::Times => lhs.clone() * rhs.clone(),
            Op::Divided => lhs.clone() / rhs.clone(),
            Op::D => match (&lhs, &rhs) {
                (Value::Int(l), Value::Int(r)) if *l >= 0 && *r >= 0 => {
                    Ok(Value::Dice(Distribution::n_die_m(*l as usize, *r as usize)))
                }
                (l, r) => Err(format!("Can't create dice from values {l:?} and {r:?}")),
            },
            Op::Assign => match (&lhs, &rhs) {
                (_, Value::VarName(_)) => Err("if this prints, you've messed up".to_string()),
                (Value::VarName(v), r) => {
                    self.variables.insert(v.to_string(), r.clone());
                    Ok(r.clone())
                }
                _ => Err(format!(
                    "Assign operation must be given a variable, found {lhs} and {rhs}"
                )),
            },
        };
        if let Ok(res) = res {
            res
        } else {
            match (lhs, rhs) {
                (Value::VarName(v), r) => self.eval_binop_core(self.get_var(v), r, op),
                (l, Value::VarName(v)) => self.eval_binop_core(l, self.get_var(v), op),
                _ => panic!("{}", res.err().unwrap()),
            }
        }
    }

    fn get_var(&self, var: String) -> Value {
        self.variables
            .get(&var)
            .expect(&format!("Attempted to access nonexistent variable {var}"))
            .clone()
    }

    fn eval_prefix(&mut self, prefix: &Prefix) -> Value {
        let rhs = self.eval_expr(&prefix.rhs);
        self.eval_prefix_core(rhs, prefix.prefix)
    }

    fn eval_prefix_core(&mut self, rhs: Value, op: Op) -> Value {
        let res = match op {
            Op::Minus => -rhs.clone(),
            p => unreachable!("Invalid prefix operator {p:?}"),
        };
        if let Ok(res) = res {
            res
        } else {
            match rhs {
                Value::VarName(v) => self.eval_prefix_core(self.get_var(v), op),
                _ => panic!("{}", res.err().unwrap()),
            }
        }
    }

    fn eval_postfix(&mut self, postfix: &Postfix) -> Value {
        let rhs = self.eval_expr(&postfix.lhs);
        self.eval_postfix_core(rhs, postfix.postfix)
    }

    fn eval_postfix_core(&mut self, lhs: Value, op: Op) -> Value {
        let res = match op {
            Op::Times => Ok(Value::Int(0)),           // how have you done this
            Op::Divided => Err("amogus".to_string()), // how have you done this
            p => unreachable!("Invalid prefix operator {p:?}"),
        };
        if let Ok(res) = res {
            res
        } else {
            match lhs {
                Value::VarName(v) => self.eval_prefix_core(self.get_var(v), op),
                _ => panic!("{}", res.err().unwrap()),
            }
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Dice(Distribution),
    VarName(String),
    Void,
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "i{i}"),
            Self::Float(fl) => write!(f, "f{fl}"),
            Self::Dice(dice) => write!(f, "d{}-{}", dice.min(), dice.max()),
            Self::VarName(name) => write!(f, "{name}"),
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
            Self::VarName(v) => write!(f, "{v}"),
            Self::Void => write!(f, "()"),
        }
    }
}

impl Add for Value {
    type Output = Result<Self, String>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 + d2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 + f2),
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 + i2),

            (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
                Self::Dice(d + Distribution::from_vec(vec![i]))
            }
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                Self::Float(f + i as f32)
            }

            (s, r) => return Err(format!("Can't add values of type {s:?} and {r:?}")),
        })
    }
}

impl Sub for Value {
    type Output = Result<Self, String>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 - d2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 - f2),
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 - i2),

            (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
                Self::Dice(d - Distribution::from_vec(vec![i]))
            }
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                Self::Float(f - i as f32)
            }

            (s, r) => return Err(format!("Can't subtract values of type {s:?} and {r:?}")),
        })
    }
}

impl Mul for Value {
    type Output = Result<Self, String>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 * d2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 * f2),
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 * i2),

            (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
                Self::Dice(d * Distribution::from_vec(vec![i]))
            }
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                Self::Float(f * i as f32)
            }

            (s, r) => return Err(format!("Can't multiply values of type {s:?} and {r:?}")),
        })
    }
}

impl Div for Value {
    type Output = Result<Self, String>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Dice(d1), Self::Dice(d2)) => Self::Dice(d1 / d2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 / f2),
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 / i2),

            (Self::Dice(d), Self::Int(i)) | (Self::Int(i), Self::Dice(d)) => {
                Self::Dice(d / Distribution::from_vec(vec![i]))
            }
            (Self::Float(f), Self::Int(i)) | (Self::Int(i), Self::Float(f)) => {
                Self::Float(f / i as f32)
            }

            (s, r) => return Err(format!("Can't divide values of type {s:?} and {r:?}")),
        })
    }
}

impl Neg for Value {
    type Output = Result<Self, String>;

    fn neg(self) -> Self::Output {
        Ok(match self {
            Value::Int(i) => Value::Int(-i),
            Value::Float(f) => Value::Float(-f),
            Value::Dice(d) => Value::Dice(Distribution::from_vec(vec![0]) - d),
            _ => return Err(format!("Can't negate the value '{self}'")),
        })
    }
}
