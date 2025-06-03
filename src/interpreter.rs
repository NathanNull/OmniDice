use std::collections::HashMap;

use crate::{
    distribution::Distribution,
    parser::{Accessor, Assign, Binop, Expr, ExprContents, Literal, Op, Postfix, Prefix, Program},
    types::Value,
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

    pub fn run(&mut self) -> (Value, &HashMap<String, Value>) {
        (
            match self.ast.clone() {
                Program::Scope(exprs) => {
                    let mut last = Value::Void;
                    for expr in exprs {
                        last = self.eval_expr(&expr);
                    }
                    last
                }
            },
            &self.variables,
        )
    }

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        let res = match &expr.contents {
            ExprContents::Accessor(acc) => self.eval_accessor(acc),
            ExprContents::Literal(number) => self.eval_literal(number),
            ExprContents::Binop(binop) => self.eval_binop(binop),
            ExprContents::Prefix(prefix) => self.eval_prefix(prefix),
            ExprContents::Postfix(postfix) => self.eval_postfix(postfix),
            ExprContents::Assign(assign) => self.eval_assign(assign),
        };
        assert_eq!(res.get_type(), expr.output);
        res
    }

    fn eval_accessor(&mut self, acc: &Accessor) -> Value {
        match acc {
            Accessor::Variable(ident) => self.get_var(ident),
            Accessor::Property(base, property) => self.eval_expr(&base).get_property(property),
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
        let res = match binop.op {
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
        };
        res
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
        let _rhs = self.eval_expr(&postfix.lhs);
        match postfix.postfix {
            p => unreachable!("Invalid postfix operator {p:?}"),
        }
    }

    fn eval_assign(&mut self, assign: &Assign) -> Value {
        let val = self.eval_expr(&assign.val);
        match &assign.assignee {
            Accessor::Variable(name) => self.set_var(name.clone(), val.clone()),
            Accessor::Property(base, prop) => self.eval_expr(&base).set_property(prop, val.clone()),
        }
        val
    }
}