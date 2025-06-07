use std::{collections::HashMap};

use crate::{
    parser::{
        Accessor, Array, Assign, AssignType, Binop, Conditional, Expr, ExprContents, Postfix,
        Prefix, Scope, While, Tuple as TupleExpr
    },
    types::{Arr, Downcast, Tuple, Value, Void},
};

pub struct Interpreter {
    ast: Expr,
    variables: Vec<HashMap<String, Value>>,
}

impl Interpreter {
    pub fn new(ast: Expr) -> Self {
        Self {
            ast,
            variables: vec![],
        }
    }

    pub fn run(&mut self) -> Value {
        let ast = self.ast.clone();
        self.eval_expr(&ast)
    }

    fn eval_scope(&mut self, scope: &Scope) -> Value {
        let mut last: Value = Box::new(Void);
        // Variables created in scope die when it ends
        self.variables.push(HashMap::new());
        for expr in scope {
            last = self.eval_expr(&expr);
        }
        self.variables.pop();
        last
    }

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        let res = match &expr.contents {
            ExprContents::Accessor(acc) => self.eval_accessor(acc),
            ExprContents::Literal(val) => val.clone(),
            ExprContents::Binop(binop) => self.eval_binop(binop),
            ExprContents::Prefix(prefix) => self.eval_prefix(prefix),
            ExprContents::Postfix(postfix) => self.eval_postfix(postfix),
            ExprContents::Assign(assign) => self.eval_assign(assign),
            ExprContents::Scope(scope) => self.eval_scope(scope),
            ExprContents::Conditional(cond) => self.eval_conditional(cond),
            ExprContents::While(wh) => self.eval_while(wh),
            ExprContents::Array(arr) => self.eval_array(arr),
            ExprContents::Tuple(tup) => self.eval_tuple(tup),
        };
        assert_eq!(
            &res.get_type(),
            &expr.output,
            "Expression {expr:?} evaluated to a different type ({:?}) than expected ({:?}). This is notable.",
            res.get_type(),
            expr.output
        );
        res
    }

    fn eval_accessor(&mut self, acc: &Accessor) -> Value {
        match acc {
            Accessor::Variable(ident) => self.get_var(ident).clone(),
            Accessor::Property(base, property) => self.eval_expr(&base).get_prop(property),
        }
    }

    fn eval_binop(&mut self, binop: &Binop) -> Value {
        let lhs = self.eval_expr(&binop.lhs);
        let rhs = self.eval_expr(&binop.rhs);
        lhs.bin_op(&rhs, binop.op)
    }

    fn get_var(&mut self, var: &str) -> &mut Value {
        for scope in self.variables.iter_mut().rev() {
            if let Some(val) = scope.get_mut(var) {
                return val;
            }
        }
        panic!("Attempted to access nonexistent variable {var}");
    }

    fn set_var(&mut self, var: String, val: Value) {
        if self.variables.is_empty() {
            print!("!!!!! tried to set variable while no scope alive");
            self.variables.push(HashMap::new());
        }
        for scope in &mut self.variables {
            if scope.contains_key(&var) {
                scope.insert(var, val);
                return;
            }
        }
        self.variables.last_mut().unwrap().insert(var, val);
    }

    fn eval_prefix(&mut self, prefix: &Prefix) -> Value {
        let rhs = self.eval_expr(&prefix.rhs);
        rhs.pre_op(prefix.op)
    }

    fn eval_postfix(&mut self, postfix: &Postfix) -> Value {
        let lhs = self.eval_expr(&postfix.lhs);
        lhs.post_op(postfix.op)
    }

    fn eval_assign(&mut self, assign: &Assign) -> Value {
        let val = self.eval_expr(&assign.val);
        match &assign.assignee {
            Accessor::Variable(name) => {
                if assign.a_type == AssignType::Reassign {
                    assert_eq!(
                        val.get_type(),
                        self.get_var(name).get_type(),
                        "Tried to reassign variable {name} to a different type"
                    );
                }
                self.set_var(name.clone(), val.dup());
            }
            Accessor::Property(base, prop) => {
                let base_val = self.eval_expr(&base);
                assert_eq!(
                    val.get_type(),
                    base_val.get_type().prop_type(prop).unwrap(),
                    "Tried to reassign property {:?} to a different type",
                    assign.assignee
                );
                base_val.set_prop(prop, val.dup());
            }
        }
        val
    }

    fn eval_conditional(&mut self, cond: &Conditional) -> Value {
        if let Some(condition) = self.eval_expr(&cond.condition).downcast::<bool>() {
            if condition {
                self.eval_expr(&cond.result)
            } else if let Some(otherwise) = cond.otherwise.as_ref() {
                self.eval_expr(&otherwise)
            } else {
                Box::new(Void)
            }
        } else {
            unreachable!("Conditional statements should always return boolean values")
        }
    }

    fn eval_while(&mut self, wh: &While) -> Value {
        loop {
            if let Some(condition) = self.eval_expr(&wh.condition).downcast::<bool>() {
                if condition {
                    self.eval_expr(&wh.result);
                } else {
                    break;
                }
            } else {
                unreachable!("Conditional statements should always return boolean values")
            }
        }
        Box::new(Void)
    }

    fn eval_array(&mut self, arr: &Array) -> Value {
        let mut res = vec![];
        let mut ty = None;
        for expr in &arr.elements {
            res.push(self.eval_expr(expr));
            let next_type = res.last().unwrap().get_type();
            if let Some(t) = &mut ty {
                assert_eq!(t, &next_type, "Array types didn't match");
            } else {
                ty = Some(next_type);
            }
        }
        Box::new(Arr::new(res))
    }

    fn eval_tuple(&mut self, tup: &TupleExpr) -> Value {
        let mut res = vec![];
        for expr in &tup.elements {
            res.push(self.eval_expr(expr));
            // TODO: maybe add type checking here, if I feel I need it for some reason
        }
        Box::new(Tuple::new(res))
    }
}
