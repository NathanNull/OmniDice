use std::{collections::HashMap, fmt::Debug};

use crate::{
    builtins::BUILTINS,
    parser::{
        Accessor, Array, Assign, AssignType, Binop, Call, Conditional, Expr, ExprContents, For,
        Function, GenericSpecify, Postfix, Prefix, Scope, Tuple as TupleExpr, While,
    },
    types::{Arr, ArrT, Datatype, Downcast, Func, InnerFunc, Maybe, Tuple, Value, Void},
};

pub struct VarScope<T: Debug> {
    pub vars: HashMap<String, T>,
    pub blocking: bool,
}

impl<T: Debug> Debug for VarScope<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "VarScope([{}], blocking? {})",
            self.vars
                .iter()
                .map(|(v, _)| v.clone())
                .collect::<Vec<_>>()
                .join(", "),
            self.blocking
        )
    }
}

pub struct Interpreter {
    ast: Expr,
    variables: Vec<VarScope<Value>>,
}

impl Interpreter {
    pub fn new(ast: Expr) -> Self {
        Self {
            ast,
            variables: vec![VarScope {
                vars: BUILTINS.clone(),
                blocking: true,
            }],
        }
    }

    pub fn run(&mut self) {
        let ast = self.ast.clone();
        self.eval_expr(&ast);
    }

    fn eval_scope(&mut self, scope: &Scope) -> Value {
        let mut last: Value = Box::new(Void);
        // Variables created in scope die when it ends
        self.variables.push(VarScope {
            vars: HashMap::new(),
            blocking: false,
        });
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
            ExprContents::For(fo) => self.eval_for(fo),
            ExprContents::Array(arr) => self.eval_array(
                arr,
                expr.output
                    .downcast::<ArrT>()
                    .expect("Array should be array")
                    .entry
                    .clone(),
            ),
            ExprContents::Tuple(tup) => self.eval_tuple(tup),
            ExprContents::Function(func) => self.eval_function(func),
            ExprContents::Call(call) => self.eval_call(call, expr.output.clone()),
            ExprContents::GenericSpecify(gspec) => self.eval_specify_generics(gspec),
        };
        assert_eq!(
            &res.get_type(),
            &expr.output,
            "Expression {expr:?} evaluated to a different type ({}) than expected ({}). This is notable.",
            res.get_type(),
            expr.output
        );
        res
    }

    fn eval_accessor(&mut self, acc: &Accessor) -> Value {
        match acc {
            Accessor::Variable(ident) => self.get_var(ident).clone(),
            Accessor::Property(base, property) => self.eval_expr(&base).get_prop(property),
            Accessor::Index(indexed, index) => {
                self.eval_expr(&indexed).get_index(self.eval_expr(&index))
            }
        }
    }

    fn eval_binop(&mut self, binop: &Binop) -> Value {
        let lhs = self.eval_expr(&binop.lhs);
        let rhs = self.eval_expr(&binop.rhs);
        lhs.bin_op(&rhs, binop.op)
    }

    fn get_var(&mut self, var: &str) -> &Value {
        for scope in self.variables.iter().rev() {
            if let Some(val) = scope.vars.get(var) {
                return val;
            }
            if scope.blocking {
                break;
            }
        }
        panic!(
            "Attempted to access nonexistent variable {var}, vars: {:?}",
            self.variables
        );
    }

    fn set_var(&mut self, var: String, val: Value) {
        if self.variables.is_empty() {
            panic!("Tried to set variable while no scope alive");
        }
        self.variables.last_mut().unwrap().vars.insert(var, val);
    }

    fn update_var(&mut self, var: String, val: Value) {
        if self.variables.is_empty() {
            panic!("Tried to set variable while no scope alive");
        }
        for scope in self.variables.iter_mut().rev() {
            if scope.vars.contains_key(&var) {
                scope.vars.insert(var, val);
                return;
            }
            if scope.blocking {
                break;
            }
        }
        panic!(
            "Tried to update unknown variable {var}, (vars: {:?})",
            self.variables
        );
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
            Accessor::Variable(name) => match assign.a_type {
                AssignType::Reassign => {
                    assert_eq!(
                        val.get_type(),
                        self.get_var(name).get_type(),
                        "Tried to reassign variable {name} to a different type"
                    );
                    self.update_var(name.clone(), val.dup());
                }
                AssignType::Create => self.set_var(name.clone(), val.dup()),
            },
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
            Accessor::Index(indexed, index) => {
                let base_val = self.eval_expr(&indexed);
                assert_eq!(
                    val.get_type(),
                    base_val.get_type().index_type(&index.output).unwrap(),
                    "Tried to reassign property {:?} to a different type",
                    assign.assignee
                );
                base_val.set_index(self.eval_expr(&index), val.dup());
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

    fn eval_for(&mut self, fo: &For) -> Value {
        let iter = self
            .eval_expr(&fo.iter)
            .get_prop("iter")
            .call(vec![], self, None);
        self.variables.push(VarScope {
            vars: HashMap::new(),
            blocking: false,
        });
        loop {
            let next_val = iter.get_prop("next").call(vec![], self, None);
            match next_val.downcast::<Maybe>() {
                Some(Maybe {
                    output: _,
                    contents: Some(c),
                }) => {
                    self.set_var(fo.var.clone(), c);
                    self.eval_expr(&fo.body);
                }
                Some(Maybe {
                    output: _,
                    contents: None,
                }) => break,
                None => panic!("Invalid for loop"),
            }
        }
        self.variables.pop();
        Box::new(Void)
    }

    fn eval_array(&mut self, arr: &Array, expected_type: Datatype) -> Value {
        let mut res = vec![];
        for expr in &arr.elements {
            res.push(self.eval_expr(expr));
            let next_type = res.last().unwrap().get_type();
            assert_eq!(expected_type, next_type, "Array types didn't match");
        }
        Box::new(Arr::new(res, expected_type))
    }

    fn eval_tuple(&mut self, tup: &TupleExpr) -> Value {
        let mut res = vec![];
        for expr in &tup.elements {
            res.push(self.eval_expr(expr));
            // TODO: maybe add type checking here, if I feel I need it for some reason
        }
        Box::new(Tuple::new(res))
    }

    fn eval_function(&mut self, func: &Function) -> Value {
        Box::new(Func {
            params: func.params.iter().map(|(_, t)| t.clone()).collect(),
            output: func.contents.output.clone(),
            generic: func.generic.clone(),
            owner_t: None,
            contents: InnerFunc::Code(
                *func.contents.clone(),
                func.params.iter().map(|(n, _)| n.clone()).collect(),
                HashMap::from_iter(
                    func.contents
                        .used_variables()
                        .filter(|v| !func.params.iter().any(|(n, _)| v == n))
                        .map(|v| {
                            let val = self.get_var(&v).clone();
                            (v, val)
                        }),
                ),
            ),
        })
    }

    fn eval_call(&mut self, call: &Call, expected_type: Datatype) -> Value {
        let base = self.eval_expr(&call.base);
        let params = call
            .params
            .iter()
            .map(|p| self.eval_expr(p))
            .collect::<Vec<_>>();
        base.call(params, self, Some(expected_type))
    }

    pub fn call_function(&mut self, preset_vals: HashMap<String, Value>, func: &Expr) -> Value {
        self.variables.push(VarScope {
            vars: preset_vals,
            blocking: true,
        });
        let res = self.eval_expr(func);
        self.variables.pop();
        res
    }

    fn eval_specify_generics(&mut self, gspec: &GenericSpecify) -> Value {
        self.eval_expr(&gspec.base).insert_generics(&gspec.types)
    }
}
