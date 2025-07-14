use std::{collections::HashMap, fmt::Debug};

use crate::{
    builtins::BUILTINS,
    error::{RuntimeError, RuntimeErrorType},
    parser::{
        Accessor, Array, Assign, AssignType, Binop, Call, Conditional, Expr, ExprContents, For,
        Function, GenericSpecify, Postfix, Prefix, Return, Scope, Tuple as TupleExpr, While,
    },
    types::{Arr, ArrT, Datatype, Downcast, Func, InnerFunc, Maybe, Tuple, Val, Value, Void},
};

pub struct VarScope<T: Debug> {
    pub vars: HashMap<String, T>,
    pub blocking: bool,
    pub name: &'static str,
}

impl<T: Debug> Debug for VarScope<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}([{}]{})",
            self.name,
            self.vars
                .iter()
                .map(|(v, _)| v.clone())
                .collect::<Vec<_>>()
                .join(", "),
            if self.blocking { ", blocking" } else { "" },
        )
    }
}

pub struct Interpreter {
    ast: Expr,
    variables: Vec<VarScope<Value>>,
    is_const: bool,
}

impl Interpreter {
    pub fn new(ast: Expr) -> Self {
        Self {
            ast,
            variables: vec![VarScope {
                vars: BUILTINS.clone(),
                blocking: true,
                name: "builtins",
            }],
            is_const: false,
        }
    }

    pub fn new_const() -> Self {
        Self {
            ast: Expr {
                contents: ExprContents::Value(Box::new(Void)),
                output: Box::new(Void),
            },
            variables: vec![],
            is_const: true,
        }
    }

    pub fn run(&mut self) {
        let ast = self.ast.clone();
        match self.eval_expr(&ast) {
            Ok(ok) => assert_eq!(
                &ok,
                &(Box::new(Void) as Value),
                "All programs should return void"
            ),
            Err(err) => println!("{err}"),
        }
    }

    fn eval_scope(&mut self, scope: &Scope) -> Result<Value, RuntimeError> {
        let mut last: Value = Box::new(Void);
        // Variables created in scope die when it ends
        self.variables.push(VarScope {
            vars: HashMap::new(),
            blocking: false,
            name: "scope",
        });
        for expr in scope {
            last = self.eval_expr(&expr).inspect_err(|_| {
                self.variables.pop();
            })?;
        }
        self.variables.pop();
        Ok(last)
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        let res = match (&expr.contents, self.is_const) {
            // const-legal expressions (everything that has no side effects and doesn't loop)
            (ExprContents::Value(val), _) => Ok(val.clone()),
            (ExprContents::Binop(binop), _) => self.eval_binop(binop),
            (ExprContents::Prefix(prefix), _) => self.eval_prefix(prefix),
            (ExprContents::Postfix(postfix), _) => self.eval_postfix(postfix),
            (ExprContents::Scope(scope), _) => self.eval_scope(scope),
            (ExprContents::Accessor(acc), _) => self.eval_accessor(acc),
            (ExprContents::Array(arr), _) => self.eval_array(
                arr,
                expr.output
                    .downcast::<ArrT>()
                    .ok_or_else(|| RuntimeError::partial("Array should be array"))?
                    .entry
                    .clone(),
            ),
            (ExprContents::Tuple(tup), _) => self.eval_tuple(tup),
            (ExprContents::Conditional(cond), _) => self.eval_conditional(cond),
            (ExprContents::Function(func), _) => self.eval_function(func),
            (ExprContents::GenericSpecify(gspec), _) => self.eval_specify_generics(gspec),

            // runtime-only expressions (side effects and loops)
            (ExprContents::Assign(assign), false) => self.eval_assign(assign),
            (ExprContents::While(wh), false) => self.eval_while(wh),
            (ExprContents::For(fo), false) => self.eval_for(fo),
            (ExprContents::Call(call), false) => self.eval_call(call, expr.output.clone()),
            (ExprContents::Return(ret), false) => self.eval_return(ret),
            (ExprContents::Break(_), false) => self.eval_break(),
            (ExprContents::Continue(_), false) => self.eval_continue(),
            (_, true) => {
                return Err(RuntimeError::partial(
                    "Can't evaluate this at compile time",
                ));
            }
        }?;
        if res.get_type() != expr.output {
            return Err(RuntimeError::partial(&format!(
                "Expression {expr:?} evaluated to a different type ({}) than expected ({}). This is notable.",
                res.get_type(),
                expr.output
            )));
        }
        Ok(res)
    }

    fn eval_accessor(&mut self, acc: &Accessor) -> Result<Value, RuntimeError> {
        match acc {
            Accessor::Variable(ident, loc) => {
                Ok(self.get_var(ident).map_err(|e| e.stack_loc(*loc))?.clone())
            }
            Accessor::Property(base, property, _) => self.eval_expr(&base)?.get_prop(property),
            Accessor::Index(indexed, index, _) => {
                self.eval_expr(&indexed)?.get_index(self.eval_expr(&index)?)
            }
        }
    }

    fn get_var(&mut self, var: &str) -> Result<&Value, RuntimeError> {
        for scope in self.variables.iter().rev() {
            if let Some(val) = scope.vars.get(var) {
                return Ok(val);
            }
            if scope.blocking {
                break;
            }
        }
        Err(RuntimeError::partial(&format!(
            "Attempted to access nonexistent variable {var}, vars: {:?}",
            self.variables
        )))
    }

    fn set_var(&mut self, var: String, val: Value) -> Result<(), RuntimeError> {
        if let Some(v) = self.variables.last_mut() {
            v.vars.insert(var, val);
            Ok(())
        } else {
            Err(RuntimeError::partial(
                "UNREACHABLE: tried to set variable while no scope alive",
            ))
        }
    }

    fn update_var(&mut self, var: String, val: Value) -> Result<(), RuntimeError> {
        if self.variables.is_empty() {
            return Err(RuntimeError::partial(
                "UNREACHABLE: Tried to set variable while no scope alive",
            ));
        }
        for scope in self.variables.iter_mut().rev() {
            if scope.vars.contains_key(&var) {
                scope.vars.insert(var, val);
                return Ok(());
            }
            if scope.blocking {
                break;
            }
        }
        return Err(RuntimeError::partial(&format!(
            "UNREACHABLE: Tried to update unknown variable {var}, (vars: {:?})",
            self.variables
        )));
    }

    fn eval_binop(&mut self, binop: &Binop) -> Result<Value, RuntimeError> {
        (binop.res)(&binop.lhs, &binop.rhs, self).map_err(|e| e.stack_loc(binop.op_loc))
    }

    fn eval_prefix(&mut self, prefix: &Prefix) -> Result<Value, RuntimeError> {
        (prefix.res)(&prefix.rhs, self).map_err(|e| e.stack_loc(prefix.op_loc))
    }

    fn eval_postfix(&mut self, postfix: &Postfix) -> Result<Value, RuntimeError> {
        (postfix.res)(&postfix.lhs, self).map_err(|e| e.stack_loc(postfix.op_loc))
    }

    fn eval_assign(&mut self, assign: &Assign) -> Result<Value, RuntimeError> {
        let val = self.eval_expr(&assign.val)?;
        match &assign.assignee {
            Accessor::Variable(name, vpos) => match assign.a_type {
                AssignType::Reassign => {
                    if match self.get_var(name) {
                        Err(_) => true,
                        Ok(v) => v.get_type() != val.get_type(),
                    } {
                        return Err(RuntimeError::single(
                            &format!("Tried to reassign variable {name} to a different type"),
                            *vpos,
                        ));
                    }
                    self.update_var(name.clone(), val.dup())
                        .map_err(|e| e.stack_loc(*vpos))?;
                }
                AssignType::Create => self
                    .set_var(name.clone(), val.dup())
                    .map_err(|e| e.stack_loc(*vpos))?,
            },
            Accessor::Property(base, prop, loc) => {
                let base_val = self.eval_expr(&base)?;
                if base_val
                    .get_type()
                    .prop_type(prop)
                    .is_none_or(|pt| val.get_type() != pt)
                {
                    return Err(RuntimeError::single(
                        &format!(
                            "Tried to reassign property {:?} to a different type",
                            assign.assignee
                        ),
                        *loc,
                    ));
                }
                base_val
                    .set_prop(prop, val.dup())
                    .map_err(|e| e.stack_loc(assign.a_loc))?;
            }
            Accessor::Index(indexed, index, loc) => {
                let base_val = self.eval_expr(&indexed)?;
                if base_val
                    .get_type()
                    .index_type(&index.output)
                    .is_none_or(|it| it != val.get_type())
                {
                    return Err(RuntimeError::single(
                        &format!(
                            "Tried to reassign property {:?} to a different type",
                            assign.assignee
                        ),
                        *loc,
                    ));
                };
                base_val
                    .set_index(self.eval_expr(&index)?, val.dup())
                    .map_err(|e| e.stack_loc(assign.a_loc))?;
            }
        }
        Ok(val)
    }

    fn eval_conditional(&mut self, cond: &Conditional) -> Result<Value, RuntimeError> {
        if let Some(condition) = self.eval_expr(&cond.condition)?.downcast::<bool>() {
            if condition {
                self.eval_expr(&cond.result)
            } else if let Some(otherwise) = cond.otherwise.as_ref() {
                self.eval_expr(&otherwise)
            } else {
                Ok(Box::new(Void))
            }
        } else {
            return Err(RuntimeError::partial(
                "UNREACHABLE: Conditional statements should always return boolean values",
            ));
        }
    }

    fn eval_while(&mut self, wh: &While) -> Result<Value, RuntimeError> {
        loop {
            if let Some(condition) = self.eval_expr(&wh.condition)?.downcast::<bool>() {
                if condition {
                    match self.eval_expr(&wh.result) {
                        Ok(v) if v != Box::new(Void) as Value => {
                            return Err(RuntimeError::partial(
                                "While loop returned non-void value",
                            ));
                        }
                        Ok(_) => (),
                        Err(e) if e.err_type().is_break() => break,
                        Err(e) if e.err_type().is_continue() => continue,
                        Err(e) => return Err(e),
                    }
                } else {
                    break;
                }
            } else {
                return Err(RuntimeError::partial(
                    "UNREACHABLE: Conditional statements should always return boolean values",
                ));
            }
        }
        Ok(Box::new(Void))
    }

    fn eval_for(&mut self, fo: &For) -> Result<Value, RuntimeError> {
        let iter = self
            .eval_expr(&fo.iter)?
            .get_prop("iter")
            .map_err(|e| e.stack_loc(fo.iter_loc))?
            .call(vec![], self, None)?;
        self.variables.push(VarScope {
            vars: HashMap::new(),
            blocking: false,
            name: "for",
        });
        loop {
            let next_val = iter
                .get_prop("next")
                .map_err(|e| e.stack_loc(fo.iter_loc))
                .inspect_err(|_| {
                    self.variables.pop();
                })?
                .call(vec![], self, None)
                .inspect_err(|_| {
                    self.variables.pop();
                })?;
            match next_val.downcast::<Maybe>() {
                Some(Maybe {
                    output: _,
                    contents: Some(c),
                }) => {
                    self.set_var(fo.var.clone(), c)
                        .map_err(|e| e.stack_loc(fo.iter_loc))
                        .inspect_err(|_| {
                            self.variables.pop();
                        })?;
                    match self.eval_expr(&fo.body) {
                        Ok(v) if v != Box::new(Void) as Value => {
                            return Err(RuntimeError::partial("For loop returned non-void value"));
                        }
                        Ok(_) => (),
                        Err(e) if e.err_type().is_break() => break,
                        Err(e) if e.err_type().is_continue() => continue,
                        Err(e) => return Err(e),
                    }
                }
                Some(Maybe {
                    output: _,
                    contents: None,
                }) => break,
                None => {
                    return Err(RuntimeError::partial(
                        "UNREACHABLE: Iterator functions should always return Maybe",
                    ));
                }
            }
        }
        self.variables.pop();
        Ok(Box::new(Void))
    }

    fn eval_array(&mut self, arr: &Array, expected_type: Datatype) -> Result<Value, RuntimeError> {
        let mut res = vec![];
        for expr in &arr.elements {
            res.push(self.eval_expr(expr)?);
            let next_type = res.last().unwrap().get_type();
            if expected_type != next_type {
                return Err(match arr.elements.first() {
                    Some(_) => RuntimeError::partial("Array types didn't match"),
                    None => RuntimeError::partial(
                        "UNREACHABLE: array types don't match without any elements in the array",
                    ),
                });
            }
        }
        Ok(Box::new(Arr::new(res, expected_type)))
    }

    fn eval_tuple(&mut self, tup: &TupleExpr) -> Result<Value, RuntimeError> {
        let mut res = vec![];
        for expr in &tup.elements {
            res.push(self.eval_expr(expr)?);
        }
        Ok(Box::new(Tuple::new(res)))
    }

    fn eval_function(&mut self, func: &Function) -> Result<Value, RuntimeError> {
        Ok(Box::new(Func {
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
                        .map(|v| -> Result<_, RuntimeError> {
                            let val = self.get_var(&v)?.clone();
                            Ok((v, val))
                        })
                        .collect::<Result<Vec<_>, RuntimeError>>()?,
                ),
            ),
        }))
    }

    fn eval_call(&mut self, call: &Call, expected_type: Datatype) -> Result<Value, RuntimeError> {
        let base = self.eval_expr(&call.base)?;
        let params = {
            let mut res = vec![];
            for p in call.params.iter() {
                res.push(self.eval_expr(p)?)
            }
            res
        };
        base.call(params, self, Some(expected_type))
            .map_err(|e| e.stack_loc(call.loc))
    }

    pub fn call_function(
        &mut self,
        preset_vals: HashMap<String, Value>,
        func: &Expr,
    ) -> Result<Value, RuntimeError> {
        self.variables.push(VarScope {
            vars: preset_vals,
            blocking: true,
            name: "func",
        });
        let res = self.eval_expr(func);
        self.variables.pop();
        match res {
            Err(err) if err.err_type().is_return() => match err.err_type() {
                RuntimeErrorType::Return(v) => Ok(v.clone()),
                _ => unreachable!("Just checked if it was a Return"),
            },
            r => r,
        }
    }

    fn eval_specify_generics(&mut self, gspec: &GenericSpecify) -> Result<Value, RuntimeError> {
        self.eval_expr(&gspec.base)?.insert_generics(&gspec.types)
    }

    fn eval_return(&mut self, ret: &Return) -> Result<Value, RuntimeError> {
        Err(RuntimeError::special(RuntimeErrorType::Return(
            self.eval_expr(&ret.ret)?,
        )))
    }

    fn eval_break(&mut self) -> Result<Value, RuntimeError> {
        Err(RuntimeError::special(RuntimeErrorType::Break))
    }

    fn eval_continue(&mut self) -> Result<Value, RuntimeError> {
        Err(RuntimeError::special(RuntimeErrorType::Continue))
    }

    pub fn try_eval_as<T: Val + Clone>(&mut self, expr: &Expr) -> Result<T, RuntimeError> {
        self.eval_expr(expr)?
            .downcast::<T>()
            .ok_or_else(|| RuntimeError::partial("Expression evaluated to the wrong type"))
    }
}
