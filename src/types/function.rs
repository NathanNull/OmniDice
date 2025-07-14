use std::collections::HashMap;

use crate::{op_list, parser::Expr, type_init};

use super::*;

#[derive(Clone)]
pub struct Func {
    pub params: Vec<Datatype>,
    pub owner_t: Option<Datatype>,
    pub output: Datatype,
    pub generic: Vec<String>,
    pub contents: InnerFunc,
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Func {{ contents: {:?} }}", self.contents)
    }
}

type RustFunc = fn(Vec<Value>, &mut Interpreter, Option<Datatype>) -> Result<Value, RuntimeError>;

#[derive(Clone)]
pub enum InnerFunc {
    Code(Expr, Vec<String>, HashMap<String, Value>),
    Rust(RustFunc, Option<Value>),
}

impl Debug for InnerFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InnerFunc::Code(expr, _, _) => write!(f, "Code({expr:?})"),
            InnerFunc::Rust(func, _) => write!(f, "Internal({func:?})"),
        }
    }
}

impl InnerFunc {
    fn eval(
        &self,
        generics: HashMap<String, Datatype>,
        interpreter: &mut Interpreter,
        mut params: Vec<Value>,
        expected_output: Option<Datatype>,
    ) -> Result<Value, RuntimeError> {
        match self {
            InnerFunc::Code(expr, param_names, captured_scope) => {
                let mut preset_vals = captured_scope.clone();
                for (name, val) in param_names
                    .iter()
                    .zip(params.into_iter())
                    .map(|(name, val)| (name.clone(), val))
                {
                    preset_vals.insert(name, val);
                }
                let body = expr
                    .replace_generics(&generics)
                    .map_err(|e| RuntimeError::partial(&e))?;
                interpreter.call_function(preset_vals, &body)
            }
            InnerFunc::Rust(func, owner) => {
                if let Some(owner) = owner.clone() {
                    params = [owner].into_iter().chain(params.into_iter()).collect()
                }
                (func)(params, interpreter, expected_output)
            }
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Self, RuntimeError> {
        Ok(match self {
            InnerFunc::Code(expr, param_names, captured_scope) => InnerFunc::Code(
                *expr
                    .replace_generics(generics)
                    .map_err(|e| RuntimeError::partial(&e))?,
                param_names.clone(),
                captured_scope.clone(),
            ),
            InnerFunc::Rust(_, _) => self.clone(),
        })
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "func ({})->{}",
            self.params
                .iter()
                .map(|p| format!("{p}"))
                .collect::<Vec<_>>()
                .join(", "),
            self.output,
        )
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.get_type() == other.get_type()
    }
}

type_init!(FuncT {nodisplay}, Func, "func", params: Vec<Datatype>, output: Datatype, generic: Vec<String>, owner_t: Option<Datatype>);

impl Display for FuncT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "func<{}>({})->{}",
            if self.generic.len() > 0 {
                format!(
                    "{}",
                    self.generic
                        .iter()
                        .map(|p| format!("{p}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            } else {
                "".to_string()
            },
            self.params
                .iter()
                .map(|p| format!("{p}"))
                .collect::<Vec<_>>()
                .join(", "),
            self.output
        )
    }
}

impl FuncT {
    fn get_generics(
        &self,
        params: Vec<Datatype>,
        expected_output: &Option<Datatype>,
    ) -> Option<HashMap<String, Datatype>> {
        if params.len() != self.params.len() {
            return None;
        }
        let mut generic_matches = HashMap::new();
        for (given, expected) in params
            .iter()
            .zip(self.params.iter())
            .chain(expected_output.iter().map(|o| (o, &self.output)))
        {
            let inserted = expected.insert_generics(&generic_matches)?;
            let generics = inserted.try_match(given)?;
            for (g, ty) in generics {
                if let Some(prev_ty) = generic_matches.get(&g) {
                    if *prev_ty != ty {
                        return None;
                    }
                } else {
                    generic_matches.insert(g, ty);
                }
            }
        }
        Some(generic_matches)
    }

    pub fn make_rust(self, func: RustFunc) -> Func {
        Func {
            params: self.params,
            output: self.output,
            generic: self.generic,
            owner_t: None,
            contents: InnerFunc::Rust(func, None),
        }
    }

    pub fn make_rust_member(mut self, func: RustFunc, owner: Value) -> Result<Func, RuntimeError> {
        self = self.with_owner(owner.get_type())?;
        Ok(Func {
            params: self.params,
            output: self.output,
            generic: self.generic,
            owner_t: self.owner_t,
            contents: InnerFunc::Rust(func, Some(owner)),
        })
    }

    pub fn with_owner(mut self, owner: Datatype) -> Result<Self, RuntimeError> {
        let me_owner = self
            .owner_t
            .as_ref()
            .ok_or_else(|| RuntimeError::partial("Can't make this function into a member"))?;
        let generics = me_owner.try_match(&owner).ok_or_else(|| {
            RuntimeError::partial(&format!(
                "Invalid owner for this function (expected {}, found {})",
                me_owner, owner
            ))
        })?;
        for (name, _) in &generics {
            self.generic.remove(
                self.generic
                    .iter()
                    .enumerate()
                    .find(|g| (g.1 == name))
                    .ok_or_else(|| {
                        RuntimeError::partial(&format!(
                            "Couldn't find generic {:?} in {:?}",
                            name, self.generic
                        ))
                    })?
                    .0,
            );
        }
        self.params
            .iter_mut()
            .map(|p| {
                *p = p
                    .insert_generics(&generics)
                    .ok_or_else(|| RuntimeError::partial("Invalid generic here"))?;
                Ok(())
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.output = self
            .output
            .insert_generics(&generics)
            .ok_or_else(|| RuntimeError::partial("Invalid generic here"))?;
        self.owner_t = match &self.owner_t {
            Some(o) => Some(
                o.insert_generics(&generics)
                    .ok_or_else(|| RuntimeError::partial("Invalid generic here"))?,
            ),
            None => None,
        };
        Ok(self)
    }
}

impl Type for FuncT {
    fn real_call_result(
        &self,
        params: Vec<Datatype>,
        expected_output: Option<Datatype>,
    ) -> Option<(Datatype, CallFn)> {
        let generic_matches = self.get_generics(params, &expected_output)?;
        let res = self.output.insert_generics(&generic_matches)?;
        if res.get_generics().iter().any(|g| self.generic.contains(g)) {
            println!("Unconstrained generic");
            return None;
        }
        fn call(
            me: &Expr,
            args: &Vec<Expr>,
            i: &mut Interpreter,
            out: Option<Datatype>,
        ) -> OpResult {
            let me = i.try_eval_as::<Func>(me)?;
            let generics = me
                .get_type()
                .downcast::<FuncT>()
                .ok_or_else(|| RuntimeError::partial("Func type isn't FuncT"))?
                .get_generics(args.iter().map(|p| p.output.clone()).collect(), &out)
                .ok_or_else(|| RuntimeError::partial("Couldn't resolve generics"))?;
            let params = args
                .into_iter()
                .map(|a| i.eval_expr(a))
                .collect::<Result<_, _>>()?;
            me.contents.eval(generics, i, params, out)
        }
        Some((
            if let Some(o) = expected_output {
                o.assert_same(&res)
            } else {
                res
            },
            call,
        ))
    }

    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
        if other.downcast::<FuncSumT>().is_some() {
            op_list!(op => {
                Plus(l: Func, r: FuncSum) -> (FuncSumT {f_types: vec![]}) |l,r| Ok(FuncSum::new(vec![Box::new(l), Box::new(r)]));
            })
        } else if other.downcast::<FuncT>().is_some() {
            op_list!(op => {
                Plus(l: Func, r: Func) -> (FuncSumT {f_types: vec![]}) |l,r| Ok(FuncSum::new(vec![Box::new(l), Box::new(r)]));
            })
        } else {
            None
        }
    }

    fn possible_call(&self) -> bool {
        true
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        let mut params = vec![];
        for t in &self.params {
            params.push(t.insert_generics(generics)?);
        }
        let owner_t = if let Some(t) = self.owner_t.as_ref() {
            Some(t.insert_generics(generics)?)
        } else {
            None
        };
        Some(Box::new(Self {
            params,
            output: self.output.insert_generics(generics)?,
            generic: self.generic.clone(),
            owner_t,
        }))
    }
    fn get_generics(&self) -> Vec<String> {
        self.output
            .get_generics()
            .into_iter()
            .chain(
                self.params
                    .iter()
                    .map(|p| p.get_generics().into_iter())
                    .flatten(),
            )
            .filter(|g| !self.generic.contains(g))
            .collect()
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        let other = other.downcast::<Self>()?;
        let mut generics = self.get_generics(other.params, &Some(other.output))?;
        match (&self.owner_t, &other.owner_t) {
            (Some(l), Some(r)) => {
                for (k, v) in l.try_match(r)? {
                    if let Some(val) = generics.get(&k) {
                        if *val != v {
                            return None;
                        }
                    } else {
                        generics.insert(k.clone(), v.clone());
                    }
                }
            }
            (None, None) => (),
            _ => return None,
        }
        Some(generics)
    }

    fn specify_generics(&self, generics: &Vec<Datatype>) -> Option<Datatype> {
        if generics.len() != self.generic.len() {
            return None;
        }
        let g_map = HashMap::from_iter(
            self.generic
                .iter()
                .zip(generics.iter())
                .map(|(n, t)| (n.clone(), t.clone())),
        );
        let mut new_params = vec![];
        for p in &self.params {
            new_params.push(p.insert_generics(&g_map)?);
        }
        let new_output = self.output.insert_generics(&g_map)?;
        let new_owner_t = if let Some(t) = self.owner_t.as_ref() {
            Some(t.insert_generics(&g_map)?)
        } else {
            None
        };
        Some(Box::new(FuncT {
            params: new_params,
            output: new_output,
            generic: vec![],
            owner_t: new_owner_t,
        }))
    }
}
impl Val for Func {
    fn insert_generics(&self, generics: &Vec<Datatype>) -> Result<Value, RuntimeError> {
        let new_ty = self
            .get_type()
            .specify_generics(generics)
            .ok_or_else(|| RuntimeError::partial("Couldn't resolve generics"))?
            .downcast::<FuncT>()
            .ok_or_else(|| RuntimeError::partial("Generic specification result wasn't a Func"))?;
        Ok(Box::new(Func {
            params: new_ty.params,
            owner_t: new_ty.owner_t,
            output: new_ty.output,
            generic: vec![],
            contents: self.contents.insert_generics(&HashMap::from_iter(
                self.generic
                    .iter()
                    .zip(generics.iter())
                    .map(|(n, t)| (n.clone(), t.clone())),
            ))?,
        }))
    }
}
