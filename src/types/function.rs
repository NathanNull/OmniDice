use std::collections::HashMap;

use crate::{invalid, parser::Expr, type_init};

use super::*;

#[derive(Clone, Debug)]
pub struct Func {
    pub params: Vec<Datatype>,
    pub owner_t: Option<Datatype>,
    pub output: Datatype,
    pub generic: Vec<String>,
    pub contents: InnerFunc,
}

type RustFunc = fn(Vec<Value>, &mut Interpreter, Option<Datatype>) -> Value;

#[derive(Clone, Debug)]
pub enum InnerFunc {
    Code(Expr, Vec<String>, HashMap<String, Value>),
    Rust(RustFunc, Option<Value>),
}

impl InnerFunc {
    fn eval(
        &self,
        generics: HashMap<String, Datatype>,
        interpreter: &mut Interpreter,
        mut params: Vec<Value>,
        expected_output: Option<Datatype>,
    ) -> Value {
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
                    .expect("Couldn't replace generics");
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

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Self {
        match self {
            InnerFunc::Code(expr, param_names, captured_scope) => InnerFunc::Code(
                *expr
                    .replace_generics(generics)
                    .expect("couldn't replace generics"),
                param_names.clone(),
                captured_scope.clone(),
            ),
            InnerFunc::Rust(_, _) => self.clone(),
        }
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

    pub fn make_rust_member(mut self, func: RustFunc, owner: Value) -> Func {
        self = self.with_owner(owner.get_type()).expect("Invalid owner");
        Func {
            params: self.params,
            output: self.output,
            generic: self.generic,
            owner_t: self.owner_t,
            contents: InnerFunc::Rust(func, Some(owner)),
        }
    }

    pub fn with_owner(mut self, owner: Datatype) -> Option<Self> {
        let me_owner = self
            .owner_t
            .as_ref()
            .expect("Can't make this function into a member");
        let generics = me_owner.try_match(&owner).expect(&format!(
            "Invalid owner for this function (expected {}, found {})",
            me_owner, owner
        ));
        for (name, _) in &generics {
            self.generic.remove(
                self.generic
                    .iter()
                    .enumerate()
                    .find(|g| (g.1 == name))
                    .expect(&format!(
                        "Couldn't find generic {:?} in {:?}",
                        name, self.generic
                    ))
                    .0,
            );
        }
        self.params
            .iter_mut()
            .for_each(|p| *p = p.insert_generics(&generics).expect("Invalid generic here"));
        self.output = self
            .output
            .insert_generics(&generics)
            .expect("Invalid generic here");
        self.owner_t = self
            .owner_t
            .as_ref()
            .map(|o| o.insert_generics(&generics).expect("Invalid generic here"));
        Some(self)
    }
}

impl Type for FuncT {
    fn real_call_result(
        &self,
        params: Vec<Datatype>,
        expected_output: Option<Datatype>,
    ) -> Option<Datatype> {
        let generic_matches = self.get_generics(params, &expected_output)?;
        let res = self.output.insert_generics(&generic_matches)?;
        if res.get_generics().iter().any(|g| self.generic.contains(g)) {
            println!("Unconstrained generic");
            return None;
        }
        Some(if let Some(o) = expected_output {
            o.assert_same(&res)
        } else {
            res
        })
    }

    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if op == Op::Plus && other.possible_call() {
            Some(Box::new(FuncSumT {
                f_types: vec![self.dup(), other.dup()],
            }))
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
    fn call(
        &self,
        params: Vec<Value>,
        interpreter: &mut Interpreter,
        expected_output: Option<Datatype>,
    ) -> Value {
        let generics = self
            .get_type()
            .downcast::<FuncT>()
            .unwrap()
            .get_generics(
                params.iter().map(|p| p.get_type()).collect(),
                &expected_output,
            )
            .unwrap();
        self.contents
            .eval(generics, interpreter, params, expected_output)
    }

    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if op == Op::Plus && other.get_type().possible_call() {
            Box::new(FuncSum::new(vec![self.dup(), other.dup()]))
        } else {
            invalid!(op, self, other);
        }
    }
    fn insert_generics(&self, generics: &Vec<Datatype>) -> Value {
        let new_ty = self
            .get_type()
            .specify_generics(generics)
            .expect("Invalid generic specification")
            .downcast::<FuncT>()
            .unwrap();
        Box::new(Func {
            params: new_ty.params,
            owner_t: new_ty.owner_t,
            output: new_ty.output,
            generic: vec![],
            contents: self.contents.insert_generics(&HashMap::from_iter(
                self.generic
                    .iter()
                    .zip(generics.iter())
                    .map(|(n, t)| (n.clone(), t.clone())),
            )),
        })
    }
}
