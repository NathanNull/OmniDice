use std::collections::HashMap;

use crate::{invalid, parser::Expr, type_init};

use super::*;

#[derive(Debug, Clone)]
pub struct MaybeOwnerTy(pub Option<Datatype>);

impl Display for MaybeOwnerTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ty) = self.0.as_ref() {
            write!(f, "{ty}")
        } else {
            write!(f, "unowned")
        }
    }
}

#[derive(Clone, Debug)]
pub struct Func {
    pub params: TypeList,
    pub owner_t: MaybeOwnerTy,
    pub output: Datatype,
    pub generic: GenericList,
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
                let body = expr.replace_generics(&generics);
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
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "func ({})->{}",
            self.params
                .0
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

type_init!(FuncT, Func, "func", params: TypeList, output: Datatype, generic: GenericList, owner_t: MaybeOwnerTy);

impl FuncT {
    fn get_generics(
        &self,
        params: Vec<Datatype>,
        expected_output: &Option<Datatype>,
    ) -> Option<HashMap<String, Datatype>> {
        if params.len() != self.params.0.len() {
            return None;
        }
        let mut generic_matches = HashMap::new();
        for (given, expected) in params
            .iter()
            .zip(self.params.0.iter())
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
            owner_t: MaybeOwnerTy(None),
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
            .0
            .as_ref()
            .expect("Can't make this function into a member");
        let generics = me_owner
            .try_match(&owner)
            .expect("Invalid owner for this type");
        for (name, _) in &generics {
            self.generic.0.remove(
                self.generic
                    .0
                    .iter()
                    .enumerate()
                    .find(|g| (g.1 == name))
                    .unwrap()
                    .0,
            );
        }
        self.params
            .0
            .iter_mut()
            .for_each(|p| *p = p.insert_generics(&generics).expect("Invalid generic here"));
        self.output = self
            .output
            .insert_generics(&generics)
            .expect("Invalid generic here");
        self.owner_t = MaybeOwnerTy(
            self.owner_t
                .0
                .as_ref()
                .map(|o| o.insert_generics(&generics).expect("Invalid generic here")),
        );
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
        if res
            .get_generics()
            .iter()
            .any(|g| self.generic.0.contains(g))
        {
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
                f_types: TypeList(vec![self.dup(), other.dup()]),
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
        for t in &self.params.0 {
            params.push(t.insert_generics(generics)?);
        }
        let owner_t = MaybeOwnerTy(if let Some(t) = self.owner_t.0.as_ref() {
            Some(t.insert_generics(generics)?)
        } else {
            None
        });
        Some(Box::new(Self {
            params: TypeList(params),
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
                    .0
                    .iter()
                    .map(|p| p.get_generics().into_iter())
                    .flatten(),
            )
            .filter(|g| !self.generic.0.contains(g))
            .collect()
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        let other = other.downcast::<Self>()?;
        let mut generics = self.get_generics(other.params.0, &Some(other.output))?;
        match (&self.owner_t.0, &other.owner_t.0) {
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
}
