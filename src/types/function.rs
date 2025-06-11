use std::collections::HashMap;

use crate::{invalid, parser::Expr, type_init};

use super::*;

#[derive(Clone, Debug)]
pub struct Func {
    pub params: TypeList,
    pub output: Datatype,
    pub generic: GenericList,
    pub param_names: Vec<String>,
    pub contents: Expr,
    pub captured_scope: HashMap<String, Value>,
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
            self.contents.output,
        )
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.get_type() == other.get_type()
    }
}

type_init!(FuncT, Func, "func", params: TypeList, output: Datatype, generic: GenericList);

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
}

impl Type for FuncT {
    fn real_call_result(
        &self,
        params: Vec<Datatype>,
        expected_output: Option<Datatype>,
    ) -> Option<Datatype> {
        let generic_matches = self.get_generics(params, &expected_output)?;
        let res = self.output.insert_generics(&generic_matches)?;
        if res.get_generics().iter().any(|g|self.generic.0.contains(g)) {
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
        Some(Box::new(Self {
            params: TypeList(params),
            output: self.output.insert_generics(generics)?,
            generic: self.generic.clone(),
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
        let mut preset_vals = self.captured_scope.clone();
        for (name, val) in self
            .param_names
            .iter()
            .zip(params.into_iter())
            .map(|(name, val)| (name.clone(), val))
        {
            preset_vals.insert(name, val);
        }
        let body = self.contents.replace_generics(&generics);
        interpreter.call_function(preset_vals, &body)
    }

    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if op == Op::Plus && other.get_type().possible_call() {
            Box::new(FuncSum::new(vec![self.dup(), other.dup()]))
        } else {
            invalid!(op, self, other);
        }
    }
}
