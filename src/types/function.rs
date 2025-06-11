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
    fn get_generics(&self, params: Vec<Datatype>) -> Option<HashMap<String, Datatype>> {
        let mut generic_matches = HashMap::new();
        for (given, expected) in params.iter().zip(self.params.0.iter()) {
            let generics = expected
                .insert_generics(&generic_matches)?
                .try_match(given)?;
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
    fn call_result(&self, params: Vec<Datatype>) -> Option<Datatype> {
        let generic_matches = self.get_generics(params)?;
        self.output.insert_generics(&generic_matches)
    }

    fn bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
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
}
impl Val for Func {
    fn call(&self, params: Vec<Value>, interpreter: &mut Interpreter) -> Value {
        let generics = self
            .get_type()
            .downcast::<FuncT>()
            .unwrap()
            .get_generics(params.iter().map(|p| p.get_type()).collect())
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
        interpreter.call_function(preset_vals, &self.contents.replace_generics(&generics))
    }

    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if op == Op::Plus && other.get_type().possible_call() {
            Box::new(FuncSum::new(vec![self.dup(), other.dup()]))
        } else {
            invalid!(op, self, other);
        }
    }
}
