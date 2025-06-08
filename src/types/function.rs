use std::collections::HashMap;

use crate::{parser::Expr, type_init};

use super::*;

#[derive(Clone, Debug)]
pub struct Func {
    pub params: TypeList,
    pub output: Datatype,
    pub param_names: Vec<String>,
    pub contents: Expr,
    pub captured_scope: HashMap<String, Value>,
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "func ({})->{} (captured: {:?})",
            self.params
                .0
                .iter()
                .map(|p| format!("{p}"))
                .collect::<Vec<_>>()
                .join(", "),
            self.contents.output,
            self.captured_scope
        )
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.get_type() == other.get_type()
    }
}

type_init!(FuncT, Func, "func", params: TypeList, output: Datatype);

// TODO: idk if this needed anything but if it does, add it
impl Type for FuncT {
    fn call_result(&self, params: Vec<Datatype>) -> Option<Datatype> {
        if params
            .iter()
            .zip(self.params.0.iter())
            .all(|(called, expected)| called == expected)
        {
            Some(self.output.clone())
        } else {
            None
        }
    }
}
impl Val for Func {
    fn call(&self, params: Vec<Value>, interpreter: &mut Interpreter) -> Value {
        let mut preset_vals = self.captured_scope.clone();
        for (name, val) in self
            .param_names
            .iter()
            .zip(params.into_iter())
            .map(|(name, val)| (name.clone(), val))
        {
            preset_vals.insert(name, val);
        }
        interpreter.call_function(preset_vals, &self.contents)
    }
}
