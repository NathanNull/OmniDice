use crate::type_init;

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncPointer<I, O>(fn(I) -> O);

impl<I, O> Display for FuncPointer<I, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "???")
    }
}

#[derive(Debug, Clone)]
pub struct RustFunc {
    pub params_to_output: FuncPointer<Vec<Datatype>, Option<Datatype>>,
    pub contents: fn(Vec<Value>) -> Value,
}

impl Display for RustFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "foreign function")
    }
}

impl PartialEq for RustFunc {
    fn eq(&self, other: &Self) -> bool {
        self.get_type() == other.get_type()
    }
}

impl RustFunc {
    pub fn new(
        params_to_output: fn(Vec<Datatype>) -> Option<Datatype>,
        contents: fn(Vec<Value>) -> Value,
    ) -> Self {
        Self {
            params_to_output: FuncPointer(params_to_output),
            contents
        }
    }
}

type_init!(RustFuncT, RustFunc, "func", params_to_output: FuncPointer<Vec<Datatype>,Option<Datatype>>);

// TODO: idk if this needed anything but if it does, add it
impl Type for RustFuncT {
    fn call_result(&self, params: Vec<Datatype>) -> Option<Datatype> {
        (self.params_to_output.0)(params)
    }
}
impl Val for RustFunc {
    fn call(&self, params: Vec<Value>, _interpreter: &mut Interpreter) -> Value {
        (self.contents)(params)
    }
}
