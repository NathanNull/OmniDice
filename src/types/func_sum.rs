use crate::{invalid, type_init};

use super::*;

#[derive(Clone, Debug)]
pub struct FuncSum {
    pub fns: Vec<Value>,
    pub f_types: TypeList,
}

impl Display for FuncSum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "funcsum({})",
            self.fns
                .iter()
                .map(|p| format!("{p}"))
                .collect::<Vec<_>>()
                .join(", "),
        )
    }
}

impl PartialEq for FuncSum {
    fn eq(&self, other: &Self) -> bool {
        self.get_type() == other.get_type()
    }
}

impl FuncSum {
    pub fn new(fns: Vec<Value>) -> Self {
        Self {
            f_types: fns.iter().map(|f| f.get_type()).collect(),
            fns,
        }
    }
}

type_init!(FuncSumT, FuncSum, "func", f_types: TypeList);

// TODO: idk if this needed anything but if it does, add it
// TODO: what did I mean by this
impl Type for FuncSumT {
    fn real_call_result(&self, params: Vec<Datatype>, expected_output: Option<Datatype>) -> Option<Datatype> {
        for f in self.f_types.0.iter().rev() {
            if let Some(res) = f.call_result(params.clone(), expected_output.clone()) {
                return Some(res);
            }
        }
        None
    }
    fn possible_call(&self) -> bool {
        true
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
    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        let mut f_types = vec![];
        for t in &self.f_types.0 {
            f_types.push(t.insert_generics(generics)?);
        }
        Some(Box::new(Self {
            f_types: TypeList(f_types),
        }))
    }
    fn try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        let other = other.downcast::<Self>()?;
        let mut vars = HashMap::new();
        for t in &self.f_types.0 {
            for v in &other.f_types.0 {
                for (name, var) in t.try_match(v)? {
                    vars.insert(name, var);
                }
            }
        }
        Some(vars)
    }
    fn get_generics(&self) -> Vec<String> {
        self.f_types.0.iter().map(|f|f.get_generics().into_iter()).flatten().collect()
    }
}
impl Val for FuncSum {
    fn call(&self, params: Vec<Value>, interpreter: &mut Interpreter, expected_output: Option<Datatype>) -> Value {
        for (ty, f) in self.f_types.0.iter().zip(self.fns.iter()).rev() {
            if let Some(_) = ty.call_result(params.iter().map(|v| v.get_type()).collect(), expected_output.clone()) {
                return f.call(params, interpreter, expected_output);
            }
        }
        invalid!("Call", self, ());
    }
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if op == Op::Plus && other.get_type().possible_call() {
            Box::new(FuncSum::new(vec![self.dup(), other.dup()]))
        } else {
            invalid!(op, self, other);
        }
    }
}
