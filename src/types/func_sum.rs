use crate::{op_list, type_init};

use super::*;

// TODO: this should really be combined with the base Func, probably as another enum variant.

#[derive(Clone, Debug)]
pub struct FuncSum {
    pub fns: Vec<Value>,
    pub f_types: Vec<Datatype>,
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

type_init!(FuncSumT {nodisplay}, FuncSum, "func", f_types: Vec<Datatype>);

impl Display for FuncSumT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fsum({})",
            self.f_types
                .iter()
                .map(|f| format!("{f}"))
                .collect::<Vec<_>>()
                .join(" + ")
        )
    }
}

impl Type for FuncSumT {
    fn real_call_result(
        &self,
        params: Vec<Datatype>,
        expected_output: Option<Datatype>,
    ) -> Option<(Datatype, CallFn)> {
        for f in self.f_types.iter().rev() {
            if let Some(res) = f.call_result(params.clone(), expected_output.clone()) {
                return Some(res);
            }
        }
        None
    }
    fn possible_call(&self) -> bool {
        true
    }

    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
        if other.downcast::<FuncSumT>().is_some() {
            op_list!(op => {
                Plus(l: FuncSum, r: FuncSum) -> (FuncSumT {f_types: vec![]}) |l,r| Ok(FuncSum::new(vec![Box::new(l), Box::new(r)]));
            })
        } else if other.downcast::<FuncT>().is_some() {
            op_list!(op => {
                Plus(l: FuncSum, r: Func) -> (FuncSumT {f_types: vec![]}) |l,r| Ok(FuncSum::new(vec![Box::new(l), Box::new(r)]));
            })
        } else {
            None
        }
    }
    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        let mut f_types = vec![];
        for t in &self.f_types {
            f_types.push(t.insert_generics(generics)?);
        }
        Some(Box::new(Self { f_types }))
    }
    fn try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        let other = other.downcast::<Self>()?;
        let mut vars = HashMap::new();
        for t in &self.f_types {
            for v in &other.f_types {
                for (name, var) in t.try_match(v)? {
                    vars.insert(name, var);
                }
            }
        }
        Some(vars)
    }
    fn get_generics(&self) -> Vec<String> {
        self.f_types
            .iter()
            .map(|f| f.get_generics().into_iter())
            .flatten()
            .collect()
    }
}
impl Val for FuncSum {}
