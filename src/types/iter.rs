use crate::{invalid, type_init};

use super::*;

#[derive(Debug, Clone)]
pub struct Iter {
    pub next_fn: Value,
    pub output: Datatype,
}

impl PartialEq for Iter {
    fn eq(&self, other: &Self) -> bool {
        &self.next_fn == &other.next_fn && self.output == other.output
    }
}

impl Display for Iter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Iterator<{}>", self.output)
    }
}

type_init!(IterT, Iter, "iter", output: Datatype);

fn next_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<IterT>() {
        Some(Box::new(MaybeT { output: me.output }))
    } else {
        None
    }
}

fn next_fn(params: Vec<Value>, i: &mut Interpreter) -> Value {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<Iter>() {
        Box::new(
            me.next_fn
                .call(vec![], i)
                .downcast::<Maybe>()
                .expect("Invalid iter function return value"),
        )
    } else {
        invalid!("Call", "next", params)
    }
}

impl Type for IterT {
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "next" => Some(Box::new(RustFuncT::new_member(next_sig, self.dup()))),
            _ => None,
        }
    }
}
impl Val for Iter {
    fn get_prop(&self, name: &str) -> Value {
        match name {
            "next" => Box::new(RustFunc::new_member(next_sig, next_fn, self.dup())),
            _ => invalid!("Prop", self, name),
        }
    }
}
