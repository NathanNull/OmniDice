use crate::{invalid, type_init};

use super::*;

#[derive(Debug, Clone)]
pub struct Maybe {
    pub output: Datatype,
    pub contents: Option<Value>
}

impl PartialEq for Maybe {
    fn eq(&self, other: &Self) -> bool {
        &self.contents == &other.contents && self.output == other.output
    }
}

impl Display for Maybe {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.contents {
            Some(c) => write!(f, "Filled({c})"),
            None => write!(f, "Null"),
        }
    }
}

type_init!(MaybeT, Maybe, "maybe", output: Datatype);

fn unwrap_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<MaybeT>() {
        Some(me.output)
    } else {
        None
    }
}

fn unwrap_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<Maybe>() {
        me.contents.unwrap()
    } else {
        invalid!("Call", "unwrap", params);
    }
}

impl Type for MaybeT {
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "unwrap" => Some(Box::new(RustFuncT::new_member(unwrap_sig, self.dup()))),
            "filled" => Some(Box::new(BoolT)),
            _ => None,
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(Box::new(Self {
            output: self.output.insert_generics(generics)?,
        }))
    }
    fn try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        self.output.try_match(&other.downcast::<Self>()?.output)
    }
}
impl Val for Maybe {
    fn get_prop(&self, name: &str) -> Value {
        match name {
            "unwrap" => Box::new(RustFunc::new_member(unwrap_sig, unwrap_fn, self.dup())),
            "filled" => Box::new(self.contents.is_some()),
            _ => invalid!("Prop", self, name),
        }
    }
}