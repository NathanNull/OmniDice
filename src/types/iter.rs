use std::sync::LazyLock;

use crate::{gen_fn_map, invalid, type_init};

use super::*;

mod funcs;
use funcs::*;

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

gen_fn_map!(
    ITER_FNS,
    ("next", NEXT_SIG, next_fn),
    ("map", MAP_SIG, map_fn),
    ("iter", IDENT_SIG, ident_fn),
    ("filter", FILTER_SIG, filter_fn),
    ("fold", FOLD_SIG, fold_fn),
);

impl Type for IterT {
    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            n if ITER_FNS.contains_key(n) => {
                Some(Box::new(ITER_FNS[n].0.clone().with_owner(self.dup()).ok()?))
            }
            "to_map" => TO_MAP_SIG
                .clone()
                .with_owner(self.dup())
                .map(|f| Box::new(f) as Datatype)
                .ok(),
            _ => None,
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(Box::new(Self {
            output: self.output.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        self.output.try_match(&other.downcast::<Self>()?.output)
    }
    fn get_generics(&self) -> Vec<String> {
        self.output.get_generics()
    }
}
impl Val for Iter {
    fn get_prop(&self, name: &str) -> Result<Value, RuntimeError> {
        match name {
            n if ITER_FNS.contains_key(n) => Ok(Box::new(
                ITER_FNS[n]
                    .0
                    .clone()
                    .make_rust_member(ITER_FNS[n].1, self.dup())?,
            )),
            "to_map" => Ok(Box::new(
                TO_MAP_SIG.clone().make_rust_member(to_map_fn, self.dup())?,
            )),
            _ => invalid!("Prop", self, name),
        }
    }
}
