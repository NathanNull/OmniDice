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



impl Type for IterT {
    fn real_prop_type(
        &self,
        name: &str,
    ) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name, self,
            ("next", NEXT_SIG, next_fn, next_prop),
            ("map", MAP_SIG, map_fn, map_prop),
            ("iter", IDENT_SIG, ident_fn, ident_prop),
            ("filter", FILTER_SIG, filter_fn, filter_prop),
            ("fold", FOLD_SIG, fold_fn, fold_prop),
            ("to_map", TO_MAP_SIG, to_map_fn, to_map_prop),
        )
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Datatype, String> {
        Ok(Box::new(Self {
            output: self.output.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
        self.output.try_match(
            &other
                .downcast::<Self>()
                .ok_or_else(|| format!("Can't match {self} with {other}"))?
                .output,
        )
    }
    fn get_generics(&self) -> Vec<String> {
        self.output.get_generics()
    }
}
impl Val for Iter {}
