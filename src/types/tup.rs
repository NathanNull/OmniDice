use std::sync::RwLockReadGuard;

use crate::{invalid, mut_type_init, type_init};

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct _InnerTuple {
    entries: TypeList,
    pub elements: Vec<Value>,
}

impl Display for _InnerTuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let len = self.elements.len();
        for (i, ele) in self.elements.iter().enumerate() {
            write!(f, "{}", ele)?;
            if i != len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl _InnerTuple {
    fn new(elements: Vec<Value>) -> Self {
        Self {
            entries: elements.iter().map(|e| e.get_type()).collect(),
            elements,
        }
    }
}

mut_type_init!(Tuple, _InnerTuple);

impl Tuple {
    pub fn new(elements: Vec<Value>) -> Self {
        Self::make(_InnerTuple::new(elements))
    }
}

type_init!(TupT, Tuple, "tuple", (RwLockReadGuard<_InnerTuple>), entries: TypeList);

fn as_idx(prop: &str) -> Option<usize> {
    prop.strip_prefix('i').and_then(|n| n.parse::<usize>().ok())
}

impl Type for TupT {
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        if let Some(idx) = as_idx(name) {
            self.entries.0.get(idx).cloned()
        } else {
            None
        }
    }
}
impl Val for Tuple {
    fn get_prop(&self, name: &str) -> Value {
        if let Some(idx) = as_idx(name) {
            self.inner().elements[idx].clone()
        } else {
            invalid!("Prop", self, ());
        }
    }

    fn set_prop(&self, prop: &str, value: Value) {
        if let Some(idx) = as_idx(prop) {
            self.inner_mut().elements[idx] = value
        } else {
            invalid!("Prop", self, ());
        }
    }
}
