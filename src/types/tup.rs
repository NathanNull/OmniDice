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
    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        if let Some(idx) = as_idx(name) {
            self.entries.0.get(idx).cloned()
        } else {
            None
        }
    }
    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        let mut entries = vec![];
        for t in &self.entries.0 {
            entries.push(t.insert_generics(generics)?);
        }
        Some(Box::new(Self {
            entries: TypeList(entries),
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        let other = other.downcast::<Self>()?;
        let mut vars = HashMap::new();
        for t in &self.entries.0 {
            for v in &other.entries.0 {
                for (name, var) in t.try_match(v)? {
                    vars.insert(name, var);
                }
            }
        }
        Some(vars)
    }
    fn get_generics(&self) -> Vec<String> {
        self.entries.0.iter().map(|e|e.get_generics().into_iter()).flatten().collect()
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
