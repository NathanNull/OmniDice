use std::sync::RwLockReadGuard;

use crate::{invalid, mut_type_init, type_init};

use super::*;

#[derive(Clone, PartialEq)]
pub struct _InnerTuple {
    entries: Vec<Datatype>,
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

impl Debug for _InnerTuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let len = self.elements.len();
        for (i, ele) in self.elements.iter().enumerate() {
            write!(f, "{:?}", ele)?;
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

type_init!(TupT {nodisplay}, Tuple, "tuple", (RwLockReadGuard<_InnerTuple>), entries: Vec<Datatype>);

impl Display for TupT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({},)",
            self.entries
                .iter()
                .map(|e| format!("{e}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn as_idx(prop: &str) -> Option<usize> {
    prop.strip_prefix('i').and_then(|n| n.parse::<usize>().ok())
}

impl Type for TupT {
    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        if let Some(idx) = as_idx(name) {
            self.entries.get(idx).cloned()
        } else {
            None
        }
    }
    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        let mut entries = vec![];
        for t in &self.entries {
            entries.push(t.insert_generics(generics)?);
        }
        Some(Box::new(Self { entries }))
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        let other = other.downcast::<Self>()?;
        let mut vars = HashMap::new();
        if self.entries.len() != other.entries.len() {
            return None;
        }
        for (t, v) in self.entries.iter().zip(other.entries.iter()) {
            let matched = t.try_match(v)?;
            for (name, var) in matched {
                if let Some(res) = vars.get(&name) {
                    if *res != *var {
                        return None;
                    }
                } else {
                    vars.insert(name, var);
                }
            }
        }
        Some(vars)
    }
    fn get_generics(&self) -> Vec<String> {
        self.entries
            .iter()
            .map(|e| e.get_generics().into_iter())
            .flatten()
            .collect()
    }

    fn is_hashable(&self) -> bool {
        self.entries.iter().all(|e| e.is_hashable())
    }
}
impl Val for Tuple {
    fn get_prop(&self, name: &str) -> Result<Value, RuntimeError> {
        if let Some(idx) = as_idx(name) {
            Ok(self.inner().elements[idx].clone())
        } else {
            invalid!("Prop", self, ());
        }
    }

    fn set_prop(&self, prop: &str, value: Value) -> Result<(), RuntimeError> {
        if let Some(idx) = as_idx(prop) {
            self.inner_mut().elements[idx] = value;
            Ok(())
        } else {
            invalid!("Prop", self, ());
        }
    }

    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        self.inner()
            .elements
            .iter()
            .map(|e| e.as_ref().hash(h))
            .collect::<Result<Vec<_>, _>>()
            .map(|_| ())
    }
}
