use std::sync::RwLockReadGuard;

use crate::{mut_type_init, type_init};

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

macro_rules! prop_at_idx {
    ($idx: expr) => {{
        fn get_prop(me: &Expr, i: &mut Interpreter) -> OpResult {
            Ok(i.try_eval_as::<Tuple>(me)?.inner().elements[$idx].clone())
        }
        fn set_prop(me: &Expr, val: &Expr, i: &mut Interpreter) -> VoidResult {
            i.try_eval_as::<Tuple>(me)?.inner_mut().elements[$idx] = i.eval_expr(val)?;
            Ok(())
        }
        (get_prop, set_prop)
    }};
}

macro_rules! idx_props {
    ($($idx: literal),*) => {
        [
            $(prop_at_idx!($idx)),*
        ]
    };
}

// Admittedly kinda hacky solution to needing all of the prop getter functions available at compile time,
// just make a macro that can write as many as you (should) need. Tbf if you're writing code that uses 33-element
// tuples, you're probably doing something wrong anyway.
const PROPS: [(UnOpFn, SetFn); 32] = idx_props!(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31
);

impl Type for TupT {
    fn real_prop_type(&self, name: &str) -> Option<(Datatype, Option<UnOpFn>, Option<SetFn>)> {
        if let Some(idx) = as_idx(name) {
            self.entries.get(idx).and_then(|e| {
                let (get, set) = PROPS.get(idx)?;
                Some((e.clone(), Some(*get), Some(*set)))
            })
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
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        self.inner()
            .elements
            .iter()
            .map(|e| e.as_ref().hash(h))
            .collect::<Result<Vec<_>, _>>()
            .map(|_| ())
    }
}
