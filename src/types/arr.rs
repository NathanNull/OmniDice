use super::*;
use crate::{invalid, type_init};

#[derive(Debug, Clone, PartialEq)]
pub struct Arr {
    entry: Datatype,
    elements: Vec<Value>,
}
impl Display for Arr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let len = self.elements.len();
        for (i, ele) in self.elements.iter().enumerate() {
            write!(f, "{ele}")?;
            if i != len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}
impl Arr {
    pub fn new(elements: Vec<Value>) -> Self {
        Self {
            entry: elements
                .first()
                .map(|e| e.get_type())
                .unwrap_or_else(|| Box::new(Void)),
            elements,
        }
    }
}
type_init!(ArrT, Arr, "array", entry : Datatype);
impl Type for ArrT {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if &other == self {
            match op {
                Op::Plus => Some(self.dup()),
                _ => None,
            }
        } else {
            None
        }
    }

    fn prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "length" => Some(Box::new(Int)),
            _ => None,
        }
    }
}
impl Val for Arr {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if other.get_type() == self.get_type() {
            let rhs = other
                .try_downcast_ref::<Self>()
                .expect("Just checked if it was the right type");
            match op {
                Op::Plus => Box::new(Arr::new(
                    self.elements
                        .iter()
                        .chain(rhs.elements.iter())
                        .cloned()
                        .collect(),
                )),
                _ => invalid!(op, self, other)
            }
        } else {
            invalid!(op, self, other);
        }
    }

    fn get_prop(&self, name: &str) -> Value {
        match name {
            "length" => Box::new(self.elements.len() as i32),
            _ => invalid!("Prop", self, name),
        }
    }
}
