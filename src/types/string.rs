use crate::{invalid, type_init};
use super::*;

type_init!(StringT, String, "string");
impl Type for StringT {
    fn bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if other == &StringT {
            match op {
                Op::Plus => Some(Box::new(StringT)),
                Op::Equal => Some(Box::new(BoolT)),
                Op::NotEqual => Some(Box::new(BoolT)),
                _ => None,
            }
        } else {
            None
        }
    }

    fn prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "length" => Some(Box::new(IntT)),
            _ => None,
        }
    }
}

impl Val for String {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.downcast::<String>() {
            match op {
                Op::Plus => Box::new(self.clone() + &rhs),
                Op::Equal => Box::new(self == &rhs),
                Op::NotEqual => Box::new(self != &rhs),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other)
        }
    }

    fn get_prop(&self, name: &str) -> Value {
        match name {
            "length" => todo!("how make mutable"),//&mut (Box::new(self.len() as i32) as Box<dyn Val>),
            _ => invalid!("Prop", self, name),
        }
    }
}