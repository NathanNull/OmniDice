use crate::{invalid, type_init};
use super::*;

type_init!(VString, String, "string");
impl Type for VString {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if other == VString {
            match op {
                Op::Plus => Some(Box::new(VString)),
                Op::Equal => Some(Box::new(Bool)),
                Op::NotEqual => Some(Box::new(Bool)),
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