use crate::{invalid, type_init};
use super::*;

type_init!(StringT, String, "string");
impl Type for StringT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
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

    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "length" => Some(Box::new(IntT)),
            _ => None,
        }
    }

    fn is_hashable(&self) -> bool {
        true
    }
}

impl Val for String {
    fn bin_op(&self, other: &Value, op: Op) -> Result<Value, RuntimeError> {
        if let Some(rhs) = other.downcast::<String>() {
            Ok(match op {
                Op::Plus => Box::new(self.clone() + &rhs),
                Op::Equal => Box::new(self == &rhs),
                Op::NotEqual => Box::new(self != &rhs),
                _ => invalid!(op, self, other),
            })
        } else {
            invalid!(op, self, other)
        }
    }

    fn get_prop(&self, name: &str) -> Result<Value, RuntimeError> {
        Ok(match name {
            "length" => Box::new(self.len() as i32),
            _ => invalid!("Prop", self, name),
        })
    }

    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write(self.as_bytes());
        Ok(())
    }
}