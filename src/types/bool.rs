use crate::{invalid, type_init};
use super::*;

type_init!(Bool, bool, "bool");
impl Type for Bool {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if other == Bool {
            match op {
                Op::And | Op::Or | Op::Equal | Op::NotEqual => Some(self.dup()),
                _ => None,
            }
        } else {
            None
        }
    }

    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Not => Some(self.dup()),
            _ => None,
        }
    }
}
impl Val for bool {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.downcast::<bool>() {
            Box::new(match op {
                Op::And => *self && rhs,
                Op::Or => *self || rhs,
                Op::Equal => self == &rhs,
                Op::NotEqual => self != &rhs,
                _ => invalid!(op, self, other),
            })
        } else {
            invalid!(op, self, other)
        }
    }

    fn pre_op(&self, op: Op) -> Value {
        match op {
            Op::Not => Box::new(!self),
            _ => invalid!(op, self, ()),
        }
    }
}