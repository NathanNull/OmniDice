use crate::{invalid, type_init};
use super::*;

type_init!(DiceT, Distribution, "dice");
impl Type for DiceT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if (other == &IntT || other == &DiceT) && NUM_OPS.contains(&op) {
            Some(Box::new(DiceT))
        } else {
            None
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(Box::new(DiceT)),
            _ => None,
        }
    }
    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "mean" | "stddev" => Some(Box::new(FloatT)),
            "max" | "min" => Some(Box::new(IntT)),
            _ => None,
        }
    }
}
impl Val for Distribution {
    fn bin_op(&self, other: &Value, op: Op) -> Result<Value, RuntimeError> {
        if let Some(rhs) = other.downcast::<i32>() {
            Ok(match op {
                Op::Plus => Box::new(self.clone() + rhs.into()),
                Op::Minus => Box::new(self.clone() - rhs.into()),
                Op::Times => Box::new(self.clone() * rhs.into()),
                Op::Divided => Box::new(self.clone() / rhs.into()),
                _ => invalid!(op, self, other),
            })
        } else if let Some(rhs) = other.downcast::<Distribution>() {
            Ok(match op {
                Op::Plus => Box::new(self.clone() + rhs.clone()),
                Op::Minus => Box::new(self.clone() - rhs.clone()),
                Op::Times => Box::new(self.clone() * rhs.clone()),
                Op::Divided => Box::new(self.clone() / rhs.clone()),
                _ => invalid!(op, self, other),
            })
        } else {
            invalid!(op, self, other)
        }
    }

    fn pre_op(&self, op: Op) -> Result<Value, RuntimeError> {
        Ok(match op {
            Op::Minus => Box::new(self.clone() * Into::<_>::into(-1)),
            _ => invalid!(op, self, ()),
        })
    }

    fn get_prop(&self, name: &str) -> Result<Value, RuntimeError> {
        Ok(match name {
            "mean" => Box::new(self.mean()),
            "stddev" => Box::new(self.stddev()),
            "min" => Box::new(self.min()),
            "max" => Box::new(self.max()),
            _ => invalid!("Prop", self, name),
        })
    }
}
