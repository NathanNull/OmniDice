use crate::{invalid, type_init};
use super::*;

type_init!(Dice, Distribution, "dice");
impl Type for Dice {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if (other == Int || other == Dice) && NUM_OPS.contains(&op) {
            Some(Box::new(Dice))
        } else {
            None
        }
    }
    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(Box::new(Dice)),
            _ => None,
        }
    }
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "mean" => Some(Box::new(Float)),
            "max" | "min" => Some(Box::new(Int)),
            _ => None,
        }
    }
}
impl Val for Distribution {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.try_downcast_ref::<i32>() {
            match op {
                Op::Plus => Box::new(self.clone() + rhs.into()),
                Op::Minus => Box::new(self.clone() - rhs.into()),
                Op::Times => Box::new(self.clone() * rhs.into()),
                Op::Divided => Box::new(self.clone() / rhs.into()),
                _ => invalid!(op, self, other),
            }
        } else if let Some(rhs) = other.try_downcast_ref::<Distribution>() {
            match op {
                Op::Plus => Box::new(self.clone() + rhs.clone()),
                Op::Minus => Box::new(self.clone() - rhs.clone()),
                Op::Times => Box::new(self.clone() * rhs.clone()),
                Op::Divided => Box::new(self.clone() / rhs.clone()),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other)
        }
    }

    fn pre_op(&self, op: Op) -> Value {
        match op {
            Op::Minus => Box::new(self.clone() * (&-1).into()),
            _ => invalid!(op, self, ()),
        }
    }

    fn get_prop(&self, name: &str) -> Value {
        match name {
            "mean" => Box::new(self.mean()),
            "min" => Box::new(self.min()),
            "max" => Box::new(self.max()),
            _ => invalid!("Prop", self, name),
        }
    }
}
