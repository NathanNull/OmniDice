use crate::{invalid, type_init};
use super::*;

type_init!(Float, f32, "float");
impl Type for Float {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if (other == Float || other == Int) && NUM_OPS.contains(&op) {
            Some(Box::new(Float))
        } else if other == Float && ORD_OPS.contains(&op) {
            Some(Box::new(Bool))
        } else {
            None
        }
    }
    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(Box::new(Float)),
            _ => None,
        }
    }
}
impl Val for f32 {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.try_downcast_ref::<f32>() {
            match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(self / rhs),
                Op::Equal => Box::new(self == rhs),
                Op::NotEqual => Box::new(self != rhs),
                Op::Greater => Box::new(self > rhs),
                Op::Less => Box::new(self < rhs),
                Op::Geq => Box::new(self >= rhs),
                Op::Leq => Box::new(self <= rhs),
                _ => invalid!(op, self, other),
            }
        } else if let Some(rhs) = other.try_downcast_ref::<i32>().map(|i| *i as f32) {
            match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(self / rhs),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other)
        }
    }
    fn pre_op(&self, op: Op) -> Value {
        match op {
            Op::Minus => Box::new(-self),
            _ => invalid!(op, self, ()),
        }
    }
}

type_init!(Int, i32, "int");
impl Type for Int {
    fn bin_op_result(&self, other: Datatype, op: Op) -> Option<Datatype> {
        if other == Int {
            if NUM_OPS.contains(&op) || op == Op::Mod {
                Some(Box::new(Int))
            } else if ORD_OPS.contains(&op) {
                Some(Box::new(Bool))
            } else if other == Int && op == Op::D {
                Some(Box::new(Dice))
            } else {
                None
            }
        } else {
            None
        }
    }
    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(Box::new(Float)),
            _ => None,
        }
    }
}
impl Val for i32 {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if let Some(rhs) = other.try_downcast_ref::<i32>() {
            match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(self / rhs),
                Op::Equal => Box::new(self == rhs),
                Op::NotEqual => Box::new(self != rhs),
                Op::Greater => Box::new(self > rhs),
                Op::Less => Box::new(self < rhs),
                Op::Geq => Box::new(self >= rhs),
                Op::Leq => Box::new(self <= rhs),
                Op::Mod => Box::new(self % rhs),
                Op::D => Box::new(Distribution::n_die_m(*self as usize, *rhs as usize)),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other)
        }
    }

    fn pre_op(&self, op: Op) -> Value {
        match op {
            Op::Minus => Box::new(-self),
            _ => invalid!(op, self, ()),
        }
    }
}