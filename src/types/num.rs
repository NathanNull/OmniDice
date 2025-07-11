use super::*;
use crate::{invalid, type_init};

type_init!(FloatT, f32, "float");
impl Type for FloatT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if (other == &FloatT || other == &IntT) && NUM_OPS.contains(&op) {
            Some(Box::new(FloatT))
        } else if other == &FloatT && ORD_OPS.contains(&op) {
            Some(Box::new(BoolT))
        } else {
            None
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(Box::new(FloatT)),
            _ => None,
        }
    }
    fn is_hashable(&self) -> bool {
        true
    }
}
impl Val for f32 {
    fn bin_op(&self, other: &Value, op: Op) -> Result<Value, RuntimeError> {
        if let Some(rhs) = other.downcast::<f32>() {
            Ok(match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(self / rhs),
                Op::Equal => Box::new(self == &rhs),
                Op::NotEqual => Box::new(self != &rhs),
                Op::Greater => Box::new(self > &rhs),
                Op::Less => Box::new(self < &rhs),
                Op::Geq => Box::new(self >= &rhs),
                Op::Leq => Box::new(self <= &rhs),
                _ => invalid!(op, self, other),
            })
        } else if let Some(rhs) = other.downcast::<i32>().map(|i| i as f32) {
            Ok(match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(self / rhs),
                _ => invalid!(op, self, other),
            })
        } else {
            invalid!(op, self, other)
        }
    }
    fn pre_op(&self, op: Op) -> Result<Value, RuntimeError> {
        Ok(match op {
            Op::Minus => Box::new(-self),
            _ => invalid!(op, self, ()),
        })
    }
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write_u32(self.to_bits());
        Ok(())
    }
}

type_init!(IntT, i32, "int");
impl Type for IntT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if other == &IntT {
            if NUM_OPS.contains(&op) || op == Op::Mod {
                Some(Box::new(IntT))
            } else if ORD_OPS.contains(&op) {
                Some(Box::new(BoolT))
            } else if other == &IntT && op == Op::D {
                Some(Box::new(DiceT))
            } else if other == &IntT && (op == Op::Range || op == Op::RangeEq) {
                Some(Box::new(RangeT))
            } else {
                None
            }
        } else if other == &FloatT && NUM_OPS.contains(&op) {
            Some(Box::new(FloatT))
        } else if other
            == &(ArrT {
                entry: Box::new(IntT),
            })
            || other
                == &(MapT {
                    key: Box::new(IntT),
                    value: Box::new(IntT),
                })
        {
            Some(Box::new(DiceT))
        } else {
            None
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Option<Datatype> {
        match op {
            Op::Minus => Some(Box::new(IntT)),
            _ => None,
        }
    }
    fn is_hashable(&self) -> bool {
        true
    }
}
impl Val for i32 {
    fn bin_op(&self, other: &Value, op: Op) -> Result<Value, RuntimeError> {
        if let Some(rhs) = other.downcast::<i32>() {
            Ok(match op {
                Op::Plus => Box::new(self + rhs),
                Op::Minus => Box::new(self - rhs),
                Op::Times => Box::new(self * rhs),
                Op::Divided => Box::new(
                    self.checked_div(rhs)
                        .ok_or_else(|| RuntimeError::partial("Attempted to divide by zero"))?,
                ),
                Op::Equal => Box::new(self == &rhs),
                Op::NotEqual => Box::new(self != &rhs),
                Op::Greater => Box::new(self > &rhs),
                Op::Less => Box::new(self < &rhs),
                Op::Geq => Box::new(self >= &rhs),
                Op::Leq => Box::new(self <= &rhs),
                Op::Mod => Box::new(self % rhs),
                Op::D => Box::new(Distribution::n_die_m(*self as usize, rhs as usize)),
                Op::Range => Box::new(Range::new(*self, rhs - 1)),
                Op::RangeEq => Box::new(Range::new(*self, rhs)),
                _ => invalid!(op, self, other),
            })
        } else if let Some(rhs) = other.downcast::<f32>() {
            Ok(match op {
                Op::Plus => Box::new(*self as f32 + rhs),
                Op::Minus => Box::new(*self as f32 - rhs),
                Op::Times => Box::new(*self as f32 * rhs),
                Op::Divided => Box::new(*self as f32 / rhs),
                _ => invalid!(op, self, other),
            })
        } else if let Some(rhs) = other.downcast::<Arr>() {
            Ok(Box::new(
                Distribution::from_vec(
                    rhs.inner()
                        .elements
                        .iter()
                        .map(|e| {
                            e.downcast::<i32>().ok_or_else(|| {
                                RuntimeError::partial(
                                    "Dice constructor array elements were not integers",
                                )
                            })
                        })
                        .collect::<Result<_, _>>()?,
                )
                .multiroll(*self as usize),
            ))
        } else if let Some(rhs) = other.downcast::<Map>() {
            Ok(Box::new(
                Distribution::from_weights(
                    rhs.inner()
                        .elements
                        .iter()
                        .map(|(k, v)| {
                            Ok((
                                k.downcast::<i32>().ok_or_else(|| {
                                    RuntimeError::partial(
                                        "Dice constructor map keys were not integers",
                                    )
                                })?,
                                v.downcast::<i32>().ok_or_else(|| {
                                    RuntimeError::partial(
                                        "Dice constructor map values were not integers",
                                    )
                                })? as usize,
                            ))
                        })
                        .collect::<Result<_, _>>()?,
                )
                .multiroll(*self as usize),
            ))
        } else {
            invalid!(op, self, other)
        }
    }

    fn pre_op(&self, op: Op) -> Result<Value, RuntimeError> {
        match op {
            Op::Minus => Ok(Box::new(-self)),
            _ => invalid!(op, self, ()),
        }
    }
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write_i32(*self);
        Ok(())
    }
}
