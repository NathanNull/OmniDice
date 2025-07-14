use super::*;
use crate::{invalid, op_list, type_init};

type_init!(DiceT, Distribution, "dice");
impl Type for DiceT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
        if other == &IntT {
            op_list!(op => {
                Plus(l: Distribution, r: i32) -> (DiceT) |l,r: i32|Ok(l + r.into());
                Minus(l: Distribution, r: i32) -> (DiceT) |l,r: i32|Ok(l - r.into());
                Times(l: Distribution, r: i32) -> (DiceT) |l,r: i32|Ok(l * r.into());
                Divided(l: Distribution, r: i32) -> (DiceT) |l,r: i32|Ok(l / r.into());
            })
        } else if other == &DiceT {
            op_list!(op => {
                Plus(l: Distribution, r: Distribution) -> (DiceT) |l,r|Ok(l + r);
                Minus(l: Distribution, r: Distribution) -> (DiceT) |l,r|Ok(l - r);
                Times(l: Distribution, r: Distribution) -> (DiceT) |l,r|Ok(l * r);
                Divided(l: Distribution, r: Distribution) -> (DiceT) |l,r|Ok(l / r);
            })
        } else {
            None
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Option<(Datatype, UnOpFn)> {
        op_list!(op => {
            Minus(v: Distribution) -> (DiceT) |v|Ok(v * (-1).into());
        })
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
