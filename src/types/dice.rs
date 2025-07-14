use super::*;
use crate::{op_list, type_init};

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
    fn real_prop_type(&self, name: &str) -> Option<(Datatype, Option<UnOpFn>, Option<SetFn>)> {
        fn get_mean(me: &Expr, i: &mut Interpreter) -> OpResult {
            Ok(Box::new(i.try_eval_as::<Distribution>(me)?.mean()))
        }
        fn get_stddev(me: &Expr, i: &mut Interpreter) -> OpResult {
            Ok(Box::new(i.try_eval_as::<Distribution>(me)?.stddev()))
        }
        fn get_max(me: &Expr, i: &mut Interpreter) -> OpResult {
            Ok(Box::new(i.try_eval_as::<Distribution>(me)?.max()))
        }
        fn get_min(me: &Expr, i: &mut Interpreter) -> OpResult {
            Ok(Box::new(i.try_eval_as::<Distribution>(me)?.min()))
        }
        match name {
            "mean" => Some((Box::new(FloatT), Some(get_mean), None)),
            "stddev" => Some((Box::new(FloatT), Some(get_stddev), None)),
            "max" => Some((Box::new(IntT), Some(get_max), None)),
            "min" => Some((Box::new(IntT), Some(get_min), None)),
            _ => None,
        }
    }
}
impl Val for Distribution {}
