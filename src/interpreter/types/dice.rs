use std::sync::LazyLock;

use super::*;
use crate::{gen_fn_map, op_list, type_init};

static MEAN_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(FloatT),
    generic: vec![],
    owner_t: Some(Box::new(DiceT)),
});

fn mean_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let dice = params[0]
        .downcast::<Distribution>()
        .ok_or_else(|| RuntimeError::partial("Expected DiceT owner"))?;
    Ok(Box::new(dice.mean()))
}

static STDDEV_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(FloatT),
    generic: vec![],
    owner_t: Some(Box::new(DiceT)),
});

fn stddev_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let dice = params[0]
        .downcast::<Distribution>()
        .ok_or_else(|| RuntimeError::partial("Expected DiceT owner"))?;
    Ok(Box::new(dice.stddev()))
}

static MIN_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IntT),
    generic: vec![],
    owner_t: Some(Box::new(DiceT)),
});

fn min_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let dice = params[0]
        .downcast::<Distribution>()
        .ok_or_else(|| RuntimeError::partial("Expected DiceT owner"))?;
    Ok(Box::new(dice.min()))
}

static MAX_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IntT),
    generic: vec![],
    owner_t: Some(Box::new(DiceT)),
});

fn max_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let dice = params[0]
        .downcast::<Distribution>()
        .ok_or_else(|| RuntimeError::partial("Expected DiceT owner"))?;
    Ok(Box::new(dice.max()))
}

type_init!(DiceT, Distribution, "dice");

#[cfg_attr(feature="serde", typetag::serde)]
impl Type for DiceT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Result<(Datatype, BinOpFn), String> {
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
            Err(format!("Can't operate {self} {op:?} {other}"))
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Result<(Datatype, UnOpFn), String> {
        op_list!(op => {
            Minus(v: Distribution) -> (DiceT) |v|Ok(v * (-1).into());
        })
    }
    fn real_prop_type(&self, name: &str) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name, self, "Dice", 
            ("mean", MEAN_SIG, mean_fn, mean_prop),
            ("stddev", STDDEV_SIG, stddev_fn, stddev_prop),
            ("max", MAX_SIG, max_fn, max_prop),
            ("min", MIN_SIG, min_fn, min_prop),
        )
    }
}

#[cfg_attr(feature="serde", typetag::serde)]
impl Val for Distribution {}
