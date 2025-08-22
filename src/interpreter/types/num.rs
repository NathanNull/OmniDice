use std::sync::LazyLock;

use super::*;
use crate::{gen_fn_map, op_list, type_init};

static F2I_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IntT),
    generic: vec![],
    owner_t: Some(Box::new(FloatT)),
});

fn f2i_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let float = params[0]
        .downcast::<f32>()
        .ok_or_else(|| RuntimeError::partial("Expected DiceT owner"))?;
    Ok(Box::new(float as i32))
}

static I2F_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(FloatT),
    generic: vec![],
    owner_t: Some(Box::new(IntT)),
});

fn i2f_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let float = params[0]
        .downcast::<i32>()
        .ok_or_else(|| RuntimeError::partial("Expected DiceT owner"))?;
    Ok(Box::new(float as f32))
}

type_init!(FloatT, f32, "float");

#[typetag::serde]
impl Type for FloatT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Result<(Datatype, BinOpFn), String> {
        if other == &FloatT {
            op_list!(op => {
                Plus(l: f32, r: f32) -> (FloatT) |l,r| Ok(l + r);
                Minus(l: f32, r: f32) -> (FloatT) |l,r| Ok(l - r);
                Times(l: f32, r: f32) -> (FloatT) |l,r| Ok(l * r);
                Divided(l: f32, r: f32) -> (FloatT) |l,r| Ok(l / r);
                Equal(l: f32, r: f32) -> (BoolT) |l,r| Ok(l == r);
                NotEqual(l: f32, r: f32) -> (BoolT) |l,r| Ok(l != r);
                Greater(l: f32, r: f32) -> (BoolT) |l,r| Ok(l > r);
                Less(l: f32, r: f32) -> (BoolT) |l,r| Ok(l < r);
                Geq(l: f32, r: f32) -> (BoolT) |l,r| Ok(l >= r);
                Leq(l: f32, r: f32) -> (BoolT) |l,r| Ok(l <= r);
            })
        } else {
            Err(format!("Can't operate {self} {op:?} {other}"))
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Result<(Datatype, UnOpFn), String> {
        op_list!(op => {
            Minus(v: f32) -> (FloatT) |v: f32|Ok(-v);
        })
    }
    fn prop_type(&self, name: &str) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name,
            self,
            "Float",
            ("to_int", F2I_SIG, f2i_fn, f2i_fn_prop)
        )
    }
    fn is_hashable(&self) -> bool {
        true
    }
}

#[typetag::serde]
impl Val for f32 {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write_u32(self.to_bits());
        Ok(())
    }
}

type_init!(IntT, i32, "int");

#[typetag::serde]
impl Type for IntT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Result<(Datatype, BinOpFn), String> {
        if other == &IntT {
            op_list!(op => {
                Plus(l: i32, r: i32) -> (IntT) |l,r|Ok(l + r);
                Minus(l: i32, r: i32) -> (IntT) |l,r|Ok(l - r);
                Times(l: i32, r: i32) -> (IntT) |l,r|Ok(l * r);
                Divided(l: i32, r: i32) -> (IntT) |l: i32,r|Ok(l.checked_div(r)
                        .ok_or_else(|| RuntimeError::partial("Attempted to divide by zero"))?,
                );
                Equal(l: i32, r: i32) -> (BoolT) |l,r|Ok(l == r);
                NotEqual(l: i32, r: i32) -> (BoolT) |l,r|Ok(l != r);
                Greater(l: i32, r: i32) -> (BoolT) |l,r|Ok(l > r);
                Less(l: i32, r: i32) -> (BoolT) |l,r|Ok(l < r);
                Geq(l: i32, r: i32) -> (BoolT) |l,r|Ok(l >= r);
                Leq(l: i32, r: i32) -> (BoolT) |l,r|Ok(l <= r);
                Mod(l: i32, r: i32) -> (IntT) |l,r|Ok(l % r);
                D(l: i32, r: i32) -> (DiceT) |l:i32 ,r: i32|Ok(Distribution::n_die_m(l as usize, r as usize));
                Range(l: i32, r: i32) -> (RangeT) |l:i32 ,r: i32|Ok(Range::new(l, r - 1));
                RangeEq(l: i32, r: i32) -> (RangeT) |l:i32 ,r: i32|Ok(Range::new(l, r));
            })
        } else if other
            == &(ArrT {
                entry: Box::new(IntT),
            })
        {
            op_list!(op => {
                D(l: i32, r: Arr) -> (DiceT) |l: i32, r: Arr| Ok(
                    Distribution::from_vec(
                        r.inner()
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
                    .multiroll(l as usize),
                );
            })
        } else if other
            == &(MapT {
                key: Box::new(IntT),
                value: Box::new(IntT),
            })
        {
            op_list!(op => {
                D(l: i32, r: Map) -> (DiceT) |l: i32, r: Map| Ok(
                    Distribution::from_weights(
                        r.inner()
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
                    .multiroll(l as usize),
                );
            })
        } else {
            Err(format!("Can't operate {self} {op:?} {other}"))
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Result<(Datatype, UnOpFn), String> {
        op_list!(op => {
            Minus(v: i32) -> (IntT) |v: i32|Ok(-v);
        })
    }
    fn prop_type(&self, name: &str) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name,
            self,
            "Int",
            ("to_float", I2F_SIG, i2f_fn, i2f_fn_prop)
        )
    }
    fn is_hashable(&self) -> bool {
        true
    }
}

#[typetag::serde]
impl Val for i32 {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write_i32(*self);
        Ok(())
    }
}
