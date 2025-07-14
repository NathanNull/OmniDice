use super::*;
use crate::{op_list, type_init};

type_init!(FloatT, f32, "float");
impl Type for FloatT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
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
        } else if other == &IntT {
            op_list!(op => {
                Plus(l: f32, r: i32) -> (FloatT) |l,r|Ok(l + r as f32);
                Minus(l: f32, r: i32) -> (FloatT) |l,r|Ok(l - r as f32);
                Times(l: f32, r: i32) -> (FloatT) |l,r|Ok(l * r as f32);
                Divided(l: f32, r: i32) -> (FloatT) |l,r|Ok(l / r as f32);
            })
        } else {
            None
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Option<(Datatype, UnOpFn)> {
        op_list!(op => {
            Minus(v: f32) -> (FloatT) |v: f32|Ok(-v);
        })
    }
    fn is_hashable(&self) -> bool {
        true
    }
}
impl Val for f32 {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write_u32(self.to_bits());
        Ok(())
    }
}

type_init!(IntT, i32, "int");
impl Type for IntT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
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
        } else if other == &FloatT {
            op_list!(op => {
                Plus(l: i32, r: f32) -> (FloatT) |l,r|Ok(l as f32 + r);
                Minus(l: i32, r: f32) -> (FloatT) |l,r|Ok(l as f32 - r);
                Times(l: i32, r: f32) -> (FloatT) |l,r|Ok(l as f32 * r);
                Divided(l: i32, r: f32) -> (FloatT) |l,r|Ok(l as f32 / r);
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
            None
        }
    }
    fn real_pre_op_result(&self, op: Op) -> Option<(Datatype, UnOpFn)> {
        op_list!(op => {
            Minus(v: i32) -> (IntT) |v: i32|Ok(-v);
        })
    }
    fn is_hashable(&self) -> bool {
        true
    }
}
impl Val for i32 {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write_i32(*self);
        Ok(())
    }
}
