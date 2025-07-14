use super::*;
use crate::{op_list, type_init};

type_init!(BoolT, bool, "bool");
impl Type for BoolT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
        if other == &BoolT {
            // TODO: consider not using the macro here so that I can allow short-circuiting
            // maybe just pull out and/or
            op_list!(op => {
                And(l: bool, r: bool) -> (BoolT) |l,r| Ok(l && r);
                Or(l: bool, r: bool) -> (BoolT) |l,r| Ok(l || r);
                Equal(l: bool, r: bool) -> (BoolT) |l,r| Ok(l == r);
                NotEqual(l: bool, r: bool) -> (BoolT) |l,r| Ok(l != r);
            })
        } else {
            None
        }
    }

    fn real_pre_op_result(&self, op: Op) -> Option<(Datatype, UnOpFn)> {
        op_list!(op => {
            Not(v: bool) -> (BoolT) |v: bool| Ok(!v);
        })
    }
    fn is_hashable(&self) -> bool {
        true
    }
}
impl Val for bool {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write_u8(*self as u8);
        Ok(())
    }
}
