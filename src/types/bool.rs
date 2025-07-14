use super::*;
use crate::{op_list, type_init};

type_init!(BoolT, bool, "bool");
impl Type for BoolT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
        if other == &BoolT {
            // And and Or are pulled out separately so short-circuiting works
            if op == Op::And {
                fn and(l: &Expr, r: &Expr, i: &mut Interpreter) -> OpResult {
                    Ok(Box::new(
                        i.try_eval_as::<bool>(l)? && i.try_eval_as::<bool>(r)?,
                    ))
                }
                Some((self.dup(), and))
            } else if op == Op::Or {
                fn or(l: &Expr, r: &Expr, i: &mut Interpreter) -> OpResult {
                    Ok(Box::new(
                        i.try_eval_as::<bool>(l)? || i.try_eval_as::<bool>(r)?,
                    ))
                }
                Some((self.dup(), or))
            } else {
                op_list!(op => {
                    Equal(l: bool, r: bool) -> (BoolT) |l,r| Ok(l == r);
                    NotEqual(l: bool, r: bool) -> (BoolT) |l,r| Ok(l != r);
                })
            }
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
