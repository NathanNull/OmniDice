use std::sync::LazyLock;

use super::*;
use crate::{gen_fn_map, od_typedef, op_list, type_init};

static LENGTH_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func() -> IntT owner StringT}));

fn length_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let string = params[0]
        .downcast::<String>()
        .ok_or_else(|| RuntimeError::partial("strlen owner isn't string"))?;
    Ok(Box::new(string.len() as i32))
}

type_init!(StringT, String, "string");

#[cfg_attr(feature = "serde", typetag::serde)]
impl Type for StringT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Result<(Datatype, BinOpFn), String> {
        if other == &StringT {
            op_list!(op => {
                Plus(l: String, r: String) -> (StringT) |l,r: String|Ok(l+r.as_str());
                Equal(l: String, r: String) -> (BoolT) |l,r|Ok(l==r);
                NotEqual(l: String, r: String) -> (BoolT) |l,r|Ok(l!=r);
            })
        } else {
            Err(format!("Can't operate {self} {op:?} {other}"))
        }
    }

    fn real_prop_type(
        &self,
        name: &str,
    ) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name,
            self,
            "String",
            ("length", LENGTH_SIG, length_fn, length_prop)
        )
    }

    fn real_index_type(
        &self,
        index: &Datatype,
    ) -> Result<(Datatype, Option<BinOpFn>, Option<SetAtFn>), String> {
        if index == &IntT {
            fn get_fn(me: &Expr, idx: &Expr, i: &mut Interpreter) -> OpResult {
                let idx = i.try_eval_as::<i32>(idx)?;
                Ok(Box::new(
                    i.try_eval_as::<String>(me)?
                        .get(idx as usize..=idx as usize)
                        .ok_or_else(|| {
                            RuntimeError::partial(&format!("String can't be indexed at {idx}"))
                        })?
                        .to_string(),
                ))
            }
            Ok((self.dup(), Some(get_fn), None))
        } else if index == &RangeT {
            fn get_fn(me: &Expr, idx: &Expr, i: &mut Interpreter) -> OpResult {
                let range = i.try_eval_as::<Range>(idx)?;
                let me = i.try_eval_as::<String>(me)?;
                Ok(Box::new(
                    me.get((range.inner().curr + 1) as usize..=range.inner().last as usize)
                        .ok_or_else(|| {
                            RuntimeError::partial(&format!("String can't be indexed over {range}"))
                        })?
                        .to_string(),
                ))
            }
            Ok((self.dup(), Some(get_fn), None))
        } else {
            Err(format!("Can't index {self} with {index}"))
        }
    }

    fn is_hashable(&self) -> bool {
        true
    }
}

#[cfg_attr(feature = "serde", typetag::serde)]
impl Val for String {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write(self.as_bytes());
        Ok(())
    }
}
