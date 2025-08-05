use std::sync::LazyLock;

use super::*;
use crate::{gen_fn_map, op_list, type_init};

static LENGTH_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IntT),
    generic: vec![],
    owner_t: Some(Box::new(StringT)),
});

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

gen_fn_map!(STRING_FNS, ("length", LENGTH_SIG, length_fn, length_prop));

type_init!(StringT, String, "string");
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

    fn real_prop_type(&self, name: &str) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        match name {
            n if STRING_FNS.contains_key(n) => {
                let f = &STRING_FNS[n];
                Ok((
                    Box::new(f.0.clone().with_owner(self.dup()).map_err(|e|e.info())?),
                    Some(f.2),
                    None,
                ))
            }
            _ => Err(format!("Can't get property {name} of {self}")),
        }
    }

    fn is_hashable(&self) -> bool {
        true
    }
}

impl Val for String {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write(self.as_bytes());
        Ok(())
    }
}
