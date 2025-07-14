use std::sync::LazyLock;

use super::*;
use crate::{invalid, op_list, type_init};

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

type_init!(StringT, String, "string");
impl Type for StringT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
        if other == &StringT {
            op_list!(op => {
                Plus(l: String, r: String) -> (StringT) |l,r: String|Ok(l+r.as_str());
                Equal(l: String, r: String) -> (BoolT) |l,r|Ok(l==r);
                NotEqual(l: String, r: String) -> (BoolT) |l,r|Ok(l!=r);
            })
        } else {
            None
        }
    }

    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "length" => Some(LENGTH_SIG.dup()),
            _ => None,
        }
    }

    fn is_hashable(&self) -> bool {
        true
    }
}

impl Val for String {
    fn get_prop(&self, name: &str) -> Result<Value, RuntimeError> {
        Ok(match name {
            "length" => Box::new(LENGTH_SIG.clone().make_rust_member(length_fn, self.dup())?),
            _ => invalid!("Prop", self, name),
        })
    }

    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write(self.as_bytes());
        Ok(())
    }
}
