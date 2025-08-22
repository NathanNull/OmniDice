use std::sync::RwLockReadGuard;

use crate::{mut_type_init, type_init};

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct _InnerRef {
    ty: Datatype,
    val: Value,
}

impl PartialEq for _InnerRef {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty && &self.val == &other.val
    }
}

impl Display for _InnerRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R({})", self.val)
    }
}

impl _InnerRef {
    fn new(val: Value) -> Self {
        Self {
            ty: val.get_type(),
            val,
        }
    }
}

mut_type_init!(Ref, _InnerRef);

impl Ref {
    pub fn new(val: Value) -> Self {
        Self::make(_InnerRef::new(val))
    }
}

type_init!(RefT, Ref, "ref", (RwLockReadGuard<_InnerRef>), ty: Datatype);

#[typetag::serde]
impl Type for RefT {
    fn real_prop_type(
        &self,
        name: &str,
    ) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        if name == "inner" {
            fn get_inner(me: &Expr, i: &mut Interpreter) -> OpResult {
                Ok(i.try_eval_as::<Ref>(me)?.inner().val.clone())
            }
            fn set_inner(me: &Expr, val: &Expr, i: &mut Interpreter) -> VoidResult {
                i.try_eval_as::<Ref>(me)?.inner_mut().val = i.eval_expr(val)?;
                Ok(())
            }
            Ok((self.ty.dup(), Some(get_inner), Some(set_inner)))
        } else {
            self.ty.prop_type(name)
        }
    }

    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Result<(Datatype, BinOpFn), String> {
        self.ty.bin_op_result(other, op)
    }

    fn real_pre_op_result(&self, op: Op) -> Result<(Datatype, UnOpFn), String> {
        self.ty.pre_op_result(op)
    }

    fn real_post_op_result(&self, op: Op) -> Result<(Datatype, UnOpFn), String> {
        self.ty.post_op_result(op)
    }

    fn real_call_result(
        &self,
        params: Vec<Datatype>,
        expected_output: Option<Datatype>,
    ) -> Result<(Datatype, CallFn), String> {
        self.ty.call_result(params, expected_output)
    }

    fn real_index_type(&self, index: &Datatype) -> Result<(Datatype, Option<BinOpFn>, Option<SetAtFn>), String> {
        self.ty.index_type(index)
    }

    fn possible_call(&self) -> bool {
        self.ty.possible_call()
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Datatype, String> {
        Ok(Box::new(Self {
            ty: self.ty.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
        self.ty.try_match(
            &other
                .downcast::<Self>()
                .ok_or_else(|| format!("Can't match {self} with {other}"))?
                .ty,
        )
    }
    fn get_generics(&self) -> Vec<String> {
        self.ty.get_generics()
    }
    fn is_hashable(&self) -> bool {
        self.ty.is_hashable()
    }
}

#[typetag::serde]
impl Val for Ref {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        self.inner().val.as_ref().hash(h)
    }
}
