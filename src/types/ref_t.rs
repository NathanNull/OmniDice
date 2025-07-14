use std::sync::RwLockReadGuard;

use crate::{mut_type_init, type_init};

use super::*;

#[derive(Debug, Clone)]
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

impl Type for RefT {
    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        if name == "inner" {
            Some(self.ty.dup())
        } else {
            self.ty.prop_type(name)
        }
    }

    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<(Datatype, BinOpFn)> {
        self.ty.bin_op_result(other, op)
    }

    fn real_pre_op_result(&self, op: Op) -> Option<(Datatype, UnOpFn)> {
        self.ty.pre_op_result(op)
    }

    fn real_post_op_result(&self, op: Op) -> Option<(Datatype, UnOpFn)> {
        self.ty.post_op_result(op)
    }

    fn real_call_result(
        &self,
        params: Vec<Datatype>,
        expected_output: Option<Datatype>,
    ) -> Option<Datatype> {
        self.ty.call_result(params, expected_output)
    }

    fn real_index_type(&self, index: &Datatype) -> Option<Datatype> {
        self.ty.index_type(index)
    }

    fn possible_call(&self) -> bool {
        self.ty.possible_call()
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(Box::new(Self {
            ty: self.ty.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        self.ty.try_match(&other.downcast::<Self>()?.ty)
    }
    fn get_generics(&self) -> Vec<String> {
        self.ty.get_generics()
    }
    fn is_hashable(&self) -> bool {
        self.ty.is_hashable()
    }
}

impl Val for Ref {
    fn get_prop(&self, name: &str) -> Result<Value, RuntimeError> {
        if name == "inner" {
            Ok(self.inner().val.clone())
        } else {
            self.inner().val.get_prop(name)
        }
    }

    fn set_prop(&self, prop: &str, value: Value) -> Result<(), RuntimeError> {
        if prop == "inner" {
            self.inner_mut().val = value;
            Ok(())
        } else {
            self.inner().val.set_prop(prop, value)
        }
    }

    fn get_index(&self, index: Value) -> Result<Value, RuntimeError> {
        self.inner().val.get_index(index)
    }

    fn set_index(&self, index: Value, value: Value) -> Result<(), RuntimeError> {
        self.inner().val.set_index(index, value)
    }

    fn call(
        &self,
        params: Vec<Value>,
        interpreter: &mut Interpreter,
        expected_output: Option<Datatype>,
    ) -> Result<Value, RuntimeError> {
        self.inner().val.call(params, interpreter, expected_output)
    }
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        self.inner().val.as_ref().hash(h)
    }
}
