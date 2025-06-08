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
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        if name == "inner" {
            Some(self.ty.dup())
        } else {
            self.ty.prop_type(name)
        }
    }

    fn bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        self.ty.bin_op_result(other, op)
    }

    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        self.ty.pre_op_result(op)
    }

    fn post_op_result(&self, op: Op) -> Option<Datatype> {
        self.ty.post_op_result(op)
    }

    fn call_result(&self, params: Vec<Datatype>) -> Option<Datatype> {
        self.ty.call_result(params)
    }

    fn index_type(&self, index: &Datatype) -> Option<Datatype> {
        self.ty.index_type(index)
    }

    fn possible_call(&self) -> bool {
        self.ty.possible_call()
    }
}

impl Val for Ref {
    fn get_prop(&self, name: &str) -> Value {
        if name == "inner" {
            self.inner().val.clone()
        } else {
            self.inner().val.get_prop(name)
        }
    }

    fn set_prop(&self, prop: &str, value: Value) {
        if prop == "inner" {
            self.inner_mut().val = value
        } else {
            self.inner().val.set_prop(prop, value);
        }
    }

    fn bin_op(&self, other: &Value, op: Op) -> Value {
        self.inner().val.bin_op(other, op)
    }

    fn pre_op(&self, op: Op) -> Value {
        self.inner().val.pre_op(op)
    }

    fn post_op(&self, op: Op) -> Value {
        self.inner().val.post_op(op)
    }

    fn get_index(&self, index: Value) -> Value {
        self.inner().val.get_index(index)
    }

    fn set_index(&self, index: Value, value: Value) {
        self.inner().val.set_index(index, value);
    }

    fn call(&self, params: Vec<Value>, interpreter: &mut Interpreter) -> Value {
        self.inner().val.call(params, interpreter)
    }
}
