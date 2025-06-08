use std::sync::MutexGuard;

use crate::{invalid, mut_type_init, type_init};

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

type_init!(RefT, Ref, "ref", (MutexGuard<_InnerRef>), ty: Datatype);

impl Type for RefT {
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        if name == "inner" {
            Some(self.ty.dup())
        } else {
            None
        }
    }
}

impl Val for Ref {
    fn get_prop(&self, name: &str) -> Value {
        if name == "inner" {
            self.inner().val.dup()
        } else {
            invalid!("Prop", self, ())
        }
    }

    fn set_prop(&self, prop: &str, value: Value) {
        if prop == "inner" {
            self.inner().val = value
        } else {
            invalid!("Prop (set)", self, ())
        }
    }
}
