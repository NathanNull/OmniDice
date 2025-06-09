use crate::{invalid, type_init};

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncPointer<I, O>(fn(I) -> O);

impl<I, O> Display for FuncPointer<I, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "???")
    }
}

#[derive(Debug, Clone)]
pub struct MaybeOwnerTy(Option<Datatype>);

impl Display for MaybeOwnerTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ty) = self.0.as_ref() {
            write!(f, "{ty}")
        } else {
            write!(f, "unowned")
        }
    }
}


#[derive(Clone)]
pub struct RustFunc {
    pub signature: FuncPointer<Vec<Datatype>, Option<Datatype>>,
    pub owner_ty: MaybeOwnerTy,
    pub contents: fn(Vec<Value>) -> Value,
    pub owner: Option<Value>,
}

impl Debug for RustFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RustFunc({:?})", self.contents)
    }
}

impl Display for RustFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "foreign function")
    }
}

impl PartialEq for RustFunc {
    fn eq(&self, other: &Self) -> bool {
        self.get_type() == other.get_type()
    }
}

impl RustFunc {
    fn new(
        signature: fn(Vec<Datatype>) -> Option<Datatype>,
        contents: fn(Vec<Value>) -> Value,
        owner: Option<Value>,
    ) -> Self {
        Self {
            signature: FuncPointer(signature),
            contents,
            owner_ty: MaybeOwnerTy(owner.as_ref().map(|o| o.get_type())),
            owner,
        }
    }

    pub fn new_member(
        signature: fn(Vec<Datatype>) -> Option<Datatype>,
        contents: fn(Vec<Value>) -> Value,
        owner: Value,
    ) -> Self {
        Self::new(signature, contents, Some(owner))
    }

    pub fn new_const(
        signature: fn(Vec<Datatype>) -> Option<Datatype>,
        contents: fn(Vec<Value>) -> Value,
    ) -> Self {
        Self::new(signature, contents, None)
    }
}

type_init!(RustFuncT, RustFunc, "func", owner_ty: MaybeOwnerTy, signature: FuncPointer<Vec<Datatype>,Option<Datatype>>);

impl RustFuncT {
    fn new(
        signature: fn(Vec<Datatype>) -> Option<Datatype>,
        owner: Option<Datatype>,
    ) -> Self {
        Self {
            signature: FuncPointer(signature),
            owner_ty: MaybeOwnerTy(owner),
        }
    }

    pub fn new_member(
        signature: fn(Vec<Datatype>) -> Option<Datatype>,
        owner: Datatype,
    ) -> Self {
        Self::new(signature, Some(owner))
    }
}

// TODO: idk if this needed anything but if it does, add it
impl Type for RustFuncT {
    fn call_result(&self, mut params: Vec<Datatype>) -> Option<Datatype> {
        if let MaybeOwnerTy(Some(ty)) = self.owner_ty.clone() {
            params = [ty].into_iter().chain(params.into_iter()).collect()
        }
        (self.signature.0)(params)
    }
    fn possible_call(&self) -> bool {
        true
    }

    fn bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if op == Op::Plus && other.possible_call() {
            Some(Box::new(FuncSumT {
                f_types: TypeList(vec![self.dup(), other.dup()]),
            }))
        } else {
            None
        }
    }
}
impl Val for RustFunc {
    fn call(&self, mut params: Vec<Value>, _interpreter: &mut Interpreter) -> Value {
        if let Some(owner) = self.owner.clone() {
            params = [owner].into_iter().chain(params.into_iter()).collect()
        }
        (self.contents)(params)
    }
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if op == Op::Plus && other.get_type().possible_call() {
            Box::new(FuncSum::new(vec![self.dup(), other.dup()]))
        } else {
            invalid!(op, self, other);
        }
    }
}
