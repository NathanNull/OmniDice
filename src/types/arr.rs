use std::sync::RwLockReadGuard;

use super::*;
use crate::{gen_fn_map, invalid, mut_type_init, type_init};

#[derive(Debug, Clone, PartialEq)]
pub struct _InnerArr {
    entry: Datatype,
    elements: Vec<Value>,
}

impl Display for _InnerArr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let len = self.elements.len();
        for (i, ele) in self.elements.iter().enumerate() {
            write!(f, "{}", ele)?;
            if i != len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

impl _InnerArr {
    fn new(elements: Vec<Value>) -> Self {
        Self {
            entry: elements
                .first()
                .map(|e| e.get_type())
                .unwrap_or_else(|| Box::new(Void)),
            elements,
        }
    }
}

mut_type_init!(Arr, _InnerArr);

impl Arr {
    pub fn new(elements: Vec<Value>) -> Self {
        Self::make(_InnerArr::new(elements))
    }
}

type_init!(ArrT, Arr, "array", (RwLockReadGuard<_InnerArr>), entry: Datatype);

fn push_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut p_iter = params.iter();
    if let Some(arr) = p_iter.next().and_then(|p| p.downcast::<ArrT>()) {
        if p_iter.next().is_some_and(|p| *p == arr.entry) && p_iter.next().is_none() {
            return Some(Box::new(Void));
        }
    }
    None
}

fn push_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Arr>() {
        if let Some(to_push) = p_iter.next() {
            assert_eq!(arr.inner().entry, to_push.get_type(), "Invalid type");
            arr.inner_mut().elements.push(to_push.clone());
            return Box::new(Void);
        }
    }
    invalid!("Call", "push", params)
}

fn pop_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<ArrT>() {
        if p_iter.next().is_none() {
            return Some(arr.entry);
        }
    }
    None
}

fn pop_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Arr>() {
        return arr
            .inner_mut()
            .elements
            .pop()
            .expect("Can't pop from empty array");
    }
    invalid!("Call", "push", params)
}

fn iter_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<ArrT>() {
        if p_iter.next().is_none() {
            return Some(Box::new(IterT { output: arr.entry }));
        }
    }
    None
}

fn iter_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Arr>() {
        if p_iter.next().is_none() {
            return Box::new(Iter {
                output: arr.inner().entry.clone(),
                next_fn: Box::new(RustFunc::new_member(
                    iter_ret_sig,
                    iter_ret_fn,
                    Box::new(Tuple::new(vec![Box::new(0), arr.dup()])),
                )),
            });
        }
    }
    invalid!("Call", "iter", params);
}

fn iter_ret_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut it = params.iter().cloned();
    let me = it.next_as::<TupT>()?;
    let arr = me.entries.0.get(1).and_then(|v| v.downcast::<ArrT>())?;
    Some(Box::new(MaybeT { output: arr.entry }))
}

fn iter_ret_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    let mut it = params.iter().cloned();
    let me = it.next_as::<Tuple>().expect("Invalid function call");
    let idx = me.inner().elements[0]
        .downcast::<i32>()
        .expect("Invalid function call") as usize;
    *me.inner_mut()
        .elements
        .get_mut(0)
        .unwrap()
        .downcast_mut::<i32>()
        .unwrap() += 1;
    let arr = me
        .inner()
        .elements
        .get(1)
        .and_then(|v| v.downcast::<Arr>())
        .expect("Invalid function call");
    return Box::new(Maybe {
        output: arr.inner().entry.clone(),
        contents: arr.inner().elements.get(idx).map(|e| e.dup()),
    });
}

gen_fn_map!(
    ARR_FNS,
    ("push", push_sig, push_fn),
    ("pop", pop_sig, pop_fn),
    ("iter", iter_sig, iter_fn)
);

impl Type for ArrT {
    fn bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if other == self {
            match op {
                Op::Plus => Some(self.dup()),
                _ => None,
            }
        } else {
            None
        }
    }

    fn prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "length" => Some(Box::new(Int)),
            n if ARR_FNS.contains_key(n) => {
                Some(Box::new(RustFuncT::new_member(ARR_FNS[n].0, self.dup())))
            }
            _ => None,
        }
    }

    fn index_type(&self, index: &Datatype) -> Option<Datatype> {
        if index == &Int {
            Some(self.entry.clone())
        } else if let Some(tup) = (index.dup() as Box<dyn Any>).downcast_ref::<TupT>() {
            if tup.entries.0.iter().all(|e| e == &Int) {
                Some(self.dup())
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl Val for Arr {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if other.get_type() == self.get_type() {
            let rhs = other
                .downcast::<Self>()
                .expect("Just checked if it was the right type");
            match op {
                Op::Plus => Box::new(Arr::new(
                    self.inner()
                        .elements
                        .iter()
                        .chain(rhs.inner().elements.iter())
                        .cloned()
                        .collect(),
                )),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other);
        }
    }

    fn get_prop(&self, name: &str) -> Value {
        match name {
            "length" => Box::new(self.inner().elements.len() as i32),
            n if ARR_FNS.contains_key(n) => {
                Box::new(RustFunc::new_member(ARR_FNS[n].0, ARR_FNS[n].1, self.dup()))
            }
            _ => invalid!("Prop", self, name),
        }
    }

    fn get_index(&self, index: Value) -> Value {
        if let Some(idx) = index.downcast::<i32>() {
            self.inner()
                .elements
                .get(idx as usize)
                .expect("Invalid index")
                .dup()
        } else if let Some(idxs) = index.downcast::<Tuple>() {
            let eles = &self.inner().elements;
            Box::new(Self::new(
                idxs.inner()
                    .elements
                    .iter()
                    .map(|idx| {
                        if let Some(i) = idx.downcast::<i32>() {
                            eles.get(i as usize).expect("Invalid index").dup()
                        } else {
                            invalid!("Index", self, idx.get_type())
                        }
                    })
                    .collect(),
            ))
        } else {
            invalid!("Index", self, index.get_type())
        }
    }

    fn set_index(&self, index: Value, value: Value) {
        if let Some(idx) = index.downcast::<i32>() {
            assert_eq!(value.get_type(), self.inner().entry);
            *self
                .inner_mut()
                .elements
                .get_mut(idx as usize)
                .expect("Invalid index") = value;
        } else if let Some(idxs) = index.downcast::<Tuple>() {
            let entry = self.inner().entry.clone();
            let vals = value.downcast::<Arr>().unwrap();
            assert_eq!(vals.inner().entry, entry);
            idxs.inner()
                .elements
                .iter()
                .zip(vals.inner().elements.iter())
                .for_each(|(idx, val)| {
                    if let Some(i) = idx.downcast::<i32>() {
                        *self
                            .inner_mut()
                            .elements
                            .get_mut(i as usize)
                            .expect("Invalid index") = val.dup();
                    } else {
                        invalid!("Index", self, idx.get_type())
                    }
                });
        } else {
            invalid!("Index", self, index.get_type())
        }
    }
}
