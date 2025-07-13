use std::sync::{LazyLock, RwLockReadGuard};

use super::*;
use crate::{gen_fn_map, invalid, mut_type_init, type_init};

#[derive(Clone, PartialEq)]
pub struct _InnerArr {
    entry: Datatype,
    pub elements: Vec<Value>,
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

impl Debug for _InnerArr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let len = self.elements.len();
        for (i, ele) in self.elements.iter().enumerate() {
            write!(f, "{:?}", ele)?;
            if i != len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

impl _InnerArr {
    fn new(elements: Vec<Value>, ty: Datatype) -> Self {
        Self {
            entry: ty,
            elements,
        }
    }
}

mut_type_init!(Arr, _InnerArr);

impl Arr {
    pub fn new(elements: Vec<Value>, ty: Datatype) -> Self {
        Self::make(_InnerArr::new(elements, ty))
    }
}

type_init!(ArrT, Arr, "array", (RwLockReadGuard<_InnerArr>), entry: Datatype);

static TV1_NAME: &str = "__T";
static TV1: LazyLock<Datatype> = LazyLock::new(|| Box::new(TypeVar::Var(TV1_NAME.to_string())));

static PUSH_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV1.clone()],
    output: Box::new(Void),
    generic: vec![TV1_NAME.to_string()],
    owner_t: Some(Box::new(ArrT { entry: TV1.clone() })),
});

fn push_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Arr>() {
        if let Some(to_push) = p_iter.next() {
            if arr.inner().entry != to_push.get_type() {
                return Err(RuntimeError::partial("pushed element is the wrong type"));
            }
            arr.inner_mut().elements.push(to_push.clone());
            return Ok(Box::new(Void));
        }
    }
    invalid!("Call", "push", params)
}

static POP_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MaybeT {
        output: TV1.clone(),
    }),
    generic: vec![TV1_NAME.to_string()],
    owner_t: Some(Box::new(ArrT { entry: TV1.clone() })),
});

fn pop_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Arr>() {
        let contents = arr.inner_mut().elements.pop();
        let output = arr.inner().entry.clone();
        return Ok(Box::new(Maybe { output, contents }));
    }
    invalid!("Call", "push", params)
}

// fn iter_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     let mut p_iter = params.iter().cloned();
//     if let Some(arr) = p_iter.next_as::<ArrT>() {
//         if p_iter.next().is_none() {
//             return Some(Box::new(IterT { output: arr.entry }));
//         }
//     }
//     None
// }

static ITER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IterT {
        output: TV1.clone(),
    }),
    generic: vec![TV1_NAME.to_string()],
    owner_t: Some(Box::new(ArrT { entry: TV1.clone() })),
});

pub static ITER_RET_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MaybeT {
        output: TV1.clone(),
    }),
    generic: vec![TV1_NAME.to_string()],
    owner_t: Some(Box::new(TupT {
        entries: vec![Box::new(IntT), Box::new(ArrT { entry: TV1.clone() })],
    })),
});

fn iter_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Arr>() {
        if p_iter.next().is_none() {
            return Ok(Box::new(Iter {
                output: arr.inner().entry.clone(),
                next_fn: Box::new(ITER_RET_SIG.clone().make_rust_member(
                    iter_ret_fn,
                    Box::new(Tuple::new(vec![Box::new(0), arr.dup()])),
                )?),
            }));
        }
    }
    invalid!("Call", "iter", params);
}

pub fn iter_ret_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    let me = it
        .next_as::<Tuple>()
        .ok_or_else(|| RuntimeError::partial("arriter owner isn't tuple"))?;
    let idx = me.inner().elements[0]
        .downcast::<i32>()
        .ok_or_else(|| RuntimeError::partial("arriter owner first isn't an index"))?
        as usize;
    *me.inner_mut().elements[0]
        .downcast_mut::<i32>()
        .ok_or_else(|| RuntimeError::partial("arriter owner first isn't an index"))? += 1;
    let arr = me
        .inner()
        .elements
        .get(1)
        .and_then(|v| v.downcast::<Arr>())
        .ok_or_else(|| RuntimeError::partial("arriter owner second isn't an array"))?;
    return Ok(Box::new(Maybe {
        output: arr.inner().entry.clone(),
        contents: arr.inner().elements.get(idx).map(|e| e.dup()),
    }));
}

static LENGTH_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IntT),
    generic: vec![TV1_NAME.to_string()],
    owner_t: Some(Box::new(ArrT { entry: TV1.clone() })),
});

fn length_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let arr = params[0]
        .downcast::<Arr>()
        .ok_or_else(|| RuntimeError::partial("arrlen owner isn't array"))?;
    Ok(Box::new(arr.inner().elements.len() as i32))
}

gen_fn_map!(
    ARR_FNS,
    ("push", PUSH_SIG, push_fn),
    ("pop", POP_SIG, pop_fn),
    ("iter", ITER_SIG, iter_fn),
    ("length", LENGTH_SIG, length_fn),
);

impl Type for ArrT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if other == self {
            match op {
                Op::Plus => Some(self.dup()),
                _ => None,
            }
        } else {
            None
        }
    }

    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            n if ARR_FNS.contains_key(n) => {
                Some(ARR_FNS[n].0.clone().with_owner(self.dup()).ok()?.dup())
            }
            _ => None,
        }
    }

    fn real_index_type(&self, index: &Datatype) -> Option<Datatype> {
        if index == &IntT {
            Some(self.entry.clone())
        } else if let Some(tup) = (index.dup() as Box<dyn Any>).downcast_ref::<TupT>() {
            if tup.entries.iter().all(|e| e == &IntT) {
                Some(self.dup())
            } else {
                None
            }
        } else if index == &RangeT {
            Some(self.dup())
        } else {
            None
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(Box::new(Self {
            entry: self.entry.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        self.entry.try_match(&other.downcast::<Self>()?.entry)
    }
    fn get_generics(&self) -> Vec<String> {
        self.entry.get_generics()
    }
    fn is_hashable(&self) -> bool {
        self.entry.is_hashable()
    }
}

impl Val for Arr {
    fn bin_op(&self, other: &Value, op: Op) -> Result<Value, RuntimeError> {
        if other.get_type() == self.get_type() {
            let rhs = other.downcast::<Self>().ok_or_else(|| {
                RuntimeError::partial(
                    "arrbinop lhs and rhs aren't the same, despite just testing for that",
                )
            })?;
            match op {
                Op::Plus => Ok(Box::new(Self::new(
                    self.inner()
                        .elements
                        .iter()
                        .chain(rhs.inner().elements.iter())
                        .cloned()
                        .collect(),
                    self.inner().entry.clone(),
                ))),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other);
        }
    }

    fn get_prop(&self, name: &str) -> Result<Value, RuntimeError> {
        match name {
            n if ARR_FNS.contains_key(n) => Ok(Box::new(
                ARR_FNS[n]
                    .0
                    .clone()
                    .make_rust_member(ARR_FNS[n].1, self.dup())?,
            )),
            _ => invalid!("Prop", self, name),
        }
    }

    fn get_index(&self, index: Value) -> Result<Value, RuntimeError> {
        if let Some(idx) = index.downcast::<i32>() {
            Ok(self
                .inner()
                .elements
                .get(idx as usize)
                .ok_or_else(|| {
                    RuntimeError::partial(&format!("Array has no element at index {idx}"))
                })?
                .dup())
        } else if let Some(idxs) = index.downcast::<Tuple>() {
            let eles = &self.inner().elements;
            Ok(Box::new(Self::new(
                idxs.inner()
                    .elements
                    .iter()
                    .map(|idx| {
                        if let Some(i) = idx.downcast::<i32>() {
                            Ok(eles
                                .get(i as usize)
                                .ok_or_else(|| {
                                    RuntimeError::partial(&format!(
                                        "Array has no element at index {i}"
                                    ))
                                })?
                                .dup())
                        } else {
                            invalid!("Index", self, idx.get_type())
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                self.inner().entry.clone(),
            )))
        } else if let Some(range) = index.downcast::<Range>() {
            let eles = &self.inner().elements;
            Ok(Box::new(Self::new(
                (range.inner().curr + 1..range.inner().last)
                    .map(|idx| {
                        Ok(eles
                            .get(idx as usize)
                            .ok_or_else(|| {
                                RuntimeError::partial(&format!(
                                    "Array has no element at index {idx}"
                                ))
                            })?
                            .dup())
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                self.inner().entry.clone(),
            )))
        } else {
            invalid!("Index", self, index.get_type())
        }
    }

    fn set_index(&self, index: Value, value: Value) -> Result<(), RuntimeError> {
        if let Some(idx) = index.downcast::<i32>() {
            if value.get_type() != self.inner().entry {
                return Err(RuntimeError::partial(&format!(
                    "Tried to set array index to different type ({}) than allowed ({})",
                    value.get_type(),
                    self.inner().entry
                )));
            }
            *self
                .inner_mut()
                .elements
                .get_mut(idx as usize)
                .ok_or_else(|| {
                    RuntimeError::partial(&format!("Array has no element at index {idx}"))
                })? = value;
        } else if let Some(idxs) = index.downcast::<Tuple>() {
            let entry = self.inner().entry.clone();
            let vals = value.downcast::<Arr>().ok_or_else(|| {
                RuntimeError::partial("Array setindex using tuple as index must use array as value")
            })?;
            if vals.inner().entry != entry {
                return Err(RuntimeError::partial(&format!(
                    "Tried to set array index to different type ({}) than allowed ({})",
                    vals.inner().entry,
                    entry
                )));
            }
            idxs.inner()
                .elements
                .iter()
                .zip(vals.inner().elements.iter())
                .map(|(idx, val)| {
                    if let Some(i) = idx.downcast::<i32>() {
                        *self
                            .inner_mut()
                            .elements
                            .get_mut(i as usize)
                            .ok_or_else(|| {
                                RuntimeError::partial(&format!("Array has no element at index {i}"))
                            })? = val.dup();
                    } else {
                        invalid!("Index", self, idx.get_type())
                    }
                    Ok(())
                })
                .collect::<Result<Vec<_>, _>>()?;
        } else if let Some(range) = index.downcast::<Range>() {
            let entry = self.inner().entry.clone();
            let vals = value.downcast::<Arr>().ok_or_else(|| {
                RuntimeError::partial("Array setindex using range as index must use array as value")
            })?;
            if vals.inner().entry != entry {
                return Err(RuntimeError::partial(&format!(
                    "Tried to set array index to different type ({}) than allowed ({})",
                    vals.inner().entry,
                    entry
                )));
            }
            (range.inner().curr + 1..range.inner().last)
                .zip(vals.inner().elements.iter())
                .map(|(idx, val)| {
                    *self
                        .inner_mut()
                        .elements
                        .get_mut(idx as usize)
                        .ok_or_else(|| {
                            RuntimeError::partial(&format!("Array has no element at index {idx}"))
                        })? = val.dup();
                    Ok(())
                })
                .collect::<Result<Vec<_>, _>>()?;
        } else {
            invalid!("Index", self, index.get_type())
        }
        Ok(())
    }

    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        self.inner()
            .elements
            .iter()
            .map(|e| e.as_ref().hash(h))
            .collect::<Result<Vec<_>, _>>()
            .map(|_| ())
    }
}
