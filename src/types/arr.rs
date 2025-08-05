use std::sync::{LazyLock, RwLockReadGuard};

use super::*;
use crate::{gen_fn_map, invalid, mut_type_init, op_list, type_init};

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

fn push_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
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

fn pop_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Arr>() {
        let contents = arr.inner_mut().elements.pop();
        let output = arr.inner().entry.clone();
        return Ok(Box::new(Maybe { output, contents }));
    }
    invalid!("Call", "push", params)
}

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

fn iter_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
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

pub fn iter_ret_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
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

fn length_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let arr = params[0]
        .downcast::<Arr>()
        .ok_or_else(|| RuntimeError::partial("arrlen owner isn't array"))?;
    Ok(Box::new(arr.inner().elements.len() as i32))
}

impl Type for ArrT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Result<(Datatype, BinOpFn), String> {
        if other == self {
            op_list!(op => {
                Plus(l: Arr, r: Arr) -> (self.clone()) |l: Arr,r: Arr| Ok(Arr::new(
                    l.inner()
                        .elements
                        .iter()
                        .chain(r.inner().elements.iter())
                        .cloned()
                        .collect(),
                    l.inner().entry.clone(),
                ));
            })
        } else {
            Err(format!("Type {self} has no binary operations"))
        }
    }

    fn real_prop_type(&self, name: &str) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name, self,
            ("push", PUSH_SIG, push_fn, push_prop),
            ("pop", POP_SIG, pop_fn, pop_prop),
            ("iter", ITER_SIG, iter_fn, iter_prop),
            ("length", LENGTH_SIG, length_fn, length_prop),
        )
    }

    fn real_index_type(&self, index: &Datatype) -> Result<(Datatype, BinOpFn, SetAtFn), String> {
        if index == &IntT {
            fn get_fn(me: &Expr, idx: &Expr, i: &mut Interpreter) -> OpResult {
                let idx = i.try_eval_as::<i32>(idx)?;
                Ok(i.try_eval_as::<Arr>(me)?
                    .inner()
                    .elements
                    .get(idx as usize)
                    .ok_or_else(|| {
                        RuntimeError::partial(&format!("Array has no element at index {idx}"))
                    })?
                    .dup())
            }
            fn set_fn(me: &Expr, idx: &Expr, val: &Expr, i: &mut Interpreter) -> VoidResult {
                let val = i.eval_expr(val)?;
                let me = i.try_eval_as::<Arr>(me)?;
                let idx = i.try_eval_as::<i32>(idx)?;
                if val.get_type() != me.inner().entry {
                    return Err(RuntimeError::partial(&format!(
                        "Tried to set array index to different type ({}) than allowed ({})",
                        val.get_type(),
                        me.inner().entry
                    )));
                }
                *me.inner_mut()
                    .elements
                    .get_mut(idx as usize)
                    .ok_or_else(|| {
                        RuntimeError::partial(&format!("Array has no element at index {idx}"))
                    })? = val;
                Ok(())
            }
            Ok((self.entry.clone(), get_fn, set_fn))
        } else if index == &RangeT {
            fn get_fn(me: &Expr, idx: &Expr, i: &mut Interpreter) -> OpResult {
                let range = i.try_eval_as::<Range>(idx)?;
                let me = i.try_eval_as::<Arr>(me)?;
                let eles = &me.inner().elements;
                Ok(Box::new(Arr::new(
                    (range.inner().curr + 1..=range.inner().last)
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
                    me.inner().entry.clone(),
                )))
            }
            fn set_fn(me: &Expr, idx: &Expr, val: &Expr, i: &mut Interpreter) -> VoidResult {
                let vals = i.try_eval_as::<Arr>(val)?;
                let me = i.try_eval_as::<Arr>(me)?;
                let range = i.try_eval_as::<Range>(idx)?;
                let entry = me.inner().entry.clone();
                if vals.inner().entry != entry {
                    return Err(RuntimeError::partial(&format!(
                        "Tried to set array index to different type ({}) than allowed ({})",
                        vals.inner().entry,
                        entry
                    )));
                }
                (range.inner().curr + 1..=range.inner().last)
                    .zip(vals.inner().elements.iter())
                    .map(|(idx, val)| {
                        *me.inner_mut()
                            .elements
                            .get_mut(idx as usize)
                            .ok_or_else(|| {
                                RuntimeError::partial(&format!(
                                    "Array has no element at index {idx}"
                                ))
                            })? = val.dup();
                        Ok(())
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(())
            }
            Ok((self.dup(), get_fn, set_fn))
        } else {
            Err(format!("Can't index {self} with {index}"))
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Datatype, String> {
        Ok(Box::new(Self {
            entry: self.entry.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
        self.entry.try_match(&other.downcast::<Self>().ok_or_else(||format!("Can't match {self} with {other}"))?.entry)
    }
    fn get_generics(&self) -> Vec<String> {
        self.entry.get_generics()
    }
    fn is_hashable(&self) -> bool {
        self.entry.is_hashable()
    }
}

impl Val for Arr {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        self.inner()
            .elements
            .iter()
            .map(|e| e.as_ref().hash(h))
            .collect::<Result<Vec<_>, _>>()
            .map(|_| ())
    }
}
