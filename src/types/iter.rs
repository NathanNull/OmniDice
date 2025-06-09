use crate::{gen_fn_map, invalid, type_init};

use super::*;

#[derive(Debug, Clone)]
pub struct Iter {
    pub next_fn: Value,
    pub output: Datatype,
}

impl PartialEq for Iter {
    fn eq(&self, other: &Self) -> bool {
        &self.next_fn == &other.next_fn && self.output == other.output
    }
}

impl Display for Iter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Iterator<{}>", self.output)
    }
}

type_init!(IterT, Iter, "iter", output: Datatype);

fn next_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<IterT>() {
        Some(Box::new(MaybeT { output: me.output }))
    } else {
        None
    }
}

fn next_fn(params: Vec<Value>, i: &mut Interpreter) -> Value {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<Iter>() {
        Box::new(
            me.next_fn
                .call(vec![], i)
                .downcast::<Maybe>()
                .expect("Invalid iter function return value"),
        )
    } else {
        invalid!("Call", "next", params)
    }
}

fn map_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut it = params.iter().cloned();
    let me = it.next_as::<IterT>()?;
    let mapper = it.next_as::<FuncT>()?;
    let output = mapper.call_result(vec![me.output])?;
    Some(Box::new(IterT { output }))
}

fn map_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    let mut it = params.iter().cloned();
    let me = it.next_as::<Iter>().expect("Invalid call");
    let mapper = it.next_as::<Func>().expect("Invalid call");
    Box::new(Iter {
        output: mapper
            .get_type()
            .call_result(vec![me.output.dup()])
            .expect("Invalid call"),
        next_fn: Box::new(RustFunc::new_member(
            map_iter_sig,
            map_iter_fn,
            Box::new(Tuple::new(vec![me.dup(), mapper.dup()])),
        )),
    })
}

fn map_iter_sig(params: Vec<Datatype>) -> Option<Datatype> {
    let mut it = params.iter().cloned();
    let tup = it.next_as::<TupT>()?;
    let mut it = tup.entries.0.into_iter();
    let me = it.next_as::<IterT>()?;
    let mapper = it.next_as::<FuncT>()?;
    let output = mapper.call_result(vec![me.output])?;
    Some(Box::new(MaybeT { output }))
}

fn map_iter_fn(params: Vec<Value>, i: &mut Interpreter) -> Value {
    let mut it = params.iter().cloned();
    let tup = it.next_as::<Tuple>().unwrap();
    let mut it = tup.inner().elements.clone().into_iter();
    let me = it.next_as::<Iter>().unwrap();
    let mapper = it.next_as::<Func>().unwrap();
    let next = next_fn(vec![me.dup()], i).downcast::<Maybe>().unwrap();
    Box::new(Maybe {
        output: mapper
            .get_type()
            .call_result(vec![me.output.dup()])
            .unwrap(),
        contents: next.contents.map(|n| mapper.call(vec![n], i)),
    })
}

fn ident_sig(params: Vec<Datatype>) -> Option<Datatype> {
    params.first().cloned()
}

fn ident_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    params.first().unwrap().clone()
}

gen_fn_map!(
    ITER_FNS,
    ("next", next_sig, next_fn),
    ("map", map_sig, map_fn),
    ("iter", ident_sig, ident_fn)
);

impl Type for IterT {
    fn prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            n if ITER_FNS.contains_key(n) => {
                Some(Box::new(RustFuncT::new_member(ITER_FNS[n].0, self.dup())))
            }
            _ => None,
        }
    }
}
impl Val for Iter {
    fn get_prop(&self, name: &str) -> Value {
        match name {
            n if ITER_FNS.contains_key(n) => Box::new(RustFunc::new_member(
                ITER_FNS[n].0,
                ITER_FNS[n].1,
                self.dup(),
            )),
            _ => invalid!("Prop", self, name),
        }
    }
}
