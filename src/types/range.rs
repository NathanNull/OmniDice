use crate::{gen_fn_map, invalid, mut_type_init, type_init};

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct _InnerRange {
    pub curr: i32,
    pub last: i32,
}

impl Display for _InnerRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.curr, self.last)
    }
}

mut_type_init!(Range, _InnerRange);

impl Range {
    pub fn new(start: i32, end: i32) -> Self {
        Self::make(_InnerRange { curr: start - 1, last: end })
    }
}

type_init!(RangeT, Range, "range");

fn range_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
    let mut it = params.iter().cloned();
    it.next_as::<RangeT>()?;
    Some(Box::new(IterT {
        output: Box::new(IntT),
    }))
}

fn range_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut it = params.iter().cloned();
    let me = it.next_as::<Range>().expect("Invalid call");
    Box::new(Iter {
        output: Box::new(IntT),
        next_fn: Box::new(RustFunc::new_member(
            range_iter_sig,
            range_iter_fn,
            Box::new(me),
        )),
    })
}

fn range_iter_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
    let mut it = params.iter().cloned();
    it.next_as::<RangeT>()?;
    Some(Box::new(MaybeT {
        output: Box::new(IntT),
    }))
}

fn range_iter_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut it = params.iter().cloned();
    let me_outer = it.next_as::<Range>().unwrap();
    let mut me = me_outer.inner_mut();
    let contents: Option<Value> = if me.curr < me.last {
        me.curr += 1;
        Some(Box::new(me.curr))
    } else {
        None
    };
    Box::new(Maybe {
        output: Box::new(IntT),
        contents,
    })
}

gen_fn_map!(RANGE_FNS, ("iter", range_sig, range_fn));

impl Type for RangeT {
    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            n if RANGE_FNS.contains_key(n) => {
                Some(Box::new(RustFuncT::new_member(RANGE_FNS[n].0, self.dup())))
            }
            _ => None,
        }
    }
}
impl Val for Range {
    fn get_prop(&self, name: &str) -> Value {
        match name {
            n if RANGE_FNS.contains_key(n) => Box::new(RustFunc::new_member(
                RANGE_FNS[n].0,
                RANGE_FNS[n].1,
                self.dup(),
            )),
            _ => invalid!("Prop", self, name),
        }
    }
}
