use std::sync::LazyLock;

use crate::{gen_fn_map, mut_type_init, type_init};

use super::*;

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct _InnerRange {
    pub curr: i32,
    pub last: i32,
}

impl Debug for _InnerRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.curr, self.last)
    }
}

impl Display for _InnerRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.curr + 1, self.last)
    }
}

mut_type_init!(Range, _InnerRange);

impl Range {
    pub fn new(start: i32, end: i32) -> Self {
        Self::make(_InnerRange {
            curr: start - 1,
            last: end,
        })
    }
}

type_init!(RangeT, Range, "range");

static RANGE_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IterT {
        output: Box::new(IntT),
    }),
    generic: vec![],
    owner_t: Some(Box::new(RangeT)),
});

static RANGE_ITER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MaybeT {
        output: Box::new(IntT),
    }),
    generic: vec![],
    owner_t: Some(Box::new(RangeT)),
});

fn range_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    let me = it
        .next_as::<Range>()
        .ok_or_else(|| RuntimeError::partial("Range iter function owner isn't range"))?;
    Ok(Box::new(Iter {
        output: Box::new(IntT),
        next_fn: Box::new(RANGE_ITER_SIG.clone().make_rust_member(
            range_iter_fn,
            "range_iter_fn".to_string(),
            Box::new(me),
        )?),
    }))
}

fn range_iter_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    let me_outer = it
        .next_as::<Range>()
        .ok_or_else(|| RuntimeError::partial("Range iter function owner isn't range"))?;
    let mut me = me_outer.inner_mut();
    let contents: Option<Value> = if me.curr < me.last {
        me.curr += 1;
        Some(Box::new(me.curr))
    } else {
        None
    };
    Ok(Box::new(Maybe {
        output: Box::new(IntT),
        contents,
    }))
}

#[typetag::serde]
impl Type for RangeT {
    fn real_prop_type(
        &self,
        name: &str,
    ) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name,
            self,
            "Range",
            ("iter", RANGE_SIG, range_fn, range_prop)
        )
    }
    fn is_hashable(&self) -> bool {
        true
    }
}

#[typetag::serde]
impl Val for Range {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        h.write_i32(self.inner().curr);
        h.write_i32(self.inner().last);
        Ok(())
    }
}
