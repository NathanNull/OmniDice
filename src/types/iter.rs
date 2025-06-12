use std::sync::LazyLock;

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

static ITER_T_NAME: &str = "__IterT";
static ITER_T: LazyLock<Datatype> =
    LazyLock::new(|| Box::new(TypeVar::Var(ITER_T_NAME.to_string())));

static TV2_NAME: &str = "__T2";
static TV2: LazyLock<Datatype> = LazyLock::new(|| Box::new(TypeVar::Var(TV2_NAME.to_string())));

// fn next_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     let mut it = params.iter().cloned();
//     if let Some(me) = it.next_as::<IterT>() {
//         Some(Box::new(MaybeT { output: me.output }))
//     } else {
//         None
//     }
// }

static NEXT_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MaybeT {
        output: ITER_T.clone(),
    }),
    generic: vec![ITER_T_NAME.to_string()],
    owner_t: Some(Box::new(IterT {
        output: ITER_T.clone(),
    })),
});

fn next_fn(params: Vec<Value>, i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<Iter>() {
        Box::new(
            me.next_fn
                .call(vec![], i, None)
                .downcast::<Maybe>()
                .expect("Invalid iter function return value"),
        )
    } else {
        invalid!("Call", "next", params)
    }
}

// fn map_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     let mut it = params.iter().cloned();
//     let me = it.next_as::<IterT>()?;
//     let mapper = it.next_as::<FuncT>()?;
//     let output = mapper.call_result(vec![me.output], None)?;
//     Some(Box::new(IterT { output }))
// }

static MAPPER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![ITER_T.clone()],
    output: TV2.clone(),
    generic: vec![],
    owner_t: None,
});

static MAP_ITER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MaybeT {
        output: TV2.clone(),
    }),
    generic: vec![],
    owner_t: Some(Box::new(TupT {
        entries: vec![
            Box::new(IterT {
                output: ITER_T.clone(),
            }),
            MAPPER_SIG.dup(),
        ],
    })),
});

static MAP_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![MAPPER_SIG.dup()],
    output: Box::new(IterT {
        output: TV2.clone(),
    }),
    generic: vec![ITER_T_NAME.to_string(), TV2_NAME.to_string()],
    owner_t: Some(Box::new(IterT {
        output: ITER_T.clone(),
    })),
});

fn map_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut it = params.iter().cloned();
    let me = it.next_as::<Iter>().expect("Invalid call");
    let mapper = it.next_as::<Func>().expect("Invalid call");
    Box::new(Iter {
        output: mapper
            .get_type()
            .call_result(vec![me.output.dup()], None)
            .expect("Invalid call"),
        next_fn: Box::new(
            MAP_ITER_SIG
                .insert_generics(&HashMap::from_iter([
                    (ITER_T_NAME.to_string(), me.output.clone()),
                    (TV2_NAME.to_string(), mapper.output.clone()),
                ]))
                .unwrap()
                .downcast::<FuncT>()
                .unwrap()
                .make_rust_member(
                    map_iter_fn,
                    Box::new(Tuple::new(vec![me.dup(), mapper.dup()])),
                ),
        ),
    })
}

// fn map_iter_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     let mut it = params.iter().cloned();
//     let tup = it.next_as::<TupT>()?;
//     let mut it = tup.entries.0.into_iter();
//     let me = it.next_as::<IterT>()?;
//     let mapper = it.next_as::<FuncT>()?;
//     let output = mapper.call_result(vec![me.output], None)?;
//     Some(Box::new(MaybeT { output }))
// }

fn map_iter_fn(params: Vec<Value>, i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut it = params.iter().cloned();
    let tup = it.next_as::<Tuple>().unwrap();
    let mut it = tup.inner().elements.clone().into_iter();
    let me = it.next_as::<Iter>().unwrap();
    let mapper = it.next_as::<Func>().unwrap();
    let next = next_fn(vec![me.dup()], i, None)
        .downcast::<Maybe>()
        .unwrap();
    Box::new(Maybe {
        output: mapper
            .get_type()
            .call_result(vec![me.output.dup()], None)
            .unwrap(),
        contents: next.contents.map(|n| mapper.call(vec![n], i, None)),
    })
}

// fn ident_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     params.first().cloned()
// }

static IDENT_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: ITER_T.clone(),
    generic: vec![ITER_T_NAME.to_string()],
    owner_t: Some(ITER_T.clone()),
});

fn ident_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    params.first().unwrap().clone()
}

gen_fn_map!(
    ITER_FNS,
    ("next", NEXT_SIG, next_fn),
    ("map", MAP_SIG, map_fn),
    ("iter", IDENT_SIG, ident_fn)
);

impl Type for IterT {
    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            n if ITER_FNS.contains_key(n) => Some(Box::new(
                ITER_FNS[n]
                    .0
                    .clone()
                    .with_owner(self.dup())
                    .expect("Invalid owner"),
            )),
            _ => None,
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(Box::new(Self {
            output: self.output.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        self.output.try_match(&other.downcast::<Self>()?.output)
    }
    fn get_generics(&self) -> Vec<String> {
        self.output.get_generics()
    }
}
impl Val for Iter {
    fn get_prop(&self, name: &str) -> Value {
        match name {
            n if ITER_FNS.contains_key(n) => Box::new(
                ITER_FNS[n]
                    .0
                    .clone()
                    .make_rust_member(ITER_FNS[n].1, self.dup()),
            ),
            _ => invalid!("Prop", self, name),
        }
    }
}
