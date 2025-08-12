use std::sync::LazyLock;

use crate::{gen_fn_map, invalid, parser::ExprContents, type_init};

use super::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
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

static TV3_NAME: &str = "__T3";
static TV3: LazyLock<Datatype> = LazyLock::new(|| Box::new(TypeVar::Var(TV3_NAME.to_string())));

pub static NEXT_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MaybeT {
        output: ITER_T.clone(),
    }),
    generic: vec![ITER_T_NAME.to_string()],
    owner_t: Some(Box::new(IterT {
        output: ITER_T.clone(),
    })),
});

pub fn next_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<Iter>() {
        Ok(Box::new(
            (me.next_fn
                .get_type()
                .call_result(vec![], None)
                .map_err(|e| RuntimeError::partial(&e))?
                .1)(&me.next_fn.into(), &vec![], i, None)?
            .downcast::<Maybe>()
            .ok_or_else(|| RuntimeError::partial("Invalid iter function return value"))?,
        ))
    } else {
        invalid!("Call", "next", params)
    }
}

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

pub static MAP_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![MAPPER_SIG.dup()],
    output: Box::new(IterT {
        output: TV2.clone(),
    }),
    generic: vec![ITER_T_NAME.to_string(), TV2_NAME.to_string()],
    owner_t: Some(Box::new(IterT {
        output: ITER_T.clone(),
    })),
});

pub fn map_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    let me = it
        .next_as::<Iter>()
        .ok_or_else(|| RuntimeError::partial("map owner not iter"))?;
    let mapper = it
        .next_as::<Func>()
        .ok_or_else(|| RuntimeError::partial("map param not func"))?;
    Ok(Box::new(Iter {
        output: mapper
            .get_type()
            .call_result(vec![me.output.dup()], None)
            .map_err(|e| RuntimeError::partial(&e))?
            .0,
        next_fn: Box::new(
            MAP_ITER_SIG
                .insert_generics(&HashMap::from_iter([
                    (ITER_T_NAME.to_string(), me.output.clone()),
                    (TV2_NAME.to_string(), mapper.output.clone()),
                ]))
                .map_err(|e| RuntimeError::partial(&e))?
                .downcast::<FuncT>()
                .ok_or_else(|| RuntimeError::partial("map func degenericization isn't a function"))?
                .make_rust_member(
                    map_iter_fn,
                    "iter_map_iter_fn".to_string(),
                    Box::new(Tuple::new(vec![me.dup(), mapper.dup()])),
                )?,
        ),
    }))
}

fn map_iter_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    let tup = it
        .next_as::<Tuple>()
        .ok_or_else(|| RuntimeError::partial("map iter owner isn't a tuple"))?;
    let mut it = tup.inner().elements.clone().into_iter();
    let me = it
        .next_as::<Iter>()
        .ok_or_else(|| RuntimeError::partial("map iter owner i1 isn't an iter"))?;
    let mapper = it
        .next_as::<Func>()
        .ok_or_else(|| RuntimeError::partial("map iter owner i2 isn't a func"))?;
    let next = next_fn(vec![me.dup()], i, None)?
        .downcast::<Maybe>()
        .ok_or_else(|| RuntimeError::partial("map iter next is invalid"))?;
    let (map_res, map_call) = mapper
        .get_type()
        .call_result(vec![me.output.dup()], None)
        .map_err(|e| RuntimeError::partial(&e))?;
    Ok(Box::new(Maybe {
        output: map_res,
        contents: if let Some(c) = next.contents {
            Some((map_call)(
                &(Box::new(mapper) as Value).into(),
                &vec![c.into()],
                i,
                None,
            )?)
        } else {
            None
        },
    }))
}

static FILTERER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![ITER_T.clone()],
    output: Box::new(BoolT),
    generic: vec![],
    owner_t: None,
});

static FILTER_ITER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MaybeT {
        output: ITER_T.clone(),
    }),
    generic: vec![],
    owner_t: Some(Box::new(TupT {
        entries: vec![
            Box::new(IterT {
                output: ITER_T.clone(),
            }),
            FILTERER_SIG.dup(),
        ],
    })),
});

pub static FILTER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![FILTERER_SIG.dup()],
    output: Box::new(IterT {
        output: ITER_T.clone(),
    }),
    generic: vec![ITER_T_NAME.to_string()],
    owner_t: Some(Box::new(IterT {
        output: ITER_T.clone(),
    })),
});

pub fn filter_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    let me = it
        .next_as::<Iter>()
        .ok_or_else(|| RuntimeError::partial("Invalid owner type"))?;
    let filter = it
        .next_as::<Func>()
        .ok_or_else(|| RuntimeError::partial("Invalid filter parameter"))?;
    Ok(Box::new(Iter {
        output: me.output.clone(),
        next_fn: Box::new(
            FILTER_ITER_SIG
                .insert_generics(&HashMap::from_iter([(
                    ITER_T_NAME.to_string(),
                    me.output.clone(),
                )]))
                .map_err(|e| RuntimeError::partial(&e))?
                .downcast::<FuncT>()
                .ok_or_else(|| RuntimeError::partial("Invalid filter parameter degenericization"))?
                .make_rust_member(
                    filter_iter_fn,
                    "iter_filter_iter_fn".to_string(),
                    Box::new(Tuple::new(vec![me.dup(), filter.dup()])),
                )?,
        ),
    }))
}

fn filter_iter_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    let tup = it
        .next_as::<Tuple>()
        .ok_or_else(|| RuntimeError::partial("Invalid iter owner type"))?;
    let mut it = tup.inner().elements.clone().into_iter();
    let me = it
        .next_as::<Iter>()
        .ok_or_else(|| RuntimeError::partial("Invalid iter owner type"))?;
    let filter = it
        .next_as::<Func>()
        .ok_or_else(|| RuntimeError::partial("Invalid iter owner type"))?;
    let filter_expr = (Box::new(filter.clone()) as Value).into();
    let mut res = Maybe {
        output: me.output.clone(),
        contents: None,
    };
    let (_, call) = filter
        .get_type()
        .call_result(vec![me.output.clone()], Some(Box::new(BoolT)))
        .map_err(|e| RuntimeError::partial(&e))?;
    while let Some(next) = &next_fn(vec![me.dup()], i, None)?
        .downcast::<Maybe>()
        .ok_or_else(|| RuntimeError::partial("Invalid iter return type"))?
        .contents
    {
        let filter_res = (call)(
            &filter_expr,
            &vec![next.clone().into()],
            i,
            Some(Box::new(BoolT)),
        )?
        .downcast::<bool>()
        .ok_or_else(|| RuntimeError::partial("Invalid filter return type"))?;
        if filter_res {
            res.contents = Some(next.clone());
            break;
        }
    }
    Ok(Box::new(res))
}

static FOLDER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV2.clone(), ITER_T.clone()],
    output: TV2.clone(),
    generic: vec![TV2_NAME.to_string()],
    owner_t: None,
});

pub static FOLD_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV2.clone(), FOLDER_SIG.dup()],
    output: TV2.clone(),
    generic: vec![ITER_T_NAME.to_string(), TV2_NAME.to_string()],
    owner_t: Some(Box::new(IterT {
        output: ITER_T.clone(),
    })),
});

pub fn fold_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    let me = it
        .next_as::<Iter>()
        .ok_or_else(|| RuntimeError::partial("Invalid owner type"))?;
    let initial = it
        .next()
        .ok_or_else(|| RuntimeError::partial("Invalid parameter count"))?;
    let folder = it
        .next_as::<Func>()
        .ok_or_else(|| RuntimeError::partial("Invalid parameter type"))?;
    let folder_expr = Expr {
        contents: ExprContents::Value(Box::new(folder.clone())),
        output: folder.get_type(),
    };
    let mut curr = initial;
    let res_t = curr.get_type();
    let (_, call) = folder
        .get_type()
        .call_result(vec![res_t.clone(), me.output.clone()], None)
        .map_err(|e| RuntimeError::partial(&e))?;
    while let Some(next) = next_fn(vec![me.dup()], i, None)?
        .downcast::<Maybe>()
        .ok_or_else(|| RuntimeError::partial(""))?
        .contents
    {
        curr = (call)(&folder_expr, &vec![curr.into(), next.into()], i, None)?;
    }
    Ok(curr)
}

pub static IDENT_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: ITER_T.clone(),
    generic: vec![ITER_T_NAME.to_string()],
    owner_t: Some(ITER_T.clone()),
});

pub fn ident_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    Ok(params
        .first()
        .ok_or_else(|| RuntimeError::partial("Invalid parameter count"))?
        .clone())
}

pub static TO_MAP_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MapT {
        key: TV2.clone(),
        value: TV3.clone(),
    }),
    generic: vec![TV2_NAME.to_string(), TV3_NAME.to_string()],
    owner_t: Some(Box::new(IterT {
        output: Box::new(TupT {
            entries: vec![TV2.clone(), TV3.clone()],
        }),
    })),
});
pub fn to_map_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let iter = params
        .first()
        .ok_or_else(|| RuntimeError::partial("Invalid parameter count"))?
        .downcast::<Iter>()
        .ok_or_else(|| RuntimeError::partial("Invalid parameter type"))?;
    let mut map = HashMap::new();
    let (mut kt, mut vt) = (Box::new(Void) as Datatype, Box::new(Void) as Datatype);
    loop {
        let entry = next_fn(vec![Box::new(iter.clone())], i, None)?
            .downcast::<Maybe>()
            .ok_or_else(|| RuntimeError::partial("Invalid iter return type"))?;
        if let Some(val) = entry.contents {
            let tup = val
                .downcast::<Tuple>()
                .ok_or_else(|| RuntimeError::partial("Invalid iter return type"))?;
            let kvpair = &tup.inner().elements;
            let key = kvpair[0].clone();
            let value = kvpair[1].clone();
            kt = key.get_type();
            vt = value.get_type();
            map.insert(key, value);
        } else {
            break;
        }
    }
    Ok(Box::new(
        Map::new(map, kt, vt).map_err(|e| RuntimeError::partial(&e))?,
    ))
}

#[typetag::serde]
impl Type for IterT {
    fn real_prop_type(
        &self,
        name: &str,
    ) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name,
            self,
            "Iter",
            ("next", NEXT_SIG, next_fn, next_prop),
            ("map", MAP_SIG, map_fn, map_prop),
            ("iter", IDENT_SIG, ident_fn, ident_prop),
            ("filter", FILTER_SIG, filter_fn, filter_prop),
            ("fold", FOLD_SIG, fold_fn, fold_prop),
            ("to_map", TO_MAP_SIG, to_map_fn, to_map_prop),
        )
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Datatype, String> {
        Ok(Box::new(Self {
            output: self.output.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
        self.output.try_match(
            &other
                .downcast::<Self>()
                .ok_or_else(|| format!("Can't match {self} with {other}"))?
                .output,
        )
    }
    fn get_generics(&self) -> Vec<String> {
        self.output.get_generics()
    }
}

#[typetag::serde]
impl Val for Iter {}
