use crate::parser::ExprContents;

use super::*;

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
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
    let filter = it
        .next_as::<Func>()
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
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
                .ok_or_else(|| RuntimeError::partial("Invalid call"))?
                .make_rust_member(
                    filter_iter_fn,
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
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
    let mut it = tup.inner().elements.clone().into_iter();
    let me = it
        .next_as::<Iter>()
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
    let filter = it
        .next_as::<Func>()
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
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
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?
        .contents
    {
        let filter_res = (call)(
            &filter_expr,
            &vec![next.clone().into()],
            i,
            Some(Box::new(BoolT)),
        )?
        .downcast::<bool>()
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
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
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
    let initial = it
        .next()
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
    let folder = it
        .next_as::<Func>()
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
    let folder_expr = Expr {
        contents: ExprContents::Value(Box::new(folder.clone())),
        output: folder.get_type(),
    };
    let mut curr = initial;
    let it_t = curr.get_type();
    let (_, call) = folder
        .get_type()
        .call_result(vec![it_t.clone(), it_t.clone()], None)
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
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?
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
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?
        .downcast::<Iter>()
        .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
    let mut map = HashMap::new();
    let (mut kt, mut vt) = (Box::new(Void) as Datatype, Box::new(Void) as Datatype);
    loop {
        let entry = next_fn(vec![Box::new(iter.clone())], i, None)?
            .downcast::<Maybe>()
            .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
        if let Some(val) = entry.contents {
            let tup = val
                .downcast::<Tuple>()
                .ok_or_else(|| RuntimeError::partial("Invalid call"))?;
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
