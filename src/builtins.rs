use std::{collections::HashMap, sync::LazyLock};

use itertools::Itertools;

use crate::{
    distribution::Distribution,
    error::RuntimeError,
    gen_fn_map,
    interpreter::Interpreter,
    invalid,
    types::{
        Arr, ArrT, BoxIterUtils, Datatype, DiceT, Downcast, Func, FuncT, IntT, Iter, IterT, Maybe,
        MaybeT, Ref, RefT, StringT, Tuple, TypeVar, Val, Value, Void,
    },
};

gen_fn_map!(
    BUILTIN_FUNCS,
    ("ref", REF_SIG, ref_fn, rp),
    ("println", PRINTLN_SIG, println_fn, pp),
    ("printf", PRINTF_SIG, printf_fn, pfp),
    ("error", ERROR_SIG, error_fn, ep),
    ("format", FORMAT_SIG, format_fn, fp),
    ("filled", FILLED_SIG, filled_fn, fip),
    ("iter", ITER_SIG, iter_fn, ip),
    ("null", NULL_SIG, null_fn, np),
    ("dicemap", DICEMAP_SIG, dicemap_fn, dp)
);

pub static BUILTINS: LazyLock<HashMap<String, Value>> = LazyLock::new(|| {
    HashMap::from_iter(BUILTIN_FUNCS.iter().map(|(name, (sig, func, _))| {
        (
            name.to_string(),
            Box::new(sig.clone().make_rust(*func)) as Value,
        )
    }))
});

static TV1_NAME: &str = "__T";
static TV1: LazyLock<Datatype> = LazyLock::new(|| Box::new(TypeVar::Var(TV1_NAME.to_string())));

static REF_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV1.clone()],
    output: Box::new(RefT { ty: TV1.clone() }),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn ref_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    if params.len() == 1 {
        Ok(Box::new(Ref::new(params[0].clone())))
    } else {
        Err(RuntimeError::partial("Invalid function call"))
    }
}

static PRINTLN_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![Box::new(StringT)],
    output: Box::new(Void),
    generic: vec![],
    owner_t: None,
});

fn println_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let res = params
        .first()
        .and_then(|v| v.downcast::<String>())
        .ok_or_else(|| RuntimeError::partial("Println first parameter must be a string"))?;
    println!("{res}");
    Ok(Box::new(Void))
}

static PRINTF_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![Box::new(StringT), TV1.clone()],
    output: Box::new(Void),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn printf_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let res = format_fn(params, i, o)?
        .downcast::<String>()
        .ok_or_else(|| RuntimeError::partial("Printf first parameter must be a string"))?;
    println!("{res}");
    Ok(Box::new(Void))
}

static ERROR_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV1.clone()],
    output: Box::new(Void),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn error_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    Err(RuntimeError::partial(&format!(
        "{}",
        params
            .into_iter()
            .map(|p| format!("{p}"))
            .collect::<Vec<_>>()
            .join(" ")
    )))
}

static FORMAT_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![Box::new(StringT), TV1.clone()],
    output: Box::new(StringT),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn format_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut param_iter = params.clone().into_iter();
    let str = param_iter
        .next_as::<String>()
        .ok_or_else(|| RuntimeError::partial("Format/printf first parameter must be a string"))?;
    let next = param_iter.next().ok_or_else(|| {
        RuntimeError::partial("Format/printf second parameter must be format args")
    })?;
    let mut format_args = match next.downcast::<Tuple>() {
        Some(tup) if str.split("{}").count() > 2 => tup.inner().elements.clone(),
        _ => vec![next],
    }
    .into_iter()
    .map(|p| format!("{p}"));
    let mut str_pieces = str.split("{}").peekable();
    let mut res = "".to_string();
    while let Some(next_str_piece) = str_pieces.next() {
        res += next_str_piece;
        match format_args.next() {
            Some(arg) if str_pieces.peek().is_some() => res += &arg,
            Some(_) => return Err(RuntimeError::partial("Too many format args")),
            None if str_pieces.next().is_none() => break,
            None => return Err(RuntimeError::partial("Not enough format args")),
        }
    }
    Ok(Box::new(res))
}

static FILLED_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV1.clone()],
    output: Box::new(MaybeT {
        output: TV1.clone(),
    }),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn filled_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    if params.len() == 1 {
        Ok(Box::new(Maybe {
            output: params[0].get_type(),
            contents: Some(params[0].clone()),
        }))
    } else {
        invalid!("Call", "filled", params)
    }
}

static NULL_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(MaybeT {
        output: TV1.clone(),
    }),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn null_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    if params.len() == 0 {
        if let Some(ret) = o.and_then(|o| o.downcast::<MaybeT>()) {
            return Ok(Box::new(Maybe {
                output: ret.output,
                contents: None,
            }));
        }
    }
    invalid!("Call", "null", params)
}

static ITER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![Box::new(FuncT {
        params: vec![],
        output: Box::new(MaybeT {
            output: TV1.clone(),
        }),
        generic: vec![],
        owner_t: None,
    })],
    output: Box::new(IterT {
        output: TV1.clone(),
    }),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn iter_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let func = params
        .first()
        .and_then(|p| p.downcast::<Func>())
        .ok_or_else(|| RuntimeError::partial("Iter first parameter must be a function"))?;
    if func.params.len() > 0 {
        invalid!("Call", "iter", params);
    }
    let output = func
        .output
        .downcast::<MaybeT>()
        .ok_or_else(|| RuntimeError::partial("Iter function output must be maybe<something>"))?
        .output;
    Ok(Box::new(Iter {
        output,
        next_fn: func.dup(),
    }))
}

static DICEMAP_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![
        Box::new(ArrT {
            entry: Box::new(DiceT),
        }),
        Box::new(FuncT {
            params: vec![Box::new(ArrT {
                entry: Box::new(IntT),
            })],
            output: Box::new(IntT),
            generic: vec![],
            owner_t: None,
        }),
    ],
    output: Box::new(DiceT),
    generic: vec![],
    owner_t: None,
});

fn dicemap_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut param_iter = params.iter().cloned();
    if let Some(dice) = param_iter.next_as::<Arr>() {
        if let Some(func) = param_iter.next_as::<Func>() {
            let mut roll_iter: Box<dyn Iterator<Item = (Vec<i32>, f32)>> =
                Box::new(vec![(vec![], 1.0)].into_iter());
            // Generate all potential combinations of rolls, and their odds
            for die in dice.inner().elements.iter().map(|d| {
                d.downcast::<Distribution>()
                    .ok_or_else(|| RuntimeError::partial("Dice array contained a non-dice value"))
            }) {
                let die = die?;
                // For each current result and each possible roll of the new die, multiply their odds
                // and append the new roll to the result (with some extra nonsense to satisfy memory management)
                roll_iter = Box::new(
                    roll_iter
                        .cartesian_product(
                            die.values
                                .iter()
                                .map(|(k, v)| (*k, *v))
                                // TODO: is there a way to do this without collecting like this? Probably not but maybe worth considering.
                                .collect::<Vec<_>>()
                                .into_iter(),
                        )
                        .map(|((mut c_vec, c_odds), (new_v, new_odds))| {
                            c_vec.push(new_v);
                            (c_vec, c_odds * new_odds)
                        }),
                );
            }

            let mut results = HashMap::new();
            for (rolls, odds) in roll_iter {
                let param = Arr::new(
                    rolls.into_iter().map(|r| Box::new(r) as Value).collect(),
                    Box::new(IntT),
                );
                let res = (func
                    .get_type()
                    .call_result(
                        vec![Box::new(ArrT {
                            entry: Box::new(IntT),
                        })],
                        Some(Box::new(IntT)),
                    )
                    .map_err(|e| RuntimeError::partial(&e))?
                    .1)(
                    &func.dup().into(),
                    &vec![(Box::new(param) as Value).into()],
                    i,
                    Some(Box::new(IntT)),
                )?
                .downcast::<i32>()
                .ok_or_else(|| {
                    RuntimeError::partial("Dicemap function return value must be an integer")
                })?;
                if let Some(o) = results.get_mut(&res) {
                    *o += odds;
                } else {
                    results.insert(res, odds);
                }
            }
            return Ok(Box::new(Distribution::from_odds(
                results.into_iter().collect(),
            )));
        }
    }
    invalid!("Call", "dicemap", params);
}
