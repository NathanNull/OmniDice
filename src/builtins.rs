use std::{collections::HashMap, sync::LazyLock};

use itertools::Itertools;

use crate::{
    distribution::Distribution,
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
    ("ref", REF_SIG, ref_fn),
    ("println", PRINTLN_SIG, println_fn),
    ("printf", PRINTF_SIG, printf_fn),
    ("error", ERROR_SIG, error_fn),
    ("format", FORMAT_SIG, format_fn),
    ("filled", FILLED_SIG, filled_fn),
    ("iter", ITER_SIG, iter_fn),
    ("null", NULL_SIG, null_fn),
    ("dicemap", DICEMAP_SIG, dicemap_fn)
);

pub static BUILTINS: LazyLock<HashMap<String, Value>> = LazyLock::new(|| {
    HashMap::from_iter(BUILTIN_FUNCS.iter().map(|(name, (sig, func))| {
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

fn ref_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    if params.len() == 1 {
        Box::new(Ref::new(params[0].clone()))
    } else {
        unreachable!("Invalid function call")
    }
}

static PRINTLN_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![Box::new(StringT)],
    output: Box::new(Void),
    generic: vec![],
    owner_t: None,
});

fn println_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let res = params.first().unwrap().downcast::<String>().unwrap();
    println!("{res}");
    Box::new(Void)
}

static PRINTF_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![Box::new(StringT), TV1.clone()],
    output: Box::new(Void),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn printf_fn(params: Vec<Value>, i: &mut Interpreter, o: Option<Datatype>) -> Value {
    let res = format_fn(params, i, o).downcast::<String>().unwrap();
    println!("{res}");
    Box::new(Void)
}

static ERROR_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV1.clone()],
    output: Box::new(Void),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn error_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    panic!(
        "{}",
        params
            .into_iter()
            .map(|p| format!("{p}"))
            .collect::<Vec<_>>()
            .join(" ")
    );
}

static FORMAT_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![Box::new(StringT), TV1.clone()],
    output: Box::new(StringT),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn format_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut param_iter = params.clone().into_iter();
    let str = param_iter
        .next_as::<String>()
        .expect("Invalid function call");
    let next = param_iter.next().expect("Expected format args");
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
            Some(_) => panic!("Too many format args"),
            None if str_pieces.next().is_none() => break,
            None => panic!("Not enough format args"),
        }
    }
    Box::new(res)
}

static FILLED_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV1.clone()],
    output: Box::new(MaybeT {
        output: TV1.clone(),
    }),
    generic: vec![TV1_NAME.to_string()],
    owner_t: None,
});

fn filled_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    if params.len() == 1 {
        Box::new(Maybe {
            output: params[0].get_type(),
            contents: Some(params[0].clone()),
        })
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

fn null_fn(params: Vec<Value>, _i: &mut Interpreter, o: Option<Datatype>) -> Value {
    if params.len() == 0 {
        if let Some(ret) = o.and_then(|o| o.downcast::<MaybeT>()) {
            return Box::new(Maybe {
                output: ret.output,
                contents: None,
            });
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

fn iter_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    if params.len() == 1 {
        let func = params
            .first()
            .and_then(|p| p.downcast::<Func>())
            .expect("Invalid function call");
        if func.params.len() > 0 {
            invalid!("Call", "iter", params);
        }
        let output = func
            .output
            .downcast::<MaybeT>()
            .expect("Invalid function call")
            .output;
        return Box::new(Iter {
            output,
            next_fn: func.dup(),
        });
    }
    invalid!("Call", "iter", params);
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

fn dicemap_fn(params: Vec<Value>, i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut param_iter = params.iter().cloned();
    if let Some(dice) = param_iter.next_as::<Arr>() {
        if let Some(func) = param_iter.next_as::<Func>() {
            let mut roll_iter: Box<dyn Iterator<Item = (Vec<i32>, f32)>> =
                Box::new(vec![(vec![], 1.0)].into_iter());
            // Generate all potential combinations of rolls, and their odds
            for die in dice
                .inner()
                .elements
                .iter()
                .map(|d| d.downcast::<Distribution>().unwrap())
            {
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
                let res = func
                    .call(vec![Box::new(param)], i, Some(Box::new(IntT)))
                    .downcast::<i32>()
                    .unwrap();
                if let Some(o) = results.get_mut(&res) {
                    *o += odds;
                } else {
                    results.insert(res, odds);
                }
            }
            return Box::new(Distribution::from_odds(results.into_iter().collect()));
        }
    }
    invalid!("Call", "dicemap", params);
}
