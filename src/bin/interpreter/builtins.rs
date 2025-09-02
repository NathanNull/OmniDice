use std::{
    collections::{HashMap, VecDeque},
    sync::LazyLock,
};

use itertools::Itertools;

use super::{
    distribution::Distribution,
    error::RuntimeError,
    interpreter::Interpreter,
    types::{
        Arr, ArrT, BoxIterUtils, Datatype, DiceT, Downcast, Func, FuncT, IntT, Iter, IterT, Maybe,
        MaybeT, Never, Ref, RefT, StringT, TupT, Tuple, TypeVar, Val, Value, Void,
    },
};

use crate::{interpreter::types::FloatT, invalid, od_typedef};

macro_rules! gen_builtins {
    ($(($fname: literal, $fsig: ident, $ffn: ident)),*$(,)?) => {
        pub static BUILTINS: LazyLock<HashMap<String, Value>> = LazyLock::new(|| {
            HashMap::from_iter([$(
                (
                    $fname.to_string(),
                    Box::new((&$fsig as &FuncT).clone().make_rust($ffn, format!("builtin_{}", $fname))) as Value,
                )
            ),*])
        });
    };
}

gen_builtins!(
    ("ref", REF_SIG, ref_fn),
    ("println", PRINTLN_SIG, println_fn),
    ("printf", PRINTF_SIG, printf_fn),
    ("error", ERROR_SIG, error_fn),
    ("format", FORMAT_SIG, format_fn),
    ("filled", FILLED_SIG, filled_fn),
    ("iter", ITER_SIG, iter_fn),
    ("null", NULL_SIG, null_fn),
    ("dicemap", DICEMAP_SIG, dicemap_fn),
    ("mix", MIX_SIG, mix_fn),
    ("explode", EXPLODE_SIG, explode_fn),
    ("highest", HIGHEST_SIG, highest_fn),
    ("lowest", LOWEST_SIG, lowest_fn),
    ("highest_n", HIGHEST_N_SIG, highest_n_fn),
    ("lowest_n", LOWEST_N_SIG, lowest_n_fn),
);

static TV1: &str = "__T";

/* #region IO functions */

static PRINTLN_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func(StringT) -> Void}));
// static PRINTLN_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![Box::new(StringT)],
//     output: Box::new(Void),
//     generic: vec![],
//     owner_t: None,
// });

fn println_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let res = params
        .first()
        .and_then(|v| v.downcast::<String>())
        .ok_or_else(|| RuntimeError::partial("Println first parameter must be a string"))?;
    i.print(&(res + "\n"));
    Ok(Box::new(Void))
}

static PRINTF_SIG: LazyLock<FuncT> =
    LazyLock::new(|| od_typedef!({func<TV1>(StringT, (TV1)) -> Void}));
// static PRINTF_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![Box::new(StringT), TV1.clone()],
//     output: Box::new(Void),
//     generic: vec![TV1_NAME.to_string()],
//     owner_t: None,
// });

fn printf_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let res = format_fn(params, i, o)?
        .downcast::<String>()
        .ok_or_else(|| RuntimeError::partial("Printf first parameter must be a string"))?;
    i.print(&(res + "\n"));
    Ok(Box::new(Void))
}

static ERROR_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func<TV1>((TV1)) -> Never}));
// static ERROR_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![TV1.clone()],
//     output: Box::new(Never),
//     generic: vec![TV1_NAME.to_string()],
//     owner_t: None,
// });

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

static FORMAT_SIG: LazyLock<FuncT> =
    LazyLock::new(|| od_typedef!({func<TV1>(StringT, (TV1)) -> StringT}));
// static FORMAT_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![Box::new(StringT), TV1.clone()],
//     output: Box::new(StringT),
//     generic: vec![TV1_NAME.to_string()],
//     owner_t: None,
// });

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

/* #endregion */

/* #region Constructors */

static REF_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func<TV1>((TV1)) -> {ref (TV1)}}));
// static REF_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![TV1.clone()],
//     output: Box::new(RefT { ty: TV1.clone() }),
//     generic: vec![TV1_NAME.to_string()],
//     owner_t: None,
// });

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

static FILLED_SIG: LazyLock<FuncT> =
    LazyLock::new(|| od_typedef!({func<TV1>((TV1)) -> {maybe (TV1)}}));
// static FILLED_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![TV1.clone()],
//     output: Box::new(MaybeT {
//         output: TV1.clone(),
//     }),
//     generic: vec![TV1_NAME.to_string()],
//     owner_t: None,
// });

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

static NULL_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func<TV1>() -> {maybe (TV1)}}));
// static NULL_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![],
//     output: Box::new(MaybeT {
//         output: TV1.clone(),
//     }),
//     generic: vec![TV1_NAME.to_string()],
//     owner_t: None,
// });

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

static ITER_SIG: LazyLock<FuncT> =
    LazyLock::new(|| od_typedef!({func<TV1>({func() -> {maybe (TV1)}}) -> {iter (TV1)}}));
// static ITER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![Box::new(FuncT {
//         params: vec![],
//         output: Box::new(MaybeT {
//             output: TV1.clone(),
//         }),
//         generic: vec![],
//         owner_t: None,
//     })],
//     output: Box::new(IterT {
//         output: TV1.clone(),
//     }),
//     generic: vec![TV1_NAME.to_string()],
//     owner_t: None,
// });

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
/* #endregion */

/* #region Dice Functions */

static DICEMAP_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func([DiceT]) -> DiceT}));
// static DICEMAP_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
//     params: vec![
//         Box::new(ArrT {
//             entry: Box::new(DiceT),
//         }),
//         Box::new(FuncT {
//             params: vec![Box::new(ArrT {
//                 entry: Box::new(IntT),
//             })],
//             output: Box::new(IntT),
//             generic: vec![],
//             owner_t: None,
//         }),
//     ],
//     output: Box::new(DiceT),
//     generic: vec![],
//     owner_t: None,
// });

fn dicemap_fn(
    params: Vec<Value>,
    i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut param_iter = params.iter().cloned();
    if let Some(dice) = param_iter.next_as::<Arr>() {
        if let Some(func) = param_iter.next_as::<Func>() {
            let res = dicemap_fn_inner(
                dice.inner()
                    .elements
                    .iter()
                    .map(|d| {
                        d.downcast::<Distribution>().ok_or_else(|| {
                            RuntimeError::partial("Dice array contained a non-dice value")
                        })
                    })
                    .collect::<Result<_, _>>()?,
                |rolls| {
                    let param = Arr::new(
                        rolls.into_iter().map(|r| Box::new(r) as Value).collect(),
                        Box::new(IntT),
                    );
                    (func
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
                    })
                },
            )?;
            return Ok(Box::new(res));
        }
    }
    invalid!("Call", "dicemap", params);
}

fn dicemap_fn_inner(
    dice: Vec<Distribution>,
    mut mapper: impl FnMut(Vec<i32>) -> Result<i32, RuntimeError>,
) -> Result<Distribution, RuntimeError> {
    let mut roll_iter: Box<dyn Iterator<Item = (Vec<i32>, f32)>> =
        Box::new(vec![(vec![], 1.0)].into_iter());
    // Generate all potential combinations of rolls, and their odds
    for die in dice.iter() {
        // For each current result and each possible roll of the new die, multiply their odds
        // and append the new roll to the result (with some extra nonsense to satisfy memory management)
        roll_iter = Box::new(
            roll_iter
                .cartesian_product(
                    die.values
                        .iter()
                        .map(|(k, v)| (*k, *v))
                        // TODO: is there a way to do this without collecting? Probably not but maybe worth considering.
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
        let res = (mapper)(rolls)?;
        if let Some(o) = results.get_mut(&res) {
            *o += odds;
        } else {
            results.insert(res, odds);
        }
    }
    return Ok(Distribution::from_odds(results.into_iter().collect()));
}

static MIX_SIG: LazyLock<FuncT> =
    LazyLock::new(|| od_typedef!({func([{tup DiceT, FloatT}]) -> DiceT}));

fn mix_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let dice_arr = params
        .into_iter()
        .next_as::<Arr>()
        .ok_or_else(|| RuntimeError::partial("Mix fn invalid parameters"))?;
    let arr_elements = dice_arr
        .inner()
        .elements
        .iter()
        .map(|e| {
            e.downcast::<Tuple>()
                .and_then(|tup| {
                    let eles = &tup.inner().elements;
                    let dice = eles.get(0)?.downcast::<Distribution>()?;
                    let weight = eles.get(1)?.downcast::<f32>()?;
                    Some((dice, weight))
                })
                .ok_or_else(|| RuntimeError::partial("Mix fn invalid array"))
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Box::new(mix_fn_inner(arr_elements)))
}

fn mix_fn_inner(dice: Vec<(Distribution, f32)>) -> Distribution {
    let mut new_dice_odds = HashMap::new();
    let weight_sum = dice.iter().map(|(_, w)| w).sum::<f32>();
    for (dice, weight) in &dice {
        for (roll, odds) in &dice.values {
            if !new_dice_odds.contains_key(roll) {
                new_dice_odds.insert(*roll, 0.0);
            }
            *new_dice_odds.get_mut(roll).unwrap() += odds * weight / weight_sum;
        }
    }
    Distribution::from_odds(new_dice_odds.into_iter().collect())
}

static EXPLODE_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func(DiceT, IntT) -> DiceT}));

fn explode_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut param_iter = params.into_iter();
    let dice = param_iter
        .next_as::<Distribution>()
        .ok_or_else(|| RuntimeError::partial("Explode invalid params"))?;
    let depth = param_iter
        .next_as::<i32>()
        .ok_or_else(|| RuntimeError::partial("Explode invalid params"))?;
    Ok(Box::new(explode_fn_inner(dice, depth).ok_or_else(
        || RuntimeError::partial("can't explode empty dice"),
    )?))
}

fn explode_fn_inner(dice: Distribution, depth: i32) -> Option<Distribution> {
    if depth <= 0 {
        return Some(dice);
    }

    let mut temp_dice = dice
        .values
        .iter()
        .map(|(roll, odds)| (Distribution::from_vec(vec![*roll]), *odds))
        .collect::<Vec<_>>();
    let (last_die, last_odds) = temp_dice.pop()?;
    temp_dice.push((explode_fn_inner(dice, depth - 1)? + last_die, last_odds));
    Some(mix_fn_inner(temp_dice))
}

static HIGHEST_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func([DiceT]) -> DiceT}));

fn highest_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let dice_arr = params
        .into_iter()
        .next_as::<Arr>()
        .ok_or_else(|| RuntimeError::partial("invalid highest fn params"))?;
    let dice = dice_arr
        .inner()
        .elements
        .iter()
        .map(|e| {
            e.downcast::<Distribution>()
                .ok_or_else(|| RuntimeError::partial("Invalid highest fn array contents"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Box::new(highest_lowest_fn_inner(&dice, true)))
}

static LOWEST_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func([DiceT]) -> DiceT}));

fn lowest_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let dice_arr = params
        .into_iter()
        .next_as::<Arr>()
        .ok_or_else(|| RuntimeError::partial("invalid lowest fn params"))?;
    let dice = dice_arr
        .inner()
        .elements
        .iter()
        .map(|e| {
            e.downcast::<Distribution>()
                .ok_or_else(|| RuntimeError::partial("Invalid lowest fn array contents"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Box::new(highest_lowest_fn_inner(&dice, false)))
}

fn highest_lowest_fn_inner(dice: &[Distribution], is_highest: bool) -> Distribution {
    if dice.len() == 1 {
        return dice[0].clone();
    }
    if dice.len() == 0 {
        return 0.into();
    }
    let halfway = dice.len() / 2;
    let left = highest_lowest_fn_inner(&dice[0..halfway], is_highest);
    let right = highest_lowest_fn_inner(&dice[halfway..dice.len()], is_highest);
    let mut new_odds = HashMap::new();
    for (lroll, lodds) in &left.values {
        for (rroll, rodds) in &right.values {
            let roll = if is_highest {
                *lroll.max(rroll)
            } else {
                *lroll.min(rroll)
            };
            let odds = *lodds * *rodds;
            if !new_odds.contains_key(&roll) {
                new_odds.insert(roll, 0.);
            }
            *new_odds.get_mut(&roll).unwrap() += odds;
        }
    }
    Distribution::from_odds(new_odds.into_iter().collect())
}

static HIGHEST_N_SIG: LazyLock<FuncT> =
    LazyLock::new(|| od_typedef!({func([DiceT], IntT) -> DiceT}));

fn highest_n_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut param_iter = params.into_iter();
    let dice_arr = param_iter
        .next_as::<Arr>()
        .ok_or_else(|| RuntimeError::partial("invalid highest fn params"))?;
    let n = param_iter
        .next_as::<i32>()
        .ok_or_else(|| RuntimeError::partial("invalid highest fn params"))?
        .try_into()
        .map_err(|_| RuntimeError::partial("highest_n n must be positive"))?;
    let dice = dice_arr
        .inner()
        .elements
        .iter()
        .map(|e| {
            e.downcast::<Distribution>()
                .ok_or_else(|| RuntimeError::partial("Invalid highest fn array contents"))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let res = dicemap_fn_inner(dice, |rolls| Ok(highest_lowest_n(rolls, n, true)))?;
    Ok(Box::new(res))
}

static LOWEST_N_SIG: LazyLock<FuncT> =
    LazyLock::new(|| od_typedef!({func([DiceT], IntT) -> DiceT}));

fn lowest_n_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut param_iter = params.into_iter();
    let dice_arr = param_iter
        .next_as::<Arr>()
        .ok_or_else(|| RuntimeError::partial("invalid highest fn params"))?;
    let n = param_iter
        .next_as::<i32>()
        .ok_or_else(|| RuntimeError::partial("invalid highest fn params"))?
        .try_into()
        .map_err(|_| RuntimeError::partial("highest_n n must be positive"))?;
    let dice = dice_arr
        .inner()
        .elements
        .iter()
        .map(|e| {
            e.downcast::<Distribution>()
                .ok_or_else(|| RuntimeError::partial("Invalid highest fn array contents"))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let res = dicemap_fn_inner(dice, |rolls| Ok(highest_lowest_n(rolls, n, false)))?;
    Ok(Box::new(res))
}

fn highest_lowest_n(nums: Vec<i32>, n: usize, is_highest: bool) -> i32 {
    let mut optima = VecDeque::new();
    for num in nums {
        let mut res_iter = optima.iter().enumerate();
        let mut found = false;
        while let Some((i, val)) = res_iter.next() {
            if (*val < num && is_highest) || (!is_highest && *val > num) {
                optima.insert(i, num);
                found = true;
                break;
            }
        }
        if !found {
            optima.push_back(num);
        }
        if optima.len() > n {
            optima.pop_back();
        }
    }
    optima.iter().sum::<i32>()
}

/* #endregion */
