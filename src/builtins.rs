use std::{collections::HashMap, sync::LazyLock};

use crate::{
    gen_fn_map,
    interpreter::Interpreter,
    invalid,
    types::{
        BoxIterUtils, Datatype, Downcast, Func, FuncT, Iter, IterT, Maybe, MaybeT, Ref, RefT,
        StringT, TypeVar, Val, Value, Void,
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
    ("null", NULL_SIG, null_fn)
);

pub static BUILTINS: LazyLock<HashMap<String, Value>> = LazyLock::new(|| {
    HashMap::from_iter(BUILTIN_FUNCS.iter().map(|(name, (sig, func))| {
        (
            name.to_string(),
            Box::new(sig.clone().make_rust(*func)) as Value,
        )
    }))
});

// fn ref_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     if params.len() == 1 {
//         Some(Box::new(RefT {
//             ty: params[0].clone(),
//         }))
//     } else {
//         None
//     }
// }
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

// fn println_sig(params: Vec<Datatype>, o: Option<Datatype>) -> Option<Datatype> {
//     format_sig(params, o)
// }
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

// fn error_sig(_params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     Some(Box::new(Void))
// }

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
    let mut format_args = param_iter.map(|p| format!("{p}"));
    let mut str_pieces = str.split("{}");
    let mut res = "".to_string();
    while let Some(next_str_piece) = str_pieces.next() {
        res += next_str_piece;
        match format_args.next() {
            Some(arg) => res += &arg,
            None if str_pieces.next().is_none() => break,
            None => panic!("Not enough format args"),
        }
    }
    Box::new(res)
}

// fn filled_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     if params.len() == 1 {
//         Some(Box::new(MaybeT {
//             output: params[0].clone(),
//         }))
//     } else {
//         None
//     }
// }

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

// fn null_sig(params: Vec<Datatype>, o: Option<Datatype>) -> Option<Datatype> {
//     if params.len() == 0 {
//         if let Some(ret) = o.and_then(|o| o.downcast::<MaybeT>()) {
//             return Some(Box::new(ret));
//         }
//     }
//     None
// }

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

// fn iter_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
//     if params.len() == 1 {
//         let func = params.first().and_then(|p| p.downcast::<FuncT>())?;
//         if func.params.0.len() > 0 {
//             return None;
//         }
//         let output = func.output.downcast::<MaybeT>()?.output;
//         Some(Box::new(IterT { output }))
//     } else {
//         None
//     }
// }

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
        Box::new(Iter {
            output,
            next_fn: func.dup(),
        })
    } else {
        invalid!("Call", "iter", params);
    }
}
