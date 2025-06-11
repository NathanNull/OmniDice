use std::{collections::HashMap, sync::LazyLock};

use crate::{
    gen_fn_map,
    interpreter::Interpreter,
    invalid,
    types::{
        BoxIterUtils, Datatype, Downcast, Func, FuncT, Iter, IterT, Maybe, MaybeT, Ref, RefT,
        RustFunc, StringT, Val, Value, Void,
    },
};

gen_fn_map!(
    BUILTIN_FUNCS,
    ("ref", ref_sig, ref_fn),
    ("println", println_sig, println_fn),
    ("error", error_sig, error_fn),
    ("format", format_sig, format_fn),
    ("filled", filled_sig, filled_fn),
    ("iter", iter_sig, iter_fn),
    ("null", null_sig, null_fn)
);

pub static BUILTINS: LazyLock<HashMap<String, Value>> = LazyLock::new(|| {
    HashMap::from_iter(BUILTIN_FUNCS.iter().map(|(name, (sig, func))| {
        (
            name.to_string(),
            Box::new(RustFunc::new_const(*sig, *func)) as Value,
        )
    }))
});

fn ref_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
    if params.len() == 1 {
        Some(Box::new(RefT {
            ty: params[0].clone(),
        }))
    } else {
        None
    }
}

fn ref_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    if params.len() == 1 {
        Box::new(Ref::new(params[0].clone()))
    } else {
        unreachable!("Invalid function call")
    }
}

fn println_sig(params: Vec<Datatype>, o: Option<Datatype>) -> Option<Datatype> {
    format_sig(params, o)
}

fn println_fn(params: Vec<Value>, i: &mut Interpreter, o: Option<Datatype>) -> Value {
    let res = format_fn(params, i, o).downcast::<String>().unwrap();
    println!("{res}");
    Box::new(res)
}

fn error_sig(_params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
    Some(Box::new(Void))
}

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

fn format_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
    if params.len() > 0 && params[0] == StringT {
        Some(Box::new(StringT))
    } else {
        None
    }
}

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

fn filled_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
    if params.len() == 1 {
        Some(Box::new(MaybeT {
            output: params[0].clone(),
        }))
    } else {
        None
    }
}

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

fn null_sig(params: Vec<Datatype>, o: Option<Datatype>) -> Option<Datatype> {
    if params.len() == 0 {
        if let Some(ret) = o.and_then(|o|o.downcast::<MaybeT>()) {
            return Some(Box::new(ret))
        }
    }
    None
}

fn null_fn(params: Vec<Value>, _i: &mut Interpreter, o: Option<Datatype>) -> Value {
    if params.len() == 0 {
        if let Some(ret) = o.and_then(|o|o.downcast::<MaybeT>()) {
            return Box::new(Maybe {
                output: ret.output,
                contents: None
            })
        }
    }
    invalid!("Call", "null", params)
}

fn iter_sig(params: Vec<Datatype>, _o: Option<Datatype>) -> Option<Datatype> {
    if params.len() == 1 {
        let func = params.first().and_then(|p| p.downcast::<FuncT>())?;
        if func.params.0.len() > 0 {
            return None;
        }
        let output = func.output.downcast::<MaybeT>()?.output;
        Some(Box::new(IterT { output }))
    } else {
        None
    }
}

fn iter_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    if params.len() == 1 {
        let func = params
            .first()
            .and_then(|p| p.downcast::<Func>())
            .expect("Invalid function call");
        if func.params.0.len() > 0 {
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
