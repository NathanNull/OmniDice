use std::{collections::HashMap, sync::LazyLock};

use crate::{
    interpreter::Interpreter,
    types::{BoxIterUtils, Datatype, Downcast, Ref, RefT, RustFunc, Value, Void, StringT},
};

pub static BUILTINS: LazyLock<HashMap<String, Value>> = LazyLock::new(|| {
    HashMap::from_iter([
        (
            "ref".to_string(),
            Box::new(RustFunc::new_const(ref_sig, ref_fn)) as Value,
        ),
        (
            "println".to_string(),
            Box::new(RustFunc::new_const(println_sig, println_fn)),
        ),
        (
            "error".to_string(),
            Box::new(RustFunc::new_const(error_sig, error_fn)),
        ),
        (
            "format".to_string(),
            Box::new(RustFunc::new_const(format_sig, format_fn)),
        ),
    ])
});

fn ref_sig(params: Vec<Datatype>) -> Option<Datatype> {
    if params.len() == 1 {
        Some(Box::new(RefT {
            ty: params[0].clone(),
        }))
    } else {
        None
    }
}

fn ref_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    if params.len() == 1 {
        Box::new(Ref::new(params[0].clone()))
    } else {
        unreachable!("Invalid function call")
    }
}

fn println_sig(params: Vec<Datatype>) -> Option<Datatype> {
    format_sig(params)
}

fn println_fn(params: Vec<Value>, i: &mut Interpreter) -> Value {
    let res = format_fn(params, i).downcast::<String>().unwrap();
    println!("{res}");
    Box::new(res)
}

fn error_sig(_params: Vec<Datatype>) -> Option<Datatype> {
    Some(Box::new(Void))
}

fn error_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
    panic!(
        "{}",
        params
            .into_iter()
            .map(|p| format!("{p}"))
            .collect::<Vec<_>>()
            .join(" ")
    );
}

fn format_sig(params: Vec<Datatype>) -> Option<Datatype> {
    if params.len() > 0 && params[0] == StringT {
        Some(Box::new(StringT))
    } else {
        None
    }
}

fn format_fn(params: Vec<Value>, _i: &mut Interpreter) -> Value {
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
