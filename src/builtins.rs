use std::{collections::HashMap, sync::LazyLock};

use crate::{
    invalid,
    types::{Datatype, Downcast, Ref, RefT, RustFunc, Value, Void, string::VString},
};

pub static BUILTINS: LazyLock<HashMap<String, Value>> = LazyLock::new(|| {
    HashMap::from_iter([
        (
            "ref".to_string(),
            Box::new(RustFunc::new(ref_ptoo, ref_fn)) as Value,
        ),
        (
            "println".to_string(),
            Box::new(RustFunc::new(println_ptoo, println_fn)),
        ),
        (
            "error".to_string(),
            Box::new(RustFunc::new(error_ptoo, error_fn)),
        ),
        (
            "format".to_string(),
            Box::new(RustFunc::new(format_ptoo, format_fn)),
        ),
    ])
});

fn ref_ptoo(params: Vec<Datatype>) -> Option<Datatype> {
    if params.len() == 1 {
        Some(Box::new(RefT {
            ty: params[0].clone(),
        }))
    } else {
        None
    }
}

fn ref_fn(params: Vec<Value>) -> Value {
    if params.len() == 1 {
        Box::new(Ref::new(params[0].clone()))
    } else {
        unreachable!("Invalid function call")
    }
}

fn println_ptoo(params: Vec<Datatype>) -> Option<Datatype> {
    format_ptoo(params)
}

fn println_fn(params: Vec<Value>) -> Value {
    let res = format_fn(params).downcast::<String>().unwrap();
    println!("{res}");
    Box::new(res)
}

fn error_ptoo(_params: Vec<Datatype>) -> Option<Datatype> {
    Some(Box::new(Void))
}

fn error_fn(params: Vec<Value>) -> Value {
    panic!(
        "{}",
        params
            .into_iter()
            .map(|p| format!("{p}"))
            .collect::<Vec<_>>()
            .join(" ")
    );
}

fn format_ptoo(params: Vec<Datatype>) -> Option<Datatype> {
    if params.len() > 0 && params[0] == VString {
        Some(Box::new(VString))
    } else {
        None
    }
}

fn format_fn(params: Vec<Value>) -> Value {
    let mut param_iter = params.clone().into_iter();
    if let Some(str) = param_iter.next().unwrap().downcast::<String>() {
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
        return Box::new(res);
    }
    invalid!("Call", "format", params);
}
