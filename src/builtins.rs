use std::{collections::HashMap, sync::LazyLock};

use crate::types::{Datatype, Ref, RefT, RustFunc, Value, Void};

pub static BUILTINS: LazyLock<HashMap<String, Value>> = LazyLock::new(|| {
    HashMap::from_iter([(
        "ref".to_string(),
        Box::new(RustFunc::new(ref_ptoo, ref_fn)) as Value,
    ), (
        "println".to_string(),
        Box::new(RustFunc::new(println_ptoo, println_fn))
    )])
});

fn ref_ptoo(params: Vec<Datatype>) -> Option<Datatype> {
    if params.len() == 1 {
        Some(Box::new(RefT {ty: params[0].clone()}))
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

fn println_ptoo(_params: Vec<Datatype>) -> Option<Datatype> {
    Some(Box::new(Void))
}

fn println_fn(params: Vec<Value>) -> Value {
    println!("{}", params.into_iter().map(|p|format!("{p}")).collect::<Vec<_>>().join(" "));
    Box::new(Void)
}