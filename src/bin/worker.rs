use std::{env, fs, path::Path};
use web_sys::{
    DedicatedWorkerGlobalScope, MessageEvent, console,
    js_sys::{self, Array},
    wasm_bindgen::{JsCast, JsValue, prelude::Closure},
};

use crate::interpreter::{cache, run_code};

mod interpreter;

pub fn main() {
    console_error_panic_hook::set_once();
    console::log_1(&"worker starting".into());
    let scope = DedicatedWorkerGlobalScope::from(JsValue::from(js_sys::global()));
    let scope_clone = scope.clone();
    let onmessage = Closure::wrap(Box::new(move |msg: MessageEvent| {
        // this is dumb
        let scope_clone = scope_clone.clone();
        let scope_clone_clone = scope_clone.clone();
        console::log_1(&"got message".into());

        let code = msg.data().as_string().expect("message to be a string");
        let res = run_code(
            &code,
            None,
            Box::new(move |out| {
                scope_clone_clone
                    .post_message(&Array::of2(&"result".into(), &out.into()))
                    .expect("posting result message succeeds")
            }),
        );
        match res {
            Ok(_) => {}
            Err(err) => {
                scope_clone
                    .post_message(&Array::of2(&"result".into(), &err.write(&code).into()))
                    .expect("posting result message succeeds");
            }
        }

        scope_clone
            .post_message(&Array::of1(&"ready".into()))
            .expect("posting result message succeeds");
    }) as Box<dyn Fn(MessageEvent)>);

    scope.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
    onmessage.forget();

    scope
        .post_message(&Array::of1(&"ready".into()).into())
        .expect("posting ready message succeeds");
}

#[allow(unused)]
fn basic_code_run() {
    let args = env::args().collect::<Vec<_>>();
    let filename = args.get(1).expect("Must pass file name to run");
    let code = fs::read_to_string(filename).expect("Couldn't read code file");

    let cache = cache::try_load_cache(&code, Path::new(filename));
    if cache.is_some() {
        println!("Using cached AST")
    }
    match interpreter::run_code(&code, cache, Box::new(|s| print!("{s}"))) {
        Ok(ast) => cache::write_cache(&code, Path::new(filename), ast),
        Err(err) => println!("{}", err.write(&code)),
    }
}
