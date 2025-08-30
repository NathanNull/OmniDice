use leptos::prelude::*;
use leptos_meta::{provide_meta_context, Style};
use std::{env, fs, path::Path};

mod cache;
mod interpreter;
mod app;

use app::App;

fn main() {
    provide_meta_context();

    mount_to_body(|| view! {
        <Style id="leptos">{include_str!("../index.css")}</Style>
        <App />
    });

    _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();
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
    match interpreter::run_code(&code, cache, Box::new(|s|print!("{s}"))) {
        Ok(ast) => cache::write_cache(&code, Path::new(filename), ast),
        Err(err) => println!("{}", err.write(&code)),
    }
}
