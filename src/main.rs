use itertools::Itertools;
use serde_cbor::{from_slice, to_vec};
use sha2::{Digest, Sha256};
use std::{
    env,
    fmt::Display,
    fs,
    io::{Read, Write},
    path::Path,
};

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use tokeniter::TokenIter;

use crate::{error::LineIndex, parser::Expr};

mod builtins;
mod distribution;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod tokeniter;
mod types;

const PRINT_TOKENS: bool = false;
const PRINT_AST: bool = true;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let filename = args.get(1).expect("Must pass file name to run");

    let code = fs::read_to_string(filename).expect("Couldn't read code file");
    let mut hasher = Sha256::new();
    hasher.update(code.as_bytes());
    let new_hash = hasher.finalize().to_vec();

    let mut filepath = Path::new(filename)
        .canonicalize()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    filepath.push("_compiled");
    let mut filename = Path::new(filename).file_stem().unwrap().to_os_string();
    filename.push(".odc");
    filepath.push(filename);

    // Use the cached AST if the code's hash matches the old one
    let ast = match fs::File::open(filepath.clone()) {
        Ok(mut file) => {
            let mut old_hash = [0u8; 32];
            file.read_exact(&mut old_hash)
                .expect("Couldn't read cached code hash");
            if old_hash == *new_hash {
                println!("Using cached program");
                let mut buf = vec![];
                file.read_to_end(&mut buf)
                    .expect("Couldn't read cached AST");
                match from_slice(&buf) {
                    Ok(ret) => ret,
                    Err(err) => panic!("Failed to deserialize: {err:#?}"),
                }
            } else {
                match parse(&code, new_hash, &filepath) {
                    Some(ret) => ret,
                    None => return,
                }
            }
        }
        Err(_) => match parse(&code, new_hash, &filepath) {
            Some(ret) => ret,
            None => return,
        },
    };

    println!("Program output:");
    match Interpreter::new(*ast).run() {
        Ok(_) => (),
        Err(err) => {
            write_err(
                &err,
                err.base_pos()
                    .expect(&format!("Positionless error: {:?}", err.info())),
                &code,
            );
            return;
        }
    }
}

fn parse(code: &str, code_hash: Vec<u8>, cache_path: &Path) -> Option<Box<Expr>> {
    let tokens = match Lexer::new(code).lex() {
        Ok(tokens) => tokens,
        Err(err) => {
            write_err(&err, err.location, &code);
            return None;
        }
    };

    if PRINT_TOKENS {
        let mut tk_iter = TokenIter::new(tokens.iter().cloned());
        let mut posns = vec![];
        while let Some(_) = tk_iter.next() {
            posns.push(tk_iter.pos);
        }
        println!(
            "Tokens: [\n\t{}\n]\n",
            tokens
                .iter()
                .zip(posns)
                .map(|(t, pos)| format!("({}, {} @ {:?})", t.0, t.1, pos))
                .join("\n\t")
        );
    }

    let ast = match Parser::new(tokens).parse() {
        Ok(ast) => ast,
        Err(err) => {
            write_err(&err, err.location, &code);
            return None;
        }
    };

    if PRINT_AST {
        println!("AST: {ast}\n");
    }

    match cache_path.parent() {
        Some(parent) => fs::create_dir_all(parent).expect("Directory creation shouldn't fail"),
        None => (),
    }
    match fs::File::create(cache_path) {
        Ok(mut file) => {
            file.write_all(&code_hash)
                .expect("File writing shouldn't fail");
            file.write_all(
                &to_vec(&ast)
                    .map_err(|e| e.to_string())
                    .expect("Serialization shouldn't fail"),
            )
            .expect("File writing shouldn't fail");
        }
        Err(_) => println!("Couldn't cache AST"),
    }

    Some(ast)
}

fn write_err<T: Display>(err: &T, pos: LineIndex, code: &str) {
    println!(
        "\n\n{}\n{}{}\n{err}",
        code.lines()
            .nth(pos.0 - 1) // Good old off-by-one due to indexing differences
            .expect("Error past the last line of code"),
        " ".repeat(pos.1 - 1),
        "^ Error happened here",
    )
}