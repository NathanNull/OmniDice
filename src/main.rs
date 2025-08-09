use itertools::Itertools;
use std::{env, fmt::Display, fs};

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use tokeniter::TokenIter;

use crate::error::LineIndex;

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

    let tokens = match Lexer::new(&code).lex() {
        Ok(tokens) => tokens,
        Err(err) => {
            write_err(&err, err.location, &code);
            return;
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
            return;
        }
    };

    if PRINT_AST {
        println!("AST: {ast}\n");
    }

    println!("Program output:");
    match Interpreter::new(*ast).run() {
        Ok(_) => (),
        Err(err) => {
            write_err(&err, err.base_pos().expect("Positionless error"), &code);
            return;
        }
    }
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
