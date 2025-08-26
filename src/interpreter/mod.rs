pub mod builtins;
pub mod distribution;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod tokeniter;
pub mod types;

use itertools::Itertools;
use std::{fmt::Display, usize};

use {
    error::{LexError, LineIndex, ParseError, RuntimeError},
    interpreter::Interpreter,
    lexer::Lexer,
    parser::Expr,
    parser::Parser,
    tokeniter::TokenIter,
};

const PRINT_TOKENS: bool = false;
const PRINT_AST: bool = true;

pub enum InterpreterError {
    Lex(LexError),
    Parse(ParseError),
    Runtime(RuntimeError),
}

impl InterpreterError {
    pub fn write(&self, code: &str) -> String {
        match self {
            InterpreterError::Lex(lex_error) => write_err(lex_error, lex_error.location, code),
            InterpreterError::Parse(parse_error) => {
                write_err(parse_error, parse_error.location, code)
            }
            InterpreterError::Runtime(runtime_error) => write_err(
                runtime_error,
                match runtime_error.base_pos() {
                    Some(p) => p,
                    None => LineIndex(usize::MAX, usize::MAX),
                },
                code,
            ),
        }
    }
}

fn write_err<T: Display>(err: &T, pos: LineIndex, code: &str) -> String {
    format!(
        "\n\n{}\n{}{}\n{err}",
        code.lines()
            .nth(pos.0 - 1) // Good old off-by-one due to indexing differences
            .expect(&format!("Error past the last line of code: {pos:?} ({err})")),
        " ".repeat(pos.1 - 1),
        "^ Error happened here",
    )
}

pub fn run_code(code: &str, cache: Option<Box<Expr>>, output: Box<dyn Fn(&str)>) -> Result<Box<Expr>, InterpreterError> {
    // Use the cached AST if the code's hash matches the old one
    let ast = match cache {
        Some(ast) => ast,
        None => match parse(&code) {
            Ok(ret) => ret,
            Err(e) => return Err(e),
        },
    };

    (output)("Program output:\n");
    match Interpreter::new(*ast.clone(), output).run() {
        Ok(_) => Ok(ast),
        Err(err) => Err(InterpreterError::Runtime(err)),
    }
}

fn parse(code: &str) -> Result<Box<Expr>, InterpreterError> {
    let tokens = match Lexer::new(code).lex() {
        Ok(tokens) => tokens,
        Err(e) => return Err(InterpreterError::Lex(e)),
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
        Err(e) => return Err(InterpreterError::Parse(e)),
    };

    if PRINT_AST {
        println!("AST: {ast}\n");
    }

    Ok(ast)
}
