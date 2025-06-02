use std::fs;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

mod distribution;
mod interpreter;
mod lexer;
mod parser;

fn main() {
    let code = fs::read_to_string("./code.od").expect("Couldn't read code file");
    let tokens = Lexer::new(&code).lex();
    let ast = Parser::new(tokens.clone()).parse();
    let output = Interpreter::new(ast.clone()).run();
    println!("{code}");
    println!("{tokens:?}");
    println!("{ast:?}");
    println!("{output}");
}
