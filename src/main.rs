use std::fs;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

mod distribution;
mod interpreter;
mod lexer;
mod parser;
mod types;

fn main() {
    let code = fs::read_to_string("./code.od").expect("Couldn't read code file");
    println!("{code}");
    let tokens = Lexer::new(&code).lex();
    println!("{tokens:?}");
    let mut parser = Parser::new(tokens.clone());
    let (ast, var_t_dump) = parser.parse();
    println!("{ast:?}");
    println!("{var_t_dump:?}");
    let mut interpreter = Interpreter::new(ast);
    let (output, vardump) = interpreter.run();
    println!("Output: {output}");
    println!("Vardump: {vardump:?}");
}
