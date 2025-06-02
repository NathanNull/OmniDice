use interpreter::{Interpreter, Value};
use lexer::Lexer;
use parser::Parser;

mod distribution;
mod interpreter;
mod lexer;
mod parser;

fn main() {
    let code = "(2d6+3)";
    let tokens = Lexer::new(code).lex();
    let ast = Parser::new(tokens.clone()).parse();
    let output = Interpreter::new(ast.clone()).run();
    println!("{code}");
    println!("{tokens:?}");
    println!("{ast:?}");
    println!("{output}");
}
