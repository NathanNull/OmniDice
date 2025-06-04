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
    println!("Raw code: {code}");
    let tokens = Lexer::new(&code).lex();
    println!("Tokens: {tokens:?}");
    let ast = Parser::new(tokens).parse();
    println!("AST: {ast}");
    let output = Interpreter::new(ast).run();
    println!("Output: {output}");
}

pub struct Peekable<T: Iterator> {
    inner: T,
    peeked: Vec<T::Item>,
}

impl<T: Iterator> Iterator for Peekable<T> {
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.pop() {
            Some(peeked)
        } else {
            self.inner.next()
        }
    }
}

impl<T: Iterator> Peekable<T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner,
            peeked: vec![],
        }
    }

    pub fn peek(&mut self) -> Option<&T::Item> {
        if self.peeked.is_empty() {
            if let Some(peeked) = self.inner.next() {
                self.peeked.push(peeked)
            }
        }
        self.peeked.last()
    }

    pub fn replace(&mut self, itm: T::Item) {
        self.peeked.push(itm);
    }
}
