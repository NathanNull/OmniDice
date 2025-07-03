use std::{env, fs};

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

mod builtins;
mod distribution;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod types;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let filename = args.get(1).expect("Must pass file name to run");
    println!("Filename is {filename}");
    let code = fs::read_to_string(filename).expect("Couldn't read code file");
    println!("Raw code: {code}");
    match Lexer::new(&code).lex() {
        Ok(tokens) => {
            println!("Tokens: {tokens:?}");
            let ast = Parser::new(tokens).parse();
            println!("AST: {ast}");
            println!("Program output:");
            Interpreter::new(ast).run();
        }
        Err(err) => println!("{err:?}"),
    }
}

#[derive(Debug, Clone)]
pub struct TokenWidth {
    width: usize,
    height: usize,
}

#[derive(Clone)]
pub struct TokenIter<I: std::fmt::Debug, T: Iterator<Item = (I, TokenWidth)>> {
    inner: T,
    peeked: Vec<I>,
    pos: (usize, usize),
}

impl<I: std::fmt::Debug, T: Iterator<Item = (I, TokenWidth)>> Iterator for TokenIter<I, T> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.pop() {
            Some(peeked)
        } else {
            let next = self.inner.next();
            if let Some(n) = next {
                self.step(n.1);
                Some(n.0)
            } else {
                None
            }
        }
    }
}

impl<I: std::fmt::Debug, T: Iterator<Item = (I, TokenWidth)>> TokenIter<I, T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner,
            peeked: vec![],
            pos: (1, 1),
        }
    }

    fn step(&mut self, w: TokenWidth) {
        if w.height != 0 {
            self.pos.0 += w.height;
                self.pos.1 = 0;
        }
        self.pos.1 += w.width;
    }

    pub fn peek(&mut self) -> Option<&I> {
        if self.peeked.is_empty() {
            if let Some(peeked) = self.inner.next() {
                self.step(peeked.1);
                self.peeked.push(peeked.0)
            }
        }
        self.peeked.last()
    }

    pub fn replace(&mut self, itm: I) {
        self.peeked.push(itm);
    }

    pub fn eat(&mut self, pattern: impl IntoIterator<Item = I>) -> Option<Vec<I>>
    where
        I: PartialEq,
    {
        let mut removed = vec![];
        for itm in pattern {
            if let Some(n) = self.next() {
                if n != itm {
                    self.replace(n);
                    for r in removed.into_iter().rev() {
                        self.replace(r);
                    }
                    return None;
                }
                removed.push(n)
            } else {
                for r in removed.into_iter().rev() {
                    self.replace(r);
                }
                return None;
            }
        }
        Some(removed)
    }

    pub fn expect(&mut self, ele: I)
    where
        I: PartialEq,
    {
        match self.next() {
            Some(next) if next == ele => (),
            Some(next) => panic!("Expected {ele:?}, found {next:?}"),
            None => panic!("Unexpected EOF"),
        }
    }
}

impl<T: Iterator<Item = (char, TokenWidth)>> TokenIter<char, T> {
    pub fn eat_str(&mut self, str: &str) -> bool {
        self.eat(str.chars()).is_some()
    }
}
