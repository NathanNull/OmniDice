use std::fs;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

mod builtins;
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
    println!("Output ({}):\n{}", output.get_type(), output);
}

#[derive(Clone)]
pub struct TokenIter<T: Iterator>
where
    T::Item: std::fmt::Debug,
{
    inner: T,
    peeked: Vec<T::Item>,
}

impl<T: Iterator> Iterator for TokenIter<T>
where
    T::Item: std::fmt::Debug,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.pop() {
            Some(peeked)
        } else {
            self.inner.next()
        }
    }
}

impl<T: Iterator> TokenIter<T>
where
    T::Item: std::fmt::Debug,
{
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

    pub fn eat(&mut self, pattern: impl IntoIterator<Item = T::Item>) -> Option<Vec<T::Item>>
    where
        T::Item: PartialEq,
    {
        let mut removed = vec![];
        for itm in pattern {
            let next = self.next();
            if let Some(n) = next {
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

    pub fn expect(&mut self, ele: T::Item)
    where
        T::Item: PartialEq,
    {
        match self.next() {
            Some(next) if next == ele => (),
            Some(next) => panic!("Expected {ele:?}, found {next:?}"),
            None => panic!("Unexpected EOF"),
        }
    }
}

impl<T: Iterator<Item = char>> TokenIter<T> {
    pub fn eat_str(&mut self, str: &str) -> bool {
        self.eat(str.chars()).is_some()
    }
}
