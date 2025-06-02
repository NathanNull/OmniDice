use std::{fmt::Debug, vec::IntoIter};

use crate::lexer::{Op, Token, TokenString};

#[derive(Clone, Debug)]
pub enum Program {
    Expr(Expr)
}

#[derive(Clone)]
pub enum Expr {
    Identifier(String),
    Number(Number),
    Binop(Binop),
    Prefix(Prefix),
    Postfix(Postfix),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::Number(num) => write!(f, "{:?}", num),
            Self::Binop(Binop { op, lhs, rhs }) => write!(f, "({op:?} {lhs:?} {rhs:?})"),
            Self::Prefix(Prefix { prefix, rhs }) => write!(f, "({prefix:?} {rhs:?})"),
            Self::Postfix(Postfix { postfix, lhs }) => write!(f, "({postfix:?} {lhs:?})"),
        }
    }
}

#[derive(Clone)]
pub enum Number {
    Float(f32),
    Int(i32),
}

impl Debug for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Int(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binop {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: Op,
}

#[derive(Debug, Clone)]
pub struct Prefix {
    pub prefix: Op,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Postfix {
    pub postfix: Op,
    pub lhs: Box<Expr>,
}

pub struct Parser {
    tokens: IntoIter<Token>,
    peeked: Option<Token>,
}

impl Parser {
    pub fn new(tokens: TokenString) -> Self {
        Self {
            tokens: tokens.tokens.into_iter(),
            peeked: None,
        }
    }

    fn peek(&mut self) -> Token {
        if self.peeked.is_none() {
            self.peeked = self.tokens.next()
        }
        self.peeked.clone().unwrap()
    }
    fn next(&mut self) -> Token {
        if self.peeked.is_none() {
            self.tokens.next().unwrap()
        } else {
            let res = self.peeked.clone().unwrap();
            self.peeked = None;
            res
        }
    }

    pub fn parse(&mut self) -> Program {
        Program::Expr(self.parse_expr(0))
    }

    fn parse_expr(&mut self, min_bp: u8) -> Expr {
        let mut lhs = match self.next() {
            Token::Identifier(id) => Expr::Identifier(id),
            Token::Float(f) => Expr::Number(Number::Float(f)),
            Token::Int(i) => Expr::Number(Number::Int(i)),
            Token::Op(op) => {
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = Box::new(self.parse_expr(r_bp));
                Expr::Prefix(Prefix { prefix: op, rhs })
            }
            Token::LBracket => {
                let lhs = self.parse_expr(0);
                assert_eq!(self.next(), Token::RBracket, "Expected right bracket");
                lhs
            }
            tk => panic!("Expected literal or ident, found {tk:?}"),
        };
        loop {
            let op = match self.peek() {
                Token::EOF | Token::RBracket => break,
                Token::Op(op) => op,
                tk => panic!("Expected operation, found {tk:?}"),
            };
            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.next();
                lhs = Expr::Postfix(Postfix {
                    postfix: op,
                    lhs: Box::new(lhs),
                });
                continue;
            }
            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }
            self.next();
            let rhs = self.parse_expr(r_bp);
            lhs = Expr::Binop(Binop {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            });
        }
        lhs
    }
}

fn infix_binding_power(op: Op) -> (u8, u8) {
    match op {
        Op::Plus | Op::Minus => (1, 2),
        Op::Times | Op::Divided => (3, 4),
        Op::D => (5, 6),
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Minus => ((), 7),
        _ => panic!("Invalid prefix operator: {op:?}"),
    }
}

// TODO: see if there are even any good postfix operators
// Decrement/increment maybe
// Deal with this once I add actual variable support (or an interpreter at all for that matter)
fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    match op {
        _ => None,
    }
}
