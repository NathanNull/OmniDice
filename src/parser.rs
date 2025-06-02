use std::{fmt::Debug, vec::IntoIter};

use crate::lexer::{Op, Token, TokenString};

#[derive(Clone, Debug)]
pub enum Program {
    Scope(Scope),
}

pub type Scope = Vec<Expr>;

#[derive(Clone)]
pub enum Expr {
    Identifier(String),
    Literal(Literal),
    Binop(Binop),
    Prefix(Prefix),
    Postfix(Postfix),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::Literal(num) => write!(f, "{:?}", num),
            Self::Binop(Binop { op, lhs, rhs }) => write!(f, "(b{op:?} {lhs:?} {rhs:?})"),
            Self::Prefix(Prefix { prefix, rhs }) => write!(f, "(pr{prefix:?} {rhs:?})"),
            Self::Postfix(Postfix { postfix, lhs }) => write!(f, "(po{postfix:?} {lhs:?})"),
        }
    }
}

#[derive(Clone)]
pub enum Literal {
    Float(f32),
    Int(i32),
    Void,
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Int(i) => write!(f, "{i}"),
            Self::Void => write!(f, "()"),
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
        Program::Scope(self.parse_scope())
    }

    fn parse_scope(&mut self) -> Scope {
        let mut exprs = vec![];
        let mut just_parsed = false;
        while self.peek() != Token::EOF {
            exprs.push(self.parse_expr(0));
            just_parsed = true;
            assert!(
                [Token::EOL, Token::EOF].contains(&self.peek()),
                "Expected semicolon or EOF, found {:?}",
                self.peek()
            );
            while self.peek() == Token::EOL {
                self.next();
                just_parsed = false;
            }
        }
        if !just_parsed {
            exprs.push(Expr::Literal(Literal::Void));
        }
        exprs
    }

    fn parse_expr(&mut self, min_bp: u8) -> Expr {
        let mut lhs = match self.next() {
            Token::Identifier(id) => Expr::Identifier(id),
            Token::Float(f) => Expr::Literal(Literal::Float(f)),
            Token::Int(i) => Expr::Literal(Literal::Int(i)),
            Token::Op(op) => {
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = Box::new(self.parse_expr(r_bp));
                Expr::Prefix(Prefix { prefix: op, rhs })
            }
            Token::LBracket => {
                if self.peek() == Token::RBracket { // ()
                    self.next();
                    Expr::Literal(Literal::Void)
                } else {
                    let lhs = self.parse_expr(0);
                    let next = self.next();
                    assert_eq!(
                        next,
                        Token::RBracket,
                        "Expected right bracket, found {next:?}"
                    );
                    lhs
                }
            }
            tk => panic!("Expected literal or ident, found {tk:?}"),
        };
        loop {
            let op = match self.peek() {
                Token::EOF | Token::EOL | Token::RBracket => break,
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
        Op::Plus | Op::Minus => (3, 4),
        Op::Times | Op::Divided => (5, 6),
        Op::D => (7, 8),
        Op::Assign => (1, 2),
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Minus => ((), 9),
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
