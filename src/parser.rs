use std::{collections::HashMap, fmt::Debug, vec::IntoIter};

use strum::EnumIter;

use crate::{
    lexer::{OpToken, Token, TokenString},
    types::{Datatype, OPS, PROPERTIES},
};

#[derive(PartialEq, Eq, Hash, Clone, Copy, EnumIter)]
pub enum OpType {
    Infix,
    Prefix,
    Postfix,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, EnumIter)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Divided,
    D,
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Divided => "/",
            Self::D => "d",
        };
        write!(f, "{c}")
    }
}

impl From<OpToken> for Op {
    fn from(value: OpToken) -> Self {
        match value {
            OpToken::Plus => Self::Plus,
            OpToken::Minus => Self::Minus,
            OpToken::Times => Self::Times,
            OpToken::Divided => Self::Divided,
            OpToken::D => Self::D,
            OpToken::Assign | OpToken::Access => panic!("Invalid operation {value:?}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Program {
    Scope(Scope),
}

pub type Scope = Vec<Expr>;

#[derive(Clone)]
pub enum ExprContents {
    Literal(Literal),
    Binop(Binop),
    Prefix(Prefix),
    Postfix(Postfix),
    // TODO: potential postfix ideas:
    // An operator that makes dice crit or explode
    // (write more as I think of them)
    Assign(Assign),
    Accessor(Accessor),
}

#[derive(Clone)]
pub struct Expr {
    pub contents: ExprContents,
    pub output: Datatype,
}

impl Debug for ExprContents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(num) => write!(f, "{:?}", num),
            Self::Binop(Binop { op, lhs, rhs }) => write!(f, "(b{op:?} {lhs:?} {rhs:?})"),
            Self::Prefix(Prefix { prefix, rhs }) => write!(f, "(pr{prefix:?} {rhs:?})"),
            Self::Postfix(Postfix { postfix, lhs }) => write!(f, "(po{postfix:?} {lhs:?})"),
            Self::Assign(Assign { assignee, val }) => write!(f, "(set {assignee:?} {val:?})"),
            Self::Accessor(acc) => write!(f, "{acc:?}"),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.contents.fmt(f)
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

impl Literal {
    pub fn val_type(&self) -> Datatype {
        match self {
            Self::Float(_) => Datatype::Float,
            Self::Int(_) => Datatype::Int,
            Self::Void => Datatype::Void,
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

#[derive(Debug, Clone)]
pub struct Assign {
    pub assignee: Accessor,
    pub val: Box<Expr>,
}

#[derive(Clone)]
pub enum Accessor {
    Variable(String),
    Property(Box<Expr>, String),
}

impl Debug for Accessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(name) => write!(f, "{name}"),
            Self::Property(accessor, name) => write!(f, "{accessor:?}.{name}"),
        }
    }
}

pub struct Parser {
    tokens: IntoIter<Token>,
    peeked: Option<Token>,
    var_types: HashMap<String, Datatype>,
}

const VOID: Expr = Expr {
    contents: ExprContents::Literal(Literal::Void),
    output: Datatype::Void,
};

impl Parser {
    pub fn new(tokens: TokenString) -> Self {
        Self {
            tokens: tokens.tokens.into_iter(),
            peeked: None,
            var_types: HashMap::new(),
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

    fn new_expr(&mut self, contents: ExprContents) -> Expr {
        let output = self.decide_type(&contents);
        if let ExprContents::Assign(Assign {
            assignee: Accessor::Variable(var),
            val: _,
        }) = &contents
        {
            self.var_types.insert(var.clone(), output);
        }
        Expr { output, contents }
    }

    fn decide_type(&self, contents: &ExprContents) -> Datatype {
        match contents {
            ExprContents::Accessor(acc) => match acc {
                Accessor::Variable(var) => *self
                    .var_types
                    .get(var)
                    .expect(&format!("Unknown variable {var}")),
                Accessor::Property(base, prop) => {
                    let ty = self.decide_type(&base.contents);
                    *PROPERTIES
                        .get(&(ty, prop.clone()))
                        .expect(&format!("Invalid property {prop} for type {ty:?}"))
                }
            },
            ExprContents::Literal(literal) => literal.val_type(),
            ExprContents::Binop(binop) => *OPS
                .get(&(
                    self.decide_type(&binop.lhs.contents),
                    self.decide_type(&binop.rhs.contents),
                    binop.op,
                    OpType::Infix,
                ))
                .unwrap(),
            ExprContents::Prefix(prefix) => *OPS
                .get(&(
                    Datatype::Void,
                    self.decide_type(&prefix.rhs.contents),
                    prefix.prefix,
                    OpType::Prefix,
                ))
                .unwrap(),
            ExprContents::Postfix(postfix) => *OPS
                .get(&(
                    self.decide_type(&postfix.lhs.contents),
                    Datatype::Void,
                    postfix.postfix,
                    OpType::Postfix,
                ))
                .unwrap(),
            ExprContents::Assign(assign) => self.decide_type(&assign.val.contents),
        }
    }

    pub fn parse(&mut self) -> (Program, &HashMap<String, Datatype>) {
        (Program::Scope(self.parse_scope()), &self.var_types)
    }

    fn parse_scope(&mut self) -> Scope {
        let mut exprs = vec![];
        let mut just_parsed = false;
        while self.peek() != Token::EOF {
            let contents = self.parse_expr(0);
            exprs.push(self.new_expr(contents));
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
            exprs.push(VOID);
        }
        exprs
    }

    fn parse_expr(&mut self, min_bp: u8) -> ExprContents {
        let mut lhs = match self.next() {
            Token::Identifier(id) => ExprContents::Accessor(Accessor::Variable(id)),
            Token::Float(f) => ExprContents::Literal(Literal::Float(f)),
            Token::Int(i) => ExprContents::Literal(Literal::Int(i)),
            Token::Op(op) => {
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expr(r_bp);
                self.make_expr(VOID.contents, rhs, op, OpType::Prefix)
            }
            Token::LBracket => {
                if self.peek() == Token::RBracket {
                    // ()
                    self.next();
                    VOID.contents
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
                lhs = self.make_expr(lhs, VOID.contents, op, OpType::Postfix);
                continue;
            }
            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }
            self.next();
            let rhs = self.parse_expr(r_bp);
            lhs = self.make_expr(lhs, rhs, op, OpType::Infix);
        }
        lhs
    }

    fn make_expr(
        &mut self,
        lhs: ExprContents,
        rhs: ExprContents,
        op: OpToken,
        op_type: OpType,
    ) -> ExprContents {
        match (op, op_type) {
            (
                OpToken::Plus | OpToken::Minus | OpToken::Times | OpToken::Divided | OpToken::D,
                OpType::Infix,
            ) => ExprContents::Binop(Binop {
                lhs: Box::new(self.new_expr(lhs)),
                rhs: Box::new(self.new_expr(rhs)),
                op: op.into(),
            }),
            (OpToken::Minus, OpType::Prefix) => ExprContents::Prefix(Prefix {
                prefix: op.into(),
                rhs: Box::new(self.new_expr(rhs)),
            }),
            // "d6" expands to "1d6"
            (OpToken::D, OpType::Prefix) => ExprContents::Binop(Binop {
                lhs: Box::new(self.new_expr(ExprContents::Literal(Literal::Int(1)))),
                rhs: Box::new(self.new_expr(rhs)),
                op: op.into(),
            }),
            (OpToken::Assign, OpType::Infix) => {
                let val = Box::new(self.new_expr(rhs));
                let assignee = match lhs {
                    ExprContents::Accessor(acc) => acc,
                    _ => panic!("Expected accessor, found {lhs:?}"),
                };
                match &assignee {
                    Accessor::Variable(_) => {}
                    Accessor::Property(base, prop) => {
                        assert_eq!(
                            Some(&val.output),
                            PROPERTIES.get(&(base.output, prop.clone()))
                        )
                    }
                }
                ExprContents::Assign(Assign { assignee, val })
            }
            (OpToken::Access, OpType::Infix) => {
                let rhs = match rhs {
                    ExprContents::Accessor(Accessor::Variable(var)) => var,
                    _ => panic!("Expected property, found {rhs:?}"),
                };
                ExprContents::Accessor(Accessor::Property(Box::new(self.new_expr(lhs)), rhs))
            }
            (
                OpToken::Plus
                | OpToken::Times
                | OpToken::Divided
                | OpToken::Assign
                | OpToken::Access,
                OpType::Prefix,
            ) => {
                unreachable!("Invalid prefix operation {op:?}")
            }
            (_, OpType::Postfix) => unreachable!("Invalid postfix operation {op:?}"),
        }
    }
}

fn infix_binding_power(op: OpToken) -> (u8, u8) {
    match op {
        OpToken::Assign => (2, 1),
        OpToken::Plus | OpToken::Minus => (3, 4),
        OpToken::Times | OpToken::Divided => (5, 6),
        OpToken::Access => (7, 8),
        OpToken::D => (9, 10),
    }
}

fn prefix_binding_power(op: OpToken) -> ((), u8) {
    match op {
        OpToken::Minus => ((), 11),
        OpToken::D => ((), 10),
        _ => panic!("Invalid prefix operator: {op:?}"),
    }
}

// TODO: see if there are even any good postfix operators
// Decrement/increment maybe, but then those really are just += 1 and -= 1...
fn postfix_binding_power(op: OpToken) -> Option<(u8, ())> {
    match op {
        _ => None,
    }
}
