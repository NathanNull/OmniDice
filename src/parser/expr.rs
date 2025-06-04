use std::fmt::{Debug, Display};

use strum::EnumIter;

use crate::{lexer::OpToken, types::{Datatype, Value}};

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

pub type Scope = Vec<Expr>;

#[derive(Clone)]
pub enum ExprContents {
    Literal(Value),
    Binop(Binop),
    Prefix(Prefix),
    Postfix(Postfix),
    // TODO: potential postfix ideas:
    // An operator that makes dice crit or explode (!)
    // (write more as I think of them)
    Assign(Assign),
    Accessor(Accessor),
    Scope(Scope),
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
            Self::Scope(scope) => write!(f, "{{{scope:?}}}"),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.contents.fmt(f)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (str, children): (_, Vec<&Expr>) = match &self.contents {
            ExprContents::Literal(literal) => (format!("{literal:?}"), vec![]),
            ExprContents::Binop(binop) => {
                (format!("{:?} b", binop.op), vec![&binop.lhs, &binop.rhs])
            }
            ExprContents::Prefix(prefix) => (format!("{:?} pre", prefix.prefix), vec![&prefix.rhs]),
            ExprContents::Postfix(postfix) => {
                (format!("{:?} post", postfix.postfix), vec![&postfix.lhs])
            }
            ExprContents::Assign(assign) => {
                (format!("assign {:?}", assign.assignee), vec![&assign.val])
            }
            ExprContents::Accessor(accessor) => (format!("{accessor:?}"), vec![]),
            ExprContents::Scope(exprs) => (format!("scope"), exprs.iter().collect()),
        };
        writeln!(f, "{str}")?;
        let num_children = children.len();
        for (c_idx, child) in children.into_iter().enumerate() {
            for (i, line) in format!("{child}").split("\n").enumerate() {
                if line != "" {
                    writeln!(
                        f,
                        "{} {}",
                        if c_idx == num_children - 1 {
                            if i == 0 { "└─" } else { "  " }
                        } else {
                            if i == 0 { "├─" } else { "│ " }
                        },
                        line,
                    )?
                }
            }
        }
        Ok(())
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
