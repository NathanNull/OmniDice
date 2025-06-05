use std::fmt::{Debug, Display};

use strum::EnumIter;

use crate::{
    lexer::OpToken,
    types::{Datatype, Value},
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
    Mod,
    D,
    Equal,
    NotEqual,
    Greater,
    Less,
    Geq,
    Leq,
    And,
    Or,
    Not,
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Divided => "/",
            Self::Mod => "%",
            Self::D => "d",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::Greater => ">",
            Self::Less => "<",
            Self::Geq => ">=",
            Self::Leq => "<=",
            Self::And => "&&",
            Self::Or => "||",
            Self::Not => "!",
        };
        write!(f, "{c}")
    }
}

// Convenient little macro so I don't have to manually write every conversion between Op and OpToken
macro_rules! convert_op_tokens {
    ($($op: ident),+) => {
        impl TryFrom<OpToken> for Op {
            type Error = String;

            fn try_from(value: OpToken) -> Result<Self, Self::Error> {
                Ok(match value {
                    $(OpToken::$op => Self::$op,)+
                    _ => {
                        return Err(format!("Invalid operation {value:?}"));
                    }
                })
            }
        }

        impl Into<OpToken> for Op {
            fn into(self) -> OpToken {
                match self {
                    $(Self::$op => OpToken::$op,)+
                }
            }
        }
    };
}

convert_op_tokens!(
    Plus, Minus, Times, Divided, Mod, D, Equal, NotEqual, Greater, Less, Geq, Leq, And, Or, Not
);

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
    Conditional(Conditional),
    While(While),
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
            Self::Prefix(Prefix { op: prefix, rhs }) => write!(f, "(pr{prefix:?} {rhs:?})"),
            Self::Postfix(Postfix { op: postfix, lhs }) => write!(f, "(po{postfix:?} {lhs:?})"),
            Self::Assign(Assign { assignee, val }) => write!(f, "(set {assignee:?} {val:?})"),
            Self::Accessor(acc) => write!(f, "{acc:?}"),
            Self::Scope(scope) => write!(f, "{{{scope:?}}}"),
            Self::Conditional(cond) => write!(f, "{cond:?}"),
            Self::While(wh) => write!(f, "{:?}", wh),
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
            ExprContents::Prefix(prefix) => (format!("{:?} pre", prefix.op), vec![&prefix.rhs]),
            ExprContents::Postfix(postfix) => {
                (format!("{:?} post", postfix.op), vec![&postfix.lhs])
            }
            ExprContents::Assign(assign) => {
                (format!("assign {:?}", assign.assignee), vec![&assign.val])
            }
            ExprContents::Accessor(accessor) => (format!("{accessor:?}"), vec![]),
            ExprContents::Scope(exprs) => ("scope".to_string(), exprs.iter().collect()),
            ExprContents::Conditional(cond) => ("if".to_string(), {
                let mut children: Vec<&Expr> = vec![&cond.condition, &cond.result];
                if let Some(otw) = cond.otherwise.as_ref() {
                    children.push(otw);
                }
                children
            }),
            ExprContents::While(wh) => ("while".to_string(), vec![&wh.condition, &wh.result]),
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
    pub op: Op,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Postfix {
    pub op: Op,
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

#[derive(Clone)]
pub struct Conditional {
    pub condition: Box<Expr>,
    pub result: Box<Expr>,
    pub otherwise: Option<Box<Expr>>,
}

impl Debug for Conditional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {:?} then {:?}", self.condition, self.result)?;
        if let Some(otw) = self.otherwise.as_ref() {
            write!(f, " else {otw:?}")?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct While {
    pub condition: Box<Expr>,
    pub result: Box<Expr>,
}

impl Debug for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while {:?} do {:?}", self.condition, self.result)
    }
}
