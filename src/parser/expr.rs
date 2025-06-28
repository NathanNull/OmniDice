use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    ops::Deref,
};

use strum::EnumIter;

use crate::types::{Arr, ArrT, Datatype, Downcast, Tuple as Tup, Value};

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
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
    Range,
    RangeEq,
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
            Self::Range => "..",
            Self::RangeEq => "..=",
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

pub type Scope = Vec<Expr>;

#[derive(Clone)]
pub enum ExprContents {
    Value(Value),
    Binop(Binop),
    Prefix(Prefix),
    Postfix(Postfix),
    // TODO: potential postfix ideas:
    // An operator that makes dice explode (!)
    // (write more as I think of them)
    Assign(Assign),
    Accessor(Accessor),
    Scope(Scope),
    Conditional(Conditional),
    While(While),
    For(For),
    Array(Array),
    Tuple(Tuple),
    Function(Function),
    Call(Call),
    GenericSpecify(GenericSpecify),
}

#[derive(Clone)]
pub struct Expr {
    pub contents: ExprContents,
    pub output: Datatype,
}

impl Debug for ExprContents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(num) => write!(f, "{:?}", num),
            Self::Binop(Binop { op, lhs, rhs }) => write!(f, "(b{op:?} {lhs:?} {rhs:?})"),
            Self::Prefix(Prefix { op: prefix, rhs }) => write!(f, "(pr{prefix:?} {rhs:?})"),
            Self::Postfix(Postfix { op: postfix, lhs }) => write!(f, "(po{postfix:?} {lhs:?})"),
            Self::Assign(Assign {
                assignee,
                val,
                a_type,
            }) => write!(f, "({a_type} {assignee:?} {val:?})"),
            Self::Accessor(acc) => write!(f, "{acc:?}"),
            Self::Scope(scope) => write!(f, "{{{scope:?}}}"),
            Self::Conditional(cond) => write!(f, "{cond:?}"),
            Self::While(wh) => write!(f, "{wh:?}"),
            Self::For(fo) => write!(f, "{fo:?}"),
            Self::Array(arr) => write!(f, "{arr:?}"),
            Self::Tuple(tup) => write!(f, "{tup:?}"),
            Self::Function(func) => {
                write!(f, "func {:?} -> {:?}", func.params, func.contents.output)
            }
            Self::Call(call) => write!(
                f,
                "{:?}({})",
                call.base,
                call.params
                    .iter()
                    .map(|p| format!("{p:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::GenericSpecify(gspec) => write!(
                f,
                "{:?}::<{}>",
                gspec.base,
                gspec
                    .types
                    .iter()
                    .map(|t| format!("{t:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
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
            ExprContents::Value(literal) => (format!("{} {:?}", literal.get_type(), literal), vec![]),
            ExprContents::Binop(binop) => {
                (format!("{:?} b", binop.op), vec![&binop.lhs, &binop.rhs])
            }
            ExprContents::Prefix(prefix) => (format!("{:?} pre", prefix.op), vec![&prefix.rhs]),
            ExprContents::Postfix(postfix) => {
                (format!("{:?} post", postfix.op), vec![&postfix.lhs])
            }
            ExprContents::Assign(assign) => (
                format!(
                    "{} {:?} ({})",
                    assign.a_type,
                    assign.assignee,
                    self.output.clone()
                ),
                vec![&assign.val],
            ),
            ExprContents::Accessor(accessor) => match accessor {
                Accessor::Variable(v) => (format!("{v}"), vec![]),
                Accessor::Property(base, prop) => (format!("prop {prop} of"), vec![&base]),
                Accessor::Index(indexed, index) => ("index".to_string(), vec![&indexed, &index]),
            },
            ExprContents::Scope(exprs) => ("scope".to_string(), exprs.iter().collect()),
            ExprContents::Conditional(cond) => ("if".to_string(), {
                let mut children: Vec<&Expr> = vec![&cond.condition, &cond.result];
                if let Some(otw) = cond.otherwise.as_ref() {
                    children.push(otw);
                }
                children
            }),
            ExprContents::While(wh) => ("while".to_string(), vec![&wh.condition, &wh.result]),
            ExprContents::For(fo) => (format!("for {} in", fo.var), vec![&fo.iter, &fo.body]),
            ExprContents::Array(arr) => (format!("{}", self.output), arr.elements.iter().collect()),
            ExprContents::Tuple(tup) => ("tuple".to_string(), tup.elements.iter().collect()),
            ExprContents::Function(func) => (
                format!(
                    "func{} ({})",
                    if func.generic.len() > 0 {
                        format!("<{}>", func.generic.join(", "))
                    } else {
                        "".to_string()
                    },
                    func.params
                        .iter()
                        .map(|(n, t)| format!("{n}: {t}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                vec![&func.contents],
            ),
            ExprContents::Call(call) => (
                format!("call -> {}", self.output),
                [call.base.deref()]
                    .into_iter()
                    .chain(call.params.iter().map(|p| &*p))
                    .collect(),
            ),
            ExprContents::GenericSpecify(gspec) => (
                format!(
                    "gspec <{}>",
                    gspec
                        .types
                        .iter()
                        .map(|t| format!("{t:?}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                vec![&gspec.base],
            ),
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

impl Expr {
    pub fn used_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        match &self.contents {
            ExprContents::Value(_) => Box::new([].into_iter()),
            ExprContents::Binop(binop) => {
                let l_created = binop.lhs.assigned_variables().collect::<Vec<_>>();
                Box::new(
                    binop.lhs.used_variables().chain(
                        binop
                            .rhs
                            .used_variables()
                            .filter(move |v| !l_created.contains(v)),
                    ),
                )
            }
            ExprContents::Prefix(prefix) => prefix.rhs.used_variables(),
            ExprContents::Postfix(postfix) => postfix.lhs.used_variables(),
            ExprContents::Assign(assign) => {
                let l_created = accessor_assigned_vars(&assign.assignee).collect::<Vec<_>>();
                Box::new(
                    match &assign.a_type {
                        AssignType::Reassign => accessor_used_vars(&assign.assignee),
                        AssignType::Create => Box::new([].into_iter()),
                    }
                    .chain(
                        assign
                            .val
                            .used_variables()
                            .filter(move |v| !l_created.contains(v)),
                    ),
                )
            }
            ExprContents::Accessor(accessor) => accessor_used_vars(accessor),
            ExprContents::Scope(exprs) => {
                let mut used = vec![];
                let mut assigned = vec![];
                for line in exprs {
                    assigned.extend(line.assigned_variables());
                    let l_used = line
                        .used_variables()
                        .filter(|v| !assigned.contains(v) && !used.contains(v))
                        .collect::<Vec<_>>();
                    used.extend_from_slice(&l_used);
                }
                Box::new(used.into_iter())
            }
            ExprContents::Conditional(conditional) => {
                let l_created = conditional
                    .condition
                    .assigned_variables()
                    .collect::<Vec<_>>();
                let lc_2 = l_created.clone();
                let r_created = conditional.result.assigned_variables().collect::<Vec<_>>();
                Box::new(
                    conditional
                        .condition
                        .used_variables()
                        .chain(
                            conditional
                                .result
                                .used_variables()
                                .filter(move |v| !l_created.clone().contains(v)),
                        )
                        .chain(if let Some(otw) = &conditional.otherwise {
                            Box::new(
                                otw.used_variables()
                                    .filter(move |v| !lc_2.contains(v) && !r_created.contains(v)),
                            ) as Box<dyn Iterator<Item = String>>
                        } else {
                            Box::new([].into_iter())
                        }),
                )
            }
            ExprContents::While(wh) => {
                let l_created = wh.condition.assigned_variables().collect::<Vec<_>>();
                Box::new(
                    wh.condition.used_variables().chain(
                        wh.result
                            .used_variables()
                            .filter(move |v| !l_created.contains(v)),
                    ),
                )
            }
            ExprContents::For(fo) => {
                let l_created = fo.iter.assigned_variables().collect::<Vec<_>>();
                Box::new(
                    fo.iter.used_variables().chain(
                        fo.body
                            .used_variables()
                            .filter(move |v| *v != fo.var && !l_created.contains(v)),
                    ),
                )
            }
            ExprContents::Array(array) => {
                let mut used = vec![];
                let mut assigned = vec![];
                for line in &array.elements {
                    assigned.extend(line.assigned_variables());
                    let l_used = line
                        .used_variables()
                        .filter(|v| !assigned.contains(v) && !used.contains(v))
                        .collect::<Vec<_>>();
                    used.extend_from_slice(&l_used);
                }
                Box::new(used.into_iter())
            }
            ExprContents::Tuple(tuple) => {
                let mut used = vec![];
                let mut assigned = vec![];
                for line in &tuple.elements {
                    assigned.extend(line.assigned_variables());
                    let l_used = line
                        .used_variables()
                        .filter(|v| !assigned.contains(v) && !used.contains(v))
                        .collect::<Vec<_>>();
                    used.extend_from_slice(&l_used);
                }
                Box::new(used.into_iter())
            }
            ExprContents::Function(function) => Box::new(
                function
                    .contents
                    .used_variables()
                    .filter(|v| !function.params.iter().any(|(p, _)| p == v)),
            ),
            ExprContents::Call(call) => {
                let mut used = vec![];
                let mut assigned = vec![];
                for line in [call.base.as_ref()].into_iter().chain(call.params.iter()) {
                    assigned.extend(line.assigned_variables());
                    let l_used = line
                        .used_variables()
                        .filter(|v| !assigned.contains(v) && !used.contains(v))
                        .collect::<Vec<_>>();
                    used.extend_from_slice(&l_used);
                }
                Box::new(used.into_iter())
            }
            ExprContents::GenericSpecify(gspec) => gspec.base.used_variables(),
        }
    }

    pub fn assigned_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        match &self.contents {
            ExprContents::Value(_) => Box::new([].into_iter()),
            ExprContents::Binop(binop) => Box::new(
                binop
                    .lhs
                    .assigned_variables()
                    .chain(binop.rhs.assigned_variables()),
            ),
            ExprContents::Prefix(prefix) => prefix.rhs.assigned_variables(),
            ExprContents::Postfix(postfix) => postfix.lhs.assigned_variables(),
            ExprContents::Assign(assign) => Box::new(
                match &assign.a_type {
                    AssignType::Reassign => accessor_assigned_vars(&assign.assignee),
                    AssignType::Create => match &assign.assignee {
                        Accessor::Variable(v) => Box::new([v.clone()].into_iter()),
                        _ => unreachable!("Unexpected non-variable in let statement"),
                    },
                }
                .chain(assign.val.assigned_variables()),
            ),
            ExprContents::Accessor(accessor) => accessor_assigned_vars(accessor),
            ExprContents::Scope(exprs) => {
                Box::new(exprs.iter().map(|e| e.assigned_variables()).flatten())
            }
            ExprContents::Conditional(conditional) => {
                Box::new(conditional.condition.assigned_variables().chain(
                    if let Some(otw) = &conditional.otherwise {
                        otw.assigned_variables()
                    } else {
                        Box::new([].into_iter())
                    },
                ))
            }
            ExprContents::While(wh) => wh.condition.assigned_variables(),
            ExprContents::For(fo) => fo.iter.assigned_variables(),
            ExprContents::Array(array) => Box::new(
                array
                    .elements
                    .iter()
                    .map(|e| e.assigned_variables())
                    .flatten(),
            ),
            ExprContents::Tuple(tuple) => Box::new(
                tuple
                    .elements
                    .iter()
                    .map(|e| e.assigned_variables())
                    .flatten(),
            ),
            ExprContents::Function(_) => Box::new([].into_iter()),
            ExprContents::Call(call) => Box::new(
                call.params
                    .iter()
                    .map(|p| p.assigned_variables())
                    .flatten()
                    .chain(call.base.assigned_variables()),
            ),
            ExprContents::GenericSpecify(gspec) => gspec.base.assigned_variables(),
        }
    }

    pub fn replace_generics(&self, generics: &HashMap<String, Datatype>) -> Box<Self> {
        let new_type = self
            .output
            .insert_generics(&generics)
            .expect("Invalid type");
        let new_contents = match &self.contents {
            ExprContents::Value(val) => ExprContents::Value(val.clone()),
            ExprContents::Binop(binop) => ExprContents::Binop(Binop {
                lhs: binop.lhs.replace_generics(generics),
                rhs: binop.rhs.replace_generics(generics),
                op: binop.op,
            }),
            ExprContents::Prefix(prefix) => ExprContents::Prefix(Prefix {
                op: prefix.op,
                rhs: prefix.rhs.replace_generics(generics),
            }),
            ExprContents::Postfix(postfix) => ExprContents::Postfix(Postfix {
                op: postfix.op,
                lhs: postfix.lhs.replace_generics(generics),
            }),
            ExprContents::Assign(assign) => ExprContents::Assign(Assign {
                assignee: accessor_replace_generics(&assign.assignee, generics),
                val: assign.val.replace_generics(generics),
                a_type: assign.a_type.clone(),
            }),
            ExprContents::Accessor(accessor) => {
                ExprContents::Accessor(accessor_replace_generics(accessor, generics))
            }
            ExprContents::Scope(exprs) => ExprContents::Scope(
                exprs
                    .iter()
                    .map(|e| *e.replace_generics(generics))
                    .collect(),
            ),
            ExprContents::Conditional(conditional) => ExprContents::Conditional(Conditional {
                condition: conditional.condition.replace_generics(generics),
                result: conditional.result.replace_generics(generics),
                otherwise: conditional
                    .otherwise
                    .as_ref()
                    .map(|otw| otw.replace_generics(generics)),
            }),
            ExprContents::While(wh) => ExprContents::While(While {
                condition: wh.condition.replace_generics(generics),
                result: wh.result.replace_generics(generics),
            }),
            ExprContents::For(fo) => ExprContents::For(For {
                var: fo.var.clone(),
                iter: fo.iter.replace_generics(generics),
                body: fo.body.replace_generics(generics),
            }),
            ExprContents::Array(array) => ExprContents::Array(Array {
                elements: array
                    .elements
                    .iter()
                    .map(|e| *e.replace_generics(generics))
                    .collect(),
            }),
            ExprContents::Tuple(tuple) => ExprContents::Tuple(Tuple {
                elements: tuple
                    .elements
                    .iter()
                    .map(|e| *e.replace_generics(generics))
                    .collect(),
            }),
            ExprContents::Function(function) => ExprContents::Function(Function {
                params: function
                    .params
                    .iter()
                    .map(|(n, t)| (n.clone(), t.insert_generics(generics).expect("Invalid")))
                    .collect(),
                contents: function.contents.replace_generics(generics),
                generic: function.generic.clone(),
            }),
            ExprContents::Call(call) => ExprContents::Call(Call {
                base: call.base.replace_generics(generics),
                params: call
                    .params
                    .iter()
                    .map(|p| *p.replace_generics(generics))
                    .collect(),
            }),
            ExprContents::GenericSpecify(gspec) => ExprContents::GenericSpecify(GenericSpecify {
                base: gspec.base.replace_generics(generics),
                types: gspec.types.clone(),
            }),
        };
        Box::new(Self {
            contents: new_contents,
            output: new_type,
        })
    }

    pub fn try_const_eval(&self) -> Option<Value> {
        match &self.contents {
            ExprContents::Value(val) => Some(val.clone()),
            ExprContents::Binop(binop) => {
                if let Some((lhs, rhs)) = binop
                    .lhs
                    .try_const_eval()
                    .and_then(|l| binop.rhs.try_const_eval().map(|r| (l, r)))
                {
                    Some(lhs.bin_op(&rhs, binop.op))
                } else {
                    None
                }
            }
            ExprContents::Prefix(prefix) => {
                prefix.rhs.try_const_eval().map(|rhs| rhs.pre_op(prefix.op))
            }
            ExprContents::Postfix(postfix) => postfix
                .lhs
                .try_const_eval()
                .map(|lhs| lhs.post_op(postfix.op)),
            ExprContents::Assign(_) => None,
            ExprContents::Accessor(accessor) => match accessor {
                Accessor::Variable(_) => None,
                Accessor::Property(base, prop) => base.try_const_eval().map(|b| b.get_prop(prop)),
                Accessor::Index(base, index) => {
                    if let Some((b,i)) = base
                        .try_const_eval()
                        .and_then(|l| index.try_const_eval().map(|r| (l, r)))
                    {
                        Some(b.get_index(i))
                    } else {
                        None
                    }
                }
            },
            ExprContents::Scope(_) => None,
            ExprContents::Conditional(_) => None,
            ExprContents::While(_) => None,
            ExprContents::For(_) => None,
            ExprContents::Array(array) => {
                fn arr_eval(arr: &Array, out: Datatype) -> Option<Arr> {
                    let mut vals = vec![];
                    for expr in &arr.elements {
                        vals.push(expr.try_const_eval()?)
                    }
                    Some(Arr::new(vals, out))
                }
                arr_eval(array, self.output.downcast::<ArrT>().unwrap().entry).map(|a|Box::new(a) as Value)
            },
            ExprContents::Tuple(tuple) => {
                fn tup_eval(tup: &Tuple) -> Option<Tup> {
                    let mut vals = vec![];
                    for expr in &tup.elements {
                        vals.push(expr.try_const_eval()?)
                    }
                    Some(Tup::new(vals))
                }
                tup_eval(tuple).map(|t|Box::new(t) as Value)
            },
            ExprContents::Function(_) => None,
            ExprContents::Call(_) => None,
            ExprContents::GenericSpecify(_) => None,
        }
    }
}

fn accessor_used_vars<'a>(accessor: &'a Accessor) -> Box<dyn Iterator<Item = String> + 'a> {
    match accessor {
        Accessor::Variable(v) => Box::new([v.clone()].into_iter()),
        Accessor::Property(base, _) => base.used_variables(),
        Accessor::Index(indexed, index) => {
            Box::new(indexed.used_variables().chain(index.used_variables()))
        }
    }
}

fn accessor_assigned_vars<'a>(accessor: &'a Accessor) -> Box<dyn Iterator<Item = String> + 'a> {
    match accessor {
        Accessor::Variable(_) => Box::new([].into_iter()),
        Accessor::Property(base, _) => base.assigned_variables(),
        Accessor::Index(indexed, index) => Box::new(
            indexed
                .assigned_variables()
                .chain(index.assigned_variables()),
        ),
    }
}

fn accessor_replace_generics(
    accessor: &Accessor,
    generics: &HashMap<String, Datatype>,
) -> Accessor {
    match accessor {
        Accessor::Variable(_) => accessor.clone(),
        Accessor::Property(base, prop) => {
            Accessor::Property(base.replace_generics(generics), prop.clone())
        }
        Accessor::Index(base, index) => Accessor::Index(
            base.replace_generics(generics),
            index.replace_generics(generics),
        ),
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

#[derive(Debug, Clone, PartialEq)]
pub enum AssignType {
    Create,
    Reassign,
}

impl Display for AssignType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AssignType::Create => "let",
                AssignType::Reassign => "re",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub assignee: Accessor,
    pub val: Box<Expr>,
    pub a_type: AssignType,
}

#[derive(Clone)]
pub enum Accessor {
    Variable(String),
    Property(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
}

impl Debug for Accessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(name) => write!(f, "{name}"),
            Self::Property(accessor, name) => write!(f, "{accessor:?}.{name}"),
            Self::Index(indexed, index) => write!(f, "{indexed:?}[{index:?}]"),
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

#[derive(Clone)]
pub struct For {
    pub var: String,
    pub iter: Box<Expr>,
    pub body: Box<Expr>,
}

impl Debug for For {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "for {:?} in {:?} do {:?}",
            self.var, self.iter, self.body
        )
    }
}

#[derive(Clone)]
pub struct Array {
    pub elements: Vec<Expr>,
}

impl Debug for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let len = self.elements.len();
        for (i, ele) in self.elements.iter().enumerate() {
            write!(f, "{ele:?}")?;
            if i != len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

#[derive(Clone)]
pub struct Tuple {
    pub elements: Vec<Expr>,
}

impl Debug for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let len = self.elements.len();
        for (i, ele) in self.elements.iter().enumerate() {
            write!(f, "{ele:?}")?;
            if i != len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<(String, Datatype)>,
    pub contents: Box<Expr>,
    pub generic: Vec<String>,
}

#[derive(Clone)]
pub struct Call {
    pub base: Box<Expr>,
    pub params: Vec<Expr>,
}

#[derive(Clone)]
pub struct GenericSpecify {
    pub base: Box<Expr>,
    pub types: Vec<Datatype>,
}
