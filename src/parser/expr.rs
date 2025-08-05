use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    ops::Deref,
};

use strum::EnumIter;

use crate::{
    error::{LineIndex, ParseError},
    types::{BinOpFn, CallFn, Datatype, SetAtFn, SetFn, UnOpFn, Value},
};

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

#[derive(Clone, PartialEq)]
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
    Return(Return),
    Break(Break),
    Continue(Continue),
}

#[derive(Clone, PartialEq)]
pub struct Expr {
    pub contents: ExprContents,
    pub output: Datatype,
}

impl Debug for ExprContents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(num) => write!(f, "{:?}", num),
            Self::Binop(Binop { op, lhs, rhs, .. }) => write!(f, "(b{op:?} {lhs:?} {rhs:?})"),
            Self::Prefix(Prefix {
                op: prefix, rhs, ..
            }) => write!(f, "(pr{prefix:?} {rhs:?})"),
            Self::Postfix(Postfix {
                op: postfix, lhs, ..
            }) => write!(f, "(po{postfix:?} {lhs:?})"),
            Self::Assign(Assign {
                assignee,
                val,
                a_type,
                a_loc: _,
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
            Self::Return(ret) => write!(f, "return {:?}", ret.ret),
            Self::Break(_) => write!(f, "break"),
            Self::Continue(_) => write!(f, "continue"),
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
            ExprContents::Value(literal) => {
                (format!("{} {:?}", literal.get_type(), literal), vec![])
            }
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
                Accessor::Variable(v, _) => (format!("{v}"), vec![]),
                Accessor::Property(base, prop, ..) => (format!("prop {prop} of"), vec![&base]),
                Accessor::Index(indexed, _, indices) => (
                    "index".to_string(),
                    vec![&indexed as &Expr]
                        .into_iter()
                        .chain(indices.iter().map(|i| &i.0 as &Expr))
                        .collect(),
                ),
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
                        String::new()
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
            ExprContents::Return(ret) => ("return".to_string(), vec![&ret.ret]),
            ExprContents::Break(_) => ("break".to_string(), vec![]),
            ExprContents::Continue(_) => ("continue".to_string(), vec![]),
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
            ExprContents::Return(ret) => ret.ret.used_variables(),
            ExprContents::Break(_) => Box::new(vec![].into_iter()),
            ExprContents::Continue(_) => Box::new(vec![].into_iter()),
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
                        Accessor::Variable(v, _) => Box::new([v.clone()].into_iter()),
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
            ExprContents::Return(ret) => ret.ret.assigned_variables(),
            ExprContents::Break(_) => Box::new(vec![].into_iter()),
            ExprContents::Continue(_) => Box::new(vec![].into_iter()),
        }
    }

    pub fn replace_generics(
        &self,
        generics: &HashMap<String, Datatype>,
    ) -> Result<Box<Self>, String> {
        let new_type = match self.output.insert_generics(&generics) {
            Ok(nt) => nt,
            Err(e) => return Err(e),
        };
        let new_contents = match &self.contents {
            ExprContents::Value(val) => ExprContents::Value(val.clone()),
            ExprContents::Binop(binop) => ExprContents::Binop(
                Binop::new(
                    binop.lhs.replace_generics(generics)?,
                    binop.rhs.replace_generics(generics)?,
                    binop.op,
                    binop.op_loc,
                )
                .map_err(|e| e.info)?,
            ),
            ExprContents::Prefix(prefix) => ExprContents::Prefix(
                Prefix::new(
                    prefix.op,
                    prefix.rhs.replace_generics(generics)?,
                    prefix.op_loc,
                )
                .map_err(|e| e.info)?,
            ),
            ExprContents::Postfix(postfix) => ExprContents::Postfix(
                Postfix::new(
                    postfix.op,
                    postfix.lhs.replace_generics(generics)?,
                    postfix.op_loc,
                )
                .map_err(|e| e.info)?,
            ),
            ExprContents::Assign(assign) => ExprContents::Assign(Assign {
                assignee: accessor_replace_generics(&assign.assignee, generics)?,
                val: assign.val.replace_generics(generics)?,
                a_type: assign.a_type.clone(),
                a_loc: assign.a_loc,
            }),
            ExprContents::Accessor(accessor) => {
                ExprContents::Accessor(accessor_replace_generics(accessor, generics)?)
            }
            ExprContents::Scope(exprs) => ExprContents::Scope({
                let mut res = vec![];
                for i in exprs.iter().map(|e| e.replace_generics(generics)) {
                    res.push(*i?);
                }
                res
            }),
            ExprContents::Conditional(conditional) => ExprContents::Conditional(Conditional {
                condition: conditional.condition.replace_generics(generics)?,
                result: conditional.result.replace_generics(generics)?,
                otherwise: match conditional.otherwise.as_ref() {
                    Some(otw) => Some(otw.replace_generics(generics)?),
                    None => None,
                },
            }),
            ExprContents::While(wh) => ExprContents::While(While {
                condition: wh.condition.replace_generics(generics)?,
                result: wh.result.replace_generics(generics)?,
            }),
            ExprContents::For(fo) => ExprContents::For(For {
                var: fo.var.clone(),
                iter: fo.iter.replace_generics(generics)?,
                body: fo.body.replace_generics(generics)?,
                iter_loc: fo.iter_loc,
            }),
            ExprContents::Array(array) => ExprContents::Array(Array {
                elements: {
                    let mut res = vec![];
                    for i in array.elements.iter().map(|e| e.replace_generics(generics)) {
                        res.push(*i?);
                    }
                    res
                },
            }),
            ExprContents::Tuple(tuple) => ExprContents::Tuple(Tuple {
                elements: {
                    let mut res = vec![];
                    for i in tuple.elements.iter().map(|e| e.replace_generics(generics)) {
                        res.push(*i?);
                    }
                    res
                },
            }),
            ExprContents::Function(function) => ExprContents::Function(Function {
                params: {
                    let mut res = vec![];
                    for (n, t) in function
                        .params
                        .iter()
                        .map(|(n, t)| (n.clone(), t.insert_generics(generics)))
                    {
                        res.push((
                            n,
                            match t {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            },
                        ));
                    }
                    res
                },
                contents: function.contents.replace_generics(generics)?,
                generic: function.generic.clone(),
            }),
            ExprContents::Call(call) => ExprContents::Call({
                let base = call.base.replace_generics(generics)?;
                let params = {
                    let mut res = vec![];
                    for i in call.params.iter().map(|p| p.replace_generics(generics)) {
                        res.push(*i?);
                    }
                    res
                };
                let out = if base != call.base || params != call.params {
                    None
                } else {
                    Some(call.out.clone())
                };
                Call::new(base, params, call.loc, out).map_err(|e| e.info)?
            }),
            ExprContents::GenericSpecify(gspec) => ExprContents::GenericSpecify(GenericSpecify {
                base: gspec.base.replace_generics(generics)?,
                types: gspec.types.clone(),
            }),
            ExprContents::Return(ret) => ExprContents::Return(Return {
                ret: ret.ret.replace_generics(generics)?,
            }),
            ExprContents::Break(_) => ExprContents::Break(Break),
            ExprContents::Continue(_) => ExprContents::Continue(Continue),
        };
        Ok(Box::new(Self {
            contents: new_contents,
            output: new_type,
        }))
    }
}

impl From<Value> for Expr {
    fn from(value: Value) -> Self {
        let output = value.get_type();
        Self {
            contents: ExprContents::Value(value),
            output,
        }
    }
}

fn accessor_used_vars<'a>(accessor: &'a Accessor) -> Box<dyn Iterator<Item = String> + 'a> {
    match accessor {
        Accessor::Variable(v, _) => Box::new([v.clone()].into_iter()),
        Accessor::Property(base, ..) => base.used_variables(),
        Accessor::Index(indexed, _, indices) => {
            // FIXME: this doesn't filter out variables assigned in previous indices
            let mut vars: Box<dyn Iterator<Item = String> + 'a> =
                Box::new(indexed.used_variables());
            for (index, ..) in indices {
                vars = Box::new(vars.chain(index.used_variables()))
            }
            vars
        }
    }
}

fn accessor_assigned_vars<'a>(accessor: &'a Accessor) -> Box<dyn Iterator<Item = String> + 'a> {
    match accessor {
        Accessor::Variable(_, _) => Box::new([].into_iter()),
        Accessor::Property(base, ..) => base.assigned_variables(),
        Accessor::Index(indexed, _, indices) => {
            let mut vars: Box<dyn Iterator<Item = String> + 'a> =
                Box::new(indexed.used_variables());
            for (index, ..) in indices {
                vars = Box::new(vars.chain(index.used_variables()))
            }
            vars
        }
    }
}

fn accessor_replace_generics(
    accessor: &Accessor,
    generics: &HashMap<String, Datatype>,
) -> Result<Accessor, String> {
    Ok(match accessor {
        Accessor::Variable(_, _) => accessor.clone(),
        Accessor::Property(base, prop, i, ..) => {
            Accessor::new_property(base.replace_generics(generics)?, prop.clone(), *i)
                .map_err(|e| e.info)?
        }
        Accessor::Index(base, i, indices) => Accessor::new_index(
            base.replace_generics(generics)?,
            indices
                .iter()
                .map(|(index, ..)| index.replace_generics(generics).map(|i| *i))
                .collect::<Result<Vec<_>, _>>()?,
            *i,
        )
        .map_err(|e| e.info)?,
    })
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binop {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: Op,
    pub op_loc: LineIndex,
    pub res: BinOpFn,
    pub out: Datatype,
}

impl Binop {
    pub fn new(
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: Op,
        op_loc: LineIndex,
    ) -> Result<Self, ParseError> {
        let (out, res) = lhs
            .output
            .bin_op_result(&rhs.output, op.clone())
            .map_err(|e| ParseError {
                location: op_loc,
                info: e,
            })?;
        Ok(Self {
            lhs,
            rhs,
            op,
            op_loc,
            out,
            res,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    pub op: Op,
    pub rhs: Box<Expr>,
    pub op_loc: LineIndex,
    pub res: UnOpFn,
    pub out: Datatype,
}

impl Prefix {
    pub fn new(op: Op, rhs: Box<Expr>, op_loc: LineIndex) -> Result<Self, ParseError> {
        let (out, res) = rhs
            .output
            .pre_op_result(op.clone())
            .map_err(|e| ParseError {
                location: op_loc,
                info: e,
            })?;
        Ok(Self {
            op,
            rhs,
            op_loc,
            out,
            res,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Postfix {
    pub op: Op,
    pub lhs: Box<Expr>,
    pub op_loc: LineIndex,
    pub res: UnOpFn,
    pub out: Datatype,
}

impl Postfix {
    pub fn new(op: Op, lhs: Box<Expr>, op_loc: LineIndex) -> Result<Self, ParseError> {
        let (out, res) = lhs
            .output
            .post_op_result(op.clone())
            .map_err(|e| ParseError {
                location: op_loc,
                info: e,
            })?;
        Ok(Self {
            op,
            lhs,
            op_loc,
            out,
            res,
        })
    }
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

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub assignee: Accessor,
    pub val: Box<Expr>,
    pub a_type: AssignType,
    pub a_loc: LineIndex,
}

#[derive(Clone, PartialEq)]
pub enum Accessor {
    Variable(String, LineIndex),
    Property(
        Box<Expr>,
        String,
        LineIndex,
        Option<UnOpFn>,
        Option<SetFn>,
        Datatype,
    ),
    Index(
        Box<Expr>,
        LineIndex,
        Vec<(Box<Expr>, BinOpFn, SetAtFn, Datatype)>,
    ),
}

impl Accessor {
    pub fn new_property(base: Box<Expr>, prop: String, loc: LineIndex) -> Result<Self, ParseError> {
        let (out, get, set) = base.output.prop_type(&prop).map_err(|e| ParseError {
            info: e,
            location: loc,
        })?;
        Ok(Self::Property(base, prop, loc, get, set, out))
    }

    pub fn new_index(
        indexed: Box<Expr>,
        indices: Vec<Expr>,
        loc: LineIndex,
    ) -> Result<Self, ParseError> {
        let mut res = vec![];
        for index in indices {
            let (out, get, set) =
                indexed
                    .output
                    .index_type(&index.output)
                    .map_err(|e| ParseError {
                        location: loc,
                        info: e,
                    })?;
            res.push((Box::new(index), get, set, out));
        }
        Ok(Self::Index(indexed, loc, res))
    }
}

impl Debug for Accessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(name, _) => write!(f, "{name}"),
            Self::Property(accessor, name, ..) => write!(f, "{accessor:?}.{name}"),
            Self::Index(indexed, index, ..) => write!(f, "{indexed:?}[{index:?}]"),
        }
    }
}

#[derive(Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
pub struct While {
    pub condition: Box<Expr>,
    pub result: Box<Expr>,
}

impl Debug for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while {:?} do {:?}", self.condition, self.result)
    }
}

#[derive(Clone, PartialEq)]
pub struct For {
    pub var: String,
    pub iter: Box<Expr>,
    pub body: Box<Expr>,
    pub iter_loc: LineIndex,
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

#[derive(Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub params: Vec<(String, Datatype)>,
    pub contents: Box<Expr>,
    pub generic: Vec<String>,
}

#[derive(Clone, PartialEq)]
pub struct Call {
    pub base: Box<Expr>,
    pub params: Vec<Expr>,
    pub loc: LineIndex,
    pub res: CallFn,
    pub out: Datatype,
}

impl Call {
    pub fn new(
        base: Box<Expr>,
        params: Vec<Expr>,
        loc: LineIndex,
        out: Option<Datatype>,
    ) -> Result<Self, ParseError> {
        let (new_out, res) = base
            .output
            .call_result(params.iter().map(|p| p.output.clone()).collect(), out)
            .map_err(|e| ParseError {
                location: loc,
                info: e,
            })?;
        Ok(Self {
            base,
            params,
            loc,
            res,
            out: new_out,
        })
    }
}

#[derive(Clone, PartialEq)]
pub struct GenericSpecify {
    pub base: Box<Expr>,
    pub types: Vec<Datatype>,
}

#[derive(Clone, PartialEq)]
pub struct Return {
    pub ret: Box<Expr>,
}

#[derive(Clone, PartialEq)]
pub struct Break;

#[derive(Clone, PartialEq)]
pub struct Continue;
