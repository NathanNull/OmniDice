use super::*;

#[derive(Clone, Debug)]
pub enum TypeVar {
    Var(String),
    BinOp(Datatype, Datatype, Op),
    UnaryOp(Datatype, Op, bool),
    Call(Datatype, Vec<Datatype>, Option<Datatype>),
    Index(Datatype, Datatype),
    Prop(Datatype, String),
    MustBeSame(Vec<Datatype>),
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl BaseType for TypeVar {
    fn name(&self) -> String {
        match self {
            TypeVar::Var(v) => v.clone(),
            TypeVar::BinOp(lhs, rhs, op) => format!("<{} {:?} {}>", lhs, op, rhs),
            TypeVar::UnaryOp(operand, op, pre) => {
                format!(
                    "<{} {:?} ({})>",
                    operand,
                    op,
                    if *pre { "pre" } else { "post" }
                )
            }
            TypeVar::Call(base, items, expected_output) => format!(
                "<{} call ({}){}>",
                base,
                items
                    .iter()
                    .map(|i| format!("{i}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                if let Some(o) = &expected_output {
                    format!(" -> {}", o)
                } else {
                    "".to_string()
                }
            ),
            TypeVar::Index(base, index) => format!("<{} [{}]>", base, index),
            TypeVar::Prop(base, prop) => format!("<{} [{}]>", base, prop),
            TypeVar::MustBeSame(types) => format!(
                "<{}>",
                types
                    .iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(" sameas ")
            ),
        }
    }

    fn dup(&self) -> Datatype {
        Box::new(self.clone())
    }
}

impl Type for TypeVar {
    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(match self {
            TypeVar::Var(v) => {
                if let Some(dt) = generics.get(v) {
                    dt.dup()
                } else {
                    self.dup()
                }
            }
            TypeVar::BinOp(lhs, rhs, op) => {
                let lhs = lhs.insert_generics(generics)?;
                let rhs = rhs.insert_generics(generics)?;
                lhs.bin_op_result(&rhs, *op)?
            }
            TypeVar::UnaryOp(operand, op, pre) => {
                let operand = operand.insert_generics(generics)?;
                if *pre {
                    operand.pre_op_result(*op)?
                } else {
                    operand.post_op_result(*op)?
                }
            }
            TypeVar::Call(base, items, expected_output) => {
                let base = base.insert_generics(generics)?;
                let mut params = vec![];
                for i in items {
                    params.push(i.insert_generics(generics)?)
                }
                let expected_output = if let Some(o) = expected_output {
                    Some(o.insert_generics(generics)?)
                } else {
                    None
                };
                base.call_result(params, expected_output)?
            }
            TypeVar::Index(base, index) => base
                .insert_generics(generics)?
                .index_type(&index.insert_generics(generics)?)?,
            TypeVar::Prop(base, prop) => base.insert_generics(generics)?.prop_type(&prop)?,
            TypeVar::MustBeSame(types) => {
                let mut t_iter = types.iter();
                let mut res = t_iter.next().unwrap().insert_generics(generics)?;
                for t in t_iter {
                    res = res.assert_same(&t.insert_generics(generics)?);
                }
                res
            }
        })
    }

    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        Some(match self {
            TypeVar::Var(v) => {
                let ret = HashMap::from_iter([(v.clone(), other.dup())]);
                if let Some(Self::Var(var)) = other.downcast::<Self>() {
                    if var == *v { HashMap::new() } else { ret }
                } else {
                    ret
                }
            }
            TypeVar::MustBeSame(types) => {
                let mut t_matches = vec![];
                for t in types {
                    t_matches.push(t.try_match(other)?);
                }
                let mut matches = HashMap::new();
                for k in t_matches.iter().map(|m| m.keys()).flatten() {
                    let match_val = t_matches
                        .iter()
                        .find(|m| m.contains_key(k))
                        .unwrap()
                        .get(k)
                        .unwrap();
                    matches.insert(k.clone(), match_val.clone());
                    if t_matches
                        .iter()
                        .any(|m| m.get(k).is_some_and(|v| v != match_val))
                    {
                        return None;
                    }
                }
                matches
            }
            _ => todo!("Match {self} w/ {other}"),
        })
    }

    fn assert_same(&self, other: &Datatype) -> Datatype {
        if &self.dup() == other {
            self.dup()
        } else if let Self::MustBeSame(v) = self {
            Box::new(Self::same(v.clone(), other))
        } else {
            Box::new(Self::same(vec![self.dup()], other))
        }
    }

    fn get_generics(&self) -> Vec<String> {
        match self {
            TypeVar::Var(v) => vec![v.clone()],
            TypeVar::BinOp(lhs, rhs, _) => lhs
                .get_generics()
                .into_iter()
                .chain(rhs.get_generics().into_iter())
                .collect(),
            TypeVar::UnaryOp(base, _, _) => base.get_generics(),
            TypeVar::Call(base, items, expected_output) => base
                .get_generics()
                .into_iter()
                .chain(items.iter().map(|i| i.get_generics().into_iter()).flatten())
                .chain(expected_output.iter().map(|o| o.get_generics()).flatten())
                .collect(),
            TypeVar::Index(base, index) => base
                .get_generics()
                .into_iter()
                .chain(index.get_generics().into_iter())
                .collect(),
            TypeVar::Prop(base, _) => base.get_generics(),
            TypeVar::MustBeSame(types) => {
                types.iter().map(|t| t.get_generics()).flatten().collect()
            }
        }
    }
}

impl TypeVar {
    fn same(v: Vec<Datatype>, new: &Datatype) -> Self {
        if v.contains(new) {
            Self::MustBeSame(v)
        } else if let Some(Self::MustBeSame(vals)) = new.downcast::<Self>() {
            let mut res = v;
            for val in vals {
                res = if let Self::MustBeSame(r) = Self::same(res, &val) {
                    r
                } else {
                    unreachable!()
                }
            }
            Self::MustBeSame(res)
        } else {
            Self::MustBeSame(v.into_iter().chain(vec![new.clone()]).collect())
        }
    }
}
