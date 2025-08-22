use crate::type_init;

use super::*;

#[derive(Clone, Debug, Serialize, Deserialize)]
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

#[typetag::serde]
impl Type for TypeVar {
    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Datatype, String> {
        Ok(match self {
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
                lhs.bin_op_result(&rhs, *op)?.0
            }
            TypeVar::UnaryOp(operand, op, pre) => {
                let operand = operand.insert_generics(generics)?;
                if *pre {
                    operand.pre_op_result(*op)?.0
                } else {
                    operand.post_op_result(*op)?.0
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
                base.call_result(params, expected_output)?.0
            }
            TypeVar::Index(base, index) => {
                base.insert_generics(generics)?
                    .index_type(&index.insert_generics(generics)?)?
                    .0
            }
            TypeVar::Prop(base, prop) => base.insert_generics(generics)?.prop_type(&prop)?.0,
            TypeVar::MustBeSame(types) => {
                let mut t_iter = types.iter();
                let mut res = t_iter
                    .next()
                    .ok_or_else::<String, _>(|| unreachable!("This should never be empty"))?
                    .insert_generics(generics)?;
                for t in t_iter {
                    res = res.assert_same(&t.insert_generics(generics)?);
                }
                res
            }
        })
    }

    fn real_try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
        Ok(match self {
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
                        .ok_or_else(|| format!("Can't match {self} with {other}"))?
                        .get(k)
                        .ok_or_else(|| format!("Can't match {self} with {other}"))?;
                    matches.insert(k.clone(), match_val.clone());
                    if t_matches
                        .iter()
                        .any(|m| m.get(k).is_some_and(|v| v != match_val))
                    {
                        return Err(format!("Can't match all of {types:?} with {other}"));
                    }
                }
                matches
            }
            _ => {
                return Err(format!(
                    "Can't match {self} with {other} (should be unreachable since it should get stopped in try_match before reaching this code)"
                ));
            }
        })
    }

    fn assert_same(&self, other: &Datatype) -> Datatype {
        if &self.dup() == other {
            self.dup()
        } else if let Self::MustBeSame(v) = self {
            Box::new(Self::MustBeSame(Self::same(v.clone(), other)))
        } else {
            Box::new(Self::MustBeSame(Self::same(vec![self.dup()], other)))
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
    fn same(v: Vec<Datatype>, new: &Datatype) -> Vec<Datatype> {
        if v.contains(new) {
            v
        } else if let Some(Self::MustBeSame(vals)) = new.downcast::<Self>() {
            let mut res = v;
            for val in vals {
                res = Self::same(res, &val)
            }
            res
        } else {
            v.into_iter().chain(vec![new.clone()]).collect()
        }
    }
}

type_init!(Void, Void, "()");
#[typetag::serde]
impl Type for Void {}
#[typetag::serde]
impl Val for Void {
    fn hash(&self, _: &mut dyn Hasher) -> Result<(), RuntimeError> {
        Ok(())
    }
}

impl PartialEq for Void {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

type_init!(Never, Never, "!");
#[typetag::serde]
impl Type for Never {
    fn assert_same(&self, other: &Datatype) -> Datatype {
        // Never coerces to whatever you need it to be
        other.clone()
    }
}
#[typetag::serde]
impl Val for Never {}

impl PartialEq for Never {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
