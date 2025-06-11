use super::*;

#[derive(Clone, Debug)]
pub enum TypeVar {
    Var(String),
    BinOp(Datatype, Datatype, Op),
    UnaryOp(Datatype, Op, bool),
    Call(Datatype, Vec<Datatype>),
    Index(Datatype, Datatype),
    Prop(Datatype, String),
    MustBeSame(Datatype, Datatype),
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
            TypeVar::Call(base, items) => format!(
                "<{} call ({})>",
                base,
                items
                    .iter()
                    .map(|i| format!("{i}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TypeVar::Index(base, index) => format!("<{} [{}]>", base, index),
            TypeVar::Prop(base, prop) => format!("<{} [{}]>", base, prop),
            TypeVar::MustBeSame(t1, t2) => format!("<{t1} sameas {t2}>")
        }
    }

    fn dup(&self) -> Datatype {
        Box::new(self.clone())
    }
}

impl Type for TypeVar {
    fn bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        Some(Box::new(Self::BinOp(self.dup(), other.dup(), op)))
    }

    fn call_result(&self, params: Vec<Datatype>) -> Option<Datatype> {
        Some(Box::new(Self::Call(self.dup(), params)))
    }

    fn index_type(&self, index: &Datatype) -> Option<Datatype> {
        Some(Box::new(Self::Index(self.dup(), index.dup())))
    }

    fn possible_call(&self) -> bool {
        true
    }

    fn post_op_result(&self, op: Op) -> Option<Datatype> {
        Some(Box::new(Self::UnaryOp(self.dup(), op, false)))
    }

    fn pre_op_result(&self, op: Op) -> Option<Datatype> {
        Some(Box::new(Self::UnaryOp(self.dup(), op, true)))
    }

    fn prop_type(&self, name: &str) -> Option<Datatype> {
        Some(Box::new(Self::Prop(self.dup(), name.to_string())))
    }
    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(match self {
            TypeVar::Var(v) => if let Some(dt) = generics.get(v) {
                dt.dup()
            } else {
                self.dup()
            },
            TypeVar::BinOp(lhs, rhs, op) => {
                let (lhs, rhs) = (lhs.insert_generics(generics)?, rhs.insert_generics(generics)?);
                lhs.bin_op_result(&rhs, *op)?
            },
            TypeVar::UnaryOp(operand, op, pre) => {
                let operand = operand.insert_generics(generics)?;
                if *pre {
                    operand.pre_op_result(*op)?
                } else  {
                    operand.post_op_result(*op)?
                }
            },
            TypeVar::Call(base, items) => {
                let base = base.insert_generics(generics)?;
                let mut params = vec![];
                for i in items {
                    params.push(i.insert_generics(generics)?)
                }
                base.call_result(params)?
            },
            TypeVar::Index(base, index) => {
                base.insert_generics(generics)?.index_type(&index.insert_generics(generics)?)?
            },
            TypeVar::Prop(base, prop) => {
                base.insert_generics(generics)?.prop_type(&prop)?
            },
            TypeVar::MustBeSame(t1, t2) => {
                t1.insert_generics(generics)?.assert_same(&t2.insert_generics(generics)?)
            }
        })
    }

    fn try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        Some(match self {
            TypeVar::Var(v) => HashMap::from_iter([(v.clone(), other.dup())]),
            _ => todo!()
        })
    }

    fn assert_same(&self, other: &Datatype) -> Datatype {
        Box::new(Self::MustBeSame(self.dup(), other.dup()))
    }
}
