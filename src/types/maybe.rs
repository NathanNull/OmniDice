use std::sync::LazyLock;

use crate::{invalid, type_init};

use super::*;

#[derive(Debug, Clone)]
pub struct Maybe {
    pub output: Datatype,
    pub contents: Option<Value>,
}

impl PartialEq for Maybe {
    fn eq(&self, other: &Self) -> bool {
        &self.contents == &other.contents && self.output == other.output
    }
}

impl Display for Maybe {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.contents {
            Some(c) => write!(f, "Filled({c})"),
            None => write!(f, "Null"),
        }
    }
}

type_init!(MaybeT, Maybe, "maybe", output: Datatype);

static TV1_NAME: &str = "__T";
static TV1: LazyLock<Datatype> = LazyLock::new(|| Box::new(TypeVar::Var(TV1_NAME.to_string())));

static UNWRAP_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: TV1.clone(),
    generic: vec![TV1_NAME.to_string()],
    owner_t: Some(Box::new(MaybeT {
        output: TV1.clone(),
    })),
});

fn unwrap_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Result<Value, RuntimeError> {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<Maybe>() {
        match me.contents {
            Some(v) => Ok(v),
            None => Err(RuntimeError::partial("Attempted to unwrap null value")),
        }
    } else {
        invalid!("Call", "unwrap", params);
    }
}

impl Type for MaybeT {
    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "unwrap" => Some(Box::new(UNWRAP_SIG.clone().with_owner(self.dup()).ok()?)),
            "filled" => Some(Box::new(BoolT)),
            _ => None,
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(Box::new(Self {
            output: self.output.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        self.output.try_match(&other.downcast::<Self>()?.output)
    }
    fn get_generics(&self) -> Vec<String> {
        self.output.get_generics()
    }
    fn is_hashable(&self) -> bool {
        self.output.is_hashable()
    }
}
impl Val for Maybe {
    fn get_prop(&self, name: &str) -> Result<Value, RuntimeError> {
        Ok(match name {
            "unwrap" => Box::new(UNWRAP_SIG.clone().make_rust_member(unwrap_fn, self.dup())?),
            "filled" => Box::new(self.contents.is_some()),
            _ => invalid!("Prop", self, name),
        })
    }
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        if let Some(c) = &self.contents {
            c.as_ref().hash(h)?;
        }
        Ok(())
    }
}
