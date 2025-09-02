use std::sync::LazyLock;

use crate::{gen_fn_map, invalid, od_typedef, type_init};

use super::*;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

static TV1: &str = "__T";

static UNWRAP_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func<TV1>() -> (TV1) owner {maybe (TV1)}}));

fn unwrap_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
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

static FILLED_SIG: LazyLock<FuncT> = LazyLock::new(|| od_typedef!({func<TV1>() -> BoolT owner {maybe (TV1)}}));

fn filled_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let mut it = params.iter().cloned();
    if let Some(me) = it.next_as::<Maybe>() {
        Ok(Box::new(me.contents.is_some()))
    } else {
        invalid!("Call", "unwrap", params);
    }
}

#[cfg_attr(feature = "serde", typetag::serde)]
impl Type for MaybeT {
    fn real_prop_type(
        &self,
        name: &str,
    ) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name,
            self,
            "Maybe",
            ("unwrap", UNWRAP_SIG, unwrap_fn, unwrap_prop),
            ("filled", FILLED_SIG, filled_fn, filled_prop),
        )
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Datatype, String> {
        Ok(Box::new(Self {
            output: self.output.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
        self.output.try_match(
            &other
                .downcast::<Self>()
                .ok_or_else(|| format!("Can't match {self} with {other}"))?
                .output,
        )
    }
    fn get_generics(&self) -> Vec<String> {
        self.output.get_generics()
    }
    fn is_hashable(&self) -> bool {
        self.output.is_hashable()
    }
}

#[cfg_attr(feature = "serde", typetag::serde)]
impl Val for Maybe {
    fn hash(&self, h: &mut dyn Hasher) -> Result<(), RuntimeError> {
        if let Some(c) = &self.contents {
            c.as_ref().hash(h)?;
        }
        Ok(())
    }
}
