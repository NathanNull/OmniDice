use std::sync::{LazyLock, RwLockReadGuard};

use super::*;
use crate::{
    gen_fn_map, invalid, mut_type_init, op_list, type_init,
    types::arr::{ITER_RET_SIG, iter_ret_fn},
};

#[derive(Clone, Serialize, Deserialize)]
pub struct _InnerMap {
    key: Datatype,
    value: Datatype,
    pub elements: HashMap<Value, Value>,
}

impl PartialEq for _InnerMap {
    fn eq(&self, other: &Self) -> bool {
        if !(self.key == other.key && self.value == other.value) {
            return false;
        }
        if self.elements.len() != other.elements.len() {
            return false;
        }
        self.elements
            .iter()
            .all(|(k, v)| other.elements.get(k).is_some_and(|o| o == v))
    }
}

impl Display for _InnerMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let len = self.elements.len();
        for (i, (key, val)) in self.elements.iter().enumerate() {
            write!(f, "{}: {}", key, val)?;
            if i != len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl Debug for _InnerMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let len = self.elements.len();
        for (i, (key, val)) in self.elements.iter().enumerate() {
            write!(f, "{:?}: {:?}", key, val)?;
            if i != len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl _InnerMap {
    fn new(
        elements: HashMap<Value, Value>,
        key: Datatype,
        value: Datatype,
    ) -> Result<Self, String> {
        if !key.is_hashable() {
            return Err(format!("Invalid map key type {}", key));
        }
        Ok(Self {
            key,
            value,
            elements,
        })
    }
}

mut_type_init!(Map, _InnerMap);

impl Map {
    pub fn new(
        elements: HashMap<Value, Value>,
        key: Datatype,
        value: Datatype,
    ) -> Result<Self, String> {
        Ok(Self::make(_InnerMap::new(elements, key, value)?))
    }
}

type_init!(MapT, Map, "map", (RwLockReadGuard<_InnerMap>), key: Datatype, value: Datatype);

static TV1_NAME: &str = "__T1";
static TV1: LazyLock<Datatype> = LazyLock::new(|| Box::new(TypeVar::Var(TV1_NAME.to_string())));

static TV2_NAME: &str = "__T2";
static TV2: LazyLock<Datatype> = LazyLock::new(|| Box::new(TypeVar::Var(TV2_NAME.to_string())));

static ITER_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IterT {
        output: Box::new(TupT {
            entries: vec![TV1.clone(), TV2.clone()],
        }),
    }),
    generic: vec![TV1_NAME.to_string(), TV2_NAME.to_string()],
    owner_t: Some(Box::new(MapT {
        key: TV1.clone(),
        value: TV2.clone(),
    })),
});

fn iter_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut p_iter = params.iter().cloned();
    if let Some(map) = p_iter.next_as::<Map>() {
        if p_iter.next().is_none() {
            let imap = map.inner();
            let out = Box::new(TupT {
                entries: vec![imap.key.clone(), imap.value.clone()],
            });
            // TODO: see if there's a smarter way to do this
            // rather than copying the entire map into an array
            let arr = Arr::new(
                imap.elements
                    .iter()
                    .map(|(k, v)| Box::new(Tuple::new(vec![k.clone(), v.clone()])) as Value)
                    .collect(),
                out.clone(),
            );
            return Ok(Box::new(Iter {
                output: out,
                next_fn: Box::new(ITER_RET_SIG.clone().make_rust_member(
                    iter_ret_fn,
                    "map_iter_ret_fn".to_string(),
                    Box::new(Tuple::new(vec![Box::new(0), Box::new(arr)])),
                )?),
            }));
        }
    }
    invalid!("Call", "iter", params);
}

static SET_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV1.clone(), TV2.clone()],
    output: Box::new(Void),
    generic: vec![TV1_NAME.to_string(), TV2_NAME.to_string()],
    owner_t: Some(Box::new(MapT {
        key: TV1.clone(),
        value: TV2.clone(),
    })),
});

fn set_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Map>() {
        if let Some(key) = p_iter.next() {
            if arr.inner().key != key.get_type() {
                return Err(RuntimeError::partial("Invalid key type for map"));
            }
            if let Some(val) = p_iter.next() {
                if arr.inner().value != val.get_type() {
                    return Err(RuntimeError::partial("Invalid value type for map"));
                }
                arr.inner_mut().elements.insert(key, val);
                return Ok(Box::new(Void));
            }
        }
    }
    invalid!("Call", "push", params)
}

static GET_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![TV1.clone()],
    output: TV2.clone(),
    generic: vec![TV1_NAME.to_string(), TV2_NAME.to_string()],
    owner_t: Some(Box::new(MapT {
        key: TV1.clone(),
        value: TV2.clone(),
    })),
});

fn get_fn(
    params: Vec<Value>,
    _i: &mut Interpreter,
    _o: Option<Datatype>,
) -> Result<Value, RuntimeError> {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Map>() {
        if let Some(key) = p_iter.next() {
            return Ok(arr
                .inner_mut()
                .elements
                .get(&key)
                .ok_or_else(|| {
                    RuntimeError::partial(&format!("Can't access nonexistent key {key:?} of map"))
                })?
                .clone());
        }
    }
    invalid!("Call", "push", params)
}

static LENGTH_SIG: LazyLock<FuncT> = LazyLock::new(|| FuncT {
    params: vec![],
    output: Box::new(IntT),
    generic: vec![TV1_NAME.to_string(), TV2_NAME.to_string()],
    owner_t: Some(Box::new(MapT {
        key: TV1.clone(),
        value: TV2.clone(),
    })),
});

fn length_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> OpResult {
    let arr = params[0]
        .downcast::<Map>()
        .ok_or_else(|| RuntimeError::partial("arrlen owner isn't array"))?;
    Ok(Box::new(arr.inner().elements.len() as i32))
}

#[typetag::serde]
impl Type for MapT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Result<(Datatype, BinOpFn), String> {
        if other == self {
            op_list!(op => {
                Plus(l: Map, r: Map) -> (self.clone()) |l: Map,r: Map| Map::new(
                        {
                            let mut m = l.inner().elements.clone();
                            for (k, v) in r.inner().elements.iter() {
                                m.insert(k.clone(), v.clone());
                            }
                            m
                        },
                        l.inner().key.clone(),
                        l.inner().value.clone(),
                    )
                    .map_err(|e| RuntimeError::partial(&e));
            })
        } else {
            Err(format!("Can't operate {self} {op:?} {other}"))
        }
    }

    fn real_prop_type(
        &self,
        name: &str,
    ) -> Result<(Datatype, Option<UnOpFn>, Option<SetFn>), String> {
        gen_fn_map!(
            name,
            self,
            "Map",
            ("iter", ITER_SIG, iter_fn, iter_prop),
            ("set", SET_SIG, set_fn, set_prop),
            ("get", GET_SIG, get_fn, get_prop),
            ("length", LENGTH_SIG, length_fn, length_prop),
        )
    }

    fn real_index_type(&self, index: &Datatype) -> Result<(Datatype, BinOpFn, SetAtFn), String> {
        if index == &self.key {
            fn get_fn(me: &Expr, idx: &Expr, i: &mut Interpreter) -> OpResult {
                let index = i.eval_expr(idx)?;
                Ok(i.try_eval_as::<Map>(me)?
                    .inner()
                    .elements
                    .get(&index)
                    .ok_or_else(|| {
                        RuntimeError::partial(&format!(
                            "Can't access nonexistent key {index:?} of map"
                        ))
                    })?
                    .dup())
            }
            fn set_fn(me: &Expr, idx: &Expr, val: &Expr, i: &mut Interpreter) -> VoidResult {
                let index = i.eval_expr(idx)?;
                let me = i.try_eval_as::<Map>(me)?;
                let val = i.eval_expr(val)?;
                if val.get_type() != me.inner().value {
                    return Err(RuntimeError::partial(
                        "UNREACHABLE: invalid value type for map insertion",
                    ));
                }
                *me.inner_mut().elements.get_mut(&index).ok_or_else(|| {
                    RuntimeError::partial(&format!(
                        "Can't access nonexistent key {index:?} of map, consider using .set instead."
                    ))
                })? = val;
                Ok(())
            }
            Ok((self.value.clone(), get_fn, set_fn))
        } else {
            Err(format!("Can't get index {index} of {self}"))
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Datatype, String> {
        Ok(Box::new(Self {
            key: self.key.insert_generics(generics)?,
            value: self.value.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Result<HashMap<String, Datatype>, String> {
        let other = other
            .downcast::<Self>()
            .ok_or_else(|| format!("Can't match {self} with {other}"))?;
        let mut vars = HashMap::new();
        for (t, v) in [(&self.key, &other.key), (&self.value, &other.value)] {
            let matched = t.try_match(v)?;
            for (name, var) in matched {
                if let Some(res) = vars.get(&name) {
                    if *res != *var {
                        return Err(format!("Can't match {self} with {other}"));
                    }
                } else {
                    vars.insert(name, var);
                }
            }
        }
        Ok(vars)
    }
    fn get_generics(&self) -> Vec<String> {
        self.key
            .get_generics()
            .into_iter()
            .chain(self.value.get_generics())
            .collect()
    }
}

#[typetag::serde]
impl Val for Map {}
