use std::sync::{LazyLock, RwLockReadGuard};

use super::*;
use crate::{
    gen_fn_map, invalid, mut_type_init, type_init,
    types::arr::{ITER_RET_SIG, iter_ret_fn},
};

#[derive(Clone)]
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
    fn new(elements: HashMap<Value, Value>, key: Datatype, value: Datatype) -> Self {
        if !key.is_hashable() {
            unreachable!("Invalid map key type {}", key)
        }
        Self {
            key,
            value,
            elements,
        }
    }
}

mut_type_init!(Map, _InnerMap);

impl Map {
    pub fn new(elements: HashMap<Value, Value>, key: Datatype, value: Datatype) -> Self {
        Self::make(_InnerMap::new(elements, key, value))
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

fn iter_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
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
            return Box::new(Iter {
                output: out,
                next_fn: Box::new(ITER_RET_SIG.clone().make_rust_member(
                    iter_ret_fn,
                    Box::new(Tuple::new(vec![Box::new(0), Box::new(arr)])),
                )),
            });
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

fn set_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Map>() {
        if let Some(key) = p_iter.next() {
            assert_eq!(arr.inner().key, key.get_type(), "Invalid type");
            if let Some(val) = p_iter.next() {
                assert_eq!(arr.inner().value, val.get_type(), "Invalid type");
                arr.inner_mut().elements.insert(key, val);
                return Box::new(Void);
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

fn get_fn(params: Vec<Value>, _i: &mut Interpreter, _o: Option<Datatype>) -> Value {
    let mut p_iter = params.iter().cloned();
    if let Some(arr) = p_iter.next_as::<Map>() {
        if let Some(key) = p_iter.next() {
            return arr
                .inner_mut()
                .elements
                .get(&key)
                .expect("Can't pop from empty array")
                .clone();
        }
    }
    invalid!("Call", "push", params)
}

gen_fn_map!(
    MAP_FNS,
    ("iter", ITER_SIG, iter_fn),
    ("set", SET_SIG, set_fn),
    ("get", GET_SIG, get_fn)
);

impl Type for MapT {
    fn real_bin_op_result(&self, other: &Datatype, op: Op) -> Option<Datatype> {
        if other == self {
            match op {
                Op::Plus => Some(self.dup()),
                _ => None,
            }
        } else {
            None
        }
    }

    fn real_prop_type(&self, name: &str) -> Option<Datatype> {
        match name {
            "length" => Some(Box::new(IntT)),
            n if MAP_FNS.contains_key(n) => {
                println!("Getting {n}");
                let ret = Some(
                    MAP_FNS[n]
                        .0
                        .clone()
                        .with_owner(self.dup())
                        .expect("Invalid owner")
                        .dup(),
                );
                println!("Got {ret:?}");
                ret
            }
            _ => None,
        }
    }

    fn real_index_type(&self, index: &Datatype) -> Option<Datatype> {
        if index == &self.key {
            Some(self.value.clone())
        } else if let Some(tup) = (index.dup() as Box<dyn Any>).downcast_ref::<TupT>() {
            if tup.entries.iter().all(|e| e == &self.key) {
                Some(self.dup())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn insert_generics(&self, generics: &HashMap<String, Datatype>) -> Option<Datatype> {
        Some(Box::new(Self {
            key: self.key.insert_generics(generics)?,
            value: self.value.insert_generics(generics)?,
        }))
    }
    fn real_try_match(&self, other: &Datatype) -> Option<HashMap<String, Datatype>> {
        let other = other.downcast::<Self>()?;
        let mut vars = HashMap::new();
        for (t, v) in [(&self.key, &other.key), (&self.value, &other.value)] {
            let matched = t.try_match(v)?;
            for (name, var) in matched {
                if let Some(res) = vars.get(&name) {
                    if *res != *var {
                        return None;
                    }
                } else {
                    vars.insert(name, var);
                }
            }
        }
        Some(vars)
    }
    fn get_generics(&self) -> Vec<String> {
        self.key
            .get_generics()
            .into_iter()
            .chain(self.value.get_generics())
            .collect()
    }
}

impl Val for Map {
    fn bin_op(&self, other: &Value, op: Op) -> Value {
        if other.get_type() == self.get_type() {
            let rhs = other
                .downcast::<Self>()
                .expect("Just checked if it was the right type");
            match op {
                Op::Plus => Box::new(Self::new(
                    {
                        let mut m = self.inner().elements.clone();
                        for (k, v) in rhs.inner().elements.iter() {
                            m.insert(k.clone(), v.clone());
                        }
                        m
                    },
                    self.inner().key.clone(),
                    self.inner().value.clone(),
                )),
                _ => invalid!(op, self, other),
            }
        } else {
            invalid!(op, self, other);
        }
    }

    fn get_prop(&self, name: &str) -> Value {
        match name {
            "length" => Box::new(self.inner().elements.len() as i32),
            n if MAP_FNS.contains_key(n) => Box::new(
                MAP_FNS[n]
                    .0
                    .clone()
                    .make_rust_member(MAP_FNS[n].1, self.dup()),
            ),
            _ => invalid!("Prop", self, name),
        }
    }

    fn get_index(&self, index: Value) -> Value {
        if index.get_type() == self.inner().key {
            self.inner()
                .elements
                .get(&index)
                .expect("Invalid index")
                .dup()
        } else {
            invalid!("Index", self, index.get_type())
        }
    }

    fn set_index(&self, index: Value, value: Value) {
        if index.get_type() == self.inner().key {
            assert_eq!(value.get_type(), self.inner().value);
            *self
                .inner_mut()
                .elements
                .get_mut(&index)
                .expect("Invalid index") = value;
        } else {
            invalid!("Index", self, index.get_type())
        }
    }
}
