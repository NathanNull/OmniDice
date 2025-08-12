use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

use serde::{
    Deserialize, Serialize,
    de::{self, VariantAccess, Visitor},
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, EnumIter, Serialize, Deserialize)]
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

#[derive(Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, PartialEq, Serialize)]
pub struct Expr {
    pub contents: ExprContents,
    pub output: Datatype,
}

#[doc(hidden)]
#[allow(
    non_upper_case_globals,
    unused_attributes,
    unused_qualifications,
    clippy::absolute_paths
)]
const _: () = {
    #[allow(unused_extern_crates, clippy::useless_attribute)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for Expr {
        fn deserialize<__D>(__deserializer: __D) -> _serde::__private::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            #[doc(hidden)]
            enum __Field {
                __field0,
                __field1,
                __ignore,
            }
            #[doc(hidden)]
            struct __FieldVisitor;
            #[automatically_derived]
            impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                type Value = __Field;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::__private::Formatter,
                ) -> _serde::__private::fmt::Result {
                    _serde::__private::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> _serde::__private::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => _serde::__private::Ok(__Field::__field0),
                        1u64 => _serde::__private::Ok(__Field::__field1),
                        _ => _serde::__private::Ok(__Field::__ignore),
                    }
                }
                fn visit_str<__E>(
                    self,
                    __value: &str,
                ) -> _serde::__private::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "contents" => _serde::__private::Ok(__Field::__field0),
                        "output" => _serde::__private::Ok(__Field::__field1),
                        _ => _serde::__private::Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(
                    self,
                    __value: &[u8],
                ) -> _serde::__private::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"contents" => _serde::__private::Ok(__Field::__field0),
                        b"output" => _serde::__private::Ok(__Field::__field1),
                        _ => _serde::__private::Ok(__Field::__ignore),
                    }
                }
            }
            #[automatically_derived]
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(
                    __deserializer: __D,
                ) -> _serde::__private::Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            #[doc(hidden)]
            struct __Visitor<'de> {
                marker: _serde::__private::PhantomData<Expr>,
                lifetime: _serde::__private::PhantomData<&'de ()>,
            }
            #[automatically_derived]
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = Expr;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::__private::Formatter,
                ) -> _serde::__private::fmt::Result {
                    _serde::__private::Formatter::write_str(__formatter, "struct Expr")
                }
                #[inline]
                fn visit_seq<__A>(
                    self,
                    mut __seq: __A,
                ) -> _serde::__private::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 =
                        match _serde::de::SeqAccess::next_element::<ExprContents>(&mut __seq)? {
                            _serde::__private::Some(__value) => __value,
                            _serde::__private::None => {
                                return _serde::__private::Err(_serde::de::Error::invalid_length(
                                    0usize,
                                    &"struct Expr with 2 elements",
                                ));
                            }
                        };
                    let __field1 =
                        match _serde::de::SeqAccess::next_element::<Datatype>(&mut __seq)? {
                            _serde::__private::Some(__value) => __value,
                            _serde::__private::None => {
                                return _serde::__private::Err(_serde::de::Error::invalid_length(
                                    1usize,
                                    &"struct Expr with 2 elements",
                                ));
                            }
                        };
                    _serde::__private::Ok(Expr {
                        contents: __field0,
                        output: __field1,
                    })
                }
                #[inline]
                fn visit_map<__A>(
                    self,
                    mut __map: __A,
                ) -> _serde::__private::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: _serde::__private::Option<ExprContents> =
                        _serde::__private::None;
                    let mut __field1: _serde::__private::Option<Datatype> = _serde::__private::None;
                    while let _serde::__private::Some(__key) =
                        _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if _serde::__private::Option::is_some(&__field0) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "contents",
                                        ),
                                    );
                                }
                                __field0 =
                                    _serde::__private::Some(_serde::de::MapAccess::next_value::<
                                        ExprContents,
                                    >(
                                        &mut __map
                                    )?);
                            }
                            __Field::__field1 => {
                                if _serde::__private::Option::is_some(&__field1) {
                                    return _serde::__private::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "output",
                                        ),
                                    );
                                }
                                __field1 =
                                    _serde::__private::Some(_serde::de::MapAccess::next_value::<
                                        Datatype,
                                    >(
                                        &mut __map
                                    )?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        _serde::__private::Some(__field0) => __field0,
                        _serde::__private::None => {
                            _serde::__private::de::missing_field("contents")?
                        }
                    };
                    let __field1 = match __field1 {
                        _serde::__private::Some(__field1) => __field1,
                        _serde::__private::None => _serde::__private::de::missing_field("output")?,
                    };
                    _serde::__private::Ok(Expr {
                        contents: __field0,
                        output: __field1,
                    })
                }
            }
            #[doc(hidden)]
            const FIELDS: &'static [&'static str] = &["contents", "output"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "Expr",
                FIELDS,
                __Visitor {
                    marker: _serde::__private::PhantomData::<Expr>,
                    lifetime: _serde::__private::PhantomData,
                },
            )
        }
    }
};

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

#[derive(Debug, Clone, PartialEq)]
pub struct Binop {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: Op,
    pub op_loc: LineIndex,
    pub res: BinOpFn,
    pub out: Datatype,
}

impl Serialize for Binop {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (&self.lhs, self.op, &self.rhs, self.op_loc).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Binop {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (lhs, rhs, op, op_loc) =
            <(Box<Expr>, Op, Box<Expr>, LineIndex)>::deserialize(deserializer)?;
        Ok(Self::new(lhs, op, rhs, op_loc).expect("Saved ASTs should be valid"))
    }
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

impl Serialize for Prefix {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (self.op, &self.rhs, self.op_loc).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Prefix {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (op, rhs, op_loc) = <(Op, Box<Expr>, LineIndex)>::deserialize(deserializer)?;
        Ok(Self::new(op, rhs, op_loc).expect("Saved ASTs should be valid"))
    }
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

impl Serialize for Postfix {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (&self.lhs, self.op, self.op_loc).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Postfix {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (lhs, op, op_loc) = <(Box<Expr>, Op, LineIndex)>::deserialize(deserializer)?;
        Ok(Self::new(op, lhs, op_loc).expect("Saved ASTs should be valid"))
    }
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

impl Serialize for Accessor {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Accessor::Variable(name, pos) => {
                serializer.serialize_newtype_variant("Accessor", 0, "Variable", &(name.clone(), *pos))
            }
            Accessor::Property(base, prop, pos, _, _, _) => {
                serializer.serialize_newtype_variant("Accessor", 1, "Property", &(base, prop, pos))
            }
            Accessor::Index(base, pos, indices) => serializer.serialize_newtype_variant(
                "Accessor",
                2,
                "Index",
                &(
                    base,
                    pos,
                    indices.iter().map(|(i, ..)| i).collect::<Vec<_>>(),
                ),
            ),
        }
    }
}

impl<'de> Deserialize<'de> for Accessor {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        const VARIANTS: &[&str] = &["Variable", "Property", "Index"];

        struct AccessorVisitor;
        impl<'de> Visitor<'de> for AccessorVisitor {
            type Value = Accessor;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("an Accessor")
            }

            fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::EnumAccess<'de>,
            {
                let (v, va) = data.variant::<String>()?;
                match v.as_str() {
                    "Variable" => {
                        let (name, pos) = va.newtype_variant()?;
                        Ok(Accessor::Variable(name, pos))
                    }
                    "Property" => {
                        let (base, prop, loc) = va.newtype_variant()?;
                        Ok(Accessor::new_property(base, prop, loc)
                            .expect("Saved ASTs should be valid"))
                    }
                    "Index" => {
                        let (indexed, loc, indices) = va.newtype_variant()?;
                        Ok(Accessor::new_index(indexed, indices, loc)
                            .expect("Saved ASTs should be valid"))
                    }
                    v => Err(de::Error::unknown_variant(v, VARIANTS)),
                }
            }
        }

        deserializer.deserialize_enum("Accessor", VARIANTS, AccessorVisitor)
    }
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

#[derive(Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct While {
    pub condition: Box<Expr>,
    pub result: Box<Expr>,
}

impl Debug for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while {:?} do {:?}", self.condition, self.result)
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

impl Serialize for Call {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (&self.base, &self.params, self.loc, &self.out).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Call {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (base, params, loc, out) =
            <(Box<Expr>, Vec<Expr>, LineIndex, Datatype)>::deserialize(deserializer)?;
        Ok(Self::new(base, params, loc, Some(out)).expect("Saved ASTs should be valid"))
    }
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

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct GenericSpecify {
    pub base: Box<Expr>,
    pub types: Vec<Datatype>,
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Return {
    pub ret: Box<Expr>,
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Break;

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Continue;
