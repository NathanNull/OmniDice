use crate::interpreter::types::Value;

use super::*;

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
                let l_created = assign.assignee.assigned_vars().collect::<Vec<_>>();
                Box::new(
                    match &assign.a_type {
                        AssignType::Reassign => assign.assignee.used_vars(),
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
            ExprContents::Accessor(accessor) => accessor.used_vars(),
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
                    AssignType::Reassign => assign.assignee.assigned_vars(),
                    AssignType::Create => match &assign.assignee {
                        Accessor::Variable(v, _) => Box::new([v.clone()].into_iter()),
                        _ => unreachable!("Unexpected non-variable in let statement"),
                    },
                }
                .chain(assign.val.assigned_variables()),
            ),
            ExprContents::Accessor(accessor) => accessor.assigned_vars(),
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
                assignee: assign.assignee.replace_generics(generics)?,
                val: assign.val.replace_generics(generics)?,
                a_type: assign.a_type.clone(),
                a_loc: assign.a_loc,
            }),
            ExprContents::Accessor(accessor) => {
                ExprContents::Accessor(accessor.replace_generics(generics)?)
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
                loc: wh.loc,
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

    pub fn could_contain_break(&self) -> bool {
        match &self.contents {
            ExprContents::Value(_) => false,
            ExprContents::Binop(binop) => {
                binop.lhs.could_contain_break() || binop.rhs.could_contain_break()
            }
            ExprContents::Prefix(prefix) => prefix.rhs.could_contain_break(),
            ExprContents::Postfix(postfix) => postfix.lhs.could_contain_break(),
            ExprContents::Assign(assign) => {
                assign.assignee.could_contain_break() || assign.val.could_contain_break()
            }
            ExprContents::Accessor(accessor) => accessor.could_contain_break(),
            ExprContents::Scope(exprs) => exprs.iter().any(|e| e.could_contain_break()),
            ExprContents::Conditional(conditional) => {
                conditional.condition.could_contain_break()
                    || (conditional.condition.contents != ExprContents::Value(Box::new(false))
                        && conditional.result.could_contain_break())
                    || (conditional.condition.contents != ExprContents::Value(Box::new(true))
                        && conditional
                            .otherwise
                            .as_ref()
                            .is_some_and(|e| e.could_contain_break()))
            }
            ExprContents::While(_) => false,
            ExprContents::For(_) => false,
            ExprContents::Array(array) => array.elements.iter().any(|e| e.could_contain_break()),
            ExprContents::Tuple(tuple) => tuple.elements.iter().any(|e| e.could_contain_break()),
            ExprContents::Function(_) => false,
            ExprContents::Call(call) => {
                call.base.could_contain_break()
                    || call.params.iter().any(|e| e.could_contain_break())
            }
            ExprContents::GenericSpecify(generic_specify) => {
                generic_specify.base.could_contain_break()
            }
            ExprContents::Return(_) => true,
            ExprContents::Break(_) => true,
            ExprContents::Continue(_) => false,
        }
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

impl Accessor {
    fn used_vars<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        match self {
            Accessor::Variable(v, _) => Box::new([v.clone()].into_iter()),
            Accessor::Property(base, ..) => base.used_variables(),
            Accessor::Index(indexed, _, indices) => {
                let mut vars = indexed.used_variables().collect::<Vec<_>>();
                let mut assigned_vars = indexed.assigned_variables().collect::<Vec<_>>();
                for (index, ..) in indices {
                    vars = vars
                        .into_iter()
                        .chain(
                            index
                                .used_variables()
                                .filter(|v| !assigned_vars.contains(v)),
                        )
                        .collect();
                    assigned_vars = assigned_vars
                        .into_iter()
                        .chain(index.assigned_variables())
                        .collect();
                }
                Box::new(vars.into_iter())
            }
        }
    }

    fn assigned_vars<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        match self {
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

    fn replace_generics(&self, generics: &HashMap<String, Datatype>) -> Result<Accessor, String> {
        Ok(match self {
            Accessor::Variable(_, _) => self.clone(),
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

    fn could_contain_break(&self) -> bool {
        match self {
            Accessor::Variable(..) => false,
            Accessor::Property(expr, ..) => expr.could_contain_break(),
            Accessor::Index(expr, _, items) => {
                expr.could_contain_break() || items.iter().any(|i| i.0.could_contain_break())
            }
        }
    }
}
