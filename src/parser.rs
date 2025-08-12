use std::{collections::HashMap, sync::LazyLock, vec::IntoIter};

use crate::{
    tokeniter::{TokenIter, TokenWidth},
    builtins::BUILTINS,
    error::{LineIndex, ParseError},
    interpreter::{Interpreter, VarScope},
    lexer::{Bracket, Keyword, OpLike, Token, TokenString},
    types::{
        ArrT, BoolT, Datatype, DiceT, Downcast, FloatT, FuncT, IntT, IterT, MaybeT, RefT, StringT,
        TupT, TypeVar, Void,
    },
};

pub mod expr;
pub use expr::*;

pub mod vars;

use strum::IntoEnumIterator;

pub struct Parser {
    tokens: TokenIter<Token, IntoIter<(Token, TokenWidth)>>,
    var_types: Vec<VarScope<(Datatype, bool)>>,
    typedefs: Vec<VarScope<Datatype>>,
    break_t_stack: Vec<bool>,
    return_t_stack: Vec<Datatype>,
}

static VOID: LazyLock<Expr> = LazyLock::new(|| Expr {
    contents: ExprContents::Value(Box::new(Void)),
    output: Box::new(Void),
});

static BOOL: LazyLock<Expr> = LazyLock::new(|| Expr {
    contents: ExprContents::Value(Box::new(false)),
    output: Box::new(BoolT),
});

impl Parser {
    pub fn new(tokens: TokenString) -> Self {
        Self {
            tokens: TokenIter::new(tokens.into_iter()),
            var_types: vec![VarScope {
                vars: HashMap::from_iter(
                    BUILTINS
                        .iter()
                        .map(|(name, var)| (name.clone(), (var.get_type(), false))),
                ),
                blocking: true,
                name: "builtins",
            }],
            typedefs: vec![VarScope {
                vars: HashMap::new(),
                blocking: true,
                name: "base",
            }],
            break_t_stack: vec![],
            return_t_stack: vec![],
        }
    }

    fn make_error<T>(&self, err_string: String) -> Result<T, ParseError> {
        return Err(ParseError {
            location: self.tokens.pos,
            info: err_string,
        });
    }

    fn new_expr(
        &mut self,
        contents: ExprContents,
        expected_type: Option<Datatype>,
    ) -> Result<Box<Expr>, ParseError> {
        let output = self.decide_type(&contents, expected_type)?;
        let expr = Box::new(Expr {
            output: output.clone(),
            contents,
        });
        Ok(match Interpreter::new_const().eval_expr(&expr) {
            Ok(val) => Box::new(Expr {
                contents: ExprContents::Value(val),
                output,
            }),
            Err(_) => expr,
        })
    }

    fn set_var_type(
        &mut self,
        name: String,
        dtype: Datatype,
        mutable: bool,
        initialize: bool,
    ) -> Result<(), ParseError> {
        if !initialize {
            if let Ok(old) = self.get_var_type(&name) {
                if old != dtype {
                    return self.make_error(format!(
                        "Can't reassign variable {name} of type {old:?} to type {dtype:?}",
                    ));
                }
            } else {
                return self
                    .make_error(format!("Tried to assign to uninitialized variable {name}"));
            }
        } else {
            match self.var_types.last_mut() {
                Some(sc) => sc.vars.insert(name, (dtype, mutable)),
                None => {
                    return self
                        .make_error("Tried to set variable while no scope alive".to_string());
                }
            };
        }
        Ok(())
    }

    fn get_var_type(&self, name: &str) -> Result<Datatype, ParseError> {
        for scope in self.var_types.iter().rev() {
            if let Some((dtype, _)) = scope.vars.get(name) {
                return Ok(dtype.clone());
            }
            if scope.blocking {
                break;
            }
        }
        self.make_error(format!(
            "Unknown variable {name}, consider adding 'let' before this assignment"
        ))
    }

    fn get_typedef(&self, name: &str) -> Option<Datatype> {
        for scope in self.typedefs.iter().rev() {
            if let Some(dtype) = scope.vars.get(name) {
                return Some(dtype.clone());
            }
            if scope.blocking {
                break;
            }
        }
        None
    }

    fn var_is_mutable(&self, name: &str) -> bool {
        for scope in self.var_types.iter().rev() {
            if let Some((_, mutable)) = scope.vars.get(name) {
                return *mutable;
            }
            if scope.blocking {
                break;
            }
        }
        false
    }

    fn decide_type(
        &self,
        contents: &ExprContents,
        expected_type: Option<Datatype>,
    ) -> Result<Datatype, ParseError> {
        let res = match contents {
            ExprContents::Accessor(acc) => match acc {
                Accessor::Variable(var, _) => self.get_var_type(var)?,
                Accessor::Property(.., out) => out.clone(),
                Accessor::Index(.., indices) => match indices.len() {
                    1 => indices[0].3.clone(),
                    _ => Box::new(TupT { entries: indices.iter().map(|(..,out)|out.clone()).collect() })
                },
            },
            ExprContents::Value(literal) => literal.get_type(),
            ExprContents::Binop(binop) => binop.out.clone(),
            ExprContents::Prefix(prefix) => prefix.out.clone(),
            ExprContents::Postfix(postfix) => postfix.out.clone(),
            ExprContents::Assign(assign) => assign.val.output.clone(),
            ExprContents::Scope(scope) => scope
                .last()
                .map(|expr| expr.output.clone())
                .unwrap_or(VOID.output.clone()),
            ExprContents::Conditional(cond) => cond.result.output.clone(),
            ExprContents::While(_) | ExprContents::For(_) => VOID.output.clone(),
            ExprContents::Array(arr) => Box::new(ArrT {
                entry: match arr
                    .elements
                    .first()
                    .map(|e| e.output.clone())
                    .or(expected_type
                        .as_ref()
                        .and_then(|t| t.downcast::<ArrT>())
                        .map(|arr| arr.entry))
                {
                    None => {
                        return self.make_error(
                            "Unknown type for empty array, please annotate".to_string(),
                        );
                    }
                    Some(at) => at,
                },
            }),
            ExprContents::Tuple(tup) => Box::new(TupT {
                entries: tup.elements.iter().map(|e| e.output.clone()).collect(),
            }),
            ExprContents::Function(func) => Box::new(FuncT {
                params: func.params.iter().map(|(_, t)| t.clone()).collect(),
                output: func.contents.output.clone(),
                generic: func.generic.clone(),
                owner_t: None,
            }),
            ExprContents::Call(call) => {
                let params: Vec<Datatype> = call.params.iter().map(|p| p.output.clone()).collect();
                match call
                    .base
                    .output
                    .call_result(params.clone(), expected_type.clone())
                {
                    Ok((ct, ..)) => ct,
                    Err(e) => {
                        return self.make_error(e);
                    }
                }
            }
            ExprContents::GenericSpecify(gspec) => {
                match gspec.base.output.specify_generics(&gspec.types) {
                    Ok(gt) => gt,
                    Err(e) => {
                        return self
                            .make_error(e);
                    }
                }
            }
            ExprContents::Return(ret) => ret.ret.output.clone(),
            ExprContents::Break(_) => VOID.output.clone(),
            ExprContents::Continue(_) => VOID.output.clone(),
        };
        Ok(if let Some(ty) = expected_type {
            ty.assert_same(&res)
        } else {
            res
        })
    }

    pub fn parse(&mut self) -> Result<Box<Expr>, ParseError> {
        let scope = ExprContents::Scope(self.parse_scope(false, Some(VOID.output.clone()))?);
        self.new_expr(scope, Some(VOID.output.clone()))
    }

    fn parse_scope(
        &mut self,
        is_inner: bool,
        expected_type: Option<Datatype>,
    ) -> Result<Scope, ParseError> {
        let mut exprs = vec![];
        let mut just_parsed = false;
        // Variables created in scope should only live while it does
        self.var_types.push(VarScope {
            vars: HashMap::new(),
            blocking: false,
            name: "scope",
        });
        let expected_end = vec![
            Token::EOL,
            if is_inner {
                Token::OpLike(OpLike::Bracket(Bracket::RCurly))
            } else {
                Token::EOF
            },
        ];
        while {
            while self.tokens.peek().unwrap() == &Token::EOL {
                self.tokens.next();
                just_parsed = false;
            }
            true
        } && self.tokens.peek().unwrap() != expected_end.last().unwrap()
        {
            let contents = self.parse_expr(0, &expected_end, true, None)?;
            let is_last = self.tokens.peek().unwrap() == expected_end.last().unwrap();
            exprs.push(
                *self.new_expr(contents, if is_last { expected_type.clone() } else { None })?,
            );
            just_parsed = true;
            if !expected_end.contains(self.tokens.peek().unwrap()) {
                let str = format!("Expected semicolon or EOF, found {:?}", self.tokens.peek());
                return self.make_error(str);
            }
        }
        if !just_parsed {
            exprs.push(VOID.clone());
        }
        self.var_types.pop();
        Ok(exprs)
    }

    /// expected_end is what the expression is allowed to stop on, and allow_imply_eol is whether
    /// the parser can insert semicolons where it sees that they are needed (i.e. after RCURLY).
    fn parse_expr(
        &mut self,
        min_bp: u8,
        expected_end: &Vec<Token>,
        allow_imply_eol: bool,
        expected_type: Option<Datatype>,
    ) -> Result<ExprContents, ParseError> {
        let mut imply_eol = false;
        let mut lhs = match self.tokens.next().unwrap() {
            Token::Identifier(id) => {
                ExprContents::Accessor(Accessor::Variable(id, self.tokens.pos))
            }
            Token::Literal(lit) => ExprContents::Value(lit),
            Token::Keyword(Keyword::True) => ExprContents::Value(Box::new(true)),
            Token::Keyword(Keyword::False) => ExprContents::Value(Box::new(false)),
            Token::OpLike(OpLike::Bracket(Bracket::LBracket)) => {
                if *self.tokens.peek().unwrap() == Token::OpLike(OpLike::Bracket(Bracket::RBracket))
                {
                    // ()
                    self.tokens.next();
                    VOID.contents.clone()
                } else {
                    let mut elements = vec![];
                    loop {
                        elements.push(self.parse_expr(
                            0,
                            &vec![Token::OpLike(OpLike::Bracket(Bracket::RBracket)), Token::Comma],
                            false,
                            None,
                        )?);
                        match self.tokens.next() {
                            Some(Token::Comma) => continue,
                            Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                            Some(tk) => return self.make_error(format!("Expected ] or , but found {tk}")),
                            None => return self.make_error("Unexpected EOF".to_string()),
                        }
                    }
                    match elements.len() {
                        1 => elements[0].clone(),
                        _ => ExprContents::Tuple(Tuple { elements: elements.into_iter().map(|e|self.new_expr(e, None).map(|i|*i)).collect::<Result<_,_>>()? })
                    }
                }
            }
            Token::OpLike(OpLike::Bracket(Bracket::LCurly)) => {
                let scope = ExprContents::Scope(self.parse_scope(true, None)?);
                self.tokens
                    .expect(Token::OpLike(OpLike::Bracket(Bracket::RCurly)))
                    .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                imply_eol = allow_imply_eol;
                scope
            }
            Token::OpLike(OpLike::Bracket(Bracket::LSquare)) => self.parse_array()?,
            Token::OpLike(op) => {
                if let Some(((), r_bp)) = PREFIX_BINDING_POWER.get(&op) {
                    let op_loc = self.tokens.pos;
                    let rhs = self.parse_expr(*r_bp, expected_end, false, None)?;
                    self.make_expr(VOID.contents.clone(), rhs, op, OpType::Prefix, op_loc)?
                } else {
                    return self.make_error(format!("Invalid prefix operator {op:?}"));
                }
            }
            Token::Keyword(Keyword::If) => {
                imply_eol = allow_imply_eol;
                self.parse_if(None)?
            }
            Token::Keyword(Keyword::While) => {
                imply_eol = allow_imply_eol;
                self.parse_while()?
            }
            Token::Keyword(Keyword::For) => {
                imply_eol = allow_imply_eol;
                self.parse_for()?
            }
            Token::Keyword(Keyword::Let) => {
                let ident = match self.tokens.next() {
                    Some(Token::Identifier(id)) => {
                        ExprContents::Accessor(Accessor::Variable(id, self.tokens.pos))
                    }
                    Some(tk) => {
                        return self.make_error(format!("Expected literal or ident, found {tk:?}"));
                    }
                    None => {
                        return self.make_error("Unexpected EOF".to_string());
                    }
                };

                let typehint = if self.tokens.eat([Token::Colon]).is_some() {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                self.tokens
                    .expect(Token::OpLike(OpLike::Assign))
                    .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                let op_loc = self.tokens.pos;

                let rhs = self.parse_expr(
                    INFIX_BINDING_POWER[&OpLike::Assign].1,
                    expected_end,
                    false,
                    typehint.clone(),
                )?;

                self.parse_assign(AssignType::Create, ident, rhs, typehint, op_loc)?
            }
            Token::Keyword(Keyword::Func) => {
                imply_eol = allow_imply_eol;
                self.parse_func()?
            }
            Token::Keyword(Keyword::Typedef) => {
                let name = match self.tokens.next() {
                    Some(Token::Identifier(name)) => name,
                    tk => return self.make_error(format!("Unexpected token {tk:?} in typedef")),
                };
                self.tokens
                    .expect(Token::OpLike(OpLike::Assign))
                    .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                let ty = self.parse_type()?;
                match self.typedefs.last_mut() {
                    None => return self.make_error("No typedef scope active".to_string()),
                    Some(v) => v.vars.insert(name, ty),
                };
                VOID.contents.clone()
            }
            Token::Keyword(Keyword::Return) => {
                if let Some(ret_type) = self.return_t_stack.last().cloned() {
                    let ret = self.parse_expr(
                        INFIX_BINDING_POWER[&OpLike::Assign].1,
                        expected_end,
                        allow_imply_eol,
                        Some(ret_type.clone())
                    )?;
                    let expected_type = Some(ret_type.clone());
                    let ret = self.new_expr(ret, expected_type)?;
                    ExprContents::Return(Return { ret })
                } else {
                    return self.make_error(
                        "Return statements are invalid in the root context".to_string(),
                    );
                }
            }

            Token::Keyword(Keyword::Break) => {
                if self.break_t_stack.last().is_none_or(|allowed| !*allowed) {
                    return self.make_error("Breaking is not valid in this context".to_string());
                }
                ExprContents::Break(Break)
            }

            Token::Keyword(Keyword::Continue) => {
                if self.break_t_stack.last().is_none_or(|allowed| !*allowed) {
                    return self.make_error("Continuing is not valid in this context".to_string());
                }
                ExprContents::Continue(Continue)
            }
            tk => return self.make_error(format!("Expected expression, found {tk:?}")),
        };
        loop {
            let op = match self.tokens.peek().unwrap().clone() {
                tk if expected_end.contains(&tk) => break,
                Token::OpLike(OpLike::Bracket(Bracket::LSquare)) => {
                    let op_loc = self.tokens.pos;
                    // acting like [ is a postfix operator, but parsing it completely differently because it contains an inner section
                    let (bp, _) = POSTFIX_BINDING_POWER
                        .get(&OpLike::Bracket(Bracket::LSquare))
                        .unwrap();
                    if *bp < min_bp {
                        break;
                    }
                    self.tokens.next(); // Remove the [
                    let mut indices = vec![];
                    loop {
                        indices.push(self.parse_expr(
                            0,
                            &vec![Token::OpLike(OpLike::Bracket(Bracket::RSquare)), Token::Comma],
                            false,
                            None,
                        )?);
                        match self.tokens.next() {
                            Some(Token::Comma) => continue,
                            Some(Token::OpLike(OpLike::Bracket(Bracket::RSquare))) => break,
                            Some(tk) => return self.make_error(format!("Expected ] or , but found {tk}")),
                            None => return self.make_error("Unexpected EOF".to_string()),
                        }
                    }
                    let base = self.new_expr(lhs, None)?;
                    let indices = {
                        let mut res = vec![];
                        for index in indices {
                            res.push(*self.new_expr(index, None)?);
                        }
                        res
                    };
                    
                    lhs = ExprContents::Accessor(Accessor::new_index(
                        base,
                        indices,
                        op_loc,
                    )?);
                    
                    continue;
                }
                Token::OpLike(OpLike::Bracket(Bracket::LBracket)) => {
                    let loc = self.tokens.pos;
                    // acting like ( is a postfix operator, but parsing it completely differently because it contains an inner section
                    let (bp, _) = POSTFIX_BINDING_POWER
                        .get(&OpLike::Bracket(Bracket::LBracket))
                        .unwrap();
                    if *bp < min_bp {
                        break;
                    }

                    self.tokens.next(); // Remove the (
                    let params = if self
                        .tokens
                        .eat([Token::OpLike(OpLike::Bracket(Bracket::RBracket))])
                        .is_some()
                    {
                        vec![]
                    } else {
                        let mut params = vec![];
                        loop {
                            let contents = self.parse_expr(
                                0,
                                &vec![Token::OpLike(OpLike::Bracket(Bracket::RBracket)), Token::Comma],
                                false,
                                None,
                            )?;
                            params.push(*self.new_expr(contents, None)?);
                            match self.tokens.next() {
                                Some(Token::Comma) => continue,
                                Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                                Some(tk) => return self.make_error(format!("Expected ] or , but found {tk}")),
                                None => return self.make_error("Unexpected EOF".to_string()),
                            }
                        }
                        params
                    };
                    let base = self.new_expr(lhs, None)?;
                    lhs = ExprContents::Call(Call::new(base, params, loc, expected_type.clone())?);
                    continue;
                }
                Token::OpLike(OpLike::LTurbofish) => {
                    let (bp, _) = POSTFIX_BINDING_POWER
                        .get(&OpLike::LTurbofish)
                        .unwrap();
                    if *bp < min_bp {
                        break;
                    }
                    self.tokens.next();
                    let mut types = vec![];
                    loop {
                        types.push(self.parse_type()?);
                        match self.tokens.next() {
                            None => return self.make_error("Unexpected EOF".to_string()),
                            Some(Token::OpLike(OpLike::Op(Op::Greater))) => break,
                            Some(Token::Comma) => continue,
                            Some(tk) => {
                                return self.make_error(format!(
                                    "Unexpected token {tk:?} in generic specification"
                                ));
                            }
                        }
                    }
                    lhs = ExprContents::GenericSpecify(GenericSpecify {
                        base: self.new_expr(lhs, None)?,
                        types,
                    });
                    continue;
                }
                Token::OpLike(op) => op,
                _ if imply_eol && expected_end.contains(&Token::EOL) => {
                    // automatic semicolon insertion :)
                    self.tokens.replace(Token::EOL);
                    break;
                }
                tk => return self.make_error(format!(
                    "Expected operation or {expected_end:?}, found {tk:?} (tokens left: {:?}, lhs: {lhs:?})",
                    self.tokens.clone().collect::<Vec<_>>()
                )),
            };
            if let Some((l_bp, ())) = POSTFIX_BINDING_POWER.get(&op) {
                if *l_bp < min_bp {
                    break;
                }
                self.tokens.next();
                let op_loc = self.tokens.pos;
                lhs = self.make_expr(lhs, VOID.contents.clone(), op, OpType::Postfix, op_loc)?;
                continue;
            }
            let (l_bp, r_bp) = match INFIX_BINDING_POWER.get(&op) {
                Some(bps) => bps,
                None => {
                    return self.make_error(format!(
                        "Invalid infix operator {op:?}, rest is {:?}",
                        self.tokens.clone().collect::<Vec<_>>()
                    ));
                }
            };
            if *l_bp < min_bp {
                break;
            }
            self.tokens.next();
            let op_loc = self.tokens.pos;
            let rhs = self.parse_expr(*r_bp, expected_end, false, None)?;
            lhs = self.make_expr(lhs, rhs, op, OpType::Infix, op_loc)?;
        }
        Ok(lhs)
    }

    fn parse_if(&mut self, expected_type: Option<Datatype>) -> Result<ExprContents, ParseError> {
        let (base_clause, base_expr) = self.parse_conditional_clause(expected_type, false)?;
        let else_block =
            if let Some(_) = self.tokens.eat([Token::Keyword(Keyword::Else)].into_iter()) {
                Some(
                    if let Some(_) = self.tokens.eat([Token::Keyword(Keyword::If)].into_iter()) {
                        self.parse_if(Some(base_expr.output.clone()))?
                    } else {
                        self.tokens
                            .expect(Token::OpLike(OpLike::Bracket(Bracket::LCurly)))
                            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                        let scope = self.parse_scope(true, Some(base_expr.output.clone()))?;
                        self.tokens
                            .expect(Token::OpLike(OpLike::Bracket(Bracket::RCurly)))
                            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                        ExprContents::Scope(scope)
                    },
                )
            } else {
                if base_expr.output != VOID.output {
                    return self.make_error(
                        "Conditional statement without an else clause must return Void".to_string(),
                    );
                }
                None
            };

        let else_expr = if let Some(exc) = else_block {
            let res = self.new_expr(exc, Some(base_expr.output.clone()))?;
            Some(res)
        } else {
            None
        };

        Ok(ExprContents::Conditional(Conditional {
            condition: base_clause,
            result: base_expr,
            otherwise: else_expr,
        }))
    }

    fn parse_while(&mut self) -> Result<ExprContents, ParseError> {
        let (clause, result) = self.parse_conditional_clause(Some(VOID.output.clone()), true)?;
        Ok(ExprContents::While(While {
            condition: clause,
            result: result,
        }))
    }

    fn parse_for(&mut self) -> Result<ExprContents, ParseError> {
        let var = match self.tokens.next() {
            Some(Token::Identifier(id)) => id,
            Some(t) => {
                return self.make_error(format!(
                    "Expected identifier, found {t:?} (pos {})",
                    self.tokens.pos
                ));
            }
            None => return self.make_error("Unexpected EOF".to_string()),
        };
        self.tokens
            .expect(Token::Keyword(Keyword::In))
            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
        let it = self.parse_expr(
            0,
            &vec![Token::OpLike(OpLike::Bracket(Bracket::LCurly))],
            false,
            None,
        )?;
        let iter_loc = self.tokens.pos;
        let iter = self.new_expr(it, None)?;
        let i_type = match iter
            .output
            .prop_type("iter")
            .and_then(|(i_fn, ..)| i_fn.call_result(vec![], None))
            .and_then(|(i, ..)| i.downcast::<IterT>().ok_or_else(||"Couldn't downcast".to_string()))
        {
            Ok(i_type) => i_type.output,
            Err(e) => {
                return self
                    .make_error(e);
            }
        };
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::LCurly)))
            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
        self.var_types.push(VarScope {
            vars: HashMap::new(),
            blocking: false,
            name: "for",
        });
        self.set_var_type(var.clone(), i_type, false, true)?;
        self.break_t_stack.push(true);
        let sc = ExprContents::Scope(self.parse_scope(true, Some(VOID.output.clone()))?);
        self.break_t_stack.pop();
        self.var_types.pop();
        let body = self.new_expr(sc, Some(VOID.output.clone()))?;
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::RCurly)))
            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
        Ok(ExprContents::For(For {
            var,
            iter,
            body,
            iter_loc,
        }))
    }

    fn parse_conditional_clause(
        &mut self,
        expected_type: Option<Datatype>,
        allow_break: bool,
    ) -> Result<(Box<Expr>, Box<Expr>), ParseError> {
        let expr = self.parse_expr(
            0,
            &vec![Token::OpLike(OpLike::Bracket(Bracket::LCurly))],
            false,
            Some(Box::new(BoolT)),
        )?;
        let clause = self.new_expr(expr, Some(BOOL.output.clone()))?;
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::LCurly)))
            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
        if allow_break {
            self.break_t_stack.push(true);
        }
        let scope = self.parse_scope(true, expected_type.clone())?;
        if allow_break {
            self.break_t_stack.pop();
        }
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::RCurly)))
            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
        let result = self.new_expr(ExprContents::Scope(scope), expected_type)?;
        Ok((clause, result))
    }

    fn parse_assign(
        &mut self,
        assign_type: AssignType,
        lhs: ExprContents,
        rhs: ExprContents,
        expected_type: Option<Datatype>,
        a_loc: LineIndex,
    ) -> Result<ExprContents, ParseError> {
        let val = self.new_expr(rhs, expected_type)?;
        let ty = val.output.clone();
        let assignee = match lhs {
            ExprContents::Accessor(acc) => acc,
            _ => return self.make_error(format!("Expected accessor, found {lhs:?}")),
        };
        match assign_type {
            AssignType::Reassign => {
                let old_type = match &assignee {
                    Accessor::Variable(v, _) => self.get_var_type(v)?,
                    Accessor::Property(base, prop, ..) => match base.output.prop_type(prop) {
                        Ok((pt, ..)) => pt,
                        Err(e) => {
                            return self.make_error(e);
                        }
                    },
                    Accessor::Index(_, _, indices) => {
                        if indices.len() != 1 {
                            return self.make_error("Can't use multiple-index pattern for assignment".to_string())
                        } else {
                            indices[0].3.clone()
                        }
                    }
                };
                if old_type != val.output {
                    return self.make_error(format!(
                        "Can't reassign variable of type {old_type} to different type {}",
                        val.output
                    ));
                }

                fn is_assignable(me: &mut Parser, assignee: &Accessor) -> bool {
                    match &assignee {
                        Accessor::Variable(v, _) => me.var_is_mutable(v),
                        Accessor::Property(base, ..) | Accessor::Index(base, ..) => {
                            (match &base.contents {
                                ExprContents::Accessor(accessor) => is_assignable(me, accessor),
                                // TODO: allow for other expression types to be assignable
                                // eg. somefunction().returned_param = 17;
                                // Actually I think this is legal in basically all cases, though its usefulness is questionable.
                                // I feel like there's some way this breaks if it's _ => true,
                                // but I can't see what it is right now so I'm going to leave it.
                                _ => false,
                            } && match &assignee {
                                Accessor::Property(_, _, _, _, setter, _) => setter.is_some(),
                                Accessor::Index(..) => true,
                                Accessor::Variable(..) => unreachable!("How did you even get here")
                            })
                        }
                    }
                }
                if !is_assignable(self, &assignee) {
                    return self.make_error(format!("{assignee:?} is not reassignable"));
                }
            }
            AssignType::Create => match &assignee {
                Accessor::Variable(v, _) => self.set_var_type(v.clone(), ty, true, true)?,
                _ => {
                    return self.make_error(
                        "Let assignments should only be able to parse lhs as a single variable"
                            .to_string(),
                    );
                }
            },
        }

        Ok(ExprContents::Assign(Assign {
            assignee,
            val,
            a_type: assign_type,
            a_loc,
        }))
    }

    fn parse_array(&mut self) -> Result<ExprContents, ParseError> {
        let mut elements = vec![];
        let mut expected_type = None;
        if self
            .tokens
            .eat([Token::OpLike(OpLike::Bracket(Bracket::RSquare))])
            .is_some()
        {
            return Ok(ExprContents::Array(Array { elements: vec![] }));
        }
        loop {
            let expr = self.parse_expr(
                0,
                &vec![
                    Token::Comma,
                    Token::OpLike(OpLike::Bracket(Bracket::RSquare)),
                ],
                false,
                expected_type.clone(),
            )?;
            let to_push = self.new_expr(expr, expected_type)?;
            expected_type = Some(to_push.output.clone());
            elements.push(*to_push);
            match self.tokens.next() {
                Some(Token::Comma) => (),
                Some(Token::OpLike(OpLike::Bracket(Bracket::RSquare))) => break,
                next => {
                    return self
                        .make_error(format!("Unexpected token {next:?} in array expression"));
                }
            }
        }
        Ok(ExprContents::Array(Array { elements }))
    }

    fn parse_func(&mut self) -> Result<ExprContents, ParseError> {
        let generic = if self
            .tokens
            .eat([Token::OpLike(OpLike::Op(Op::Less))])
            .is_some()
        {
            let mut generic = vec![];
            while self
                .tokens
                .eat([Token::OpLike(OpLike::Op(Op::Greater))])
                .is_none()
            {
                generic.push(match self.tokens.next() {
                    Some(Token::Identifier(s)) => s,
                    tk => return self.make_error(format!("Expected identifier, found {tk:?}")),
                });
                match self.tokens.next() {
                    Some(Token::Comma) => continue,
                    Some(Token::OpLike(OpLike::Op(Op::Greater))) => break,
                    tk => return self.make_error(format!("Unexpected token {tk:?}")),
                }
            }
            if generic.iter().any(|g| self.get_typedef(g).is_some()) {
                return self.make_error("Can't reuse generic variable".to_string());
            }
            generic
        } else {
            vec![]
        };
        self.typedefs.push(VarScope {
            vars: HashMap::from_iter(
                generic
                    .iter()
                    .map(|n| (n.clone(), Box::new(TypeVar::Var(n.clone())) as Datatype)),
            ),
            blocking: false,
            name: "func",
        });
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::LBracket)))
            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
        let mut params = vec![];
        loop {
            let name = match self.tokens.next() {
                Some(Token::Identifier(name)) => name,
                Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                tk => return self.make_error(format!("Expected identifier, found {tk:?}")),
            };
            self.tokens
                .expect(Token::Colon)
                .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
            let ty = self.parse_type()?;
            params.push((name, ty));
            match self.tokens.next() {
                Some(Token::Comma) => (),
                Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                tk => return self.make_error(format!("Unexpected token {tk:?}")),
            }
        }
        let output = if self.tokens.eat([Token::Arrow]).is_some() {
            self.parse_type()?
        } else {
            VOID.output.clone()
        };
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::LCurly)))
            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
        self.var_types.push(VarScope {
            vars: HashMap::from_iter(
                params
                    .iter()
                    .map(|(name, ty)| (name.clone(), (ty.clone(), false))),
            ),
            blocking: false,
            name: "func",
        });
        self.return_t_stack.push(output.clone());
        self.break_t_stack.push(false);
        let sc = ExprContents::Scope(self.parse_scope(true, Some(output.clone()))?);
        let contents = self.new_expr(sc, Some(output.clone()))?;
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::RCurly)))
            .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
        // Generics defined in this function go out of scope, as does the expectation of a return/break.
        self.typedefs.pop();
        self.return_t_stack.pop();
        self.break_t_stack.pop();
        Ok(ExprContents::Function(Function {
            params,
            contents,
            generic,
        }))
    }

    fn parse_type(&mut self) -> Result<Datatype, ParseError> {
        Ok(match self.tokens.next() {
            Some(Token::Identifier(name)) => match name.as_str() {
                "int" => Box::new(IntT),
                "bool" => Box::new(BoolT),
                "float" => Box::new(FloatT),
                "string" => Box::new(StringT),
                "ref" | "maybe" | "iter" => {
                    self.tokens
                        .expect(Token::OpLike(OpLike::Op(Op::Less)))
                        .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                    let referenced = self.parse_type()?;
                    self.tokens
                        .expect(Token::OpLike(OpLike::Op(Op::Greater)))
                        .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                    match name.as_str() {
                        "ref" => Box::new(RefT { ty: referenced }),
                        "maybe" => Box::new(MaybeT { output: referenced }),
                        "iter" => Box::new(IterT { output: referenced }),
                        _ => {
                            unreachable!("Must be within that set of strings to make it here");
                        }
                    }
                }
                "dice" => Box::new(DiceT),
                n => {
                    if let Some(t) = self.get_typedef(n) {
                        t
                    } else {
                        return self.make_error(format!("Unknown type {n}"));
                    }
                }
            },
            Some(Token::Keyword(Keyword::Func)) => {
                self.tokens
                    .expect(Token::OpLike(OpLike::Bracket(Bracket::LBracket)))
                    .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                let mut params = vec![];
                loop {
                    if self
                        .tokens
                        .eat([Token::OpLike(OpLike::Bracket(Bracket::RBracket))])
                        .is_some()
                    {
                        break;
                    }
                    params.push(self.parse_type()?);
                    match self.tokens.next() {
                        Some(Token::Comma) => (),
                        Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                        Some(tk) => {
                            return self
                                .make_error(format!("Unexpected token {tk:?} in function type"));
                        }
                        None => return self.make_error("Unexpected EOF".to_string()),
                    }
                }
                self.tokens
                    .expect(Token::Arrow)
                    .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                let output = self.parse_type()?;
                Box::new(FuncT {
                    params,
                    output,
                    generic: vec![],
                    owner_t: None,
                })
            }
            Some(Token::OpLike(OpLike::Bracket(b))) => match b {
                Bracket::LBracket => {
                    if self
                        .tokens
                        .eat([Token::OpLike(OpLike::Bracket(Bracket::RBracket))])
                        .is_some()
                    {
                        Box::new(Void)
                    } else {
                        let mut entries = vec![];
                        loop {
                            entries.push(self.parse_type()?);
                            match self.tokens.next() {
                                Some(Token::Comma) => (),
                                Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                                Some(tk) => {
                                    return self.make_error(format!(
                                        "Unexpected token {tk:?} in function type"
                                    ));
                                }
                                None => return self.make_error("Unexpected EOF".to_string()),
                            }
                        }
                        Box::new(TupT { entries })
                    }
                }
                Bracket::LSquare => {
                    let entry = self.parse_type()?;
                    self.tokens
                        .expect(Token::OpLike(OpLike::Bracket(Bracket::RSquare)))
                        .map_err(|e| self.make_error::<()>(e).unwrap_err())?;
                    Box::new(ArrT { entry })
                }
                tk => return self.make_error(format!("Expected type, found {tk:?}")),
            },
            Some(tk) => return self.make_error(format!("Expected type, found {tk:?}")),
            None => return self.make_error("Unexpected EOF".to_string()),
        })
    }

    fn make_expr(
        &mut self,
        lhs: ExprContents,
        rhs: ExprContents,
        op: OpLike,
        op_type: OpType,
        op_loc: LineIndex,
    ) -> Result<ExprContents, ParseError> {
        Ok(match (op, op_type) {
            (
                OpLike::Op(
                    Op::Plus
                    | Op::Minus
                    | Op::Times
                    | Op::Divided
                    | Op::Mod
                    | Op::D
                    | Op::Range
                    | Op::RangeEq
                    | Op::Equal
                    | Op::NotEqual
                    | Op::Greater
                    | Op::Less
                    | Op::Geq
                    | Op::Leq
                    | Op::And
                    | Op::Or,
                ),
                OpType::Infix,
            ) => ExprContents::Binop({
                let lhs = self.new_expr(lhs, None)?;
                let rhs = self.new_expr(rhs, None)?;
                let op = match op {
                    OpLike::Op(o) => o,
                    _ => unreachable!("Must have been an op to get here"),
                };
                Binop::new(lhs, rhs, op, op_loc)?
            }),
            (OpLike::Op(Op::Minus | Op::Not), OpType::Prefix) => ExprContents::Prefix({
                let rhs = self.new_expr(rhs, None)?;
                let op = match op {
                    OpLike::Op(o) => o,
                    _ => unreachable!("Must have been an op to get here"),
                };

                Prefix::new(op, rhs, op_loc)?
            }),
            // "d6" expands to "1d6"
            (OpLike::Op(Op::D), OpType::Prefix) => self.make_expr(
                ExprContents::Value(Box::new(1)),
                rhs,
                op,
                OpType::Infix,
                op_loc,
            )?,
            (OpLike::Assign, OpType::Infix) => {
                let prev_type = self.new_expr(lhs.clone(), None)?.output;
                self.parse_assign(AssignType::Reassign, lhs, rhs, Some(prev_type), op_loc)?
            }
            (OpLike::OpAssign(op), OpType::Infix) => {
                let val = self.make_expr(lhs.clone(), rhs, OpLike::Op(op), op_type, op_loc)?;
                self.make_expr(lhs, val, OpLike::Assign, op_type, op_loc)?
            }
            (OpLike::Access, OpType::Infix) => {
                let rhs = match rhs {
                    ExprContents::Accessor(Accessor::Variable(var, _)) => var,
                    _ => return self.make_error(format!("Expected property, found {rhs:?}")),
                };
                let base = self.new_expr(lhs, None)?;
                ExprContents::Accessor(Accessor::new_property(base, rhs, op_loc)?)
            }
            (OpLike::Bracket(_) | OpLike::LTurbofish, _) => {
                unreachable!(
                    "{:?} is not a valid operation and should not parse as such",
                    op
                )
            }
            (
                OpLike::Op(Op::Plus)
                | OpLike::Op(Op::Times)
                | OpLike::Op(Op::Divided)
                | OpLike::Op(Op::Mod)
                | OpLike::Op(Op::Range)
                | OpLike::Op(Op::RangeEq)
                | OpLike::Op(Op::Equal)
                | OpLike::Op(Op::NotEqual)
                | OpLike::Op(Op::Greater)
                | OpLike::Op(Op::Less)
                | OpLike::Op(Op::Geq)
                | OpLike::Op(Op::Leq)
                | OpLike::Op(Op::And)
                | OpLike::Op(Op::Or)
                | OpLike::Assign
                | OpLike::OpAssign(_)
                | OpLike::Access,
                OpType::Prefix,
            ) => {
                unreachable!("Invalid prefix operation {op:?}")
            }
            (OpLike::Op(Op::Not), OpType::Infix) => unreachable!("Invalid infix operation {op:?}"),
            (_, OpType::Postfix) => unreachable!("Invalid postfix operation {op:?}"),
        })
    }
}

// Ordered from lowest to highest binding power
static OP_LIST: LazyLock<Vec<(Vec<OpLike>, OpType, bool)>> = LazyLock::new(|| {
    vec![
        (
            Op::iter()
                .map(|op| OpLike::OpAssign(op))
                .chain([OpLike::Assign])
                .collect(),
            OpType::Infix,
            true,
        ),
        (
            vec![OpLike::Op(Op::Range), OpLike::Op(Op::RangeEq)],
            OpType::Infix,
            false,
        ),
        (
            vec![OpLike::Op(Op::And), OpLike::Op(Op::Or)],
            OpType::Infix,
            false,
        ),
        (vec![OpLike::Op(Op::Equal)], OpType::Infix, false),
        (
            vec![
                OpLike::Op(Op::Greater),
                OpLike::Op(Op::Less),
                OpLike::Op(Op::Geq),
                OpLike::Op(Op::Leq),
            ],
            OpType::Infix,
            false,
        ),
        (
            vec![OpLike::Op(Op::Plus), OpLike::Op(Op::Minus)],
            OpType::Infix,
            false,
        ),
        (
            vec![
                OpLike::Op(Op::Times),
                OpLike::Op(Op::Divided),
                OpLike::Op(Op::Mod),
            ],
            OpType::Infix,
            false,
        ),
        (
            vec![
                OpLike::Bracket(Bracket::LSquare),
                OpLike::Bracket(Bracket::LBracket),
            ],
            OpType::Postfix,
            false,
        ),
        (vec![OpLike::Op(Op::D)], OpType::Infix, false),
        (vec![OpLike::Op(Op::D)], OpType::Prefix, false),
        (vec![OpLike::Op(Op::Not)], OpType::Prefix, false),
        (vec![OpLike::Op(Op::Minus)], OpType::Prefix, false),
        (vec![OpLike::LTurbofish], OpType::Postfix, false),
        (vec![OpLike::Access], OpType::Infix, false),
    ]
});

static INFIX_BINDING_POWER: LazyLock<HashMap<OpLike, (u8, u8)>> = LazyLock::new(|| {
    HashMap::from_iter(
        OP_LIST
            .iter()
            .enumerate()
            .filter_map(|(idx, (tks, optype, left_higher))| {
                if *optype != OpType::Infix {
                    return None;
                }
                let base = idx as u8 * 2;
                let l_prio = if *left_higher { 1 } else { 0 };
                Some(
                    tks.iter()
                        .map(|tk| (*tk, (base + l_prio, base + 1 - l_prio)))
                        .collect::<Vec<_>>(),
                )
            })
            .flatten(),
    )
});

static PREFIX_BINDING_POWER: LazyLock<HashMap<OpLike, ((), u8)>> = LazyLock::new(|| {
    HashMap::from_iter(
        OP_LIST
            .iter()
            .enumerate()
            .filter_map(|(idx, (tks, optype, left_higher))| {
                if *optype != OpType::Prefix {
                    return None;
                }
                let base = idx as u8 * 2;
                let l_prio = if *left_higher { 1 } else { 0 };
                Some(
                    tks.iter()
                        .map(|tk| (*tk, ((), base + 1 - l_prio)))
                        .collect::<Vec<_>>(),
                )
            })
            .flatten(),
    )
});

static POSTFIX_BINDING_POWER: LazyLock<HashMap<OpLike, (u8, ())>> = LazyLock::new(|| {
    HashMap::from_iter(
        OP_LIST
            .iter()
            .enumerate()
            .filter_map(|(idx, (tks, optype, left_higher))| {
                if *optype != OpType::Postfix {
                    return None;
                }
                let base = idx as u8 * 2;
                let l_prio = if *left_higher { 1 } else { 0 };
                Some(
                    tks.iter()
                        .map(|tk| (*tk, (base + l_prio, ())))
                        .collect::<Vec<_>>(),
                )
            })
            .flatten(),
    )
});
