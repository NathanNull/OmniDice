use std::{collections::HashMap, sync::LazyLock, vec::IntoIter};

use crate::{
    TokenIter,
    builtins::BUILTINS,
    interpreter::VarScope,
    lexer::{Bracket, Keyword, OpLike, Token, TokenString},
    types::{
        ArrT, BoolT, Datatype, DiceT, Downcast, FloatT, FuncT, IntT, IterT, MaybeT, RefT, StringT,
        TupT, TypeVar, Void,
    },
};

pub mod expr;
pub use expr::*;
use strum::IntoEnumIterator;

pub struct Parser {
    tokens: TokenIter<IntoIter<Token>>,
    var_types: Vec<VarScope<(Datatype, bool)>>,
    typedefs: Vec<VarScope<Datatype>>,
}

static VOID: LazyLock<Expr> = LazyLock::new(|| Expr {
    contents: ExprContents::Literal(Box::new(Void)),
    output: Box::new(Void),
});

static BOOL: LazyLock<Expr> = LazyLock::new(|| Expr {
    contents: ExprContents::Literal(Box::new(false)),
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
            }],
            typedefs: vec![],
        }
    }

    fn new_expr(&mut self, contents: ExprContents, expected_type: Option<Datatype>) -> Box<Expr> {
        let output = self.decide_type(&contents, expected_type);
        Box::new(Expr { output, contents })
    }

    fn set_var_type(&mut self, name: String, dtype: Datatype, mutable: bool, initialize: bool) {
        if !initialize {
            if let Some(old) = self.get_var_type(&name) {
                assert_eq!(
                    &old, &dtype,
                    "Can't reassign variable {name} of type {old:?} to type {dtype:?}",
                );
            } else {
                panic!("Tried to assign to uninitialized variable {name}");
            }
        } else {
            self.var_types
                .last_mut()
                .expect("Tried to set variable while no scope alive")
                .vars
                .insert(name, (dtype, mutable));
        }
    }

    fn get_var_type(&self, name: &str) -> Option<Datatype> {
        for scope in self.var_types.iter().rev() {
            if let Some((dtype, _)) = scope.vars.get(name) {
                return Some(dtype.clone());
            }
            if scope.blocking {
                break;
            }
        }
        None
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

    fn decide_type(&self, contents: &ExprContents, expected_type: Option<Datatype>) -> Datatype {
        let res = match contents {
            ExprContents::Accessor(acc) => match acc {
                Accessor::Variable(var) => self
                    .get_var_type(var)
                    .expect(&format!("Unknown variable {var}")),
                Accessor::Property(base, prop) => {
                    let ty = &base.output;
                    ty.prop_type(&prop)
                        .expect(&format!("Invalid property {prop} for type {ty:?}"))
                }
                Accessor::Index(indexed, index) => {
                    let ty = &indexed.output;
                    ty.index_type(&index.output)
                        .expect(&format!("Invalid index {:?} for type {ty:?}", index.output))
                }
            },
            ExprContents::Literal(literal) => literal.get_type(),
            ExprContents::Binop(binop) => binop
                .lhs
                .output
                .bin_op_result(&binop.rhs.output, binop.op)
                .expect(&format!(
                    "Invalid binary operation {:?} on {}, {}",
                    binop.op, binop.lhs.output, binop.rhs.output
                )),
            ExprContents::Prefix(prefix) => {
                prefix.rhs.output.pre_op_result(prefix.op).expect(&format!(
                    "Invalid prefix operation {:?} on {}",
                    prefix.op, prefix.rhs.output
                ))
            }
            ExprContents::Postfix(postfix) => {
                postfix
                    .lhs
                    .output
                    .post_op_result(postfix.op)
                    .expect(&format!(
                        "Invalid prefix operation {:?} on {:?}",
                        postfix.op, postfix.lhs.output
                    ))
            }
            ExprContents::Assign(assign) => assign.val.output.clone(),
            ExprContents::Scope(scope) => scope
                .last()
                .map(|expr| expr.output.clone())
                .unwrap_or(VOID.output.clone()),
            ExprContents::Conditional(cond) => cond.result.output.clone(),
            ExprContents::While(_) | ExprContents::For(_) => VOID.output.clone(), // This is always Void
            ExprContents::Array(arr) => Box::new(ArrT {
                entry: arr
                    .elements
                    .first()
                    .map(|e| e.output.clone())
                    .or(expected_type
                        .as_ref()
                        .and_then(|t| t.downcast::<ArrT>())
                        .map(|arr| arr.entry))
                    .expect("Unknown type for empty array, please annotate"),
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
                call.base
                    .output
                    .call_result(params.clone(), expected_type.clone())
                    .expect(&format!(
                        "Can't call {} with params ({:?}){}",
                        call.base.output,
                        params,
                        if let Some(e) = expected_type.as_ref() {
                            format!(" and expect output {e}")
                        } else {
                            "".to_string()
                        }
                    ))
            }
            ExprContents::GenericSpecify(gspec) => {
                gspec.base.output.specify_generics(&gspec.types).expect("Couldn't specify generics for this type")
            }
        };
        if let Some(ty) = expected_type {
            ty.assert_same(&res)
        } else {
            res
        }
    }

    pub fn parse(&mut self) -> Expr {
        let scope = ExprContents::Scope(self.parse_scope(false, Some(VOID.output.clone())));
        *self.new_expr(scope, Some(VOID.output.clone()))
    }

    fn parse_scope(&mut self, is_inner: bool, expected_type: Option<Datatype>) -> Scope {
        let mut exprs = vec![];
        let mut just_parsed = false;
        // Variables created in scope should only live while it does
        self.var_types.push(VarScope {
            vars: HashMap::new(),
            blocking: false,
        });
        let expected_end = vec![
            Token::EOL,
            if is_inner {
                Token::OpLike(OpLike::Bracket(Bracket::RCurly))
            } else {
                Token::EOF
            },
        ];
        while self.tokens.peek().unwrap() != expected_end.last().unwrap() {
            let contents = self.parse_expr(0, &expected_end, true, false);
            let is_last = self.tokens.peek().unwrap() == expected_end.last().unwrap();
            exprs
                .push(*self.new_expr(contents, if is_last { expected_type.clone() } else { None }));
            just_parsed = true;
            assert!(
                expected_end.contains(self.tokens.peek().unwrap()),
                "Expected semicolon or EOF, found {:?}",
                self.tokens.peek()
            );
            while self.tokens.peek().unwrap() == &Token::EOL {
                self.tokens.next();
                just_parsed = false;
            }
        }
        if !just_parsed {
            exprs.push(VOID.clone());
        }
        self.var_types.pop();
        exprs
    }

    /// expected_end is what the expression is allowed to stop on, and allow_imply_eol is whether
    /// the parser can insert semicolons where it sees that they are needed (i.e. after RCURLY).
    /// allow_join is whether the resulting expression can be an exposed (no brackets) tuple, and
    /// should generally be false.
    fn parse_expr(
        &mut self,
        min_bp: u8,
        expected_end: &Vec<Token>,
        allow_imply_eol: bool,
        allow_join: bool,
    ) -> ExprContents {
        let mut imply_eol = false;
        let mut lhs = match self.tokens.next().unwrap() {
            Token::Identifier(id) => ExprContents::Accessor(Accessor::Variable(id)),
            Token::Literal(lit) => ExprContents::Literal(lit),
            Token::Keyword(Keyword::True) => ExprContents::Literal(Box::new(true)),
            Token::Keyword(Keyword::False) => ExprContents::Literal(Box::new(false)),
            Token::OpLike(OpLike::Bracket(Bracket::LBracket)) => {
                if *self.tokens.peek().unwrap() == Token::OpLike(OpLike::Bracket(Bracket::RBracket))
                {
                    // ()
                    self.tokens.next();
                    VOID.contents.clone()
                } else {
                    let lhs = self.parse_expr(
                        0,
                        &vec![Token::OpLike(OpLike::Bracket(Bracket::RBracket))],
                        false,
                        true,
                    );
                    let next = self.tokens.next().unwrap();
                    assert_eq!(
                        next,
                        Token::OpLike(OpLike::Bracket(Bracket::RBracket)),
                        "Expected right bracket, found {next:?}"
                    );
                    lhs
                }
            }
            Token::OpLike(OpLike::Bracket(Bracket::LCurly)) => {
                let scope = ExprContents::Scope(self.parse_scope(true, None));
                let next = self.tokens.next().unwrap();
                assert_eq!(
                    next,
                    Token::OpLike(OpLike::Bracket(Bracket::RCurly)),
                    "Expected right curly bracket, found {next:?}"
                );
                imply_eol = allow_imply_eol;
                scope
            }
            Token::OpLike(OpLike::Bracket(Bracket::LSquare)) => self.parse_array(),
            Token::OpLike(op) => {
                let ((), r_bp) = PREFIX_BINDING_POWER
                    .get(&op)
                    .expect(&format!("Invalid prefix operator {op:?}"));
                let rhs = self.parse_expr(*r_bp, expected_end, false, false);
                self.make_expr(VOID.contents.clone(), rhs, op, OpType::Prefix)
            }
            Token::Keyword(Keyword::If) => {
                imply_eol = allow_imply_eol;
                self.parse_if(None)
            }
            Token::Keyword(Keyword::While) => {
                imply_eol = allow_imply_eol;
                self.parse_while()
            }
            Token::Keyword(Keyword::For) => {
                imply_eol = allow_imply_eol;
                self.parse_for()
            }
            Token::Keyword(Keyword::Let) => {
                let ident = match self.tokens.next().expect("Unexpected EOF") {
                    Token::Identifier(id) => ExprContents::Accessor(Accessor::Variable(id)),
                    tk => panic!("Expected literal or ident, found {tk:?}"),
                };

                let typehint = if self.tokens.eat([Token::OpLike(OpLike::Colon)]).is_some() {
                    Some(self.parse_type())
                } else {
                    None
                };

                self.tokens.expect(Token::OpLike(OpLike::Assign));

                let rhs = self.parse_expr(
                    INFIX_BINDING_POWER[&OpLike::Assign].1,
                    expected_end,
                    false,
                    false,
                );

                self.parse_assign(AssignType::Create, ident, rhs, typehint)
            }
            Token::Keyword(Keyword::Func) => {
                imply_eol = allow_imply_eol;
                self.parse_func()
            }
            tk => panic!("Expected expression, found {tk:?}"),
        };
        loop {
            let op = match self.tokens.peek().unwrap().clone() {
                tk if expected_end.contains(&tk) => break,
                Token::OpLike(OpLike::Bracket(Bracket::LSquare)) => {
                    // acting like [ is a postfix operator, but parsing it completely differently because it contains an inner section
                    let (bp, _) = POSTFIX_BINDING_POWER
                        .get(&OpLike::Bracket(Bracket::LSquare))
                        .unwrap();
                    if *bp < min_bp {
                        break;
                    }
                    self.tokens.next(); // Remove the [
                    let index = self.parse_expr(
                        0,
                        &vec![Token::OpLike(OpLike::Bracket(Bracket::RSquare))],
                        false,
                        true,
                    );
                    assert_eq!(
                        self.tokens.next(),
                        Some(Token::OpLike(OpLike::Bracket(Bracket::RSquare))),
                        "Expected ]"
                    );
                    lhs = ExprContents::Accessor(Accessor::Index(
                        self.new_expr(lhs, None),
                        self.new_expr(index, None),
                    ));
                    continue;
                }
                Token::OpLike(OpLike::Bracket(Bracket::LBracket)) => {
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
                        let p = match self.parse_expr(
                            0,
                            &vec![Token::OpLike(OpLike::Bracket(Bracket::RBracket))],
                            false,
                            true,
                        ) {
                            ExprContents::Tuple(tup) => tup.elements,
                            expr => vec![*self.new_expr(expr, None)],
                        };
                        assert_eq!(
                            self.tokens.next(),
                            Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))),
                            "Expected )"
                        );
                        p
                    };
                    lhs = ExprContents::Call(Call {
                        base: self.new_expr(lhs, None),
                        params,
                    });
                    continue;
                }
                Token::OpLike(OpLike::Comma) if allow_join => {
                    let (l_bp, r_bp) = INFIX_BINDING_POWER.get(&OpLike::Comma).unwrap();
                    if *l_bp < min_bp {
                        break;
                    }
                    let mut elements = vec![*self.new_expr(lhs, None)];
                    self.tokens.next();
                    if expected_end.contains(&self.tokens.peek().unwrap()) {
                        lhs = ExprContents::Tuple(Tuple { elements });
                        break;
                    }
                    let mut new_end = expected_end.clone();
                    new_end.push(Token::OpLike(OpLike::Comma));
                    loop {
                        if !expected_end.contains(self.tokens.peek().unwrap()) {
                            let next =
                                self.parse_expr(*r_bp, &new_end, allow_imply_eol, allow_join);
                            elements.push(*self.new_expr(next, None));
                        }
                        match self.tokens.next() {
                            Some(Token::OpLike(OpLike::Comma)) => (),
                            Some(tk) if expected_end.contains(&tk) => {
                                lhs = ExprContents::Tuple(Tuple { elements });
                                self.tokens.replace(tk);
                                break;
                            }
                            tk => panic!("Unexpected token {tk:?}"),
                        }
                    }
                    break;
                }
                Token::OpLike(OpLike::LTurbofish) => {
                    self.tokens.next();
                    let mut types = vec![];
                    loop {
                        types.push(self.parse_type());
                        match self.tokens.next().expect("Unexpected EOF") {
                            Token::OpLike(OpLike::Op(Op::Greater)) => break,
                            Token::OpLike(OpLike::Comma) => continue,
                            tk => panic!("Unexpected token {tk:?} in generic specification"),
                        }
                    }
                    lhs = ExprContents::GenericSpecify(GenericSpecify {
                        base: self.new_expr(lhs, None),
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
                tk => panic!(
                    "Expected operation or {expected_end:?}, found {tk:?} (tokens left: {:?}, lhs: {lhs:?})",
                    self.tokens.clone().collect::<Vec<_>>()
                ),
            };
            if let Some((l_bp, ())) = POSTFIX_BINDING_POWER.get(&op) {
                if *l_bp < min_bp {
                    break;
                }
                self.tokens.next();
                lhs = self.make_expr(lhs, VOID.contents.clone(), op, OpType::Postfix);
                continue;
            }
            let (l_bp, r_bp) = INFIX_BINDING_POWER.get(&op).expect(&format!(
                "Invalid infix operator {op:?}, rest is {:?}",
                self.tokens.clone().collect::<Vec<_>>()
            ));
            if *l_bp < min_bp {
                break;
            }
            self.tokens.next();
            let rhs = self.parse_expr(*r_bp, expected_end, false, allow_join);
            lhs = self.make_expr(lhs, rhs, op, OpType::Infix);
        }
        lhs
    }

    fn parse_if(&mut self, expected_type: Option<Datatype>) -> ExprContents {
        let (base_clause, base_expr) = self.parse_conditional_clause(expected_type);
        let else_block =
            if let Some(_) = self.tokens.eat([Token::Keyword(Keyword::Else)].into_iter()) {
                Some(
                    if let Some(_) = self.tokens.eat([Token::Keyword(Keyword::If)].into_iter()) {
                        self.parse_if(Some(base_expr.output.clone()))
                    } else {
                        assert!(
                            self.tokens
                                .eat([Token::OpLike(OpLike::Bracket(Bracket::LCurly))].into_iter())
                                .is_some(),
                            "Expected {{, found {:?}",
                            self.tokens.peek()
                        );
                        let scope = self.parse_scope(true, Some(base_expr.output.clone()));
                        assert!(
                            self.tokens
                                .eat([Token::OpLike(OpLike::Bracket(Bracket::RCurly))].into_iter())
                                .is_some(),
                            "Expected {{, found {:?}",
                            self.tokens.peek()
                        );
                        ExprContents::Scope(scope)
                    },
                )
            } else {
                assert_eq!(
                    &base_expr.output, &VOID.output,
                    "Conditional statement without an else clause must return Void"
                );
                None
            };

        let else_expr = if let Some(exc) = else_block {
            let res = self.new_expr(exc, Some(base_expr.output.clone()));
            Some(res)
        } else {
            None
        };

        ExprContents::Conditional(Conditional {
            condition: base_clause,
            result: base_expr,
            otherwise: else_expr,
        })
    }

    fn parse_while(&mut self) -> ExprContents {
        let (clause, result) = self.parse_conditional_clause(Some(VOID.output.clone()));
        ExprContents::While(While {
            condition: clause,
            result: result,
        })
    }

    fn parse_for(&mut self) -> ExprContents {
        let var = match self.tokens.next() {
            Some(Token::Identifier(id)) => id,
            Some(t) => panic!("Expected identifier, found {t:?}"),
            None => panic!("Unexpected EOF"),
        };
        self.tokens.expect(Token::Keyword(Keyword::In));
        let it = self.parse_expr(
            0,
            &vec![Token::OpLike(OpLike::Bracket(Bracket::LCurly))],
            false,
            false,
        );
        let iter = self.new_expr(it, None);
        let i_type = match iter
            .output
            .prop_type("iter")
            .and_then(|i_fn| i_fn.call_result(vec![], None))
            .and_then(|i| i.downcast::<IterT>())
        {
            Some(i_type) => i_type.output,
            None => panic!("Non-iterable {} passed into for loop", iter.output),
        };
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::LCurly)));
        self.var_types.push(VarScope {
            vars: HashMap::new(),
            blocking: false,
        });
        self.set_var_type(var.clone(), i_type, false, true);
        let sc = ExprContents::Scope(self.parse_scope(true, Some(VOID.output.clone())));
        self.var_types.pop();
        let body = self.new_expr(sc, Some(VOID.output.clone()));
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::RCurly)));
        ExprContents::For(For { var, iter, body })
    }

    fn parse_conditional_clause(
        &mut self,
        expected_type: Option<Datatype>,
    ) -> (Box<Expr>, Box<Expr>) {
        let expr = self.parse_expr(
            0,
            &vec![Token::OpLike(OpLike::Bracket(Bracket::LCurly))],
            false,
            false,
        );
        let clause = self.new_expr(expr, Some(BOOL.output.clone()));
        assert!(
            self.tokens
                .eat([Token::OpLike(OpLike::Bracket(Bracket::LCurly))])
                .is_some(),
            "Expected {{, found {:?}",
            self.tokens.peek()
        );
        let scope = self.parse_scope(true, expected_type.clone());
        assert!(
            self.tokens
                .eat([Token::OpLike(OpLike::Bracket(Bracket::RCurly))])
                .is_some(),
            "Expected }}, found {:?}",
            self.tokens.peek()
        );
        let result = self.new_expr(ExprContents::Scope(scope), expected_type);
        (clause, result)
    }

    fn parse_assign(
        &mut self,
        assign_type: AssignType,
        lhs: ExprContents,
        rhs: ExprContents,
        expected_type: Option<Datatype>,
    ) -> ExprContents {
        let val = self.new_expr(rhs, expected_type);
        let ty = val.output.clone();
        let assignee = match lhs {
            ExprContents::Accessor(acc) => acc,
            _ => panic!("Expected accessor, found {lhs:?}"),
        };
        match assign_type {
            AssignType::Reassign => {
                assert_eq!(
                    match &assignee {
                        Accessor::Variable(v) => {
                            self.get_var_type(v).expect(&format!(
                                "Unknown variable {v}, consider adding 'let' before this assignment"
                            ))
                        }
                        Accessor::Property(base, prop) => {
                            base.output.prop_type(prop).expect(&format!(
                                "Unknown property type {prop} for base {}",
                                base.output
                            ))
                        }
                        Accessor::Index(indexed, index) => {
                            indexed.output.index_type(&index.output).expect(&format!(
                                "Unknown property type {} for base {}",
                                index.output, indexed.output
                            ))
                        }
                    },
                    val.output
                );
                fn is_assignable(me: &mut Parser, assignee: &Accessor) -> bool {
                    match &assignee {
                        Accessor::Variable(v) => me.var_is_mutable(v),
                        Accessor::Property(base, _) | Accessor::Index(base, _) => {
                            match &base.contents {
                                ExprContents::Accessor(accessor) => is_assignable(me, accessor),
                                _ => false,
                            }
                        }
                    }
                }
                assert!(
                    is_assignable(self, &assignee),
                    "{assignee:?} is not reassignable"
                );
            }
            AssignType::Create => match &assignee {
                Accessor::Variable(v) => self.set_var_type(v.clone(), ty, true, true),
                _ => unreachable!(
                    "Let assignments should only be able to parse lhs as a single variable"
                ),
            },
        }

        ExprContents::Assign(Assign {
            assignee,
            val,
            a_type: assign_type,
        })
    }

    fn parse_array(&mut self) -> ExprContents {
        let mut elements = vec![];
        let mut expected_type = None;
        if self
            .tokens
            .eat([Token::OpLike(OpLike::Bracket(Bracket::RSquare))])
            .is_some()
        {
            return ExprContents::Array(Array { elements: vec![] });
        }
        loop {
            let expr = self.parse_expr(
                0,
                &vec![
                    Token::OpLike(OpLike::Comma),
                    Token::OpLike(OpLike::Bracket(Bracket::RSquare)),
                ],
                false,
                false,
            );
            let to_push = self.new_expr(expr, expected_type);
            expected_type = Some(to_push.output.clone());
            elements.push(*to_push);
            match self.tokens.next() {
                Some(Token::OpLike(OpLike::Comma)) => (),
                Some(Token::OpLike(OpLike::Bracket(Bracket::RSquare))) => break,
                next => panic!("Unexpected token {next:?} in array expression"),
            }
        }
        ExprContents::Array(Array { elements })
    }

    fn parse_func(&mut self) -> ExprContents {
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
                    tk => panic!("Expected identifier, found {tk:?}"),
                });
                match self.tokens.next() {
                    Some(Token::OpLike(OpLike::Comma)) => continue,
                    Some(Token::OpLike(OpLike::Op(Op::Greater))) => break,
                    tk => panic!("Unexpected token {tk:?}"),
                }
            }
            assert!(
                generic.iter().all(|g| self.get_typedef(g).is_none()),
                "Can't reuse generic variable"
            );
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
        });
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::LBracket)));
        let mut params = vec![];
        loop {
            let name = match self.tokens.next() {
                Some(Token::Identifier(name)) => name,
                Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                tk => panic!("Expected identifier, found {tk:?}"),
            };
            self.tokens.expect(Token::OpLike(OpLike::Colon));
            let ty = self.parse_type();
            params.push((name, ty));
            match self.tokens.next() {
                Some(Token::OpLike(OpLike::Comma)) => (),
                Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                tk => panic!("Unexpected token {tk:?}"),
            }
        }
        self.tokens.expect(Token::Arrow);
        let output = self.parse_type();
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::LCurly)));
        self.var_types.push(VarScope {
            vars: HashMap::from_iter(
                params
                    .iter()
                    .map(|(name, ty)| (name.clone(), (ty.clone(), false))),
            ),
            blocking: false,
        });
        let sc = ExprContents::Scope(self.parse_scope(true, Some(output.clone())));
        let contents = self.new_expr(sc, Some(output.clone()));
        self.tokens
            .expect(Token::OpLike(OpLike::Bracket(Bracket::RCurly)));
        // Generics defined in this function go out of scope
        self.typedefs.pop();
        ExprContents::Function(Function {
            params,
            contents,
            generic,
        })
    }

    fn parse_type(&mut self) -> Datatype {
        match self.tokens.next() {
            Some(Token::Identifier(name)) => match name.as_str() {
                "int" => Box::new(IntT),
                "bool" => Box::new(BoolT),
                "float" => Box::new(FloatT),
                "string" => Box::new(StringT),
                "ref" | "maybe" | "iter" => {
                    self.tokens.expect(Token::OpLike(OpLike::Op(Op::Less)));
                    let referenced = self.parse_type();
                    self.tokens.expect(Token::OpLike(OpLike::Op(Op::Greater)));
                    match name.as_str() {
                        "ref" => Box::new(RefT { ty: referenced }),
                        "maybe" => Box::new(MaybeT { output: referenced }),
                        "iter" => Box::new(IterT { output: referenced }),
                        _ => unreachable!("how tho"),
                    }
                }
                "dice" => Box::new(DiceT),
                n => {
                    if let Some(t) = self.get_typedef(n) {
                        t
                    } else {
                        panic!("Unknown type {n}")
                    }
                }
            },
            Some(Token::Keyword(Keyword::Func)) => {
                self.tokens
                    .expect(Token::OpLike(OpLike::Bracket(Bracket::LBracket)));
                let mut params = vec![];
                loop {
                    if self
                        .tokens
                        .eat([Token::OpLike(OpLike::Bracket(Bracket::RBracket))])
                        .is_some()
                    {
                        break;
                    }
                    params.push(self.parse_type());
                    match self.tokens.next() {
                        Some(Token::OpLike(OpLike::Comma)) => (),
                        Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                        Some(tk) => panic!("Unexpected token {tk:?} in function type"),
                        None => panic!("Unexpected EOF"),
                    }
                }
                self.tokens.expect(Token::Arrow);
                let output = self.parse_type();
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
                            entries.push(self.parse_type());
                            match self.tokens.next() {
                                Some(Token::OpLike(OpLike::Comma)) => (),
                                Some(Token::OpLike(OpLike::Bracket(Bracket::RBracket))) => break,
                                Some(tk) => panic!("Unexpected token {tk:?} in function type"),
                                None => panic!("Unexpected EOF"),
                            }
                        }
                        Box::new(TupT { entries })
                    }
                }
                Bracket::LSquare => {
                    let entry = self.parse_type();
                    self.tokens
                        .expect(Token::OpLike(OpLike::Bracket(Bracket::RSquare)));
                    Box::new(ArrT { entry })
                }
                tk => panic!("Expected type, found {tk:?}"),
            },
            Some(tk) => panic!("Expected type, found {tk:?}"),
            None => panic!("Unexpected EOF"),
        }
    }

    fn make_expr(
        &mut self,
        lhs: ExprContents,
        rhs: ExprContents,
        op: OpLike,
        op_type: OpType,
    ) -> ExprContents {
        match (op, op_type) {
            (
                OpLike::Op(Op::Plus)
                | OpLike::Op(Op::Minus)
                | OpLike::Op(Op::Times)
                | OpLike::Op(Op::Divided)
                | OpLike::Op(Op::Mod)
                | OpLike::Op(Op::D)
                | OpLike::Op(Op::Range)
                | OpLike::Op(Op::Equal)
                | OpLike::Op(Op::NotEqual)
                | OpLike::Op(Op::Greater)
                | OpLike::Op(Op::Less)
                | OpLike::Op(Op::Geq)
                | OpLike::Op(Op::Leq)
                | OpLike::Op(Op::And)
                | OpLike::Op(Op::Or),
                OpType::Infix,
            ) => ExprContents::Binop(Binop {
                lhs: self.new_expr(lhs, None),
                rhs: self.new_expr(rhs, None),
                op: op.as_op(),
            }),
            (OpLike::Op(Op::Minus), OpType::Prefix) => ExprContents::Prefix(Prefix {
                op: op.as_op(),
                rhs: self.new_expr(rhs, None),
            }),
            (OpLike::Op(Op::Not), OpType::Prefix) => ExprContents::Prefix(Prefix {
                op: op.as_op(),
                rhs: self.new_expr(rhs, None),
            }),
            // "d6" expands to "1d6"
            (OpLike::Op(Op::D), OpType::Prefix) => ExprContents::Binop(Binop {
                lhs: self.new_expr(ExprContents::Literal(Box::new(1)), Some(Box::new(IntT))),
                rhs: self.new_expr(rhs, Some(Box::new(IntT))),
                op: op.as_op(),
            }),
            (OpLike::Assign, OpType::Infix) => {
                let prev_type = self.new_expr(lhs.clone(), None).output;
                self.parse_assign(AssignType::Reassign, lhs, rhs, Some(prev_type))
            }
            (OpLike::OpAssign(op), OpType::Infix) => {
                let val = self.make_expr(lhs.clone(), rhs, OpLike::Op(op), op_type);
                self.make_expr(lhs, val, OpLike::Assign, op_type)
            }
            (OpLike::Access, OpType::Infix) => {
                let rhs = match rhs {
                    ExprContents::Accessor(Accessor::Variable(var)) => var,
                    _ => panic!("Expected property, found {rhs:?}"),
                };
                ExprContents::Accessor(Accessor::Property(self.new_expr(lhs, None), rhs))
            }
            (OpLike::Bracket(_) | OpLike::Colon | OpLike::Comma | OpLike::LTurbofish, _) => {
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
        }
    }
}

// Ordered from lowest to highest binding power
static OP_LIST: LazyLock<Vec<(Vec<OpLike>, OpType, bool)>> = LazyLock::new(|| {
    vec![
        (vec![OpLike::Comma], OpType::Infix, false),
        (
            Op::iter()
                .map(|op| OpLike::OpAssign(op))
                .chain([OpLike::Assign])
                .collect(),
            OpType::Infix,
            true,
        ),
        (vec![OpLike::Op(Op::Range)], OpType::Infix, false),
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
        (vec![OpLike::Access], OpType::Infix, false),
        (vec![OpLike::Op(Op::D)], OpType::Infix, false),
        (vec![OpLike::Op(Op::D)], OpType::Prefix, false),
        (vec![OpLike::Op(Op::Not)], OpType::Prefix, false),
        (vec![OpLike::Op(Op::Minus)], OpType::Prefix, false),
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
