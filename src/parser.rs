use std::{collections::HashMap, sync::LazyLock, vec::IntoIter};

use crate::{
    TokenIter,
    lexer::{Bracket, Keyword, OpToken, Token, TokenString},
    types::{ArrT, Bool, Datatype, Void},
};

pub mod expr;
pub use expr::*;
use strum::IntoEnumIterator;

pub struct Parser {
    tokens: TokenIter<IntoIter<Token>>,
    var_types: Vec<HashMap<String, (Datatype, bool)>>,
}

static VOID: LazyLock<Expr> = LazyLock::new(|| Expr {
    contents: ExprContents::Literal(Box::new(Void)),
    output: Box::new(Void),
});

static BOOL: LazyLock<Expr> = LazyLock::new(|| Expr {
    contents: ExprContents::Literal(Box::new(false)),
    output: Box::new(Bool),
});

impl Parser {
    pub fn new(tokens: TokenString) -> Self {
        Self {
            tokens: TokenIter::new(tokens.into_iter()),
            var_types: vec![],
        }
    }

    fn new_expr(&mut self, contents: ExprContents) -> Box<Expr> {
        let output = self.decide_type(&contents);
        Box::new(Expr { output, contents })
    }

    fn set_var_type(&mut self, name: String, dtype: Datatype, mutable: bool) {
        if let Some(old) = self.get_var_type(&name) {
            assert_eq!(
                &old, &dtype,
                "Can't reassign variable {name} of type {old:?} to type {dtype:?}",
            );
        } else {
            if self.var_types.is_empty() {
                panic!("Tried to access variable type while no scope alive");
            }
            self.var_types
                .last_mut()
                .unwrap()
                .insert(name, (dtype, mutable));
        }
    }

    fn get_var_type(&self, name: &str) -> Option<Datatype> {
        for scope in self.var_types.iter().rev() {
            if let Some((dtype, _)) = scope.get(name) {
                return Some(dtype.clone());
            }
        }
        None
    }

    fn var_is_mutable(&self, name: &str) -> bool {
        for scope in self.var_types.iter().rev() {
            if let Some((_, mutable)) = scope.get(name) {
                return *mutable;
            }
        }
        false
    }

    fn decide_type(&self, contents: &ExprContents) -> Datatype {
        match contents {
            ExprContents::Accessor(acc) => match acc {
                Accessor::Variable(var) => self
                    .get_var_type(var)
                    .expect(&format!("Unknown variable {var}")),
                Accessor::Property(base, prop) => {
                    let ty = &base.output;
                    ty.prop_type(&prop)
                        .expect(&format!("Invalid property {prop} for type {ty:?}"))
                }
            },
            ExprContents::Literal(literal) => literal.get_type(),
            ExprContents::Binop(binop) => binop
                .lhs
                .output
                .bin_op_result(binop.rhs.output.clone(), binop.op)
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
            ExprContents::While(wh) => wh.result.output.clone(), // This is always Void
            ExprContents::Array(arr) => Box::new(ArrT {
                entry: arr
                    .elements
                    .first()
                    .map(|e| e.output.clone())
                    .unwrap_or_else(|| VOID.output.clone()),
            }),
        }
    }

    pub fn parse(&mut self) -> Expr {
        let scope = ExprContents::Scope(self.parse_scope(false));
        *self.new_expr(scope)
    }

    fn parse_scope(&mut self, is_inner: bool) -> Scope {
        let mut exprs = vec![];
        let mut just_parsed = false;
        // Variables created in scope should only live while it does
        self.var_types.push(HashMap::new());
        let expected_end = vec![
            Token::EOL,
            if is_inner {
                Token::Bracket(Bracket::RCurly)
            } else {
                Token::EOF
            },
        ];
        while self.tokens.peek().unwrap() != expected_end.last().unwrap() {
            let contents = self.parse_expr(0, &expected_end, true);
            exprs.push(*self.new_expr(contents));
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
    fn parse_expr(
        &mut self,
        min_bp: u8,
        expected_end: &Vec<Token>,
        allow_imply_eol: bool,
    ) -> ExprContents {
        let mut imply_eol = false;
        let mut lhs = match self.tokens.next().unwrap() {
            Token::Identifier(id) => ExprContents::Accessor(Accessor::Variable(id)),
            Token::Literal(lit) => ExprContents::Literal(lit),
            Token::Keyword(Keyword::True) => ExprContents::Literal(Box::new(true)),
            Token::Keyword(Keyword::False) => ExprContents::Literal(Box::new(false)),
            Token::Op(op) => {
                let ((), r_bp) = PREFIX_BINDING_POWER
                    .get(&op)
                    .expect(&format!("Invalid prefix operator {op:?}"));
                let rhs = self.parse_expr(*r_bp, expected_end, false);
                self.make_expr(VOID.contents.clone(), rhs, op, OpType::Prefix)
            }
            Token::Bracket(Bracket::Left) => {
                if *self.tokens.peek().unwrap() == Token::Bracket(Bracket::Right) {
                    // ()
                    self.tokens.next();
                    VOID.contents.clone()
                } else {
                    let lhs = self.parse_expr(0, &vec![Token::Bracket(Bracket::Right)], false);
                    let next = self.tokens.next().unwrap();
                    assert_eq!(
                        next,
                        Token::Bracket(Bracket::Right),
                        "Expected right bracket, found {next:?}"
                    );
                    lhs
                }
            }
            Token::Bracket(Bracket::LCurly) => {
                let scope = ExprContents::Scope(self.parse_scope(true));
                let next = self.tokens.next().unwrap();
                assert_eq!(
                    next,
                    Token::Bracket(Bracket::RCurly),
                    "Expected right curly bracket, found {next:?}"
                );
                imply_eol = allow_imply_eol;
                scope
            }
            Token::Bracket(Bracket::LSquare) => {
                let mut elements = vec![];
                loop {
                    let expr = self.parse_expr(
                        0,
                        &vec![Token::Comma, Token::Bracket(Bracket::RSquare)],
                        false,
                    );
                    elements.push(*self.new_expr(expr));
                    match self.tokens.next() {
                        Some(Token::Comma) => (),
                        Some(Token::Bracket(Bracket::RSquare)) => break,
                        next => panic!("Unexpected token {next:?} in array expression"),
                    }
                }
                let ty = elements.first().map(|e| &e.output);
                assert!(
                    elements.iter().all(|e| Some(&e.output) == ty),
                    "Array elements should all be the same type ({:?}), found {:?}",
                    ty.unwrap(),
                    elements.iter().map(|e| &e.output).collect::<Vec<_>>()
                );
                ExprContents::Array(Array { elements })
            }
            Token::Keyword(Keyword::If) => {
                imply_eol = allow_imply_eol;
                self.parse_if()
            }
            Token::Keyword(Keyword::While) => {
                imply_eol = allow_imply_eol;
                self.parse_while()
            }
            Token::Keyword(Keyword::Let) => {
                let is_mut = self.tokens.eat([Token::Keyword(Keyword::Mut)]).is_some();
                let ident = match self.tokens.next().expect("Unexpected EOF") {
                    Token::Identifier(id) => ExprContents::Accessor(Accessor::Variable(id)),
                    tk => panic!("Expected literal or ident, found {tk:?}"),
                };
                assert!(
                    self.tokens.eat([Token::Op(OpToken::Assign)]).is_some(),
                    "Expected =, found {:?}",
                    self.tokens.next()
                );
                let rhs =
                    self.parse_expr(INFIX_BINDING_POWER[&OpToken::Assign].1, expected_end, false);

                self.parse_assign(
                    if is_mut {
                        AssignType::Mut
                    } else {
                        AssignType::Immut
                    },
                    ident,
                    rhs,
                )
            }
            tk => panic!("Expected expression, found {tk:?}"),
        };
        loop {
            let op = match self.tokens.peek().unwrap().clone() {
                tk if expected_end.contains(&tk) => break,
                Token::Op(op) => op,
                _ if imply_eol && expected_end.contains(&Token::EOL) => {
                    // automatic semicolon insertion :)
                    self.tokens.replace(Token::EOL);
                    break;
                }
                tk => panic!(
                    "Expected operation or {expected_end:?}, found {tk:?} (tokens left: {:?})",
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
            let (l_bp, r_bp) = INFIX_BINDING_POWER
                .get(&op)
                .expect(&format!("Invalid infix operator {op:?}"));
            if *l_bp < min_bp {
                break;
            }
            self.tokens.next();
            let rhs = self.parse_expr(*r_bp, expected_end, false);
            lhs = self.make_expr(lhs, rhs, op, OpType::Infix);
        }
        lhs
    }

    fn parse_if(&mut self) -> ExprContents {
        let (base_clause, base_expr) = self.parse_conditional_clause();
        let else_block =
            if let Some(_) = self.tokens.eat([Token::Keyword(Keyword::Else)].into_iter()) {
                Some(
                    if let Some(_) = self.tokens.eat([Token::Keyword(Keyword::If)].into_iter()) {
                        self.parse_if()
                    } else {
                        assert!(
                            self.tokens
                                .eat([Token::Bracket(Bracket::LCurly)].into_iter())
                                .is_some(),
                            "Expected {{, found {:?}",
                            self.tokens.peek()
                        );
                        let scope = self.parse_scope(true);
                        assert!(
                            self.tokens
                                .eat([Token::Bracket(Bracket::RCurly)].into_iter())
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
            let res = self.new_expr(exc);
            assert_eq!(
                &res.output, &base_expr.output,
                "If statement recieved non-matching types {:?} and {:?}",
                res.output, base_expr.output
            );
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
        let (clause, result) = self.parse_conditional_clause();
        assert_eq!(&result.output, &VOID.output, "While loops must return Void");
        ExprContents::While(While {
            condition: clause,
            result: result,
        })
    }

    fn parse_conditional_clause(&mut self) -> (Box<Expr>, Box<Expr>) {
        let expr = self.parse_expr(0, &vec![Token::Bracket(Bracket::LCurly)], false);
        let clause = self.new_expr(expr);
        assert!(
            self.tokens.eat([Token::Bracket(Bracket::LCurly)]).is_some(),
            "Expected {{, found {:?}",
            self.tokens.peek()
        );
        assert_eq!(
            &clause.output, &BOOL.output,
            "Expected expression returning boolean value, found {:?} ({:?})",
            clause.output, clause.contents
        );
        let scope = self.parse_scope(true);
        assert!(
            self.tokens.eat([Token::Bracket(Bracket::RCurly)]).is_some(),
            "Expected }}, found {:?}",
            self.tokens.peek()
        );
        let result = self.new_expr(ExprContents::Scope(scope));
        (clause, result)
    }

    fn parse_assign(
        &mut self,
        assign_type: AssignType,
        lhs: ExprContents,
        rhs: ExprContents,
    ) -> ExprContents {
        /*let val = self.new_expr(rhs);
        let ty = val.output.clone();
        let res = ExprContents::Assign(Assign {
            assignee: Accessor::Variable(ident.clone()),
            val,
            a_type: if is_mut {
                AssignType::Mut
            } else {
                AssignType::Immut
            },
        });
        self.set_var_type(ident, ty, is_mut);
        res */
        let val = self.new_expr(rhs);
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
                    },
                    val.output
                );
                fn is_assignable(me: &mut Parser, assignee: &Accessor) -> bool {
                    match &assignee {
                        Accessor::Variable(v) => me.var_is_mutable(v),
                        Accessor::Property(base, _) => match &base.contents {
                            ExprContents::Accessor(accessor) => is_assignable(me, accessor),
                            _ => false,
                        },
                    }
                }
                assert!(is_assignable(self, &assignee), "{assignee:?} is not reassignable");
            }
            AssignType::Immut | AssignType::Mut => match &assignee {
                Accessor::Variable(v) => {
                    self.set_var_type(v.clone(), ty, assign_type == AssignType::Mut)
                }
                _ => unreachable!("Let assignments should only be able to parse lhs as a single variable"),
            },
        }

        ExprContents::Assign(Assign {
            assignee,
            val,
            a_type: assign_type,
        })
    }

    fn make_expr(
        &mut self,
        lhs: ExprContents,
        rhs: ExprContents,
        op: OpToken,
        op_type: OpType,
    ) -> ExprContents {
        match (op, op_type) {
            (
                OpToken::Plus
                | OpToken::Minus
                | OpToken::Times
                | OpToken::Divided
                | OpToken::Mod
                | OpToken::D
                | OpToken::Equal
                | OpToken::NotEqual
                | OpToken::Greater
                | OpToken::Less
                | OpToken::Geq
                | OpToken::Leq
                | OpToken::And
                | OpToken::Or,
                OpType::Infix,
            ) => ExprContents::Binop(Binop {
                lhs: self.new_expr(lhs),
                rhs: self.new_expr(rhs),
                op: op.try_into().unwrap(),
            }),
            (OpToken::Minus, OpType::Prefix) => ExprContents::Prefix(Prefix {
                op: op.try_into().unwrap(),
                rhs: self.new_expr(rhs),
            }),
            (OpToken::Not, OpType::Prefix) => ExprContents::Prefix(Prefix {
                op: op.try_into().unwrap(),
                rhs: self.new_expr(rhs),
            }),
            // "d6" expands to "1d6"
            (OpToken::D, OpType::Prefix) => ExprContents::Binop(Binop {
                lhs: self.new_expr(ExprContents::Literal(Box::new(1))),
                rhs: self.new_expr(rhs),
                op: op.try_into().unwrap(),
            }),
            (OpToken::Assign, OpType::Infix) => self.parse_assign(AssignType::Reassign, lhs, rhs),
            (OpToken::OpAssign(op), OpType::Infix) => {
                let val = self.make_expr(lhs.clone(), rhs, op.into(), op_type);
                self.make_expr(lhs, val, OpToken::Assign, op_type)
            }
            (OpToken::Access, OpType::Infix) => {
                let rhs = match rhs {
                    ExprContents::Accessor(Accessor::Variable(var)) => var,
                    _ => panic!("Expected property, found {rhs:?}"),
                };
                ExprContents::Accessor(Accessor::Property(self.new_expr(lhs), rhs))
            }
            (
                OpToken::Plus
                | OpToken::Times
                | OpToken::Divided
                | OpToken::Mod
                | OpToken::Equal
                | OpToken::NotEqual
                | OpToken::Greater
                | OpToken::Less
                | OpToken::Geq
                | OpToken::Leq
                | OpToken::And
                | OpToken::Or
                | OpToken::Assign
                | OpToken::OpAssign(_)
                | OpToken::Access,
                OpType::Prefix,
            ) => {
                unreachable!("Invalid prefix operation {op:?}")
            }
            (OpToken::Not, OpType::Infix) => unreachable!("Invalid infix operation {op:?}"),
            (_, OpType::Postfix) => unreachable!("Invalid postfix operation {op:?}"),
        }
    }
}

// Ordered from lowest to highest binding power
static OP_LIST: LazyLock<Vec<(Vec<OpToken>, OpType, bool)>> = LazyLock::new(|| {
    vec![
        (
            Op::iter()
                .map(|op| OpToken::OpAssign(op))
                .chain([OpToken::Assign])
                .collect(),
            OpType::Infix,
            true,
        ),
        (vec![OpToken::And, OpToken::Or], OpType::Infix, false),
        (vec![OpToken::Equal], OpType::Infix, false),
        (
            vec![OpToken::Greater, OpToken::Less, OpToken::Geq, OpToken::Leq],
            OpType::Infix,
            false,
        ),
        (vec![OpToken::Plus, OpToken::Minus], OpType::Infix, false),
        (
            vec![OpToken::Times, OpToken::Divided, OpToken::Mod],
            OpType::Infix,
            false,
        ),
        (vec![OpToken::Access], OpType::Infix, false),
        (vec![OpToken::D], OpType::Infix, false),
        (vec![OpToken::D], OpType::Prefix, false),
        (vec![OpToken::Not], OpType::Prefix, false),
        (vec![OpToken::Minus], OpType::Prefix, false),
    ]
});

static INFIX_BINDING_POWER: LazyLock<HashMap<OpToken, (u8, u8)>> = LazyLock::new(|| {
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

static PREFIX_BINDING_POWER: LazyLock<HashMap<OpToken, ((), u8)>> = LazyLock::new(|| {
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

static POSTFIX_BINDING_POWER: LazyLock<HashMap<OpToken, (u8, ())>> = LazyLock::new(|| {
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
