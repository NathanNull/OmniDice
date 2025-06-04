use std::{collections::HashMap, vec::IntoIter};

use crate::{
    lexer::{Bracket, Keyword, OpToken, Token, TokenString}, types::{Datatype, Value, OPS, PROPERTIES}, TokenIter
};

pub mod expr;
pub use expr::*;

pub struct Parser {
    tokens: TokenIter<IntoIter<Token>>,
    var_types: Vec<HashMap<String, Datatype>>,
}

const VOID: Expr = Expr {
    contents: ExprContents::Literal(Value::Void),
    output: Datatype::Void,
};

impl Parser {
    pub fn new(tokens: TokenString) -> Self {
        Self {
            tokens: TokenIter::new(tokens.into_iter()),
            var_types: vec![],
        }
    }

    fn new_expr(&mut self, contents: ExprContents) -> Expr {
        let output = self.decide_type(&contents);
        Expr { output, contents }
    }

    fn set_var_type(&mut self, name: String, dtype: Datatype) {
        if self.var_types.is_empty() {
            print!("!!!!! tried to access variable type while no scope alive");
            self.var_types.push(HashMap::new());
        }
        self.var_types.last_mut().unwrap().insert(name, dtype);
    }

    fn get_var_type(&self, name: &str) -> Option<Datatype> {
        for scope in self.var_types.iter().rev() {
            if let Some(dtype) = scope.get(name) {
                return Some(*dtype);
            }
        }
        None
    }

    fn decide_type(&self, contents: &ExprContents) -> Datatype {
        match contents {
            ExprContents::Accessor(acc) => match acc {
                Accessor::Variable(var) => self
                    .get_var_type(var)
                    .expect(&format!("Unknown variable {var}")),
                Accessor::Property(base, prop) => {
                    let ty = self.decide_type(&base.contents);
                    *PROPERTIES
                        .get(&(ty, prop.clone()))
                        .expect(&format!("Invalid property {prop} for type {ty:?}"))
                }
            },
            ExprContents::Literal(literal) => literal.get_type(),
            ExprContents::Binop(binop) => *OPS
                .get(&(
                    self.decide_type(&binop.lhs.contents),
                    self.decide_type(&binop.rhs.contents),
                    binop.op,
                    OpType::Infix,
                ))
                .unwrap(),
            ExprContents::Prefix(prefix) => *OPS
                .get(&(
                    Datatype::Void,
                    self.decide_type(&prefix.rhs.contents),
                    prefix.prefix,
                    OpType::Prefix,
                ))
                .unwrap(),
            ExprContents::Postfix(postfix) => *OPS
                .get(&(
                    self.decide_type(&postfix.lhs.contents),
                    Datatype::Void,
                    postfix.postfix,
                    OpType::Postfix,
                ))
                .unwrap(),
            ExprContents::Assign(assign) => self.decide_type(&assign.val.contents),
            ExprContents::Scope(scope) => scope
                .last()
                .map(|expr| expr.output)
                .unwrap_or(Datatype::Void),
        }
    }

    pub fn parse(&mut self) -> Expr {
        let scope = ExprContents::Scope(self.parse_scope(false));
        self.new_expr(scope)
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
            let contents = self.parse_expr(0, &expected_end);
            exprs.push(self.new_expr(contents));
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
            exprs.push(VOID);
        }
        self.var_types.pop();
        exprs
    }

    fn parse_expr(&mut self, min_bp: u8, expected_end: &Vec<Token>) -> ExprContents {
        let mut imply_eol = false;
        let mut lhs = match self.tokens.next().unwrap() {
            Token::Identifier(id) => ExprContents::Accessor(Accessor::Variable(id)),
            Token::Float(f) => ExprContents::Literal(Value::Float(f)),
            Token::Int(i) => ExprContents::Literal(Value::Int(i)),
            Token::Keyword(Keyword::True) => ExprContents::Literal(Value::Bool(true)),
            Token::Keyword(Keyword::False) => ExprContents::Literal(Value::Bool(false)),
            Token::Op(op) => {
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expr(r_bp, expected_end);
                self.make_expr(VOID.contents, rhs, op, OpType::Prefix)
            }
            Token::Bracket(Bracket::Left) => {
                if *self.tokens.peek().unwrap() == Token::Bracket(Bracket::Right) {
                    // ()
                    self.tokens.next();
                    VOID.contents
                } else {
                    let lhs = self.parse_expr(0, &vec![Token::Bracket(Bracket::Right)]);
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
                imply_eol = true;
                scope
            }
            tk => panic!("Expected literal or ident, found {tk:?}"),
        };
        loop {
            let op = match self.tokens.peek().unwrap() {
                tk if expected_end.contains(&tk) => break,
                Token::Op(op) => *op,
                _ if imply_eol && expected_end.contains(&Token::EOL) => {
                    // automatic semicolon insertion :)
                    self.tokens.replace(Token::EOL);
                    break;
                }
                tk => panic!("Expected operation or {expected_end:?}, found {tk:?}"),
            };
            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.tokens.next();
                lhs = self.make_expr(lhs, VOID.contents, op, OpType::Postfix);
                continue;
            }
            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }
            self.tokens.next();
            let rhs = self.parse_expr(r_bp, expected_end);
            lhs = self.make_expr(lhs, rhs, op, OpType::Infix);
        }
        lhs
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
                OpToken::Plus | OpToken::Minus | OpToken::Times | OpToken::Divided | OpToken::D,
                OpType::Infix,
            ) => ExprContents::Binop(Binop {
                lhs: Box::new(self.new_expr(lhs)),
                rhs: Box::new(self.new_expr(rhs)),
                op: op.into(),
            }),
            (OpToken::Minus, OpType::Prefix) => ExprContents::Prefix(Prefix {
                prefix: op.into(),
                rhs: Box::new(self.new_expr(rhs)),
            }),
            // "d6" expands to "1d6"
            (OpToken::D, OpType::Prefix) => ExprContents::Binop(Binop {
                lhs: Box::new(self.new_expr(ExprContents::Literal(Value::Int(1)))),
                rhs: Box::new(self.new_expr(rhs)),
                op: op.into(),
            }),
            (OpToken::Assign, OpType::Infix) => {
                let val = Box::new(self.new_expr(rhs));
                let assignee = match lhs {
                    ExprContents::Accessor(acc) => acc,
                    _ => panic!("Expected accessor, found {lhs:?}"),
                };
                match &assignee {
                    Accessor::Variable(v) => {
                        if let Some(dt) = self.get_var_type(v) {
                            assert_eq!(
                                dt, val.output,
                                "Can't reassign variable {v} of type {dt:?} to type {:?}",
                                val.output
                            );
                        } else {
                            self.set_var_type(v.clone(), val.output);
                        }
                    }
                    Accessor::Property(base, prop) => {
                        assert_eq!(
                            Some(&val.output),
                            PROPERTIES.get(&(base.output, prop.clone()))
                        )
                    }
                }
                ExprContents::Assign(Assign { assignee, val })
            }
            (OpToken::Access, OpType::Infix) => {
                let rhs = match rhs {
                    ExprContents::Accessor(Accessor::Variable(var)) => var,
                    _ => panic!("Expected property, found {rhs:?}"),
                };
                ExprContents::Accessor(Accessor::Property(Box::new(self.new_expr(lhs)), rhs))
            }
            (
                OpToken::Plus
                | OpToken::Times
                | OpToken::Divided
                | OpToken::Assign
                | OpToken::Access,
                OpType::Prefix,
            ) => {
                unreachable!("Invalid prefix operation {op:?}")
            }
            (_, OpType::Postfix) => unreachable!("Invalid postfix operation {op:?}"),
        }
    }
}

fn infix_binding_power(op: OpToken) -> (u8, u8) {
    match op {
        OpToken::Assign => (2, 1),
        OpToken::Plus | OpToken::Minus => (3, 4),
        OpToken::Times | OpToken::Divided => (5, 6),
        OpToken::Access => (7, 8),
        OpToken::D => (9, 10),
    }
}

fn prefix_binding_power(op: OpToken) -> ((), u8) {
    match op {
        OpToken::Minus => ((), 11),
        OpToken::D => ((), 10),
        _ => panic!("Invalid prefix operator: {op:?}"),
    }
}

// TODO: see if there are even any good postfix operators
// Decrement/increment maybe, but then those really are just += 1 and -= 1...
fn postfix_binding_power(op: OpToken) -> Option<(u8, ())> {
    match op {
        _ => None,
    }
}
