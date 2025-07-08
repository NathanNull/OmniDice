use std::{fmt::{Debug, Display}, iter::Map, str::Chars};

use crate::{TokenIter, TokenWidth, error::LexError, parser::Op, types::Value};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Literal(Value),
    OpLike(OpLike),
    EOL,
    EOF,
    Arrow,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Keyword(k) => write!(f, "{k}"),
            Self::Identifier(i) => write!(f, "{i}"),
            Self::Literal(v) => write!(f, "{v}"),
            Self::OpLike(ol) => write!(f, "{ol}"),
            Self::EOL => write!(f, ";"),
            Self::EOF => write!(f, "EOF"),
            Self::Arrow => write!(f, "->"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    True,
    False,
    If,
    Else,
    While,
    For,
    In,
    Let,
    Func,
    Typedef,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_k_{}", match self {
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::For => "for",
            Keyword::In => "in",
            Keyword::Let => "let",
            Keyword::Func => "func",
            Keyword::Typedef => "typedef",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bracket {
    LBracket,
    RBracket,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
}

impl Display for Bracket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Bracket::LBracket => "(",
            Bracket::RBracket => ")",
            Bracket::LCurly => "{",
            Bracket::RCurly => "}",
            Bracket::LSquare => "[",
            Bracket::RSquare => "]",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpLike {
    Op(Op),
    Assign,
    OpAssign(Op),
    Access,
    Bracket(Bracket),
    Colon,
    Comma,
    LTurbofish,
}

impl Display for OpLike {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpLike::Op(op) => write!(f, "{op:?}"),
            OpLike::Assign => write!(f, "="),
            OpLike::OpAssign(op) => write!(f, "{op:?}="),
            OpLike::Access => write!(f, "."),
            OpLike::Bracket(b) => write!(f, "{b}"),
            OpLike::Colon => write!(f, ":"),
            OpLike::Comma => write!(f, ","),
            OpLike::LTurbofish => write!(f, "::<"),
        }
    }
}

impl Debug for OpLike {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Self::Op(op) => &format!("{op:?}"),
            Self::Assign => "=",
            Self::OpAssign(op) => &format!("{op:?}="),
            Self::Access => ".",
            Self::Bracket(b) => &format!("{b:?}"),
            Self::Colon => ":",
            Self::Comma => ",",
            Self::LTurbofish => "::<",
        };
        write!(f, "{c}")
    }
}

pub struct Lexer<'a> {
    code: TokenIter<char, Map<Chars<'a>, fn(char) -> (char, TokenWidth)>>,
}

pub type TokenString = Vec<(Token, TokenWidth)>;

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        fn char_width(c: char) -> (char, TokenWidth) {
            (
                c,
                if c == '\n' {
                    TokenWidth {
                        width: 0,
                        height: 1,
                    }
                } else {
                    TokenWidth {
                        width: 1,
                        height: 0,
                    }
                },
            )
        }
        Self {
            code: TokenIter::new(code.chars().map(char_width)),
        }
    }

    pub fn lex(&mut self) -> Result<TokenString, LexError> {
        let mut tokens = vec![];
        let mut last_pos = self.code.pos;
        while {
            while self.code.peek().is_some_and(|c| c.is_whitespace()) {
                self.code.next();
            }
            true
        } && self.code.peek().is_some()
        {
            if self.lex_comment() {
                // it's a comment, carry on
            } else if let Some(tk) = self
                .lex_identifier()
                .or_else(|| self.lex_literal())
                .or_else(|| self.lex_special())
            {
                let p = self.code.pos;
                tokens.push((
                    tk,
                    if last_pos.0 == p.0 {
                        TokenWidth {
                            width: p.1 - last_pos.1,
                            height: 0,
                        }
                    } else {
                        TokenWidth {
                            height: p.0 - last_pos.0,
                            width: p.1,
                        }
                    },
                ));
                last_pos = p;
            } else {
                let str = self
                    .code
                    .clone()
                    .map(|c| c.to_string())
                    .collect::<Vec<_>>()
                    .concat();
                return Err(LexError {
                    info: format!(
                        "Couldn't tokenize, next is {:?}, rest is {:?}",
                        self.code.peek(),
                        str
                    ),
                    location: self.code.pos,
                });
            }
        }
        let p = self.code.pos;
        tokens.push((
            Token::EOF,
            if last_pos.0 == p.0 {
                TokenWidth {
                    width: p.1 - last_pos.1,
                    height: 0,
                }
            } else {
                TokenWidth {
                    height: p.0 - last_pos.0,
                    width: p.1,
                }
            },
        ));
        Ok(tokens)
    }

    fn lex_comment(&mut self) -> bool {
        if self.code.eat_str("//") {
            while self.code.peek().is_some_and(|c| *c != '\n') {
                self.code.next();
            }
            true
        } else if self.code.eat_str("/*") {
            while !self.code.eat_str("*/") && self.code.peek().is_some() {
                self.code.next();
            }
            true
        } else {
            false
        }
    }

    fn lex_identifier(&mut self) -> Option<Token> {
        if self.code.peek().is_none_or(|c| !c.is_alphabetic()) {
            return None;
        }
        let mut name = vec![];
        while self
            .code
            .peek()
            .is_some_and(|c| c.is_alphanumeric() || *c == '_')
        {
            name.push(self.code.next().unwrap());
        }
        let name_str = name
            .iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .concat();

        // Invalidate identifiers that represent valid dice
        if name_str
            .strip_prefix("d")
            .is_none_or(|n| n.parse::<i32>().is_err() && n != "")
        {
            Some(Token::Keyword(match name_str.as_str() {
                "true" => Keyword::True,
                "false" => Keyword::False,
                "if" => Keyword::If,
                "else" => Keyword::Else,
                "while" => Keyword::While,
                "for" => Keyword::For,
                "in" => Keyword::In,
                "let" => Keyword::Let,
                "func" => Keyword::Func,
                "typedef" => Keyword::Typedef,
                _ => return Some(Token::Identifier(name_str)),
            }))
        } else {
            // Put the tokens back so that the last taken one is replaced first
            // Turns out we didn't actually need them
            for c in name.into_iter().rev() {
                self.code.replace(c);
            }
            None
        }
    }

    fn lex_literal(&mut self) -> Option<Token> {
        self.lex_number().or_else(|| self.lex_string())
    }

    fn lex_number(&mut self) -> Option<Token> {
        if self.code.peek().is_none_or(|c| !c.is_numeric()) {
            return None;
        }
        let mut num = vec![];
        let mut last_was_dot = false;
        while self
            .code
            .peek()
            .is_some_and(|c| c.is_numeric() || *c == '.' || *c == '_')
        {
            let is_dot = self.code.peek() == Some(&'.');
            if last_was_dot && is_dot {
                let n = num.pop().unwrap();
                self.code.replace(n);
                break;
            }
            last_was_dot = is_dot;
            num.push(self.code.next().unwrap());
        }
        let num_str = num
            .into_iter()
            .filter(|c| *c != '_')
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .concat();
        if num_str.contains('.') {
            // The parse will error if there's more than 1 dot so we don't need to check for that.
            num_str
                .parse::<f32>()
                .ok()
                .map(|f| Token::Literal(Box::new(f)))
        } else {
            num_str
                .parse::<i32>()
                .ok()
                .map(|i| Token::Literal(Box::new(i)))
        }
    }

    fn lex_string(&mut self) -> Option<Token> {
        if !self.code.eat_str("\"") {
            return None;
        }
        let mut string_chars = vec![];
        while !self.code.eat_str("\"") {
            string_chars.push(if self.code.eat_str("\\") {
                if let Some(n) = self.code.next() {
                    match n {
                        '\\' => '\\',
                        '"' => '"',
                        'n' => '\n',
                        _ => {
                            for c in string_chars.into_iter().rev() {
                                self.code.replace(c);
                            }
                            return None;
                        }
                    }
                } else {
                    for c in string_chars.into_iter().rev() {
                        self.code.replace(c);
                    }
                    return None;
                }
            } else {
                match self.code.next() {
                    Some(n) => n,
                    None => {
                        for c in string_chars.into_iter().rev() {
                            self.code.replace(c);
                        }
                        return None;
                    }
                }
            });
        }
        Some(Token::Literal(Box::new(String::from_iter(string_chars))))
    }

    fn lex_special(&mut self) -> Option<Token> {
        for (pattern, res) in [
            ("->", Token::Arrow),
            ("::<", Token::OpLike(OpLike::LTurbofish)),
            ("+", Token::OpLike(OpLike::Op(Op::Plus))),
            ("-", Token::OpLike(OpLike::Op(Op::Minus))),
            ("*", Token::OpLike(OpLike::Op(Op::Times))),
            ("/", Token::OpLike(OpLike::Op(Op::Divided))),
            ("d", Token::OpLike(OpLike::Op(Op::D))),
            ("%", Token::OpLike(OpLike::Op(Op::Mod))),
            ("..=", Token::OpLike(OpLike::Op(Op::RangeEq))),
            ("..", Token::OpLike(OpLike::Op(Op::Range))),
            ("==", Token::OpLike(OpLike::Op(Op::Equal))),
            ("!=", Token::OpLike(OpLike::Op(Op::NotEqual))),
            (">=", Token::OpLike(OpLike::Op(Op::Geq))),
            ("<=", Token::OpLike(OpLike::Op(Op::Leq))),
            (">", Token::OpLike(OpLike::Op(Op::Greater))),
            ("<", Token::OpLike(OpLike::Op(Op::Less))),
            ("&&", Token::OpLike(OpLike::Op(Op::And))),
            ("||", Token::OpLike(OpLike::Op(Op::Or))),
            ("!", Token::OpLike(OpLike::Op(Op::Not))),
            ("(", Token::OpLike(OpLike::Bracket(Bracket::LBracket))),
            (")", Token::OpLike(OpLike::Bracket(Bracket::RBracket))),
            ("{", Token::OpLike(OpLike::Bracket(Bracket::LCurly))),
            ("}", Token::OpLike(OpLike::Bracket(Bracket::RCurly))),
            ("[", Token::OpLike(OpLike::Bracket(Bracket::LSquare))),
            ("]", Token::OpLike(OpLike::Bracket(Bracket::RSquare))),
            ("=", Token::OpLike(OpLike::Assign)),
            (".", Token::OpLike(OpLike::Access)),
            (",", Token::OpLike(OpLike::Comma)),
            (":", Token::OpLike(OpLike::Colon)),
            (";", Token::EOL),
        ] {
            if self.code.eat_str(pattern) {
                if let Token::OpLike(OpLike::Op(op)) = res {
                    if self.code.eat_str("=") {
                        return Some(Token::OpLike(OpLike::OpAssign(op)));
                    }
                }
                return Some(res);
            }
        }
        None
    }
}
