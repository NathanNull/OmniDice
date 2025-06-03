use std::{fmt::Debug, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Int(i32),
    Float(f32),
    LBracket,
    RBracket,
    Op(OpToken),
    EOL,
    EOF,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpToken {
    Plus,
    Minus,
    Times,
    Divided,
    D,
    Assign,
    Access,
}

impl Debug for OpToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Divided => "/",
            Self::D => "d",
            Self::Assign => "=",
            Self::Access => ".",
        };
        write!(f, "{c}")
    }
}

pub struct Lexer<'a> {
    code: Chars<'a>,
    peeked: Vec<char>,
}

#[derive(Debug, Clone)]
pub struct TokenString {
    pub tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code: code.chars(),
            peeked: vec![],
        }
    }

    pub fn lex(&mut self) -> TokenString {
        let mut tokens = vec![];
        while {
            while self.peek().is_some_and(|c| c.is_whitespace()) {
                self.next();
            }
            true
        } && self.peek().is_some()
        {
            if self.lex_comment() {
                // it's a comment, carry on
            } else if let Some(tk) = self.lex_identifier() {
                tokens.push(tk)
            } else if let Some(tk) = self.lex_number() {
                tokens.push(tk)
            } else if let Some(tk) = self.lex_special() {
                tokens.push(tk)
            } else {
                panic!(
                    "Couldn't tokenize, next is {:?}, rest is {:?}",
                    self.peek(),
                    self.code.as_str()
                )
            }
        }
        tokens.push(Token::EOF);
        TokenString { tokens }
    }

    fn peek(&mut self) -> Option<char> {
        if self.peeked.is_empty() {
            if let Some(next) = self.code.next() {
                self.peeked.push(next);
            }
        }
        self.peeked.last().copied()
    }
    fn next(&mut self) -> Option<char> {
        if let Some(res) = self.peeked.pop() {
            Some(res)
        } else {
            self.code.next()
        }
    }
    fn replace(&mut self, c: char) {
        self.peeked.push(c);
    }

    fn lex_comment(&mut self) -> bool {
        if self.peek().is_none_or(|c|c!='/') {
            return false;
        }
        self.next();
        if self.peek().is_none_or(|c|c!='/') {
            self.replace('/');
            return false;
        }
        self.next();
        while self.peek().is_some_and(|c|c != '\n') {
            self.next();
        }
        true
    }

    fn lex_identifier(&mut self) -> Option<Token> {
        if self.peek().is_none_or(|c| !c.is_alphabetic()) {
            return None;
        }
        let mut name = vec![];
        while self.peek().is_some_and(|c| c.is_alphanumeric() || c == '_') {
            name.push(self.next().unwrap());
        }
        let name_str = name
            .iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .concat();

        // Invalidate identifiers that represent valid dice
        if name_str
            .strip_prefix("d")
            .is_none_or(|n| n.parse::<i32>().is_err())
        {
            Some(Token::Identifier(name_str))
        } else {
            // Put the tokens back so that the last taken one is replaced first
            // Turns out we didn't actually need them
            for c in name.into_iter().rev() {
                self.replace(c);
            }
            None
        }
    }

    fn lex_number(&mut self) -> Option<Token> {
        if self.peek().is_none_or(|c| !c.is_numeric()) {
            return None;
        }
        let mut num = vec![];
        while self.peek().is_some_and(|c| c.is_numeric() || c == '.') {
            num.push(self.next().unwrap());
        }
        let num_str = num
            .into_iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .concat();
        if num_str.contains('.') {
            // The parse will error if there's more than 1 dot so we don't need to check for that.
            num_str.parse::<f32>().ok().map(|f| Token::Float(f))
        } else {
            num_str.parse::<i32>().ok().map(|i| Token::Int(i))
        }
    }

    fn lex_special(&mut self) -> Option<Token> {
        let c = match self.peek() {
            Some(c) => c,
            None => return None,
        };
        let res = match c {
            '+' => Token::Op(OpToken::Plus),
            '-' => Token::Op(OpToken::Minus),
            '*' => Token::Op(OpToken::Times),
            '/' => Token::Op(OpToken::Divided),
            'd' => Token::Op(OpToken::D),
            '(' => Token::LBracket,
            ')' => Token::RBracket,
            '=' => Token::Op(OpToken::Assign),
            '.' => Token::Op(OpToken::Access),
            ';' => Token::EOL,
            _ => return None,
        };
        self.next();
        Some(res)
    }
}
