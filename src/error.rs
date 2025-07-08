use std::fmt::{Debug, Display};

#[derive(Clone, Copy)]
pub struct LineIndex(pub usize, pub usize);

impl Debug for LineIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, col {}", self.0, self.1)
    }
}

#[derive(Debug)]
pub struct LexError {
    pub location: LineIndex,
    pub info: String,
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lex error: {} at {:?}", self.info, self.location)
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub location: LineIndex,
    pub info: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error: {} at {:?}", self.info, self.location)
    }
}

pub struct RuntimeError {
    err_loc: Option<LineIndex>,
    call_stack: Vec<LineIndex>,
    info: String,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Runtime Error {}: {}",
            match self.err_loc {
                Some(l) => format!("at {l:?}"),
                None => "with unknown location".to_string(),
            },
            self.info
        )?;
        for line in &self.call_stack {
            writeln!(f, "\t@{line:?}")?;
        }
        Ok(())
    }
}

impl RuntimeError {
    pub fn partial(info: &str)->Self{
        Self {
            err_loc: None,
            call_stack: vec![],
            info: info.to_string(),
        }
    }

    pub fn single(info: &str, loc: LineIndex) -> Self {
        Self {
            err_loc: Some(loc),
            call_stack: vec![loc],
            info: info.to_string()
        }
    }

    pub fn stack_loc(mut self, loc: LineIndex) -> Self {
        if self.err_loc.is_none() {
            self.err_loc = Some(loc);
        }
        self.call_stack.push(loc);
        self
    }
}