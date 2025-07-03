use std::fmt::Display;

pub type LineIndex = (usize, usize);

pub struct RuntimeError {
    pub err_loc: LineIndex,
    pub call_stack: Vec<LineIndex>,
    pub info: String,
}

#[derive(Debug)]
pub struct LexError {
    pub location: LineIndex,
    pub info: String,
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

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lex error: {} at {:?}", self.info, self.location)
    }
}