pub type LineIndex = (usize, usize);

pub struct RuntimeError {
    pub err_loc: LineIndex,
    pub call_stack: Vec<LineIndex>,
    pub info: String,
}

#[derive(Debug)]
pub struct CompileError {
    pub location: LineIndex,
    pub info: String,
}