use std::fmt;

/// Tracks position in source code for error messages.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CodePosition {
    pub line_no: usize,
    pub column_no: usize,
}

impl CodePosition {
    pub fn new(line_no: usize, column_no: usize) -> Self {
        CodePosition { line_no, column_no }
    }
}

impl fmt::Display for CodePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line_no, self.column_no)
    }
}

/// Represents span of full source code.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub start_pos: CodePosition,
    pub end_pos: CodePosition,
}

impl Span {
    pub fn new(start_pos: CodePosition, end_pos: CodePosition) -> Self {
        Span { start_pos, end_pos }
    }

    pub fn unite(&self, other: &Self) -> Self {
        Span {
            start_pos: std::cmp::min(self.start_pos, other.start_pos),
            end_pos: std::cmp::max(self.start_pos, other.start_pos),
        }
    }
}
