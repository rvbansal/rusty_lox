use std::fmt;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CodePosition {
    pub byte_pos: usize,
    pub line_no: usize,
    pub column_no: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub start_pos: CodePosition,
    pub end_pos: CodePosition,
}

impl CodePosition {
    pub fn new(byte_pos: usize, line_no: usize, column_no: usize) -> Self {
        CodePosition {
            byte_pos,
            line_no,
            column_no,
        }
    }
}

impl fmt::Display for CodePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line_no, self.column_no)
    }
}

impl Span {
    pub fn new(start_pos: CodePosition, end_pos: CodePosition) -> Self {
        Span { start_pos, end_pos }
    }

    pub fn default() -> Self {
        let default_pos = CodePosition::new(0, 0, 0);
        Span::new(default_pos, default_pos)
    }

    pub fn extend(&self, other: Self) -> Self {
        Span {
            start_pos: std::cmp::min(self.start_pos, other.start_pos),
            end_pos: std::cmp::max(self.end_pos, other.end_pos),
        }
    }

    pub fn extract_string<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.start_pos.byte_pos..self.end_pos.byte_pos)
    }
}
