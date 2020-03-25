mod ast;
mod lexer;
mod resolver;

pub use ast::*;
pub use lexer::*;
pub use resolver::*;

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub col: usize,
    pub row: usize,
    pub len: usize,
    pub file: Option<String>,
}

impl Span {
    pub fn new(col: usize, row: usize, len: usize) -> Span {
        Span {
            col,
            row,
            len,
            file: None,
        }
    }

    pub fn new_file(col: usize, row: usize, len: usize, file: String) -> Span {
        Span {
            col,
            row,
            len,
            file: Some(file),
        }
    }

    pub fn offset(&self, col_offset: isize, row_offset: isize) -> Span {
        let col = self.col as isize + col_offset;
        let row = self.row as isize + row_offset;
        Span {
            col: (self.col as isize + col_offset) as usize,
            row: (self.row as isize + row_offset) as usize,
            len: self.len,
            file: self.file.clone(),
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, {} (len: {}", self.col, self.row, self.len)?;
        match self.file.as_ref() {
            Some(file) => write!(f, ", file: {})", file),
            None => write!(f, ", <no file>)"),
        }
    }
}
