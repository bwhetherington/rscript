pub mod ast;
pub mod lexer;
pub mod resolver;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub col: usize,
    pub row: usize,
    pub len: usize,
}

impl Span {
    pub fn new(col: usize, row: usize, len: usize) -> Span {
        Span { col, row, len }
    }

    pub fn offset(&self, col_offset: isize, row_offset: isize) -> Span {
        let col = self.col as isize + col_offset;
        let row = self.row as isize + row_offset;
        Span::new(col as usize, row as usize, self.len)
    }
}
