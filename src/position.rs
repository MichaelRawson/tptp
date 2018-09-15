/// A line/column pair representing a position within an ASCII iterator
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Position {
    /// The current line
    pub line: usize,
    /// The current column
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Position { line, column }
    }
}
