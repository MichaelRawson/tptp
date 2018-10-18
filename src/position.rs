use std::fmt;

/// A line/column pair representing a position within a file
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    /// Update the position to take into account advancing by a byte
    pub fn update(&mut self, c: u8) {
        match c {
            b'\n' => {
                self.line += 1;
                self.column = 0;
            }
            _ => {
                self.column += 1;
            }
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Position::new(1, 0)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
