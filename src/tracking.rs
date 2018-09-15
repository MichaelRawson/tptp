use std::io;

use super::error::*;
use super::position::*;

pub struct Tracking<T>
where
    T: Iterator<Item = io::Result<u8>>,
{
    stream: T,
    line: usize,
    column: usize,
}

impl<T> Tracking<T>
where
    T: Iterator<Item = io::Result<u8>>,
{
    pub fn new(stream: T) -> Self {
        Tracking {
            stream,
            line: 1,
            column: 1,
        }
    }
}

impl<T> Iterator for Tracking<T>
where
    T: Iterator<Item = io::Result<u8>>,
{
    type Item = Result<(Position, u8)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.stream.next().map(|result| {
            result
                .map(|byte| {
                    let position = Position::new(self.line, self.column);
                    match byte {
                        b'\n' => {
                            self.line += 1;
                            self.column = 1;
                        }
                        _ => {
                            self.column += 1;
                        }
                    }
                    (position, byte)
                }).map_err(Reported::IO)
        })
    }
}
