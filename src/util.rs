use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;
use std::io::{BufReader, Bytes};
use std::path::PathBuf;

use super::errors::*;
use super::position::*;
use super::resolve::*;

pub struct LocalFile
{
    line: usize,
    column: usize,
    bytes: Bytes<BufReader<File>>
}

impl LocalFile {
   pub fn open(path: PathBuf) -> io::Result<Self> {
        assert!(path.is_absolute());
        let file = File::open(&path)?;
        let reader = BufReader::new(file);
        let stream = reader.bytes();

        let result = LocalFile {
            line: 1,
            column: 1,
            bytes: stream
        };
        Ok(result)
    }
}

impl Iterator for LocalFile {
    type Item = Result<(Position, u8)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.bytes.next().map(|result| result.map(|byte| {
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
        }).map_err(Error::IO))
    }
}

pub struct DefaultResolver;

impl DefaultResolver {
    fn resolve_impl(&mut self, path: PathBuf) -> io::Result<LocalFile> {
        let absolute = if path.is_relative() {
           let mut absolute = env::current_dir()?;
           absolute.push(path);
           absolute
        } else {
            path
        };

        let file = LocalFile::open(absolute)?;
        Ok(file)
    }
}

impl Resolve for DefaultResolver {
    type Source = LocalFile;

    fn resolve<S>(&mut self, input: S) -> Result<Self::Source>
        where S: Into<String> {
        let path = PathBuf::from(input.into());
        self.resolve_impl(path).map_err(Error::IO)
    }
}
