use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::{BufReader, Bytes};
use std::path::PathBuf;

use super::resolve::*;

/// A local input file, loaded by `DefaultResolver`.
pub struct LocalFile {
    bytes: Bytes<BufReader<File>>,
}

impl LocalFile {
    fn open(path: &PathBuf) -> io::Result<Self> {
        assert!(path.is_absolute());
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        let stream = reader.bytes();

        let result = LocalFile { bytes: stream };
        Ok(result)
    }
}

impl Iterator for LocalFile {
    type Item = io::Result<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        self.bytes.next()
    }
}

/// The default `Resolve` instance.
/// Behaviour is like that of many existing ATP systems.
/// Paths are resolved as either absolute paths or relative to the current directory.
pub struct DefaultResolver;

impl Resolve for DefaultResolver {
    type Source = LocalFile;

    fn resolve<S>(&mut self, input: S) -> io::Result<Self::Source>
    where
        S: Into<String>,
    {
        let path = PathBuf::from(input.into());
        let absolute = if path.is_relative() {
            let mut absolute = env::current_dir()?;
            absolute.push(path);
            absolute
        } else {
            path
        };

        let file = LocalFile::open(&absolute)?;
        Ok(file)
    }
}
