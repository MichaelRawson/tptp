use std::env::var_os;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};

use crate::error::Error;

fn open(path: &Path) -> Result<File, io::Error> {
    let path = path.canonicalize()?;
    File::open(path)
}

fn open_tptp(path: &Path) -> Result<File, io::Error> {
    let mut tptp = PathBuf::from(var_os("TPTP").unwrap_or_default());
    tptp.push(path);
    open(&tptp)
}

fn resolve_file(input: &str) -> Result<File, io::Error> {
    let path = Path::new(input);
    open(&path).or_else(|_| open_tptp(&path))
}

/// Resolve a TPTP include directive to a stream of bytes
///
/// This has the following behaviour:
///  * If the input is absolute, open that.
///  * If the input is relative, try to open relative to the current directory.
///  * Then try to open relative to the `TPTP` environment variable.
pub fn resolve(input: &str) -> Result<impl Iterator<Item = Result<u8, io::Error>>, Error> {
    let file = resolve_file(input)?;
    let buffered = BufReader::new(file);
    let iterator = buffered.bytes();
    Ok(iterator)
}
