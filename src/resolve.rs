use std::env::var_os;
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

fn open(path: &Path) -> Result<File, io::Error> {
    let path = path.canonicalize()?;
    File::open(path)
}

fn open_tptp(path: &Path) -> Result<File, io::Error> {
    let mut tptp = PathBuf::from(var_os("TPTP").unwrap_or_default());
    tptp.push(path);
    open(&tptp)
}

/// Resolve a TPTP include directive to a stream of bytes
///
/// This has the following behaviour:
///  * If the input is absolute, open that.
///  * If the input is relative, try to open relative to the current directory.
///  * Then try to open relative to the `TPTP` environment variable.
pub fn resolve_include(input: &str) -> Result<File, io::Error> {
    let path = Path::new(input);
    open(&path).or_else(|_| open_tptp(&path))
}
