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

/// Resolve a TPTP include directive to a `File`.
///
/// Files are opened read-only and relative to the current directory, or if that fails, relative to the `TPTP` environment variable.
pub fn resolve_include(input: &str) -> Result<File, io::Error> {
    let path = Path::new(input);
    open(&path).or_else(|_| open_tptp(&path))
}
