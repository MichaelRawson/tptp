use std::io;

/// A trait that dictates what should occur when an `include` directive is encountered.
/// Also used to resolve the starting path.
pub trait Resolve {
    /// An iterator over ASCII bytes
    type Source: Iterator<Item = io::Result<u8>>;

    /// Resolve an included path into an iterator.
    /// Fail with an IO error if necessary.
    fn resolve<S>(&mut self, S) -> io::Result<Self::Source>
    where
        S: Into<String>;
}
