use super::ast::Statement;
use super::error::Error;
use super::include::IncludeProcessor;
use super::resolve::Resolve;
use super::util::DefaultResolver;

/// Configuration state to build a `Reader`, parameterized over a `Resolve` instance.
pub struct ReaderBuilder<R>
where
    R: Resolve,
{
    resolver: R,
    follow_includes: bool,
}

impl ReaderBuilder<DefaultResolver> {
    /// A new builder.
    /// By default, use a `DefaultResolver` and do not follow includes.
    pub fn new() -> Self {
        ReaderBuilder {
            resolver: DefaultResolver,
            follow_includes: false,
        }
    }
}

impl<R> ReaderBuilder<R>
where
    R: Resolve,
{
    /// Use a different `Resolve` implementation
    pub fn use_resolver(mut self, resolver: R) -> Self {
        self.resolver = resolver;
        self
    }

    /// Follow `include` directives, possibly recursively
    pub fn follow_includes(mut self) -> Self {
        self.follow_includes = true;
        self
    }

    /// Finalize and create an iterator.
    /// This might fail if e.g. the starting stream cannot be opened
    pub fn read<S>(self, start: S) -> Result<Reader<R>, Error>
    where
        S: Into<String>,
    {
        let path = start.into();
        let stream = IncludeProcessor::new(self.follow_includes, self.resolver, path.clone());
        match stream {
            Ok(stream) => Ok(Reader {
                stream,
                error: false,
            }),
            Err(reported) => Err(Error {
                reported,
                includes: vec![path.clone()],
            }),
        }
    }
}

/// An iterator over `Statement`s.
/// If an error is encountered, the error is reported only once, after which the iterator returns `None`.
pub struct Reader<R>
where
    R: Resolve,
{
    stream: IncludeProcessor<R>,
    error: bool,
}

impl<R> Iterator for Reader<R>
where
    R: Resolve,
{
    type Item = Result<Statement, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error {
            return None;
        }

        let next = self.stream.next()?;
        Some(match next {
            Ok(t) => Ok(t),
            Err(reported) => {
                self.error = true;
                Err(Error {
                    reported,
                    includes: self.stream.stack().clone(),
                })
            }
        })
    }
}
