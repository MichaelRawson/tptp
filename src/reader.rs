use super::errors::Context;
use super::include::IncludeProcessor;
use super::syntax::Statement;
use super::resolve::Resolve;
use super::util::DefaultResolver;

pub struct ReaderBuilder<R>
where R: Resolve {
    resolver: R,
    follow_includes: bool
}

impl<R> ReaderBuilder<R>
where R: Resolve {
    pub fn use_resolver(mut self, resolver: R) -> Self {
        self.resolver = resolver;
        self
    }

    pub fn follow_includes(mut self) -> Self {
        self.follow_includes = true;
        self
    }

    pub fn read<S>(self, start: S) -> Result<Reader<R>, Context>
        where S: Into<String> {
        let path = start.into();
        let stream = IncludeProcessor::new(
            self.follow_includes,
            self.resolver,
            path.clone()
        );
        match stream {
            Ok(stream) => Ok(Reader {
                stream,
                error: false
            }),
            Err(error) => Err(Context {
                error,
                includes: vec![path.clone()]
            })
        }
    }
}

pub struct Reader<R>
where R: Resolve {
    stream: IncludeProcessor<R>,
    error: bool
}

impl Reader<DefaultResolver> {
    pub fn new() -> ReaderBuilder<DefaultResolver> {
        ReaderBuilder {
            resolver: DefaultResolver,
            follow_includes: false
        }
    }
}

impl<R> Iterator for Reader<R>
where R: Resolve {
    type Item = Result<Statement, Context>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error {
            return None;
        }

        let next = self.stream.next()?;
        Some(match next {
            Ok(t) => Ok(t),
            Err(error) => {
                self.error = true;
                Err(Context {
                    error,
                    includes: self.stream.stack().clone()
                })
            }
        })
    }
}
