use std::mem;
use std::vec::Vec;

use error::{Error, ErrorInfo, IncludeError};
use position::Position;
use syntax::{Name, Statement};

/// Follow include directives by calling a user-specified function.
/// `stream` could be implemented like so, for example:
/// ```rust
/// pub fn stream(
///     path: &str
/// ) -> Result<impl Iterator<Item = Result<syntax::Statement, error::ErrorWithContext>>, error::ErrorWithContext> {
///     let mut follow = follow::Follow::new(|path| {
///         let stream = resolve::resolve(path)?;
///         let lexer = lexer::Lexer::new(stream);
///         let parser = parser::Parser::new(lexer);
///         Ok(parser)
///     });
///     follow.include(path.into(), None)?;
///     Ok(follow)
/// }
///```
pub struct Follow<F, T>
where
    F: FnMut(&str) -> Result<T, Error>,
    T: Iterator<Item = Result<Statement, (Position, Error)>>,
{
    load: F,
    name_stack: Vec<String>,
    select_stack: Vec<Option<Vec<Name>>>,
    stream_stack: Vec<T>,
    error: bool,
}

impl<F, T> Follow<F, T>
where
    F: FnMut(&str) -> Result<T, Error>,
    T: Iterator<Item = Result<Statement, (Position, Error)>>,
{
    /// Create a new empty stream which calls `load` when required
    pub fn new(load: F) -> Self {
        Follow {
            load,
            name_stack: vec![],
            select_stack: vec![],
            stream_stack: vec![],
            error: false,
        }
    }

    /// Push a new path into the stream, optionally selecting statements
    pub fn include(&mut self, path: String, select: Option<Vec<Name>>) -> Result<(), ErrorInfo> {
        if self.name_stack.contains(&path) {
            let error = IncludeError::Circular(path).into();
            return Err(self.on_error(Position::default(), error));
        }

        self.name_stack.push(path);
        self.select_stack.push(select);
        let stream = (self.load)(self.name_stack.last().unwrap())
            .map_err(|e| self.on_error(Position::default(), e))?;
        self.stream_stack.push(stream);

        Ok(())
    }

    fn on_error(&mut self, position: Position, error: Error) -> ErrorInfo {
        self.error = true;
        let includes = mem::replace(&mut self.name_stack, vec![]);
        self.select_stack = vec![];
        self.stream_stack = vec![];

        ErrorInfo {
            includes,
            position,
            error,
        }
    }
}

impl<F, T> Iterator for Follow<F, T>
where
    F: FnMut(&str) -> Result<T, Error>,
    T: Iterator<Item = Result<Statement, (Position, Error)>>,
{
    type Item = Result<Statement, ErrorInfo>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error {
            return None;
        }
        loop {
            let next = self.stream_stack.last_mut()?.next();
            match next {
                Some(Ok(statement)) => {
                    use syntax::Statement::Include;
                    match statement {
                        Include(path, select) => match self.include(path, select) {
                            Ok(()) => {}
                            Err(e) => {
                                return Some(Err(e));
                            }
                        },
                        statement => match self.select_stack.last()? {
                            None => {
                                return Some(Ok(statement));
                            }
                            Some(names) if names.contains(statement.name()) => {
                                return Some(Ok(statement));
                            }
                            _ => {}
                        },
                    }
                }
                Some(Err((p, e))) => {
                    return Some(Err(self.on_error(p, e)));
                }
                None => {
                    self.name_stack.pop();
                    self.select_stack.pop();
                    self.stream_stack.pop();
                }
            }
        }
    }
}
