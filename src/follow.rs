use std::mem;
use std::vec::Vec;

use error::{Error, ErrorWithContext, IncludeError};
use position::Position;
use syntax::Statement;

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
///     follow.include(path.into())?;
///     Ok(follow)
/// }
///```
pub struct Follow<F, T>
where
    F: FnMut(&str) -> Result<T, Error>,
    T: Iterator<Item = Result<(Position, Statement), Error>>,
{
    load: F,
    file_stack: Vec<String>,
    position_stack: Vec<Position>,
    stream_stack: Vec<T>,
    error: bool,
}

impl<F, T> Follow<F, T>
where
    F: FnMut(&str) -> Result<T, Error>,
    T: Iterator<Item = Result<(Position, Statement), Error>>,
{
    /// Create a new empty stream which calls `load` when required
    pub fn new(load: F) -> Self {
        Follow {
            load,
            file_stack: vec![],
            position_stack: vec![],
            stream_stack: vec![],
            error: false,
        }
    }

    /// Push a new path into the stream
    pub fn include(&mut self, path: String) -> Result<(), ErrorWithContext> {
        if self.file_stack.contains(&path) {
            let error = IncludeError::Circular(path).into();
            return Err(self.on_error(error));
        }

        self.file_stack.push(path);
        self.position_stack.push(Position::default());

        let stream = (self.load)(self.file_stack.last().unwrap()).map_err(|e| self.on_error(e))?;
        self.stream_stack.push(stream);
        Ok(())
    }

    fn on_error(&mut self, error: Error) -> ErrorWithContext {
        self.error = true;
        let files = mem::replace(&mut self.file_stack, vec![]);
        let positions = mem::replace(&mut self.position_stack, vec![]);
        let location_stack = files.into_iter().zip(positions.into_iter()).collect();

        ErrorWithContext {
            error,
            location_stack,
        }
    }
}

impl<F, T> Iterator for Follow<F, T>
where
    F: FnMut(&str) -> Result<T, Error>,
    T: Iterator<Item = Result<(Position, Statement), Error>>,
{
    type Item = Result<Statement, ErrorWithContext>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error {
            return None;
        }
        loop {
            let next = self.stream_stack.last_mut()?.next();
            match next {
                Some(Ok((position, statement))) => {
                    *(self.position_stack.last_mut()?) = position;
                    use syntax::Statement::Include;
                    match statement {
                        Include(path) => match self.include(path) {
                            Ok(()) => {}
                            Err(e) => {
                                return Some(Err(e));
                            }
                        },
                        _ => {
                            return Some(Ok(statement));
                        }
                    }
                }
                Some(Err(e)) => {
                    return Some(Err(self.on_error(e)));
                }
                None => {
                    self.stream_stack.pop();
                    self.position_stack.pop();
                }
            }
        }
    }
}
