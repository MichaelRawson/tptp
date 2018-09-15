use std::cell::RefCell;
use std::rc::Rc;
use std::vec::Vec;

use super::ast::*;
use super::error::*;
use super::intern::StringCache;
use super::lexical::Tokenizer;
use super::position::*;
use super::resolve::Resolve;
use super::syntax::Parser;
use super::tracking::Tracking;

pub struct IncludeProcessor<R>
where
    R: Resolve,
{
    cache: Rc<RefCell<StringCache>>,
    follow: bool,
    resolver: R,
    name_stack: Vec<String>,
    stream_stack: Vec<Parser<Tokenizer<Tracking<R::Source>>>>,
}

impl<R> IncludeProcessor<R>
where
    R: Resolve,
{
    pub fn new(follow: bool, resolver: R, name: String) -> Result<Self> {
        let cache = Rc::new(RefCell::new(StringCache::new()));
        let mut new = IncludeProcessor {
            cache,
            follow,
            resolver,
            name_stack: vec![],
            stream_stack: vec![],
        };
        new.push_include(name, Position::default())?;
        Ok(new)
    }

    pub fn stack(&self) -> &Vec<String> {
        &self.name_stack
    }

    fn push_include(&mut self, name: String, position: Position) -> Result<()> {
        if self.name_stack.contains(&name) {
            let error = Include::Circular(position, name);
            return Err(Reported::Include(error));
        }

        let stream = self.resolver.resolve(name.clone()).map_err(Reported::IO)?;
        let stream = Tracking::new(stream);
        let stream = Tokenizer::new(stream, self.cache.clone());
        let stream = Parser::new(stream);
        self.name_stack.push(name);
        self.stream_stack.push(stream);
        Ok(())
    }

    fn pop_include(&mut self) {
        self.name_stack.pop();
        self.stream_stack.pop();
    }
}

impl<R> Iterator for IncludeProcessor<R>
where
    R: Resolve,
{
    type Item = Result<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.stream_stack.last_mut()?.next();
            match next {
                Some(Ok((position, statement))) => match statement {
                    Statement::Include(path) => if self.follow {
                        match self.push_include(path, position) {
                            Ok(()) => {
                                continue;
                            }
                            Err(e) => {
                                return Some(Err(e));
                            }
                        }
                    } else {
                        return Some(Ok(Statement::Include(path)));
                    },
                    s => {
                        return Some(Ok(s));
                    }
                },
                Some(Err(e)) => {
                    return Some(Err(e));
                }
                None => {
                    self.pop_include();
                    continue;
                }
            }
        }
    }
}
