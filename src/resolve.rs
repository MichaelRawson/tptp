use super::position::*;
use super::errors::Result;

pub trait Resolve {
    type Source: Iterator<Item=Result<(Position, u8)>>;
    fn resolve<S>(&mut self, S) -> Result<Self::Source>
        where S: Into<String>;
}
