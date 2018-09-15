use std::io;
use std::prelude::v1::Result as Either;

use super::lexical::Token;
use super::position::Position;

#[derive(Debug)]
pub enum LexicalError {
    UnknownByte(Position, u8),
    UnknownOperator(Position),
    UnclosedComment(Position),
    NonPrintable(Position, u8),
    BadEscape(Position, u8),
    UnclosedQuote(Position),
    UnexpectedEnd,
}

#[derive(Debug)]
pub enum SyntacticError {
    UnknownRole(Position, String),
    UnknownDefined(Position, String),
    UnexpectedToken(Position, Token),
    UnexpectedEnd,
}

#[derive(Debug)]
pub enum IncludeError {
    CircularInclude(Position, String),
}

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Lexical(LexicalError),
    Syntactic(SyntacticError),
    Include(IncludeError),
}

pub type Result<T> = Either<T, Error>;

#[derive(Debug)]
pub struct Context {
    pub error: Error,
    pub includes: Vec<String>,
}
