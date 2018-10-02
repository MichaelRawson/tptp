use std::io;
use std::prelude::v1::Result as Either;
use std::sync::Arc;

use super::lexical::Token;
use super::position::Position;

/// Errors raised during the tokenization stage
#[derive(Debug)]
pub enum Lexical {
    /// A byte outside the expected range
    UnknownByte(Position, u8),
    /// Operator not (currently) recognised
    UnknownOperator(Position),
    /// Runaway multi-line comment
    UnclosedComment(Position),
    /// Non-printable character inside a quoted string
    NonPrintable(Position, u8),
    /// Invalid escape character inside a quoted string
    BadEscape(Position, u8),
    /// Runaway quoted string
    UnclosedQuote(Position),
    /// End of iterator mid-token
    UnexpectedEnd,
}

/// Errors raised during the parsing stage
#[derive(Debug)]
pub enum Syntactic {
    /// A TPTP dialect (like TFF) that isn't supported yet
    UnsupportedDialect(Position, String),
    /// An as-yet unknown TPTP role
    UnknownRole(Position, String),
    /// An as-yet unknown TPTP defined operation
    UnknownDefined(Position, String),
    /// Syntax error
    UnexpectedToken(Position, Token),
    /// Syntax error: end of iterator mid-statement
    UnexpectedEnd,
}

/// Errors raised while processing includes
#[derive(Debug)]
pub enum Include {
    /// A circular inclusion occurred
    Circular(Position, String),
}

/// Any error that might be encountered
#[derive(Debug)]
pub enum Reported {
    /// IO error on the underlying streams
    IO(io::Error),
    /// Lexical error
    Lexical(Lexical),
    /// Syntactic error
    Syntactic(Syntactic),
    /// Include error
    Include(Include),
}

pub(crate) type Result<T> = Either<T, Reported>;

/// A reported error plus some context.
#[derive(Debug)]
pub struct Error {
    /// The error that was encountered
    pub reported: Reported,
    /// Chain of `include`s leading up to the error
    pub includes: Vec<Arc<String>>,
}
