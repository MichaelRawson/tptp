use std::io;
use std::vec::Vec;

use position::Position;
use token::Token;

/// Errors raised during lexical analysis
#[derive(Debug)]
pub enum LexicalError {
    /// A byte outside the expected range
    UnknownByte(u8),
    /// Operator not (currently) recognised
    UnknownOperator(String),
    /// Runaway multi-line comment
    UnclosedComment,
    /// Non-printable character inside a quoted string
    NonPrintable(u8),
    /// Invalid escape character inside a quoted string
    BadEscape(u8),
    /// Runaway quoted string
    UnclosedQuote,
}

/// Errors raised during the parsing stage
#[derive(Debug)]
pub enum SyntacticError {
    /// A TPTP dialect (like TFF) that isn't supported yet
    UnsupportedDialect(String),
    /// An as-yet unknown TPTP formula role
    UnknownRole(String),
    /// An as-yet unknown TPTP defined operation
    UnknownDefined(String),
    /// Syntax error
    UnexpectedToken(Token),
}

/// Errors raised while processing includes
#[derive(Debug)]
pub enum IncludeError {
    /// A circular inclusion occurred
    Circular(String),
}

/// Any error that might be encountered
#[derive(Debug)]
pub enum Error {
    /// Propagated error from the host system
    System(io::Error),
    /// Lexical error
    Lexical(LexicalError),
    /// Syntactic error
    Syntactic(SyntacticError),
    /// Include error
    Include(IncludeError),
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::System(error)
    }
}

impl From<LexicalError> for Error {
    fn from(error: LexicalError) -> Self {
        Error::Lexical(error)
    }
}

impl From<SyntacticError> for Error {
    fn from(error: SyntacticError) -> Self {
        Error::Syntactic(error)
    }
}

impl From<IncludeError> for Error {
    fn from(error: IncludeError) -> Self {
        Error::Include(error)
    }
}

/// An `Error` with position information
#[derive(Debug)]
pub struct ErrorInfo {
    /// A trace of include statements so far
    pub includes: Vec<String>,
    /// The position at which the error occurred
    pub position: Position,
    /// The error
    pub error: Error,
}
