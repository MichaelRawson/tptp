//! A crate for reading files in the TPTP format.
//!
//! # Quickstart
//! ```rust
//! extern crate tptp;
//!
//! // propagate any errors encountered for handling later
//! fn example() -> Result<(), tptp::error::ErrorInfo> {
//!
//!     // stream TPTP statements, following include directives
//!     for statement in tptp::stream("example.p")? {
//!
//!         // reading each statement might involve an error
//!         let statement = statement?;
//!
//!         // process each statement as you see fit
//!         println!("{:#?}", statement);
//!
//!     }
//!
//!     Ok(())
//! }
//! ```

/// Errors that might be raised during processing
pub mod error;
/// Follow include directives
pub mod follow;
/// Lexical analysis
pub mod lexer;
/// Parsing
pub mod parser;
/// Line/column reporting
pub mod position;
/// Resolve include() paths according to the TPTP spec
pub mod resolve;
/// Syntax trees
pub mod syntax;
/// Lexical tokens
pub mod token;

/// Convenient API to stream statements, following include directives recursively
pub fn stream(
    path: &str,
) -> Result<impl Iterator<Item = Result<syntax::Statement, error::ErrorInfo>>, error::ErrorInfo> {
    let mut follow = follow::Follow::new(|path| {
        let stream = resolve::resolve(path)?;
        let lexer = lexer::Lexer::new(stream);
        let parser = parser::Parser::new(lexer);
        Ok(parser)
    });
    follow.include(path.into(), None)?;
    Ok(follow)
}
