//! A crate for reading files in the FOF dialect of the TPTP format.
//!
//! # Quickstart
//! ```rust
//! extern crate tptp;
//!
//! // propagate any errors encountered for handling later
//! fn example() -> Result<(), tptp::error::ErrorWithContext> {
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
/// Line/column reporting
pub mod position;
/// Lexical tokens
pub mod token;
/// Lexical analysis
pub mod lexer;
/// Syntax trees
pub mod syntax;
/// Parsing
pub mod parser;
/// Resolve include() paths according to the TPTP spec
pub mod resolve;
/// Follow include directives
pub mod follow;

/// Convenient API to stream statements, following include directives recursively
pub fn stream(
    path: &str,
) -> Result<impl Iterator<Item = Result<syntax::Statement, error::ErrorWithContext>>, error::ErrorWithContext> {
    let mut follow = follow::Follow::new(|path| {
        let stream = resolve::resolve(path)?;
        let lexer = lexer::Lexer::new(stream);
        let parser = parser::Parser::new(lexer);
        Ok(parser)
    });
    follow.include(path.into())?;
    Ok(follow)
}
