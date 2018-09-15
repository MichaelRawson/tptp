//! A crate for reading files in the [TPTP](tptp.org) format.
//!
//! # Quickstart
//! ```rust
//! extern crate tptp;
//! use tptp::prelude::*;
//!
//! // propagate any errors encountered for handling later
//! fn example(start: &str) -> Result<(), Error> {
//!
//!     // configure how to read the TPTP inputs
//!     // here we just use the defaults
//!     let reader = ReaderBuilder::new().read(start)?;
//!     
//!     // stream TPTP statements
//!     for statement in reader {
//!
//!         // reading each statement might involve an error
//!         let statement = statement?;
//!
//!         // process each statement as you see fit
//!         println!("{:?}", statement);
//!     }
//!
//!     Ok(())
//! }
//! ```

mod byte;
mod include;
mod intern;
mod lexical;
mod syntax;
mod tracking;

/// Syntax trees.
pub mod ast;
/// Errors that might be raised during processing.
pub mod error;
/// Line/column reporting.
pub mod position;
/// `Reader` API.
pub mod reader;
/// Implement custom behaviour for processing include() directives.
pub mod resolve;
/// Utilities, including `DefaultResolver`.
pub mod util;

/// Convenience re-exports.
pub mod prelude {
    pub use error::*;
    pub use position::Position;
    pub use reader::{Reader, ReaderBuilder};
    pub use resolve::Resolve;
    pub use util::{DefaultResolver, LocalFile};
}
