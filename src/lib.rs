//! A crate for reading files in the TPTP format.
//!
//! # Quickstart
//! ```rust
//! fn example(bytes: &[u8]) {
//!     // stream TPTP statements
//!     for statement in tptp::parse(bytes) {
//!
//!         // reading each statement might involve an error
//!         let statement = statement.expect("parse error");
//!
//!         // process each statement as you see fit
//!         println!("{:#?}", statement);
//!
//!     }
//! }
//! ```

/// Parsed syntactic structures
pub mod syntax;

mod parser;
mod resolve;

pub use crate::parser::*;
pub use crate::resolve::*;
