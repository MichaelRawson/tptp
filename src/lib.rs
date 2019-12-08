//! A crate for reading files in the TPTP format.
//! Supplies TPTP `nom` parsers for maximum flexibility.
//!
//! # Quickstart
//! ```rust
//! use tptp::parsers::ignored_then_tptp_input;
//!
//! fn example(bytes: &[u8]) {
//!     let mut position = bytes;
//!     loop {
//!         // choose how to perform error handling
//!         let result = ignored_then_tptp_input::<()>(position);
//!         let (next, statement) = result.expect("parse error");
//!
//!         // EOF
//!         if statement.is_none() {
//!             break;
//!         }
//!         else {
//!             // process `statement` as you see fit
//!         }
//!         position = next;
//!     }
//! }
//! ```

extern crate derive_more;

/// Parsed TPTP structures
pub mod syntax;

/// `nom` parsers corresponding to [TPTP BNF](http://www.tptp.org/TPTP/SyntaxBNF.html)
pub mod parsers;

/// Resolve `include` directives
pub mod resolve;

#[cfg(test)]
mod tests;
