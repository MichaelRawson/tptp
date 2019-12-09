//! A crate for reading files in the TPTP format.
//! Supplies TPTP `nom` parsers for maximum flexibility.
//!
//! # Quickstart
//! ```rust
//! use tptp::parsers::tptp_input_iterator;
//! use tptp::syntax::Visitor;
//!
//! struct MyVisitor;
//! impl Visitor for MyVisitor {}
//!
//! fn example(bytes: &[u8]) {
//!     let mut visitor = MyVisitor;
//!     let mut parser = tptp_input_iterator::<()>(bytes);
//!     for input in &mut parser {
//!         println!("{}", &input);
//!         visitor.visit_tptp_input(input);
//!     }
//!     assert!(parser.finish().is_ok());
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
