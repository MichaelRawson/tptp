//! A crate for reading files in the TPTP format.
//! Supplies TPTP [`nom`](https://crates.io/crates/nom) parsers for maximum flexibility.
//!
//! # Quickstart
//! ```rust
//! use tptp::parsers::TPTPIterator;
//! use tptp::visitor::Visitor;
//!
//! struct MyVisitor;
//! impl<'a> Visitor<'a> for MyVisitor {}
//!
//! fn example(bytes: &[u8]) {
//!     let mut visitor = MyVisitor;
//!     let mut parser = TPTPIterator::<()>::new(bytes);
//!     for input in &mut parser {
//!         let input = input.expect("syntax error");
//!         println!("{}", &input);
//!         visitor.visit_tptp_input(&input);
//!     }
//!     assert!(parser.remaining.is_empty());
//! }
//! ```

#![no_std]
extern crate alloc;

/// `nom` parsers corresponding to [TPTP BNF](http://www.tptp.org/TPTP/SyntaxBNF.html)
pub mod parsers;

/// Parsed TPTP structures
pub mod syntax;

/// TPTP Visitor trait
pub mod visitor;

#[cfg(test)]
mod tests;
