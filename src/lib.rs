//! A crate for reading files in the TPTP format.
//!
//! Most users will want to use the `TPTPIterator` interface to parse statements from a TPTP file.
//! After you have a parsed statement, you can either manipulate it manually, or use the `Visitor` interface to ease writing traversals.
//! Individual parsers for each item of the TPTP BNF are available: generally this is a one-to-one map, but for efficiency/sanity reasons items like `integer` are not split into `signed_integer` and `unsigned_integer`.
//!
//! Parsers are built with [nom](https://github.com/Geal/nom), and this implementation detail is kept deliberately transparent.
//! If you need it, you can use nom's facilities such as error handling or streaming.
//! All parsers are a function from byte slices to `Result`.
//! The input will never be copied, only references made.
//! The crate is `#![no_std]`, but syntax trees must allocate with the current design so the `alloc` crate is required.
//!
//! Parsers are streaming, so they will signal "incomplete" on EOF, rather than success or failure, until the outcome is known.
//! Most of the time this is obvious, but the behaviour can be surprising.
//! For example, parsing `foo$` as a `lower_word` succeeds, returning the trailing `$` to parse next.
//! However, parsing `foo` is "incomplete" as there might be more input coming.
//!
//! Support for `serde` can be switched on with a feature flag as usual.
//! Structures can then be serialised, but not deseralised due to ownership issues.
//! # Quickstart
//! ```rust
//! use tptp::TPTPIterator;
//! use tptp::visitor::Visitor;
//!
//! struct MyVisitor;
//! impl<'a> Visitor<'a> for MyVisitor {}
//!
//! fn example(bytes: &[u8]) {
//!     let mut visitor = MyVisitor;
//!     let mut parser = TPTPIterator::<()>::new(bytes);
//!     for result in &mut parser {
//!         let input = result.expect("syntax error");
//!         println!("{}", &input);
//!         visitor.visit_tptp_input(&input);
//!     }
//!     assert!(parser.remaining.is_empty());
//! }
//! ```

#![no_std]
extern crate alloc;

#[macro_use]
mod utils;
/// the CNF dialect
pub mod cnf;
/// common syntax across all dialects
pub mod common;
/// the FOF dialect
pub mod fof;
/// top-level inputs, formula annotations, etc.
pub mod top;

#[cfg(test)]
mod tests;
/// visitor pattern
pub mod visitor;

/// an alias for nom's `ParseError`
pub trait Error<'a>: nom::error::ParseError<&'a [u8]> {}
impl<'a, T: nom::error::ParseError<&'a [u8]>> Error<'a> for T {}

/// an alias for nom's `IResult`
pub type Result<'a, T, E> = nom::IResult<&'a [u8], T, E>;

/// syntax items which have an associated parser
pub trait Parse<'a, E: Error<'a>>: Sized {
    fn parse(x: &'a [u8]) -> Result<'a, Self, E>;
}

/// iterator returning `tptp_input`s from a byte slice
pub struct TPTPIterator<'a, E> {
    /// the current position of the iterator in the slice
    pub remaining: &'a [u8],
    _phantom: core::marker::PhantomData<E>,
}

impl<'a, E> TPTPIterator<'a, E> {
    pub fn new(remaining: &'a [u8]) -> Self {
        let _phantom = core::marker::PhantomData;
        Self {
            remaining,
            _phantom,
        }
    }
}

impl<'a, E: Error<'a>> Iterator for TPTPIterator<'a, E> {
    type Item = core::result::Result<top::TPTPInput<'a>, E>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match common::single_ignored::<E>(self.remaining) {
                Ok((remaining, ())) => {
                    self.remaining = remaining;
                }
                Err(nom::Err::Incomplete(_)) => {
                    return None;
                }
                Err(_) => {
                    break;
                }
            }
        }

        match top::TPTPInput::parse(self.remaining) {
            Ok((remaining, input)) => {
                self.remaining = remaining;
                Some(Ok(input))
            }
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                Some(Err(e))
            }
            Err(nom::Err::Incomplete(_)) => None,
        }
    }
}
