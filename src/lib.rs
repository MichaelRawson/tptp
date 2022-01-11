//! A collection of parsers for the FOF and CNF dialects of the [TPTP](http://tptp.org) format, expressed as functions from byte slices to syntax trees.
//!
//! Most users will want to use the `TPTPIterator` interface to stream `<TPTP_input>`s from TPTP problems, but it is also possible to use parsers individually for more exotic formats.
//!
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
//!
//! # Design Points
//! ## Parser Combinators
//! Parsers are built with the [nom](https://github.com/Geal/nom) parser combinator library, and this implementation detail is kept deliberately transparent.
//! If you need them, you can use nom's facilities such as error handling or streaming.
//!
//! ## Parsers as Functions
//! All parsers are functions from byte slices to `Result<T, E>` type, representing either parsed syntax `T` or a `nom` error `E`.
//! Parsers are zero-copy, which typically means `T` will borrow from the input slice.
//! The `Parse` trait allows one to write `T::parse`.
//!
//! ## Syntax Trees
//! Explicit, strongly-typed syntax trees are constructed during parsing.
//! After you have some parsed syntax, you can either manipulate it manually, or use the `Visitor` interface if you only need to handle certain parts of the syntax.
//!
//! ## Flexible Parsing
//! Individual parsers for each item of the [TPTP BNF](http://tptp.org/TPTP/SyntaxBNF.html) are available.
//! This can be useful to parse unusual or 'hybrid' formats such as [DeepMath](https://github.com/JUrban/deepmath).
//! Generally parsers and BNF are a one-to-one map, but for efficiency/sanity reasons items like `integer` are not split into `signed_integer` and `unsigned_integer`.
//!
//! Whitespace/comments are handled _inside_ invidual BNF items, but not _outside_, as the caller should handle it.
//! For example, parsing `fof ( 1 , axiom , /* comment */ $true ) .` as a `fof_annotated` is OK, but `␣fof(1,axiom,$true).␣` is an error.
//! Naturally, `TPTPIterator` handles whitespace and comments between `<TPTP_input>`s.
//!
//! ## Streaming
//! Parsers are streaming, so they will signal "incomplete" on EOF, rather than success or failure, until the outcome is known.
//! Most of the time this is obvious, but the behaviour can be surprising.
//! For example, parsing `foo$` as a `lower_word` succeeds, returning the trailing `$` to parse next (which will fail).
//! However, parsing `foo` is "incomplete" as there might be more input coming.
//!
//! ## `#![no_std]`
//! The crate is `#![no_std]`, but recursive syntax trees must allocate so the `alloc` crate is required.
//!
//! ## Serialisation
//! Support for [`serde`](https://serde.rs/) can be switched on with a feature flag as usual.
//! Structures can then be serialised, but not deseralised due to ownership issues.

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
/// the TFX dialect
pub mod tfx;
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

/// iterator returning `TPTP_input`s from a byte slice
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
