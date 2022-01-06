use alloc::boxed::Box;
use alloc::vec::Vec;
use derive_more::Display;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::combinator::{cut, map, opt};
use nom::multi::separated_list1;
use nom::sequence::{delimited, pair, preceded, tuple};
#[cfg(feature = "serde")]
use serde::Serialize;

use crate::common::*;
use crate::fof;
use crate::utils::Separated;
use crate::{Error, Parse, Result};

enum LiteralTail<'a> {
    Equal(DefinedInfixPred, Box<fof::Term<'a>>),
    NotEqual(InfixInequality, Box<fof::Term<'a>>),
}

impl<'a> LiteralTail<'a> {
    fn finish(self, left: fof::Term<'a>) -> Literal<'a> {
        let left = Box::new(left);
        match self {
            Self::Equal(op, right) => {
                let infix = fof::DefinedInfixFormula { left, op, right };
                let defined = fof::DefinedAtomicFormula::Infix(infix);
                let atomic = fof::AtomicFormula::Defined(defined);
                Literal::Atomic(atomic)
            }
            Self::NotEqual(op, right) => {
                let infix = fof::InfixUnary { left, op, right };
                Literal::Infix(infix)
            }
        }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for LiteralTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        preceded(
            ignored,
            alt((
                map(
                    pair(
                        DefinedInfixPred::parse,
                        cut(preceded(
                            ignored,
                            map(fof::Term::parse, Box::new),
                        )),
                    ),
                    |(op, right)| LiteralTail::Equal(op, right),
                ),
                map(
                    pair(
                        InfixInequality::parse,
                        cut(preceded(
                            ignored,
                            map(fof::Term::parse, Box::new),
                        )),
                    ),
                    |(op, right)| LiteralTail::NotEqual(op, right),
                ),
            )),
        )(x)
    }
}

/// [`literal`](http://tptp.org/TPTP/SyntaxBNF.html#literal)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Literal<'a> {
    Atomic(fof::AtomicFormula<'a>),
    #[display(fmt = "~{}", _0)]
    NegatedAtomic(fof::AtomicFormula<'a>),
    Infix(fof::InfixUnary<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for Literal<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(
                preceded(
                    tag("~"),
                    cut(preceded(ignored, fof::AtomicFormula::parse)),
                ),
                Self::NegatedAtomic,
            ),
            map(
                pair(fof::PlainTerm::parse, opt(LiteralTail::parse)),
                |(left, tail)| match tail {
                    Some(tail) => {
                        let left = Box::new(fof::FunctionTerm::Plain(left));
                        let left = fof::Term::Function(left);
                        tail.finish(left)
                    }
                    None => {
                        let plain = fof::PlainAtomicFormula(left);
                        let atomic = fof::AtomicFormula::Plain(plain);
                        Self::Atomic(atomic)
                    }
                },
            ),
            map(
                pair(fof::Term::parse, LiteralTail::parse),
                |(left, tail)| tail.finish(left),
            ),
            map(fof::DefinedAtomicFormula::parse, |f| {
                Self::Atomic(fof::AtomicFormula::Defined(f))
            }),
        ))(x)
    }
}

/// [`disjunction`](http://tptp.org/TPTP/SyntaxBNF.html#disjunction)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated('|', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Disjunction<'a>(pub Vec<Literal<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for Disjunction<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            separated_list1(
                tuple((ignored, tag("|"), ignored)),
                Literal::parse,
            ),
            Self,
        )(x)
    }
}

/// [`cnf_formula`](http://tptp.org/TPTP/SyntaxBNF.html#cnf_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Formula<'a> {
    Disjunction(Disjunction<'a>),
    #[display(fmt = "({})", _0)]
    Parenthesised(Disjunction<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for Formula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            preceded(
                tag("("),
                cut(map(
                    delimited(
                        ignored,
                        Disjunction::parse,
                        pair(ignored, tag(")")),
                    ),
                    Self::Parenthesised,
                )),
            ),
            map(Disjunction::parse, Self::Disjunction),
        ))(x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    #[test]
    fn test_literal() {
        check_size::<Literal>();
        parse::<Literal>(b"p\0");
        parse::<Literal>(b"~ p\0");
        parse::<Literal>(b"f(X) = c\0");
    }

    #[test]
    fn test_disjunction() {
        check_size::<Disjunction>();
        parse::<Disjunction>(b"p\0");
        parse::<Disjunction>(b"p | ~q\0");
        parse::<Disjunction>(b"p | ~q | r\0");
    }

    #[test]
    fn test_cnf_formula() {
        check_size::<Formula>();
        parse::<Formula>(b"p\0");
        parse::<Formula>(b"( p )\0");
        parse::<Formula>(b"p | ~q\0");
    }
}
