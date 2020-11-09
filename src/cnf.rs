use alloc::boxed::Box;
use alloc::fmt;
use alloc::vec::Vec;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::combinator::{map, opt};
use nom::sequence::{delimited, pair, preceded, tuple};
#[cfg(feature = "serde")]
use serde::Serialize;

use crate::common::*;
use crate::fof;
use crate::utils::{fmt_list, separated_list1};
use crate::{Error, Parse, Result};

/// [`literal`](http://tptp.org/TPTP/SyntaxBNF.html#literal)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Literal<'a> {
    Atomic(fof::AtomicFormula<'a>),
    NegatedAtomic(fof::AtomicFormula<'a>),
    Infix(fof::InfixUnary<'a>),
}

impl<'a> fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Literal::*;
        match self {
            Atomic(a) => write!(f, "{}", a),
            NegatedAtomic(n) => write!(f, "~{}", n),
            Infix(i) => write!(f, "{}", i),
        }
    }
}

enum LiteralTail<'a> {
    Equal(DefinedInfixPred, Box<fof::Term<'a>>),
    NotEqual(InfixInequality, Box<fof::Term<'a>>),
}

fn literal_tail<'a, E: Error<'a>>(x: &'a [u8]) -> Result<LiteralTail<'a>, E> {
    preceded(
        ignored,
        alt((
            map(
                pair(
                    DefinedInfixPred::parse,
                    preceded(ignored, map(fof::Term::parse, Box::new)),
                ),
                |(op, right)| LiteralTail::Equal(op, right),
            ),
            map(
                pair(
                    InfixInequality::parse,
                    preceded(ignored, map(fof::Term::parse, Box::new)),
                ),
                |(op, right)| LiteralTail::NotEqual(op, right),
            ),
        )),
    )(x)
}

parser! {
    Literal,
    alt((
        map(
            preceded(pair(tag("~"), ignored), fof::AtomicFormula::parse),
            Self::NegatedAtomic,
        ),
        map(
            pair(fof::PlainTerm::parse, opt(literal_tail)),
            |(left, rest)| match rest {
                Some(rest) => {
                    let left = Box::new(fof::FunctionTerm::Plain(left));
                    let left = Box::new(fof::Term::Function(left));
                    match rest {
                        LiteralTail::Equal(op, right) => {
                            let infix =
                                fof::DefinedInfixFormula { left, op, right };
                            let defined =
                                fof::DefinedAtomicFormula::Infix(infix);
                            let atomic = fof::AtomicFormula::Defined(defined);
                            Self::Atomic(atomic)
                        }
                        LiteralTail::NotEqual(op, right) => {
                            let infix = fof::InfixUnary { left, op, right };
                            Self::Infix(infix)
                        }
                    }
                }
                None => {
                    let plain = fof::PlainAtomicFormula(left);
                    let atomic = fof::AtomicFormula::Plain(plain);
                    Self::Atomic(atomic)
                }
            },
        ),
        map(
            pair(map(fof::Term::parse, Box::new), literal_tail),
            |(left, rest)| match rest {
                LiteralTail::Equal(op, right) => {
                    let infix = fof::DefinedInfixFormula { left, op, right };
                    let defined = fof::DefinedAtomicFormula::Infix(infix);
                    let atomic = fof::AtomicFormula::Defined(defined);
                    Self::Atomic(atomic)
                }
                LiteralTail::NotEqual(op, right) => {
                    let infix = fof::InfixUnary { left, op, right };
                    Self::Infix(infix)
                }
            },
        ),
        map(fof::DefinedAtomicFormula::parse, |f| {
            Self::Atomic(fof::AtomicFormula::Defined(f))
        }),
    ))
}

/// [`disjunction`](http://tptp.org/TPTP/SyntaxBNF.html#disjunction)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Disjunction<'a>(pub Vec<Literal<'a>>);

impl<'a> fmt::Display for Disjunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, "|", &self.0)
    }
}

parser! {
    Disjunction,
    map(
        separated_list1(
            tuple((ignored, tag("|"), ignored)),
            Literal::parse,
        ),
        Self,
    )
}

/// [`cnf_formula`](http://tptp.org/TPTP/SyntaxBNF.html#cnf_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Formula<'a> {
    Disjunction(Disjunction<'a>),
    Parenthesised(Disjunction<'a>),
}

impl<'a> fmt::Display for Formula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Formula::*;
        match self {
            Disjunction(d) => write!(f, "{}", d),
            Parenthesised(d) => write!(f, "({})", d),
        }
    }
}

parser! {
    Formula,
    alt((
        map(
            delimited(
                pair(tag("("), ignored),
                Disjunction::parse,
                pair(ignored, tag(")")),
            ),
            Self::Parenthesised,
        ),
        map(Disjunction::parse, Self::Disjunction),
    ))
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
