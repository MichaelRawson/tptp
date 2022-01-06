use alloc::str;
use derive_more::Display;
use nom::branch::alt;
use nom::bytes::streaming::{
    escaped, tag, take_until, take_while, take_while1,
};
use nom::character::complete::multispace1;
use nom::character::streaming::{
    digit0, digit1, line_ending, not_line_ending, one_of,
};
use nom::combinator::{cut, map, opt, recognize, value};
use nom::multi::fold_many0;
use nom::sequence::{pair, preceded, terminated, tuple};
#[cfg(feature = "serde")]
use serde::Serialize;

use crate::{Error, Parse, Result};

fn to_str(bytes: &[u8]) -> &str {
    unsafe { str::from_utf8_unchecked(bytes) }
}

fn is_lower_alpha(c: u8) -> bool {
    (b'a'..=b'z').contains(&c)
}

fn is_upper_alpha(c: u8) -> bool {
    (b'A'..=b'Z').contains(&c)
}

fn is_alphanumeric(c: u8) -> bool {
    nom::character::is_alphanumeric(c) || (c == b'_')
}

fn is_visible(c: u8) -> bool {
    (b' '..=b'~').contains(&c)
}

fn is_sq_char(c: u8) -> bool {
    is_visible(c) && c != b'\'' && c != b'\\'
}

fn is_do_char(c: u8) -> bool {
    is_visible(c) && c != b'"' && c != b'\\'
}

/// one or more spaces, tabs, carriage returns or line feeds
pub fn whitespace<'a, E: Error<'a>>(x: &'a [u8]) -> Result<(), E> {
    value((), multispace1)(x)
}

/// `% a single-line comment`
pub fn comment_line<'a, E: Error<'a>>(x: &'a [u8]) -> Result<(), E> {
    preceded(
        tag("%"),
        cut(terminated(value((), not_line_ending), line_ending)),
    )(x)
}

/// `/* a comment block */`
pub fn comment_block<'a, E: Error<'a>>(x: &'a [u8]) -> Result<(), E> {
    preceded(
        tag("/*"),
        cut(terminated(value((), take_until("*/")), tag("*/"))),
    )(x)
}

/// `whitespace`, `comment_line`, or `comment_block`
pub fn single_ignored<'a, E: Error<'a>>(x: &'a [u8]) -> Result<(), E> {
    alt((whitespace, comment_line, comment_block))(x)
}

/// zero or more `whitespace`, `comment_line`, or `comment_block`
pub fn ignored<'a, E: Error<'a>>(x: &'a [u8]) -> Result<(), E> {
    fold_many0(single_ignored, || (), |_, _| ())(x)
}

/// one or more lowercase letters
pub fn lower_alpha1<'a, E: Error<'a>>(x: &'a [u8]) -> Result<&'a [u8], E> {
    take_while1(is_lower_alpha)(x)
}

/// one or more uppercase letters
pub fn upper_alpha1<'a, E: Error<'a>>(x: &'a [u8]) -> Result<&'a [u8], E> {
    take_while1(is_upper_alpha)(x)
}

/// one or more letters or digits
pub fn alphanumeric<'a, E: Error<'a>>(x: &'a [u8]) -> Result<&'a [u8], E> {
    take_while(is_alphanumeric)(x)
}

/// [`integer`](http://tptp.org/TPTP/SyntaxBNF.html#integer)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Integer<'a>(pub &'a str);

impl<'a, E: Error<'a>> Parse<'a, E> for Integer<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            recognize(preceded(
                opt(one_of("+-")),
                alt((tag("0"), preceded(one_of("123456789"), digit0))),
            )),
            |w| Self(to_str(w)),
        )(x)
    }
}

/// [`rational`](http://tptp.org/TPTP/SyntaxBNF.html#rational)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Rational<'a>(pub &'a str);

impl<'a, E: Error<'a>> Parse<'a, E> for Rational<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            recognize(tuple((
                Integer::parse,
                tag(b"/"),
                one_of("123456789"),
                digit0,
            ))),
            |w| Self(to_str(w)),
        )(x)
    }
}

/// [`real`](http://tptp.org/TPTP/SyntaxBNF.html#real)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Real<'a>(pub &'a str);

impl<'a, E: Error<'a>> Parse<'a, E> for Real<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        fn exp_integer<'a, E: Error<'a>>(x: &'a [u8]) -> Result<(), E> {
            value((), preceded(opt(one_of("+-")), digit1))(x)
        }

        map(
            recognize(tuple((
                Integer::parse,
                alt((
                    value((), pair(one_of("eE"), exp_integer)),
                    value(
                        (),
                        tuple((
                            tag("."),
                            digit1,
                            opt(pair(one_of("eE"), exp_integer)),
                        )),
                    ),
                )),
            ))),
            |w| Self(to_str(w)),
        )(x)
    }
}

/// [`lower_word`](http://tptp.org/TPTP/SyntaxBNF.html#lower_word)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LowerWord<'a>(pub &'a str);

impl<'a, E: Error<'a>> Parse<'a, E> for LowerWord<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(recognize(preceded(lower_alpha1, cut(alphanumeric))), |w| {
            Self(to_str(w))
        })(x)
    }
}

/// [`upper_word`](http://tptp.org/TPTP/SyntaxBNF.html#upper_word)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct UpperWord<'a>(pub &'a str);

impl<'a, E: Error<'a>> Parse<'a, E> for UpperWord<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(recognize(preceded(upper_alpha1, cut(alphanumeric))), |w| {
            Self(to_str(w))
        })(x)
    }
}

/// [`dollar_word`](http://tptp.org/TPTP/SyntaxBNF.html#dollar_word)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "${}", _0)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DollarWord<'a>(pub LowerWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for DollarWord<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(preceded(tag("$"), cut(LowerWord::parse)), Self)(x)
    }
}

/// [`dollar_dollar_word`](http://tptp.org/TPTP/SyntaxBNF.html#dollar_dollar_word)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "$${}", _0)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DollarDollarWord<'a>(pub LowerWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for DollarDollarWord<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(preceded(tag("$$"), cut(LowerWord::parse)), Self)(x)
    }
}

/// [`single_quoted`](http://tptp.org/TPTP/SyntaxBNF.html#single_quoted)
///
/// NB: the spec says that `'cat'` should be treated as a `lower_word` `cat`:
/// this transformation is not implemented here as it might be confusing
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "'{}'", _0)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SingleQuoted<'a>(pub &'a str);

impl<'a, E: Error<'a>> Parse<'a, E> for SingleQuoted<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            preceded(
                tag("'"),
                cut(terminated(
                    escaped(take_while1(is_sq_char), '\\', one_of("\\'")),
                    tag("'"),
                )),
            ),
            |w| Self(to_str(w)),
        )(x)
    }
}

/// [`distinct_object`](http://tptp.org/TPTP/SyntaxBNF.html#distinct_object)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "\"{}\"", _0)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DistinctObject<'a>(pub &'a str);

impl<'a, E: Error<'a>> Parse<'a, E> for DistinctObject<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            preceded(
                tag("\""),
                cut(terminated(
                    escaped(take_while1(is_do_char), '\\', one_of("\\\"")),
                    tag("\""),
                )),
            ),
            |w| Self(to_str(w)),
        )(x)
    }
}

/// [`atomic_system_word`](http://tptp.org/TPTP/SyntaxBNF.html#atomic_system_word)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct AtomicSystemWord<'a>(pub DollarDollarWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for AtomicSystemWord<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(DollarDollarWord::parse, Self)(x)
    }
}

/// [`system_functor`](http://tptp.org/TPTP/SyntaxBNF.html#system_functor)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SystemFunctor<'a>(pub AtomicSystemWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for SystemFunctor<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(AtomicSystemWord::parse, Self)(x)
    }
}

/// [`system_constant`](http://tptp.org/TPTP/SyntaxBNF.html#system_constant)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SystemConstant<'a>(pub SystemFunctor<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for SystemConstant<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(SystemFunctor::parse, Self)(x)
    }
}

/// [`number`](http://tptp.org/TPTP/SyntaxBNF.html#number)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Number<'a> {
    Integer(Integer<'a>),
    Rational(Rational<'a>),
    Real(Real<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for Number<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(Real::parse, Self::Real),
            map(Rational::parse, Self::Rational),
            map(Integer::parse, Self::Integer),
        ))(x)
    }
}

/// [`atomic_word`](http://tptp.org/TPTP/SyntaxBNF.html#atomic_word)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AtomicWord<'a> {
    Lower(LowerWord<'a>),
    SingleQuoted(SingleQuoted<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for AtomicWord<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(LowerWord::parse, Self::Lower),
            map(SingleQuoted::parse, Self::SingleQuoted),
        ))(x)
    }
}

/// [`name`](http://tptp.org/TPTP/SyntaxBNF.html#name)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Name<'a> {
    AtomicWord(AtomicWord<'a>),
    Integer(Integer<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for Name<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(AtomicWord::parse, Self::AtomicWord),
            map(Integer::parse, Self::Integer),
        ))(x)
    }
}

/// [`variable`](http://tptp.org/TPTP/SyntaxBNF.html#variable)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Variable<'a>(pub UpperWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for Variable<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(UpperWord::parse, Self)(x)
    }
}

/// [`functor`](http://tptp.org/TPTP/SyntaxBNF.html#functor)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Functor<'a>(pub AtomicWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for Functor<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(AtomicWord::parse, Self)(x)
    }
}

/// [`constant`](http://tptp.org/TPTP/SyntaxBNF.html#constant)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Constant<'a>(pub Functor<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for Constant<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(Functor::parse, Self)(x)
    }
}

/// [`atomic_defined_word`](http://tptp.org/TPTP/SyntaxBNF.html#atomic_defined_word)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct AtomicDefinedWord<'a>(pub DollarWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for AtomicDefinedWord<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(DollarWord::parse, Self)(x)
    }
}

/// [`defined_functor`](http://tptp.org/TPTP/SyntaxBNF.html#defined_functor)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedFunctor<'a>(pub AtomicDefinedWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedFunctor<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(AtomicDefinedWord::parse, Self)(x)
    }
}

/// [`defined_constant`](http://tptp.org/TPTP/SyntaxBNF.html#defined_constant)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedConstant<'a>(pub DefinedFunctor<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedConstant<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(DefinedFunctor::parse, Self)(x)
    }
}

/// [`defined_term`](http://tptp.org/TPTP/SyntaxBNF.html#defined_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedTerm<'a> {
    Number(Number<'a>),
    Distinct(DistinctObject<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(Number::parse, Self::Number),
            map(DistinctObject::parse, Self::Distinct),
        ))(x)
    }
}

/// [`unary_connective`](http://tptp.org/TPTP/SyntaxBNF.html#unary_connective)
#[derive(
    Clone, Copy, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[display(fmt = "~")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct UnaryConnective;

impl<'a, E: Error<'a>> Parse<'a, E> for UnaryConnective {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        value(Self, tag("~"))(x)
    }
}

/// [`infix_equality`](http://tptp.org/TPTP/SyntaxBNF.html#infix_equality)
#[derive(
    Clone, Copy, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[display(fmt = "=")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct InfixEquality;

impl<'a, E: Error<'a>> Parse<'a, E> for InfixEquality {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        value(Self, tag("="))(x)
    }
}

/// [`infix_inequality`](http://tptp.org/TPTP/SyntaxBNF.html#infix_inequality)
#[derive(
    Clone, Copy, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[display(fmt = "!=")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct InfixInequality;

impl<'a, E: Error<'a>> Parse<'a, E> for InfixInequality {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        value(Self, tag("!="))(x)
    }
}

/// [`nonassoc_connective`](http://tptp.org/TPTP/SyntaxBNF.html#nonassoc_connective)
#[derive(
    Clone, Copy, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum NonassocConnective {
    /// `=>`
    #[display(fmt = "=>")]
    LRImplies,
    /// `<=`
    #[display(fmt = "<=")]
    RLImplies,
    /// `<=>`
    #[display(fmt = "<=>")]
    Equivalent,
    /// `<~>`
    #[display(fmt = "<~>")]
    NotEquivalent,
    /// `~|`
    #[display(fmt = "~|")]
    NotOr,
    /// `~&`
    #[display(fmt = "~&")]
    NotAnd,
}

impl<'a, E: Error<'a>> Parse<'a, E> for NonassocConnective {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            value(Self::LRImplies, tag("=>")),
            value(Self::Equivalent, tag("<=>")),
            value(Self::RLImplies, tag("<=")),
            value(Self::NotEquivalent, tag("<~>")),
            value(Self::NotAnd, tag("~&")),
            value(Self::NotOr, tag("~|")),
        ))(x)
    }
}

/// [`assoc_connective`](http://tptp.org/TPTP/SyntaxBNF.html#assoc_connective)
#[derive(
    Clone, Copy, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AssocConnective {
    /// `&`
    #[display(fmt = "&")]
    And,
    /// `|`
    #[display(fmt = "|")]
    Or,
}

impl<'a, E: Error<'a>> Parse<'a, E> for AssocConnective {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((value(Self::And, tag("&")), value(Self::Or, tag("|"))))(x)
    }
}

/// [`defined_infix_pred`](http://tptp.org/TPTP/SyntaxBNF.html#defined_infix_pred)
#[derive(
    Clone, Copy, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedInfixPred(pub InfixEquality);

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedInfixPred {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(InfixEquality::parse, DefinedInfixPred)(x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    #[test]
    fn test_whitespace() {
        parse_unit(whitespace, b" \t\n\0");
    }

    #[test]
    fn test_comment_line() {
        parse_unit(comment_line, b"% a comment\n\0");
    }

    #[test]
    fn test_comment_block() {
        parse_unit(comment_block, b"/* a\n block * / comment\n*/\0");
    }

    #[test]
    fn test_ignored() {
        parse_unit(ignored, b"\0");
        parse_unit(ignored, b"   %test\n  /* test\ntest */  \0");
    }

    #[test]
    fn test_lower_word() {
        check_size::<LowerWord>();
        parse::<LowerWord>(b"x\0");
        parse::<LowerWord>(b"aA123\0");
    }

    #[test]
    fn test_upper_word() {
        check_size::<UpperWord>();
        parse::<UpperWord>(b"X\0");
        parse::<UpperWord>(b"Aa123\0");
    }

    #[test]
    fn test_single_quoted() {
        check_size::<SingleQuoted>();
        parse::<SingleQuoted>(b"'single quoted'\0");
        parse::<SingleQuoted>(b"'\\'\\\\'\0");
    }

    #[test]
    fn test_dollar_word() {
        check_size::<DollarWord>();
        parse::<DollarWord>(b"$dollar\0");
    }

    #[test]
    fn test_dollar_dollar_word() {
        check_size::<DollarDollarWord>();
        parse::<DollarDollarWord>(b"$$dollar\0");
    }

    #[test]
    fn test_distinct_object() {
        check_size::<DistinctObject>();
        parse::<DistinctObject>(b"\"distinct object\"\0");
        parse::<DistinctObject>(b"\"\\\"\\\\\"\0");
    }

    #[test]
    fn test_atomic_word() {
        check_size::<AtomicWord>();
        parse::<AtomicWord>(b"x\0");
        parse::<AtomicWord>(b"'single quoted'\0");
    }

    #[test]
    fn test_integer() {
        check_size::<Integer>();
        parse::<Integer>(b"0\0");
        parse::<Integer>(b"123\0");
        parse::<Integer>(b"-123\0");
    }

    #[test]
    fn test_rational() {
        check_size::<Rational>();
        parse::<Rational>(b"0/1\0");
        parse::<Rational>(b"123/456\0");
        parse::<Rational>(b"-123/456\0");
    }

    #[test]
    fn test_real() {
        check_size::<Real>();
        parse::<Real>(b"0.0\0");
        parse::<Real>(b"1E0\0");
        parse::<Real>(b"-1.23E-456\0");
        parse::<Real>(b"1e-06\0");
    }

    #[test]
    fn test_number() {
        check_size::<Number>();
        parse::<Number>(b"-123\0");
        parse::<Number>(b"-123/456\0");
        parse::<Number>(b"-1.23E-456\0");
    }

    #[test]
    fn test_name() {
        check_size::<Name>();
        parse::<Name>(b"lower_word2\0");
        parse::<Name>(b"'single quoted'\0");
        parse::<Name>(b"123\0");
    }

    #[test]
    fn test_variable() {
        check_size::<Variable>();
        parse::<Variable>(b"X\0");
    }

    #[test]
    fn test_atomic_system_word() {
        check_size::<AtomicSystemWord>();
        parse::<AtomicSystemWord>(b"$$atomic\0");
    }

    #[test]
    fn test_atomic_defined_word() {
        check_size::<AtomicDefinedWord>();
        parse::<AtomicDefinedWord>(b"$atomic\0");
    }

    #[test]
    fn test_system_functor() {
        check_size::<SystemFunctor>();
        parse::<SystemFunctor>(b"$$system_functor\0");
    }

    #[test]
    fn test_system_constant() {
        check_size::<SystemConstant>();
        parse::<SystemConstant>(b"$$system_constant\0");
    }

    #[test]
    fn test_defined_functor() {
        check_size::<DefinedFunctor>();
        parse::<DefinedFunctor>(b"$defined_functor\0");
    }

    #[test]
    fn test_defined_constant() {
        check_size::<DefinedConstant>();
        parse::<DefinedConstant>(b"$defined_constant\0");
    }

    #[test]
    fn test_defined_term() {
        check_size::<DefinedTerm>();
        parse::<DefinedTerm>(b"-123\0");
        parse::<DefinedTerm>(b"\"distinct object\"\0");
    }

    #[test]
    fn test_functor() {
        check_size::<Functor>();
        parse::<Functor>(b"functor\0");
    }

    #[test]
    fn test_constant() {
        check_size::<Constant>();
        parse::<Constant>(b"constant\0");
    }

    #[test]
    fn test_infix_equality() {
        check_size::<InfixEquality>();
        parse::<InfixEquality>(b"=\0");
    }

    #[test]
    fn test_infix_inequality() {
        check_size::<InfixInequality>();
        parse::<InfixInequality>(b"!=\0");
    }

    #[test]
    fn test_unary_connective() {
        check_size::<UnaryConnective>();
        parse::<UnaryConnective>(b"~\0");
    }

    #[test]
    fn test_nonassoc_connective() {
        check_size::<NonassocConnective>();
        parse::<NonassocConnective>(b"<=\0");
        parse::<NonassocConnective>(b"<=>\0");
        parse::<NonassocConnective>(b"=>\0");
        parse::<NonassocConnective>(b"<~>\0");
        parse::<NonassocConnective>(b"~&\0");
        parse::<NonassocConnective>(b"~|\0");
    }

    #[test]
    fn test_assoc_connective() {
        check_size::<AssocConnective>();
        parse::<AssocConnective>(b"&\0");
        parse::<AssocConnective>(b"|\0");
    }

    #[test]
    fn test_defined_infix_pred() {
        check_size::<DefinedInfixPred>();
        parse::<DefinedInfixPred>(b"=\0");
    }
}
