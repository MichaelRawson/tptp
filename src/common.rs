use alloc::fmt;
use alloc::str;
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
    c >= b'a' && c <= b'z'
}

fn is_upper_alpha(c: u8) -> bool {
    c >= b'A' && c <= b'Z'
}

fn is_alphanumeric(c: u8) -> bool {
    (c >= b'a' && c <= b'z')
        || (c >= b'A' && c <= b'Z')
        || (c >= b'0' && c <= b'9')
        || (c == b'_')
}

fn is_visible(c: u8) -> bool {
    c >= b' ' && c <= b'~'
}

fn is_sq_char(c: u8) -> bool {
    is_visible(c) && c != b'\'' && c != b'\\'
}

fn is_do_char(c: u8) -> bool {
    is_visible(c) && c != b'"' && c != b'\\'
}

unit_parser! {
    /// one or more spaces, tabs, carriage returns or line feeds
    whitespace,
    value((), multispace1)
}

unit_parser! {
    /// `% a single-line comment`
    comment_line,
    preceded(
        tag("%"),
        cut(terminated(value((), not_line_ending), line_ending)),
    )
}

unit_parser! {
    /// `/* a comment block */`
    comment_block,
    preceded(
        tag("/*"),
        cut(terminated(value((), take_until("*/")), tag("*/"))),
    )
}

unit_parser! {
    /// `whitespace`, `comment_line`, or `comment_block`
    #[inline(always)]
    single_ignored,
    alt((whitespace, comment_line, comment_block))
}

unit_parser! {
    /// zero or more `whitespace`, `comment_line`, or `comment_block`
    #[inline(always)]
    ignored,
    fold_many0(single_ignored, (), |_, _| ())
}

slice_parser! {
    /// one or more lowercase letters
    #[inline(always)]
    lower_alpha1,
    take_while1(is_lower_alpha)
}

slice_parser! {
    /// one or more uppercase letters
    #[inline(always)]
    upper_alpha1,
    take_while1(is_upper_alpha)
}

slice_parser! {
    /// one or more letters or digits
    #[inline(always)]
    alphanumeric,
    take_while(is_alphanumeric)
}

/// [`integer`](http://tptp.org/TPTP/SyntaxBNF.html#integer)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Integer<'a>(pub &'a str);
impl_unit_anon_display! {Integer}

parser! {
    Integer,
    map(
        recognize(preceded(
            opt(one_of("+-")),
            alt((tag("0"), preceded(one_of("123456789"), digit0))),
        )),
        |w| Self(to_str(w)),
    )
}

/// [`rational`](http://tptp.org/TPTP/SyntaxBNF.html#rational)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Rational<'a>(pub &'a str);
impl_unit_anon_display! {Rational}

parser! {
    Rational,
    map(
        recognize(tuple((
            Integer::parse,
            tag(b"/"),
            one_of("123456789"),
            digit0
        ))),
        |w| Self(to_str(w)),
    )
}

/// [`real`](http://tptp.org/TPTP/SyntaxBNF.html#real)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Real<'a>(pub &'a str);
impl_unit_anon_display! {Real}

fn exp_integer<'a, E: Error<'a>>(x: &'a [u8]) -> Result<(), E> {
    value((), preceded(opt(one_of("+-")), digit1))(x)
}

parser! {
    Real,
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
    )
}

/// [`lower_word`](http://tptp.org/TPTP/SyntaxBNF.html#lower_word)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LowerWord<'a>(pub &'a str);
impl_unit_anon_display! {LowerWord}

parser! {
    #[inline(always)]
    LowerWord,
    map(recognize(preceded(lower_alpha1, cut(alphanumeric))), |w| {
        Self(to_str(w))
    })
}

/// [`upper_word`](http://tptp.org/TPTP/SyntaxBNF.html#upper_word)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct UpperWord<'a>(pub &'a str);
impl_unit_anon_display! {UpperWord}

parser! {
    #[inline(always)]
    UpperWord,
    map(recognize(preceded(upper_alpha1, cut(alphanumeric))), |w| {
        Self(to_str(w))
    })
}

/// [`dollar_word`](http://tptp.org/TPTP/SyntaxBNF.html#dollar_word)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DollarWord<'a>(pub LowerWord<'a>);

impl<'a> fmt::Display for DollarWord<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

parser! {
    DollarWord,
    map(preceded(tag("$"), cut(LowerWord::parse)), Self)
}

/// [`dollar_dollar_word`](http://tptp.org/TPTP/SyntaxBNF.html#dollar_dollar_word)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DollarDollarWord<'a>(pub LowerWord<'a>);

impl<'a> fmt::Display for DollarDollarWord<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "$${}", self.0)
    }
}

parser! {
    DollarDollarWord,
    map(preceded(tag("$$"), cut(LowerWord::parse)), Self)
}

/// [`single_quoted`](http://tptp.org/TPTP/SyntaxBNF.html#single_quoted)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SingleQuoted<'a>(pub &'a str);

impl<'a> fmt::Display for SingleQuoted<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

parser! {
    SingleQuoted,
    map(
        preceded(
            tag("'"),
            cut(terminated(
                escaped(take_while1(is_sq_char), '\\', one_of("\\'")),
                tag("'"),
            )),
        ),
        |w| Self(to_str(w)),
    )
}

/// [`distinct_object`](http://tptp.org/TPTP/SyntaxBNF.html#distinct_object)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DistinctObject<'a>(pub &'a str);

impl<'a> fmt::Display for DistinctObject<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

parser! {
    DistinctObject,
    map(
        preceded(
            tag("\""),
            cut(terminated(
                escaped(take_while1(is_do_char), '\\', one_of("\\\"")),
                tag("\""),
            )),
        ),
        |w| Self(to_str(w)),
    )
}

/// [`atomic_system_word`](http://tptp.org/TPTP/SyntaxBNF.html#atomic_system_word)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct AtomicSystemWord<'a>(pub DollarDollarWord<'a>);
impl_unit_anon_display! {AtomicSystemWord}

parser! {
    AtomicSystemWord,
    map(DollarDollarWord::parse, Self)
}

/// [`system_functor`](http://tptp.org/TPTP/SyntaxBNF.html#system_functor)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SystemFunctor<'a>(pub AtomicSystemWord<'a>);
impl_unit_anon_display! {SystemFunctor}

parser! {
    SystemFunctor,
    map(AtomicSystemWord::parse, Self)
}

/// [`system_constant`](http://tptp.org/TPTP/SyntaxBNF.html#system_constant)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SystemConstant<'a>(pub SystemFunctor<'a>);
impl_unit_anon_display! {SystemConstant}

parser! {
    SystemConstant,
    map(SystemFunctor::parse, Self)
}

/// [`number`](http://tptp.org/TPTP/SyntaxBNF.html#number)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Number<'a> {
    Integer(Integer<'a>),
    Rational(Rational<'a>),
    Real(Real<'a>),
}
impl_enum_anon_display! {Number, Integer, Rational, Real}

parser! {
    Number,
    alt((
        map(Real::parse, Self::Real),
        map(Rational::parse, Self::Rational),
        map(Integer::parse, Self::Integer),
    ))
}

/// [`atomic_word`](http://tptp.org/TPTP/SyntaxBNF.html#atomic_word)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AtomicWord<'a> {
    Lower(LowerWord<'a>),
    SingleQuoted(SingleQuoted<'a>),
}
impl_enum_anon_display! {AtomicWord, Lower, SingleQuoted}

parser! {
    #[inline(always)]
    AtomicWord,
    alt((
        map(LowerWord::parse, Self::Lower),
        map(SingleQuoted::parse, Self::SingleQuoted),
    ))
}

/// [`name`](http://tptp.org/TPTP/SyntaxBNF.html#name)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Name<'a> {
    AtomicWord(AtomicWord<'a>),
    Integer(Integer<'a>),
}
impl_enum_anon_display! {Name, AtomicWord, Integer}

parser! {
    #[inline(always)]
    Name,
    alt((
        map(AtomicWord::parse, Self::AtomicWord),
        map(Integer::parse, Self::Integer),
    ))
}

/// [`variable`](http://tptp.org/TPTP/SyntaxBNF.html#variable)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Variable<'a>(pub UpperWord<'a>);
impl_unit_anon_display! {Variable}

parser! {
    Variable,
    map(UpperWord::parse, Self)
}

/// [`functor`](http://tptp.org/TPTP/SyntaxBNF.html#functor)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Functor<'a>(pub AtomicWord<'a>);
impl_unit_anon_display! {Functor}

parser! {
    Functor,
    map(AtomicWord::parse, Self)
}

/// [`constant`](http://tptp.org/TPTP/SyntaxBNF.html#constant)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Constant<'a>(pub Functor<'a>);
impl_unit_anon_display! {Constant}

parser! {
    Constant,
    map(Functor::parse, Self)
}

/// [`atomic_defined_word`](http://tptp.org/TPTP/SyntaxBNF.html#atomic_defined_word)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct AtomicDefinedWord<'a>(pub DollarWord<'a>);
impl_unit_anon_display! {AtomicDefinedWord}

parser! {
    AtomicDefinedWord,
    map(DollarWord::parse, Self)
}

/// [`defined_functor`](http://tptp.org/TPTP/SyntaxBNF.html#defined_functor)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedFunctor<'a>(pub AtomicDefinedWord<'a>);
impl_unit_anon_display! {DefinedFunctor}

parser! {
    DefinedFunctor,
    map(AtomicDefinedWord::parse, Self)
}

/// [`defined_constant`](http://tptp.org/TPTP/SyntaxBNF.html#defined_constant)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedConstant<'a>(pub DefinedFunctor<'a>);
impl_unit_anon_display! {DefinedConstant}

parser! {
    DefinedConstant,
    map(DefinedFunctor::parse, Self)
}

/// [`defined_term`](http://tptp.org/TPTP/SyntaxBNF.html#defined_term)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedTerm<'a> {
    Number(Number<'a>),
    Distinct(DistinctObject<'a>),
}
impl_enum_anon_display! {DefinedTerm, Number, Distinct}

parser! {
    DefinedTerm,
    alt((
        map(Number::parse, Self::Number),
        map(DistinctObject::parse, Self::Distinct),
    ))
}

/// [`unary_connective`](http://tptp.org/TPTP/SyntaxBNF.html#unary_connective)
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct UnaryConnective;

impl fmt::Display for UnaryConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "~")
    }
}

parser_no_lifetime! {
    UnaryConnective,
    value(Self, tag("~"))
}

/// [`infix_equality`](http://tptp.org/TPTP/SyntaxBNF.html#infix_equality)
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct InfixEquality;

impl fmt::Display for InfixEquality {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "=")
    }
}

parser_no_lifetime! {
    InfixEquality,
    value(Self, tag("="))
}

/// [`infix_inequality`](http://tptp.org/TPTP/SyntaxBNF.html#infix_inequality)
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct InfixInequality;

impl fmt::Display for InfixInequality {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "!=")
    }
}

parser_no_lifetime! {
    InfixInequality,
    value(Self, tag("!="))
}

/// [`nonassoc_connective`](http://tptp.org/TPTP/SyntaxBNF.html#nonassoc_connective)
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum NonassocConnective {
    /// `=>`
    LRImplies,
    /// `<=`
    RLImplies,
    /// `<=>`
    Equivalent,
    /// `<~>`
    NotEquivalent,
    /// `~|`
    NotOr,
    /// `~&`
    NotAnd,
}

impl fmt::Display for NonassocConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::NonassocConnective::*;
        match self {
            LRImplies => write!(f, "=>"),
            RLImplies => write!(f, "<="),
            Equivalent => write!(f, "<=>"),
            NotEquivalent => write!(f, "<~>"),
            NotOr => write!(f, "~|"),
            NotAnd => write!(f, "~&"),
        }
    }
}

parser_no_lifetime! {
    NonassocConnective,
    alt((
        value(Self::LRImplies, tag("=>")),
        value(Self::Equivalent, tag("<=>")),
        value(Self::RLImplies, tag("<=")),
        value(Self::NotEquivalent, tag("<~>")),
        value(Self::NotAnd, tag("~&")),
        value(Self::NotOr, tag("~|")),
    ))
}

/// [`assoc_connective`](http://tptp.org/TPTP/SyntaxBNF.html#assoc_connective)
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AssocConnective {
    /// `&`
    And,
    /// `|`
    Or,
}

impl fmt::Display for AssocConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AssocConnective::*;
        match self {
            And => write!(f, "&"),
            Or => write!(f, "|"),
        }
    }
}

parser_no_lifetime! {
    AssocConnective,
    alt((
        value(Self::And, tag("&")),
        value(Self::Or, tag("|")),
    ))
}

/// [`defined_infix_pred`](http://tptp.org/TPTP/SyntaxBNF.html#defined_infix_pred)
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedInfixPred(pub InfixEquality);
impl_unit_display! {DefinedInfixPred}

parser_no_lifetime! {
    DefinedInfixPred,
    map(InfixEquality::parse, DefinedInfixPred)
}
