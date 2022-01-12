use alloc::boxed::Box;
use alloc::vec::Vec;
use derive_more::Display;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::combinator::{map, opt, value};
use nom::multi::separated_list1;
use nom::sequence::{delimited, pair, preceded, tuple};
#[cfg(feature = "serde")]
use serde::Serialize;

use crate::common::*;
use crate::utils::{fold_many0_once, GarbageFirstVec, Separated};
use crate::{Error, Parse, Result};

/// [`fof_arguments`](http://tptp.org/TPTP/SyntaxBNF.html#fof_arguments)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated(',', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Arguments<'a>(pub Vec<Term<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for Arguments<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            separated_list1(
                delimited(ignored, tag(","), ignored),
                Term::parse,
            ),
            Self,
        )(x)
    }
}

/// [`fof_system_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_system_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum SystemTerm<'a> {
    Constant(SystemConstant<'a>),
    #[display(fmt = "{}({})", _0, _1)]
    Function(SystemFunctor<'a>, Box<Arguments<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for SystemTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(SystemFunctor::parse, opt(preceded(ignored, parens))),
            |(f, args)| match args {
                None => Self::Constant(SystemConstant(f)),
                Some(args) => Self::Function(f, Box::new(args)),
            },
        )(x)
    }
}

/// [`fof_plain_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_plain_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PlainTerm<'a> {
    Constant(Constant<'a>),
    #[display(fmt = "{}({})", _0, _1)]
    Function(Functor<'a>, Box<Arguments<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for PlainTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(Functor::parse, opt(preceded(ignored, parens))),
            |(f, args)| match args {
                None => Self::Constant(Constant(f)),
                Some(args) => Self::Function(f, Box::new(args)),
            },
        )(x)
    }
}

/// [`fof_defined_plain_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_plain_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedPlainTerm<'a> {
    Constant(DefinedConstant<'a>),
    #[display(fmt = "{}({})", _0, _1)]
    Function(DefinedFunctor<'a>, Box<Arguments<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedPlainTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(DefinedFunctor::parse, opt(preceded(ignored, parens))),
            |(f, args)| match args {
                None => Self::Constant(DefinedConstant(f)),
                Some(args) => Self::Function(f, Box::new(args)),
            },
        )(x)
    }
}

/// [`fof_defined_atomic_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_atomic_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedAtomicTerm<'a>(pub DefinedPlainTerm<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedAtomicTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(DefinedPlainTerm::parse, Self)(x)
    }
}

/// [`fof_defined_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedTerm<'a> {
    Defined(crate::common::DefinedTerm<'a>),
    Atomic(DefinedAtomicTerm<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(crate::common::DefinedTerm::parse, Self::Defined),
            map(DefinedAtomicTerm::parse, Self::Atomic),
        ))(x)
    }
}

/// [`fof_function_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_function_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum FunctionTerm<'a> {
    Plain(PlainTerm<'a>),
    System(SystemTerm<'a>),
    Defined(DefinedTerm<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for FunctionTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(PlainTerm::parse, Self::Plain),
            map(SystemTerm::parse, Self::System),
            map(DefinedTerm::parse, Self::Defined),
        ))(x)
    }
}

/// [`fof_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Term<'a> {
    Function(Box<FunctionTerm<'a>>),
    Variable(Variable<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for Term<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(Variable::parse, Self::Variable),
            map(map(FunctionTerm::parse, Box::new), Self::Function),
        ))(x)
    }
}

/// [`fof_quantifier`](http://tptp.org/TPTP/SyntaxBNF.html#fof_quantifier)
#[derive(
    Clone, Copy, Display, Debug, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Quantifier {
    /// `!`
    #[display(fmt = "!")]
    Forall,
    /// `?`
    #[display(fmt = "?")]
    Exists,
}

impl<'a, E: Error<'a>> Parse<'a, E> for Quantifier {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((value(Self::Forall, tag("!")), value(Self::Exists, tag("?"))))(x)
    }
}

/// [`fof_system_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_system_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SystemAtomicFormula<'a>(pub SystemTerm<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for SystemAtomicFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(SystemTerm::parse, Self)(x)
    }
}

/// [`fof_plain_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_plain_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct PlainAtomicFormula<'a>(pub PlainTerm<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for PlainAtomicFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(PlainTerm::parse, Self)(x)
    }
}

pub(crate) struct DefinedInfixFormulaTail<'a>(
    pub(crate) DefinedInfixPred,
    pub(crate) Box<Term<'a>>,
);

impl<'a> DefinedInfixFormulaTail<'a> {
    pub(crate) fn finish(self, left: Term<'a>) -> DefinedInfixFormula {
        let left = Box::new(left);
        let op = self.0;
        let right = self.1;
        DefinedInfixFormula { left, op, right }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedInfixFormulaTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                DefinedInfixPred::parse,
                preceded(ignored, map(Term::parse, Box::new)),
            ),
            |(op, right)| Self(op, right),
        )(x)
    }
}

/// [`fof_defined_infix_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_infix_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}{}", left, op, right)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedInfixFormula<'a> {
    pub left: Box<Term<'a>>,
    pub op: DefinedInfixPred,
    pub right: Box<Term<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedInfixFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                Term::parse,
                preceded(ignored, DefinedInfixFormulaTail::parse),
            ),
            |(left, tail)| tail.finish(left),
        )(x)
    }
}

/// [`fof_defined_plain_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_plain_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedPlainFormula<'a>(pub DefinedPlainTerm<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedPlainFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(DefinedPlainTerm::parse, Self)(x)
    }
}

/// [`fof_defined_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedAtomicFormula<'a> {
    Plain(DefinedPlainFormula<'a>),
    Infix(DefinedInfixFormula<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedAtomicFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(DefinedInfixFormula::parse, Self::Infix),
            map(DefinedPlainFormula::parse, Self::Plain),
        ))(x)
    }
}

/// [`fof_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AtomicFormula<'a> {
    Plain(PlainAtomicFormula<'a>),
    Defined(DefinedAtomicFormula<'a>),
    System(SystemAtomicFormula<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for AtomicFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(
                pair(
                    PlainTerm::parse,
                    opt(preceded(ignored, DefinedInfixFormulaTail::parse)),
                ),
                |(left, tail)| match tail {
                    Some(tail) => {
                        let left = Box::new(FunctionTerm::Plain(left));
                        let left = Term::Function(left);
                        let infix = tail.finish(left);
                        let defined = DefinedAtomicFormula::Infix(infix);
                        Self::Defined(defined)
                    }
                    None => Self::Plain(PlainAtomicFormula(left)),
                },
            ),
            map(SystemAtomicFormula::parse, Self::System),
            map(DefinedAtomicFormula::parse, Self::Defined),
        ))(x)
    }
}

/// [`fof_variable_list`](http://tptp.org/TPTP/SyntaxBNF.html#fof_variable_list)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated(',', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VariableList<'a>(pub Vec<Variable<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for VariableList<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            separated_list1(
                delimited(ignored, tag(","), ignored),
                Variable::parse,
            ),
            Self,
        )(x)
    }
}

/// [`fof_quantified_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_quantified_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}[{}]:{}", quantifier, bound, formula)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct QuantifiedFormula<'a> {
    pub quantifier: Quantifier,
    pub bound: VariableList<'a>,
    pub formula: Box<UnitFormula<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for QuantifiedFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            tuple((
                Quantifier::parse,
                delimited(ignored, brackets, ignored),
                preceded(tag(":"), preceded(ignored, UnitFormula::parse)),
            )),
            |(quantifier, bound, formula)| Self {
                quantifier,
                bound,
                formula: Box::new(formula),
            },
        )(x)
    }
}

pub(crate) struct InfixUnaryTail<'a>(
    pub(crate) InfixInequality,
    pub(crate) Box<Term<'a>>,
);

impl<'a> InfixUnaryTail<'a> {
    pub(crate) fn finish(self, left: Term<'a>) -> InfixUnary<'a> {
        let left = Box::new(left);
        let op = self.0;
        let right = self.1;
        InfixUnary { left, op, right }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for InfixUnaryTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                InfixInequality::parse,
                preceded(ignored, map(Term::parse, Box::new)),
            ),
            |(op, right)| Self(op, right),
        )(x)
    }
}

/// [`fof_infix_unary`](http://tptp.org/TPTP/SyntaxBNF.html#fof_infix_unary)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}{}", left, op, right)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct InfixUnary<'a> {
    pub left: Box<Term<'a>>,
    pub op: InfixInequality,
    pub right: Box<Term<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for InfixUnary<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(Term::parse, preceded(ignored, InfixUnaryTail::parse)),
            |(left, tail)| tail.finish(left),
        )(x)
    }
}

/// [`fof_unary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_unary_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnaryFormula<'a> {
    #[display(fmt = "{}{}", _0, _1)]
    Unary(UnaryConnective, Box<UnitFormula<'a>>),
    InfixUnary(InfixUnary<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnaryFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(
                pair(
                    UnaryConnective::parse,
                    preceded(ignored, UnitFormula::parse),
                ),
                |(c, f)| Self::Unary(c, Box::new(f)),
            ),
            map(InfixUnary::parse, Self::InfixUnary),
        ))(x)
    }
}

/// [`fof_unitary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_unitary_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitaryFormula<'a> {
    Quantified(QuantifiedFormula<'a>),
    Atomic(Box<AtomicFormula<'a>>),
    #[display(fmt = "({})", _0)]
    Parenthesised(Box<LogicFormula<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitaryFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(QuantifiedFormula::parse, Self::Quantified),
            map(parens, |f| Self::Parenthesised(Box::new(f))),
            map(map(AtomicFormula::parse, Box::new), Self::Atomic),
        ))(x)
    }
}

enum UnitFormulaTail<'a> {
    Equal(DefinedInfixPred, Box<Term<'a>>),
    NotEqual(InfixInequality, Box<Term<'a>>),
}

impl<'a> UnitFormulaTail<'a> {
    fn finish(self, left: PlainTerm<'a>) -> UnitFormula<'a> {
        let left = FunctionTerm::Plain(left);
        let left = Term::Function(Box::new(left));
        let left = Box::new(left);
        match self {
            Self::Equal(op, right) => {
                let infix = DefinedInfixFormula { left, op, right };
                let defined = DefinedAtomicFormula::Infix(infix);
                let atomic = AtomicFormula::Defined(defined);
                let atomic = Box::new(atomic);
                let unitary = UnitaryFormula::Atomic(atomic);
                UnitFormula::Unitary(unitary)
            }
            Self::NotEqual(op, right) => {
                let infix = InfixUnary { left, op, right };
                let unary = UnaryFormula::InfixUnary(infix);
                UnitFormula::Unary(unary)
            }
        }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitFormulaTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(
                pair(
                    DefinedInfixPred::parse,
                    preceded(ignored, map(Term::parse, Box::new)),
                ),
                |(op, right)| Self::Equal(op, right),
            ),
            map(
                pair(
                    InfixInequality::parse,
                    preceded(ignored, map(Term::parse, Box::new)),
                ),
                |(op, right)| Self::NotEqual(op, right),
            ),
        ))(x)
    }
}

/// [`fof_unit_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_unit_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitFormula<'a> {
    Unitary(UnitaryFormula<'a>),
    Unary(UnaryFormula<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(
                pair(
                    PlainTerm::parse,
                    opt(preceded(ignored, UnitFormulaTail::parse)),
                ),
                |(left, tail)| match tail {
                    Some(tail) => tail.finish(left),
                    None => {
                        let plain = PlainAtomicFormula(left);
                        let atomic = AtomicFormula::Plain(plain);
                        let atomic = Box::new(atomic);
                        let unitary = UnitaryFormula::Atomic(atomic);
                        Self::Unitary(unitary)
                    }
                },
            ),
            map(UnaryFormula::parse, Self::Unary),
            map(UnitaryFormula::parse, Self::Unitary),
        ))(x)
    }
}

struct AssocTail<'a, const SEP: u8>(GarbageFirstVec<UnitFormula<'a>>);

impl<'a, const SEP: u8> AssocTail<'a, SEP> {
    fn finish(self, left: UnitFormula<'a>) -> Vec<UnitFormula<'a>> {
        self.0.finish(left)
    }
}

impl<'a, E: Error<'a>, const SEP: u8> Parse<'a, E> for AssocTail<'a, SEP> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        let (x, second) =
            preceded(tag(&[SEP]), preceded(ignored, UnitFormula::parse))(x)?;
        let mut result = GarbageFirstVec::default();
        result.push(second);
        fold_many0_once(
            preceded(
                delimited(ignored, tag(&[SEP]), ignored),
                UnitFormula::parse,
            ),
            result,
            |mut result, next| {
                result.push(next);
                result
            },
        )(x)
        .map(|(x, units)| (x, Self(units)))
    }
}

struct OrTail<'a>(AssocTail<'a, b'|'>);

impl<'a> OrTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> OrFormula<'a> {
        OrFormula(self.0.finish(left))
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for OrTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(AssocTail::parse, Self)(x)
    }
}

/// [`fof_or_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_or_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated('|', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct OrFormula<'a>(pub Vec<UnitFormula<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for OrFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(UnitFormula::parse, preceded(ignored, OrTail::parse)),
            |(first, tail)| tail.finish(first),
        )(x)
    }
}

struct AndTail<'a>(AssocTail<'a, b'&'>);

impl<'a> AndTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> AndFormula<'a> {
        AndFormula(self.0.finish(left))
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for AndTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(AssocTail::parse, Self)(x)
    }
}

/// [`fof_and_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_and_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated('&', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct AndFormula<'a>(pub Vec<UnitFormula<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for AndFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(UnitFormula::parse, preceded(ignored, AndTail::parse)),
            |(first, tail)| tail.finish(first),
        )(x)
    }
}

enum BinaryAssocTail<'a> {
    Or(OrTail<'a>),
    And(AndTail<'a>),
}

impl<'a> BinaryAssocTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> BinaryAssoc {
        match self {
            Self::Or(tail) => BinaryAssoc::Or(tail.finish(left)),
            Self::And(tail) => BinaryAssoc::And(tail.finish(left)),
        }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for BinaryAssocTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(OrTail::parse, BinaryAssocTail::Or),
            map(AndTail::parse, BinaryAssocTail::And),
        ))(x)
    }
}

/// [`fof_binary_assoc`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_assoc)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryAssoc<'a> {
    Or(OrFormula<'a>),
    And(AndFormula<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for BinaryAssoc<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                UnitFormula::parse,
                preceded(ignored, BinaryAssocTail::parse),
            ),
            |(first, tail)| tail.finish(first),
        )(x)
    }
}

struct BinaryNonassocTail<'a>(NonassocConnective, Box<UnitFormula<'a>>);

impl<'a> BinaryNonassocTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> BinaryNonassoc<'a> {
        let left = Box::new(left);
        let op = self.0;
        let right = self.1;
        BinaryNonassoc { left, op, right }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for BinaryNonassocTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                NonassocConnective::parse,
                preceded(ignored, map(UnitFormula::parse, Box::new)),
            ),
            |(connective, right)| Self(connective, right),
        )(x)
    }
}

/// [`fof_binary_nonassoc`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_nonassoc)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}{}", left, op, right)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct BinaryNonassoc<'a> {
    pub left: Box<UnitFormula<'a>>,
    pub op: NonassocConnective,
    pub right: Box<UnitFormula<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for BinaryNonassoc<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                UnitFormula::parse,
                preceded(ignored, BinaryNonassocTail::parse),
            ),
            |(left, tail)| tail.finish(left),
        )(x)
    }
}

enum BinaryFormulaTail<'a> {
    Assoc(BinaryAssocTail<'a>),
    Nonassoc(BinaryNonassocTail<'a>),
}

impl<'a> BinaryFormulaTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> BinaryFormula<'a> {
        match self {
            Self::Assoc(tail) => BinaryFormula::Assoc(tail.finish(left)),
            Self::Nonassoc(tail) => BinaryFormula::Nonassoc(tail.finish(left)),
        }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for BinaryFormulaTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(BinaryAssocTail::parse, Self::Assoc),
            map(BinaryNonassocTail::parse, Self::Nonassoc),
        ))(x)
    }
}

/// [`fof_binary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryFormula<'a> {
    Assoc(BinaryAssoc<'a>),
    Nonassoc(BinaryNonassoc<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for BinaryFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                UnitFormula::parse,
                preceded(ignored, BinaryFormulaTail::parse),
            ),
            |(left, tail)| tail.finish(left),
        )(x)
    }
}

/// [`fof_logic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_logic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum LogicFormula<'a> {
    Binary(BinaryFormula<'a>),
    Unary(UnaryFormula<'a>),
    Unitary(UnitaryFormula<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for LogicFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                UnitFormula::parse,
                opt(preceded(ignored, BinaryFormulaTail::parse)),
            ),
            |(left, tail)| match tail {
                Some(tail) => Self::Binary(tail.finish(left)),
                None => match left {
                    UnitFormula::Unary(u) => Self::Unary(u),
                    UnitFormula::Unitary(u) => Self::Unitary(u),
                },
            },
        )(x)
    }
}

/// [`fof_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Formula<'a>(pub LogicFormula<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for Formula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(LogicFormula::parse, Formula)(x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    #[test]
    fn test_fof_arguments() {
        check_size::<Arguments>();
        parse::<Arguments>(b"c\0");
        parse::<Arguments>(b"X\0");
        parse::<Arguments>(b"X, f ( X )\0");
    }

    #[test]
    fn test_fof_plain_term() {
        check_size::<PlainTerm>();
        parse::<PlainTerm>(b"c\0");
        parse::<PlainTerm>(b"f ( X )\0");
        parse::<PlainTerm>(b"f ( X, g ( Y ) )\0");
    }

    #[test]
    fn test_fof_system_term() {
        check_size::<SystemTerm>();
        parse::<SystemTerm>(b"$$c\0");
        parse::<SystemTerm>(b"$$f ( X )\0");
        parse::<SystemTerm>(b"$$f ( X, g ( Y ) )\0");
    }

    #[test]
    fn test_fof_defined_plain_term() {
        check_size::<DefinedPlainTerm>();
        parse::<DefinedPlainTerm>(b"$c\0");
        parse::<DefinedPlainTerm>(b"$f ( X )\0");
    }

    #[test]
    fn test_fof_defined_atomic_term() {
        check_size::<DefinedAtomicTerm>();
        parse::<DefinedAtomicTerm>(b"$defined_atomic_term\0");
    }

    #[test]
    fn test_fof_defined_term() {
        check_size::<super::DefinedTerm>();
        parse::<super::DefinedTerm>(b"$defined_term\0");
        parse::<super::DefinedTerm>(b"-123\0");
    }

    #[test]
    fn test_fof_function_term() {
        check_size::<FunctionTerm>();
        parse::<FunctionTerm>(b"f(X)\0");
        parse::<FunctionTerm>(b"$defined\0");
        parse::<FunctionTerm>(b"$$system\0");
    }

    #[test]
    fn test_fof_term() {
        check_size::<Term>();
        parse::<Term>(b"f(X)\0");
        parse::<Term>(b"X\0");
    }

    #[test]
    fn test_fof_plain_atomic_formula() {
        check_size::<PlainAtomicFormula>();
        parse::<PlainAtomicFormula>(b"f ( X, g ( Y ) )\0");
    }

    #[test]
    fn test_fof_defined_plain_formula() {
        check_size::<DefinedPlainFormula>();
        parse::<DefinedPlainFormula>(b"$defined_plain_formula\0");
    }

    #[test]
    fn test_fof_defined_infix_formula() {
        check_size::<DefinedInfixFormula>();
        parse::<DefinedInfixFormula>(b"f(X) = c\0");
    }

    #[test]
    fn test_fof_defined_atomic_formula() {
        check_size::<DefinedAtomicFormula>();
        parse::<DefinedAtomicFormula>(b"$true\0");
        parse::<DefinedAtomicFormula>(b"$false\0");
        parse::<DefinedAtomicFormula>(b"f(X) = c\0");
    }

    #[test]
    fn test_fof_system_atomic_formula() {
        check_size::<SystemAtomicFormula>();
        parse::<SystemAtomicFormula>(b"$$system\0");
    }

    #[test]
    fn test_fof_atomic_formula() {
        check_size::<AtomicFormula>();
        parse::<AtomicFormula>(b"$true\0");
        parse::<AtomicFormula>(b"f(X) = Y\0");
        parse::<AtomicFormula>(b"p(X)\0");
        parse::<AtomicFormula>(b"$$system\0");
    }

    #[test]
    fn test_fof_variable_list() {
        check_size::<VariableList>();
        parse::<VariableList>(b"X\0");
        parse::<VariableList>(b"X , Y\0");
        parse::<VariableList>(b"X , Y , Z\0");
    }

    #[test]
    fn test_fof_quantifier() {
        check_size::<Quantifier>();
        parse::<Quantifier>(b"!\0");
        parse::<Quantifier>(b"?\0");
    }

    #[test]
    fn test_fof_quantified_formula() {
        check_size::<QuantifiedFormula>();
        parse::<QuantifiedFormula>(b"! [ X ] : $true\0");
        parse::<QuantifiedFormula>(b"? [ X , Y , Z ] : $true\0");
    }

    #[test]
    fn test_fof_infix_unary() {
        check_size::<InfixUnary>();
        parse::<InfixUnary>(b"f(X) != c\0");
    }

    #[test]
    fn test_fof_unary_formula() {
        check_size::<UnaryFormula>();
        parse::<UnaryFormula>(b"~ $true\0");
        parse::<UnaryFormula>(b"f(X) != c\0");
    }

    #[test]
    fn test_fof_unitary_formula() {
        check_size::<UnitaryFormula>();
        parse::<UnitaryFormula>(b"( $true )\0");
        parse::<UnitaryFormula>(b"$true\0");
        parse::<UnitaryFormula>(b"![X]: $true\0");
    }

    #[test]
    fn test_fof_unit_formula() {
        check_size::<UnitFormula>();
        parse::<UnitFormula>(b"($true)\0");
        parse::<UnitFormula>(b"~$true\0");
    }

    #[test]
    fn test_fof_binary_nonassoc() {
        check_size::<BinaryNonassoc>();
        parse::<BinaryNonassoc>(b"p => q\0");
        parse::<BinaryNonassoc>(b"p ~| q\0");
    }

    #[test]
    fn test_fof_or_formula() {
        check_size::<OrFormula>();
        parse::<OrFormula>(b"p | q | r\0");
    }

    #[test]
    fn test_fof_and_formula() {
        check_size::<AndFormula>();
        parse::<AndFormula>(b"p & q\0");
    }

    #[test]
    fn test_fof_binary_assoc() {
        check_size::<BinaryAssoc>();
        parse::<BinaryAssoc>(b"p | q | r\0");
        parse::<BinaryAssoc>(b"p & q\0");
    }

    #[test]
    fn test_fof_binary_formula() {
        check_size::<BinaryFormula>();
        parse::<BinaryFormula>(b"p => q\0");
        parse::<BinaryFormula>(b"p | q | r\0");
    }

    #[test]
    fn test_fof_logic_formula() {
        check_size::<LogicFormula>();
        parse::<LogicFormula>(b"~p\0");
        parse::<LogicFormula>(b"p => q\0");
        parse::<LogicFormula>(b"p & q\0");
        parse::<LogicFormula>(b"p | q | r\0");
        parse::<LogicFormula>(b"p\0");
        parse::<LogicFormula>(b"~p => q\0");
    }

    #[test]
    fn test_fof_formula() {
        check_size::<Formula>();
        parse::<Formula>(b"p\0");
        parse::<Formula>(b"~~p\0");
        parse::<Formula>(b"(p)\0");
        parse::<Formula>(b"$true|$false\0");
        parse::<Formula>(b"(![X,Y,Z]:?[Q]:Q!=p(A))&p&(q=>r)\0");
    }
}
