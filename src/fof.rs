use alloc::boxed::Box;
use alloc::vec::Vec;
use derive_more::Display;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::combinator::{map, opt, value};
use nom::multi::separated_list1;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
#[cfg(feature = "serde")]
use serde::Serialize;

use crate::common;
use crate::utils::{fold_many0_once, GarbageFirstVec, Separated};
use crate::{Error, Parse, Result};

/// [`fof_arguments`](http://tptp.org/TPTP/SyntaxBNF.html#fof_arguments)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "({})", "Separated(',', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Arguments<'a>(pub Vec<Term<'a>>);

parser! {
    Arguments,
    map(
        delimited(
            tag("("),
            separated_list1(
                tag(","),
                delimited(common::ignored, Term::parse, common::ignored),
            ),
            tag(")"),
        ),
        Self,
    )
}

/// [`fof_system_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_system_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum SystemTerm<'a> {
    Constant(common::SystemConstant<'a>),
    #[display(fmt = "{}{}", _0, _1)]
    Function(common::SystemFunctor<'a>, Box<Arguments<'a>>),
}

parser! {
    SystemTerm,
    map(
        pair(
            common::SystemFunctor::parse,
            opt(preceded(common::ignored, Arguments::parse)),
        ),
        |(f, args)| match args {
            None => SystemTerm::Constant(common::SystemConstant(f)),
            Some(args) => SystemTerm::Function(f, Box::new(args)),
        },
    )
}

/// [`fof_plain_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_plain_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PlainTerm<'a> {
    Constant(common::Constant<'a>),
    #[display(fmt = "{}{}", _0, _1)]
    Function(common::Functor<'a>, Box<Arguments<'a>>),
}

parser! {
    PlainTerm,
    map(
        pair(
            common::Functor::parse,
            opt(preceded(common::ignored, Arguments::parse)),
        ),
        |(f, args)| match args {
            None => Self::Constant(common::Constant(f)),
            Some(args) => Self::Function(f, Box::new(args)),
        },
    )
}

/// [`fof_defined_plain_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_plain_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedPlainTerm<'a> {
    Constant(common::DefinedConstant<'a>),
    #[display(fmt = "{}{}", _0, _1)]
    Function(common::DefinedFunctor<'a>, Box<Arguments<'a>>),
}

parser! {
    DefinedPlainTerm,
    map(
        pair(
            common::DefinedFunctor::parse,
            opt(preceded(common::ignored, Arguments::parse)),
        ),
        |(f, args)| match args {
            None => Self::Constant(common::DefinedConstant(f)),
            Some(args) => Self::Function(f, Box::new(args)),
        },
    )
}

/// [`fof_defined_atomic_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_atomic_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedAtomicTerm<'a>(pub DefinedPlainTerm<'a>);

parser! {
    DefinedAtomicTerm,
    map(DefinedPlainTerm::parse, Self)
}

/// [`fof_defined_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedTerm<'a> {
    Defined(common::DefinedTerm<'a>),
    Atomic(DefinedAtomicTerm<'a>),
}

parser! {
    DefinedTerm,
    alt((
        map(common::DefinedTerm::parse, Self::Defined),
        map(DefinedAtomicTerm::parse, Self::Atomic),
    ))
}

/// [`fof_function_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_function_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum FunctionTerm<'a> {
    Plain(PlainTerm<'a>),
    System(SystemTerm<'a>),
    Defined(DefinedTerm<'a>),
}

parser! {
    FunctionTerm,
    alt((
        map(PlainTerm::parse, Self::Plain),
        map(SystemTerm::parse, Self::System),
        map(DefinedTerm::parse, Self::Defined),
    ))
}

/// [`fof_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Term<'a> {
    Function(Box<FunctionTerm<'a>>),
    Variable(common::Variable<'a>),
}

parser! {
    Term,
    alt((
        map(common::Variable::parse, Self::Variable),
        map(map(FunctionTerm::parse, Box::new), Self::Function),
    ))
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

parser_no_lifetime! {
    Quantifier,
    alt((
        value(Self::Forall, tag("!")),
        value(Self::Exists, tag("?")),
    ))
}

/// [`fof_system_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_system_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SystemAtomicFormula<'a>(pub SystemTerm<'a>);

parser! {
    SystemAtomicFormula,
    map(SystemTerm::parse, Self)
}

/// [`fof_plain_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_plain_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct PlainAtomicFormula<'a>(pub PlainTerm<'a>);

parser! {
    PlainAtomicFormula,
    map(PlainTerm::parse, Self)
}

struct DefinedInfixFormulaTail<'a>(common::DefinedInfixPred, Box<Term<'a>>);

impl<'a> DefinedInfixFormulaTail<'a> {
    fn finish(self, left: Box<Term<'a>>) -> DefinedInfixFormula {
        let op = self.0;
        let right = self.1;
        DefinedInfixFormula { left, op, right }
    }
}

parser! {
    DefinedInfixFormulaTail,
    map(
        pair(
            preceded(common::ignored, common::DefinedInfixPred::parse),
            preceded(common::ignored, map(Term::parse, Box::new)),
        ),
        |(op, right)| Self(op, right)
    )
}

/// [`fof_defined_infix_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_infix_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}{}", left, op, right)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedInfixFormula<'a> {
    pub left: Box<Term<'a>>,
    pub op: common::DefinedInfixPred,
    pub right: Box<Term<'a>>,
}

parser! {
    DefinedInfixFormula,
    map(
        pair(map(Term::parse, Box::new), DefinedInfixFormulaTail::parse),
        |(left, tail)| tail.finish(left)
    )
}

/// [`fof_defined_plain_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_plain_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedPlainFormula<'a>(pub DefinedPlainTerm<'a>);

parser! {
    DefinedPlainFormula,
    map(DefinedPlainTerm::parse, Self)
}

/// [`fof_defined_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedAtomicFormula<'a> {
    Plain(DefinedPlainFormula<'a>),
    Infix(DefinedInfixFormula<'a>),
}

parser! {
    DefinedAtomicFormula,
    alt((
        map(DefinedInfixFormula::parse, Self::Infix),
        map(DefinedPlainFormula::parse, Self::Plain),
    ))
}

/// [`fof_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AtomicFormula<'a> {
    Plain(PlainAtomicFormula<'a>),
    Defined(DefinedAtomicFormula<'a>),
    System(SystemAtomicFormula<'a>),
}

parser! {
    AtomicFormula,
    alt((
        map(
            pair(PlainTerm::parse, opt(DefinedInfixFormulaTail::parse)),
            |(left, tail)| match tail {
                Some(tail) => {
                    let left = Box::new(FunctionTerm::Plain(left));
                    let left = Box::new(Term::Function(left));
                    let infix = tail.finish(left);
                    let defined = DefinedAtomicFormula::Infix(infix);
                    Self::Defined(defined)
                }
                None => Self::Plain(PlainAtomicFormula(left)),
            },
        ),
        map(SystemAtomicFormula::parse, Self::System),
        map(DefinedAtomicFormula::parse, Self::Defined),
    ))
}

/// [`fof_variable_list`](http://tptp.org/TPTP/SyntaxBNF.html#fof_variable_list)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated(',', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VariableList<'a>(pub Vec<common::Variable<'a>>);

parser! {
    VariableList,
    map(
        separated_list1(
            tuple((common::ignored, tag(","), common::ignored)),
            common::Variable::parse,
        ),
        Self,
    )
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

parser! {
    QuantifiedFormula,
    map(
        tuple((
            Quantifier::parse,
            delimited(
                tuple((common::ignored, tag("["), common::ignored)),
                VariableList::parse,
                tuple((
                    common::ignored,
                    tag("]"),
                    common::ignored,
                    tag(":"),
                    common::ignored,
                )),
            ),
            map(UnitFormula::parse, Box::new),
        )),
        |(quantifier, bound, formula)| Self {
            quantifier,
            bound,
            formula,
        },
    )
}

struct InfixUnaryTail<'a>(common::InfixInequality, Box<Term<'a>>);

impl<'a> InfixUnaryTail<'a> {
    fn finish(self, left: Term<'a>) -> InfixUnary<'a> {
        let left = Box::new(left);
        let op = self.0;
        let right = self.1;
        InfixUnary { left, op, right }
    }
}

parser! {
    InfixUnaryTail,
    map(
        pair(
            preceded(common::ignored, common::InfixInequality::parse),
            preceded(common::ignored, map(Term::parse, Box::new)),
        ),
        |(op, right)| Self(op, right)
    )
}

/// [`fof_infix_unary`](http://tptp.org/TPTP/SyntaxBNF.html#fof_infix_unary)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}{}", left, op, right)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct InfixUnary<'a> {
    pub left: Box<Term<'a>>,
    pub op: common::InfixInequality,
    pub right: Box<Term<'a>>,
}

parser! {
    InfixUnary,
    map(
        pair(Term::parse, InfixUnaryTail::parse),
        |(left, tail)| tail.finish(left)
    )
}

/// [`fof_unary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_unary_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnaryFormula<'a> {
    #[display(fmt = "{}{}", _0, _1)]
    Unary(common::UnaryConnective, Box<UnitFormula<'a>>),
    InfixUnary(InfixUnary<'a>),
}

parser! {
    UnaryFormula,
    alt((
        map(
            pair(
                common::UnaryConnective::parse,
                preceded(common::ignored, UnitFormula::parse),
            ),
            |(c, f)| Self::Unary(c, Box::new(f)),
        ),
        map(InfixUnary::parse, Self::InfixUnary),
    ))
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

parser! {
    UnitaryFormula,
    alt((
        map(QuantifiedFormula::parse, Self::Quantified),
        map(
            delimited(
                pair(tag("("), common::ignored),
                map(LogicFormula::parse, Box::new),
                pair(common::ignored, tag(")")),
            ),
            Self::Parenthesised,
        ),
        map(map(AtomicFormula::parse, Box::new), Self::Atomic),
    ))
}

enum UnitFormulaTail<'a> {
    Equal(common::DefinedInfixPred, Box<Term<'a>>),
    NotEqual(common::InfixInequality, Box<Term<'a>>),
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

parser! {
    UnitFormulaTail,
    preceded(
        common::ignored,
        alt((
            map(
                pair(
                    common::DefinedInfixPred::parse,
                    preceded(common::ignored, map(Term::parse, Box::new)),
                ),
                |(op, right)| Self::Equal(op, right),
            ),
            map(
                pair(
                    common::InfixInequality::parse,
                    preceded(common::ignored, map(Term::parse, Box::new)),
                ),
                |(op, right)| Self::NotEqual(op, right),
            ),
        )),
    )
}

/// [`fof_unit_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_unit_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitFormula<'a> {
    Unitary(UnitaryFormula<'a>),
    Unary(UnaryFormula<'a>),
}

parser! {
    UnitFormula,
    alt((
        map(
            pair(PlainTerm::parse, opt(UnitFormulaTail::parse)),
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
    ))
}

fn assoc_tail<'a, E: Error<'a>>(
    sep: u8,
) -> impl Fn(&'a [u8]) -> Result<'a, GarbageFirstVec<UnitFormula<'a>>, E> {
    move |x| {
        let sep = &[sep];
        let (x, second) = preceded(
            terminated(tag(sep), common::ignored),
            UnitFormula::parse,
        )(x)?;
        let mut result = GarbageFirstVec::default();
        result.push(second);
        fold_many0_once(
            preceded(
                delimited(common::ignored, tag(sep), common::ignored),
                UnitFormula::parse,
            ),
            result,
            |mut result, next| {
                result.push(next);
                result
            },
        )(x)
    }
}

struct OrTail<'a>(GarbageFirstVec<UnitFormula<'a>>);

impl<'a> OrTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> OrFormula<'a> {
        OrFormula(self.0.finish(left))
    }
}

parser! {
    OrTail,
    map(assoc_tail(b'|'), Self)
}

/// [`fof_or_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_or_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated('|', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct OrFormula<'a>(pub Vec<UnitFormula<'a>>);

parser! {
    OrFormula,
    map(
        pair(UnitFormula::parse, preceded(common::ignored, OrTail::parse)),
        |(first, tail)| tail.finish(first)
    )
}

struct AndTail<'a>(GarbageFirstVec<UnitFormula<'a>>);

impl<'a> AndTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> AndFormula<'a> {
        AndFormula(self.0.finish(left))
    }
}

parser! {
    AndTail,
    map(assoc_tail(b'&'), Self)
}

/// [`fof_and_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_and_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated('&', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct AndFormula<'a>(pub Vec<UnitFormula<'a>>);

parser! {
    AndFormula,
    map(
        pair(UnitFormula::parse, preceded(common::ignored, AndTail::parse)),
        |(first, tail)| tail.finish(first)
    )
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

parser! {
    BinaryAssocTail,
    alt((
        map(OrTail::parse, BinaryAssocTail::Or),
        map(AndTail::parse, BinaryAssocTail::And)
    ))
}

/// [`fof_binary_assoc`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_assoc)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryAssoc<'a> {
    Or(OrFormula<'a>),
    And(AndFormula<'a>),
}

parser! {
    BinaryAssoc,
    map(
        pair(
            UnitFormula::parse,
            preceded(common::ignored, BinaryAssocTail::parse)
        ),
        |(first, tail)| tail.finish(first)
    )
}

struct BinaryNonassocTail<'a>(
    common::NonassocConnective,
    Box<UnitFormula<'a>>,
);

impl<'a> BinaryNonassocTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> BinaryNonassoc<'a> {
        let left = Box::new(left);
        let op = self.0;
        let right = self.1;
        BinaryNonassoc { left, op, right }
    }
}

parser! {
    BinaryNonassocTail,
    map(
        pair(
            common::NonassocConnective::parse,
            preceded(common::ignored, map(UnitFormula::parse, Box::new)),
        ),
        |(connective, right)| Self(connective, right)
    )
}

/// [`fof_binary_nonassoc`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_nonassoc)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}{}", left, op, right)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct BinaryNonassoc<'a> {
    pub left: Box<UnitFormula<'a>>,
    pub op: common::NonassocConnective,
    pub right: Box<UnitFormula<'a>>,
}

parser! {
    BinaryNonassoc,
    map(
        pair(
            UnitFormula::parse,
            preceded(common::ignored, BinaryNonassocTail::parse)
        ),
        |(left, tail)| tail.finish(left)
    )
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

parser! {
    BinaryFormulaTail,
    alt((
        map(BinaryAssocTail::parse, Self::Assoc),
        map(BinaryNonassocTail::parse, Self::Nonassoc),
    ))
}

/// [`fof_binary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryFormula<'a> {
    Assoc(BinaryAssoc<'a>),
    Nonassoc(BinaryNonassoc<'a>),
}

parser! {
    BinaryFormula,
    map(
        pair(
            UnitFormula::parse,
            preceded(common::ignored, BinaryFormulaTail::parse)
        ),
        |(left, tail)| tail.finish(left)
    )
}

/// [`fof_logic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_logic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum LogicFormula<'a> {
    Binary(BinaryFormula<'a>),
    Unary(UnaryFormula<'a>),
    Unitary(UnitaryFormula<'a>),
}

parser! {
    LogicFormula,
    map(
        pair(
            UnitFormula::parse,
            opt(preceded(common::ignored, BinaryFormulaTail::parse))
        ),
        |(left, tail)| match tail {
            Some(tail) => Self::Binary(tail.finish(left)),
            None => match left {
                UnitFormula::Unary(u) => Self::Unary(u),
                UnitFormula::Unitary(u) => Self::Unitary(u),
            }
        }
    )
}

/// [`fof_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Formula<'a>(pub LogicFormula<'a>);

parser! {
    Formula,
    map(LogicFormula::parse, Formula)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    #[test]
    fn test_fof_arguments() {
        check_size::<Arguments>();
        parse::<Arguments>(b"( c )\0");
        parse::<Arguments>(b"( X )\0");
        parse::<Arguments>(b"( X, f ( X ) )\0");
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
        check_size::<DefinedTerm>();
        parse::<DefinedTerm>(b"$defined_term\0");
        parse::<DefinedTerm>(b"-123\0");
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
