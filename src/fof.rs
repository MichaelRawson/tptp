use alloc::boxed::Box;
use alloc::fmt;
use alloc::vec;
use alloc::vec::Vec;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::character::streaming::one_of;
use nom::combinator::{map, opt, peek, value};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
#[cfg(feature = "serde")]
use serde::Serialize;

use crate::common;
use crate::utils::{fmt_list, fold_many0, separated_list1};
use crate::{Error, Parse, Result};

/// [`fof_arguments`](http://tptp.org/TPTP/SyntaxBNF.html#fof_arguments)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Arguments<'a>(pub Vec<Term<'a>>);

impl<'a> fmt::Display for Arguments<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        fmt_list(f, ",", &self.0)?;
        write!(f, ")")
    }
}

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
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum SystemTerm<'a> {
    Constant(common::SystemConstant<'a>),
    Function(common::SystemFunctor<'a>, Box<Arguments<'a>>),
}

impl<'a> fmt::Display for SystemTerm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Constant(c) => write!(f, "{}", c),
            Self::Function(name, args) => write!(f, "{}{}", name, args),
        }
    }
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
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PlainTerm<'a> {
    Constant(common::Constant<'a>),
    Function(common::Functor<'a>, Box<Arguments<'a>>),
}

impl<'a> fmt::Display for PlainTerm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Constant(c) => write!(f, "{}", c),
            Self::Function(name, args) => write!(f, "{}{}", name, args),
        }
    }
}

parser! {
    PlainTerm,
    map(
        pair(
            common::Functor::parse,
            opt(preceded(common::ignored, Arguments::parse)),
        ),
        |(f, args)| match args {
            None => PlainTerm::Constant(common::Constant(f)),
            Some(args) => PlainTerm::Function(f, Box::new(args)),
        },
    )
}

/// [`fof_defined_plain_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_plain_term)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedPlainTerm<'a>(pub common::DefinedConstant<'a>);
impl_unit_anon_display! {DefinedPlainTerm}

parser! {
    DefinedPlainTerm,
    map(common::DefinedConstant::parse, Self)
}

/// [`fof_defined_atomic_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_atomic_term)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedAtomicTerm<'a>(pub DefinedPlainTerm<'a>);
impl_unit_anon_display! {DefinedAtomicTerm}

parser! {
    DefinedAtomicTerm,
    map(DefinedPlainTerm::parse, Self)
}

/// [`fof_defined_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_term)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedTerm<'a> {
    Defined(common::DefinedTerm<'a>),
    Atomic(DefinedAtomicTerm<'a>),
}
impl_enum_anon_display! {DefinedTerm, Defined, Atomic}

parser! {
    DefinedTerm,
    alt((
        map(common::DefinedTerm::parse, Self::Defined),
        map(DefinedAtomicTerm::parse, Self::Atomic),
    ))
}

/// [`fof_function_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_function_term)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum FunctionTerm<'a> {
    Plain(PlainTerm<'a>),
    Defined(DefinedTerm<'a>),
}
impl_enum_anon_display! {FunctionTerm, Plain, Defined}

parser! {
    FunctionTerm,
    alt((
        map(PlainTerm::parse, Self::Plain),
        map(DefinedTerm::parse, Self::Defined),
    ))
}

/// [`fof_term`](http://tptp.org/TPTP/SyntaxBNF.html#fof_term)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Term<'a> {
    Function(Box<FunctionTerm<'a>>),
    Variable(common::Variable<'a>),
}
impl_enum_anon_display! {Term, Function, Variable}

parser! {
    Term,
    alt((
        map(common::Variable::parse, Self::Variable),
        map(map(FunctionTerm::parse, Box::new), Self::Function),
    ))
}

/// [`fof_quantifier`](http://tptp.org/TPTP/SyntaxBNF.html#fof_quantifier)
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Quantifier {
    Forall,
    Exists,
}

impl fmt::Display for Quantifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Forall => write!(f, "!"),
            Self::Exists => write!(f, "?"),
        }
    }
}

parser_no_lifetime! {
    Quantifier,
    alt((
        value(Self::Forall, tag("!")),
        value(Self::Exists, tag("?")),
    ))
}

/// [`fof_system_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_system_atomic_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SystemAtomicFormula<'a>(pub SystemTerm<'a>);
impl_unit_anon_display! {SystemAtomicFormula}

parser! {
    SystemAtomicFormula,
    map(SystemTerm::parse, Self)
}

/// [`fof_plain_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_plain_atomic_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct PlainAtomicFormula<'a>(pub PlainTerm<'a>);
impl_unit_anon_display! {PlainAtomicFormula}

parser! {
    PlainAtomicFormula,
    map(PlainTerm::parse, Self)
}

/// [`fof_defined_infix_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_infix_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedInfixFormula<'a> {
    pub left: Box<Term<'a>>,
    pub op: common::DefinedInfixPred,
    pub right: Box<Term<'a>>,
}

impl<'a> fmt::Display for DefinedInfixFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.left, self.op, self.right)
    }
}

fn defined_infix_formula_tail<'a, E: Error<'a>>(
    x: &'a [u8],
) -> Result<(common::DefinedInfixPred, Box<Term>), E> {
    pair(
        preceded(common::ignored, common::DefinedInfixPred::parse),
        preceded(common::ignored, map(Term::parse, Box::new)),
    )(x)
}

parser! {
    DefinedInfixFormula,
    map(
        pair(map(Term::parse, Box::new), defined_infix_formula_tail),
        |(left, (op, right))| Self { left, op, right },
    )
}

/// [`fof_defined_plain_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_plain_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedPlainFormula<'a>(pub DefinedPlainTerm<'a>);
impl_unit_anon_display! {DefinedPlainFormula}

parser! {
    DefinedPlainFormula,
    map(DefinedPlainTerm::parse, Self)
}

/// [`fof_defined_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_defined_atomic_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedAtomicFormula<'a> {
    Plain(DefinedPlainFormula<'a>),
    Infix(DefinedInfixFormula<'a>),
}
impl_enum_anon_display! {DefinedAtomicFormula, Plain, Infix}

parser! {
    DefinedAtomicFormula,
    alt((
        map(DefinedInfixFormula::parse, Self::Infix),
        map(DefinedPlainFormula::parse, Self::Plain),
    ))
}

/// [`fof_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_atomic_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AtomicFormula<'a> {
    Plain(PlainAtomicFormula<'a>),
    Defined(DefinedAtomicFormula<'a>),
    System(SystemAtomicFormula<'a>),
}
impl_enum_anon_display! {AtomicFormula, Plain, Defined, System}

parser! {
    AtomicFormula,
    alt((
        map(
            pair(PlainTerm::parse, opt(defined_infix_formula_tail)),
            |(left, possible_right)| match possible_right {
                Some((op, right)) => {
                    let left = Box::new(FunctionTerm::Plain(left));
                    let left = Box::new(Term::Function(left));
                    let infix = DefinedInfixFormula { left, op, right };
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
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct VariableList<'a>(pub Vec<common::Variable<'a>>);

impl<'a> fmt::Display for VariableList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, ",", &self.0)
    }
}

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
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct QuantifiedFormula<'a> {
    pub quantifier: Quantifier,
    pub bound: VariableList<'a>,
    pub formula: Box<UnitFormula<'a>>,
}

impl<'a> fmt::Display for QuantifiedFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]:{}", self.quantifier, self.bound, self.formula)
    }
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

/// [`fof_infix_unary`](http://tptp.org/TPTP/SyntaxBNF.html#fof_infix_unary)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct InfixUnary<'a> {
    pub left: Box<Term<'a>>,
    pub op: common::InfixInequality,
    pub right: Box<Term<'a>>,
}

impl<'a> fmt::Display for InfixUnary<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.left, self.op, self.right)
    }
}

fn infix_unary_tail<'a, E: Error<'a>>(
    x: &'a [u8],
) -> Result<(common::InfixInequality, Box<Term>), E> {
    pair(
        preceded(common::ignored, common::InfixInequality::parse),
        preceded(common::ignored, map(Term::parse, Box::new)),
    )(x)
}

parser! {
    InfixUnary,
    map(
        pair(map(Term::parse, Box::new), infix_unary_tail),
        |(left, (op, right))| Self { left, op, right },
    )
}

/// [`fof_unary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_unary_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnaryFormula<'a> {
    Unary(common::UnaryConnective, Box<UnitFormula<'a>>),
    InfixUnary(InfixUnary<'a>),
}

impl<'a> fmt::Display for UnaryFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unary(connective, u) => write!(f, "{}{}", connective, u),
            Self::InfixUnary(i) => write!(f, "{}", i),
        }
    }
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
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitaryFormula<'a> {
    Quantified(QuantifiedFormula<'a>),
    Atomic(Box<AtomicFormula<'a>>),
    Parenthesised(Box<LogicFormula<'a>>),
}

impl<'a> fmt::Display for UnitaryFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Quantified(q) => write!(f, "{}", q),
            Self::Atomic(a) => write!(f, "{}", a),
            Self::Parenthesised(p) => write!(f, "({})", p),
        }
    }
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

/// [`fof_unit_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_unit_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitFormula<'a> {
    Unitary(UnitaryFormula<'a>),
    Unary(UnaryFormula<'a>),
}
impl_enum_anon_display! {UnitFormula, Unitary, Unary}

enum UnitFormulaTail<'a> {
    Equal(common::DefinedInfixPred, Box<Term<'a>>),
    NotEqual(common::InfixInequality, Box<Term<'a>>),
}

fn unit_formula_tail<'a, E: Error<'a>>(
    x: &'a [u8],
) -> Result<UnitFormulaTail, E> {
    preceded(
        common::ignored,
        alt((
            map(
                pair(
                    common::DefinedInfixPred::parse,
                    preceded(common::ignored, map(Term::parse, Box::new)),
                ),
                |(op, right)| UnitFormulaTail::Equal(op, right),
            ),
            map(
                pair(
                    common::InfixInequality::parse,
                    preceded(common::ignored, map(Term::parse, Box::new)),
                ),
                |(op, right)| UnitFormulaTail::NotEqual(op, right),
            ),
        )),
    )(x)
}

parser! {
    UnitFormula,
    alt((
        map(
            pair(PlainTerm::parse, opt(unit_formula_tail)),
            |(left, rest)| match rest {
                Some(rest) => {
                    let left = Box::new(FunctionTerm::Plain(left));
                    let left = Box::new(Term::Function(left));
                    match rest {
                        UnitFormulaTail::Equal(op, right) => {
                            let infix =
                                DefinedInfixFormula { left, op, right };
                            let defined = DefinedAtomicFormula::Infix(infix);
                            let atomic = AtomicFormula::Defined(defined);
                            let atomic = Box::new(atomic);
                            let unitary = UnitaryFormula::Atomic(atomic);
                            Self::Unitary(unitary)
                        }
                        UnitFormulaTail::NotEqual(op, right) => {
                            let infix = InfixUnary { left, op, right };
                            let unary = UnaryFormula::InfixUnary(infix);
                            Self::Unary(unary)
                        }
                    }
                }
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

/// [`fof_or_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_or_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct OrFormula<'a>(pub Vec<UnitFormula<'a>>);

impl<'a> fmt::Display for OrFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, "|", &self.0)
    }
}

fn assoc_impl<'a, E: Error<'a>>(
    first: UnitFormula<'a>,
    sep: u8,
    x: &'a [u8],
) -> Result<'a, Vec<UnitFormula<'a>>, E> {
    let (x, second) =
        preceded(pair(tag(&[sep]), common::ignored), UnitFormula::parse)(x)?;
    fold_many0(
        preceded(
            tuple((common::ignored, tag(&[sep]), common::ignored)),
            UnitFormula::parse,
        ),
        vec![first, second],
        |mut result, next| {
            result.push(next);
            result
        },
    )(x)
}

fn or_impl<'a, E: Error<'a>>(
    first: UnitFormula<'a>,
    x: &'a [u8],
) -> Result<'a, OrFormula<'a>, E> {
    let (x, formulae) = assoc_impl(first, b'|', x)?;
    Ok((x, OrFormula(formulae)))
}

parser! {
    OrFormula,
    |x| {
        let (x, first) = terminated(UnitFormula::parse, common::ignored)(x)?;
        or_impl(first, x)
    }
}

/// [`fof_and_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_and_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct AndFormula<'a>(pub Vec<UnitFormula<'a>>);

impl<'a> fmt::Display for AndFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, "&", &self.0)
    }
}

fn and_impl<'a, E: Error<'a>>(
    first: UnitFormula<'a>,
    x: &'a [u8],
) -> Result<'a, AndFormula<'a>, E> {
    let (x, formulae) = assoc_impl(first, b'&', x)?;
    Ok((x, AndFormula(formulae)))
}

parser! {
    AndFormula,
    |x| {
        let (x, first) = terminated(UnitFormula::parse, common::ignored)(x)?;
        and_impl(first, x)
    }
}

/// [`fof_binary_assoc`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_assoc)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryAssoc<'a> {
    Or(OrFormula<'a>),
    And(AndFormula<'a>),
}
impl_enum_anon_display! {BinaryAssoc, Or, And}

fn binary_assoc_impl<'a, E: Error<'a>>(
    first: UnitFormula<'a>,
    sep: char,
    x: &'a [u8],
) -> Result<'a, BinaryAssoc<'a>, E> {
    if sep == '|' {
        let (x, or) = or_impl(first, x)?;
        Ok((x, BinaryAssoc::Or(or)))
    } else {
        let (x, and) = and_impl(first, x)?;
        Ok((x, BinaryAssoc::And(and)))
    }
}

parser! {
    BinaryAssoc,
    |x| {
        let (x, (first, sep)) = pair(
            UnitFormula::parse,
            preceded(common::ignored, peek(one_of("&|"))),
        )(x)?;
        binary_assoc_impl(first, sep, x)
    }
}

/// [`fof_binary_nonassoc`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_nonassoc)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct BinaryNonassoc<'a> {
    pub left: Box<UnitFormula<'a>>,
    pub op: common::NonassocConnective,
    pub right: Box<UnitFormula<'a>>,
}

impl<'a> fmt::Display for BinaryNonassoc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.left, self.op, self.right)
    }
}

fn binary_nonassoc_impl<'a, E: Error<'a>>(
    left: UnitFormula<'a>,
    x: &'a [u8],
) -> Result<'a, BinaryNonassoc<'a>, E> {
    let (x, (op, right)) = pair(
        common::NonassocConnective::parse,
        preceded(common::ignored, map(UnitFormula::parse, Box::new)),
    )(x)?;
    let left = Box::new(left);
    Ok((x, BinaryNonassoc { left, op, right }))
}

parser! {
    BinaryNonassoc,
    |x| {
        let (x, left) = terminated(UnitFormula::parse, common::ignored)(x)?;
        binary_nonassoc_impl(left, x)
    }
}

/// [`fof_binary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_binary_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryFormula<'a> {
    Nonassoc(BinaryNonassoc<'a>),
    Assoc(BinaryAssoc<'a>),
}
impl_enum_anon_display! {BinaryFormula, Nonassoc, Assoc}

fn binary_formula_impl<'a, E: Error<'a>>(
    first: UnitFormula<'a>,
    sep: char,
    x: &'a [u8],
) -> Result<'a, BinaryFormula<'a>, E> {
    match sep {
        '&' | '|' => {
            let (x, assoc) = binary_assoc_impl(first, sep, x)?;
            Ok((x, BinaryFormula::Assoc(assoc)))
        }
        _ => {
            let (x, nonassoc) = binary_nonassoc_impl(first, x)?;
            Ok((x, BinaryFormula::Nonassoc(nonassoc)))
        }
    }
}

parser! {
    BinaryFormula,
    |x| {
        let (x, (first, sep)) = pair(
            UnitFormula::parse,
            preceded(common::ignored, peek(one_of("&|~<="))),
        )(x)?;
        binary_formula_impl(first, sep, x)
    }
}

/// [`fof_logic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_logic_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum LogicFormula<'a> {
    Binary(BinaryFormula<'a>),
    Unary(UnaryFormula<'a>),
    Unitary(UnitaryFormula<'a>),
}
impl_enum_anon_display! {LogicFormula, Binary, Unary, Unitary}

parser! {
    LogicFormula,
    |x| {
        let (x_after_first, first) = UnitFormula::parse(x)?;
        let (x_after_ignored, _) = common::ignored(x_after_first)?;
        let (_, sep) = peek(opt(one_of("&|~<=")))(x_after_ignored)?;
        match sep {
            Some(sep) => {
                let (x, binary) =
                    binary_formula_impl(first, sep, x_after_ignored)?;
                Ok((x, LogicFormula::Binary(binary)))
            }
            None => {
                let result = match first {
                    UnitFormula::Unary(u) => LogicFormula::Unary(u),
                    UnitFormula::Unitary(u) => LogicFormula::Unitary(u),
                };
                Ok((x_after_first, result))
            }
        }
    }
}

/// [`fof_formula`](http://tptp.org/TPTP/SyntaxBNF.html#fof_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Formula<'a>(pub LogicFormula<'a>);
impl_unit_anon_display! {Formula}

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
        parse::<DefinedPlainTerm>(b"$defined_plain_term\0");
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
