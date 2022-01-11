use alloc::boxed::Box;
use alloc::vec::Vec;
use derive_more::Display;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::combinator::{map, opt};
use nom::multi::separated_list1;
use nom::sequence::{delimited, pair, preceded, tuple};

use crate::common;
use crate::common::*;
use crate::fof;
use crate::utils::{fold_many0_once, GarbageFirstVec, Separated};
use crate::{Error, Parse, Result};

/// [`tff_type_arguments`](http://tptp.org/TPTP/SyntaxBNF.html#tff_type_arguments)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated(',', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TypeArguments<'a>(pub Vec<AtomicType<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for TypeArguments<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            separated_list1(
                delimited(ignored, tag(","), ignored),
                AtomicType::parse,
            ),
            Self,
        )(x)
    }
}

/// [`tff_atomic_type`](http://tptp.org/TPTP/SyntaxBNF.html#tff_atomic_type)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AtomicType<'a> {
    Constant(TypeConstant<'a>),
    Defined(DefinedType<'a>),
    Variable(common::Variable<'a>),
    #[display(fmt = "{}({})", _0, _1)]
    Function(TypeFunctor<'a>, Box<TypeArguments<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for AtomicType<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(DefinedType::parse, Self::Defined),
            map(common::Variable::parse, Self::Variable),
            map(
                pair(TypeFunctor::parse, opt(preceded(ignored, parens))),
                |(f, args)| match args {
                    Some(args) => Self::Function(f, Box::new(args)),
                    None => Self::Constant(TypeConstant(f)),
                },
            ),
        ))(x)
    }
}

struct TypedVariableTail<'a>(AtomicType<'a>);

impl<'a> TypedVariableTail<'a> {
    fn finish(self, variable: common::Variable<'a>) -> TypedVariable<'a> {
        let Self(typ) = self;
        TypedVariable { variable, typ }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for TypedVariableTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        preceded(
            delimited(ignored, tag(":"), ignored),
            map(AtomicType::parse, Self),
        )(x)
    }
}

/// [`tff_typed_variable`](http://tptp.org/TPTP/SyntaxBNF.html#tff_typed_variable)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}:{}", variable, typ)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TypedVariable<'a> {
    pub variable: common::Variable<'a>,
    pub typ: AtomicType<'a>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for TypedVariable<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(common::Variable::parse, TypedVariableTail::parse),
            |(variable, tail)| tail.finish(variable),
        )(x)
    }
}

/// [`tff_variable`](http://tptp.org/TPTP/SyntaxBNF.html#tff_variable)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Variable<'a> {
    Typed(TypedVariable<'a>),
    Untyped(common::Variable<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for Variable<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(common::Variable::parse, opt(TypedVariableTail::parse)),
            |(var, tail)| match tail {
                Some(tail) => Self::Typed(tail.finish(var)),
                None => Self::Untyped(var),
            },
        )(x)
    }
}

/// [`tff_variable_list`](http://tptp.org/TPTP/SyntaxBNF.html#tff_variable_list)
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

/// [`tff_unitary_type`](http://tptp.org/TPTP/SyntaxBNF.html#tff_unitary_type)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitaryType<'a> {
    Atomic(AtomicType<'a>),
    #[display(fmt = "({})", _0)]
    Product(XprodType<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitaryType<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(AtomicType::parse, Self::Atomic),
            map(parens, Self::Product),
        ))(x)
    }
}

/// [`tff_xprod_type`](http://tptp.org/TPTP/SyntaxBNF.html#tff_xprod_type)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated('*', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct XprodType<'a>(pub Vec<UnitaryType<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for XprodType<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            separated_list1(
                delimited(ignored, tag("*"), ignored),
                UnitaryType::parse,
            ),
            Self,
        )(x)
    }
}

/// [`tff_mapping_type`](http://tptp.org/TPTP/SyntaxBNF.html#tff_mapping_type)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}>{}", domain, range)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct MappingType<'a> {
    pub domain: Box<UnitaryType<'a>>,
    pub range: AtomicType<'a>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for MappingType<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                UnitaryType::parse,
                preceded(
                    delimited(ignored, tag(">"), ignored),
                    AtomicType::parse,
                ),
            ),
            |(domain, range)| Self {
                domain: Box::new(domain),
                range,
            },
        )(x)
    }
}

/// [`tf1_quantified_type`](http://tptp.org/TPTP/SyntaxBNF.html#tf1_quantified_type)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "!>[{}]:{}", bound, typ)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct QuantifiedType<'a> {
    pub bound: VariableList<'a>,
    pub typ: Box<Monotype<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for QuantifiedType<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        preceded(
            tag("!>"),
            preceded(
                ignored,
                map(
                    pair(
                        delimited(
                            tag("["),
                            delimited(ignored, VariableList::parse, ignored),
                            tag("]"),
                        ),
                        preceded(
                            delimited(ignored, tag(":"), ignored),
                            Monotype::parse,
                        ),
                    ),
                    |(bound, typ)| Self {
                        bound,
                        typ: Box::new(typ),
                    },
                ),
            ),
        )(x)
    }
}

/// [`tff_monotype`](http://tptp.org/TPTP/SyntaxBNF.html#tff_monotype)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Monotype<'a> {
    Atomic(AtomicType<'a>),
    #[display(fmt = "({})", _0)]
    Mapping(MappingType<'a>),
    Quantified(QuantifiedType<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for Monotype<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(AtomicType::parse, Self::Atomic),
            map(parens, Self::Mapping),
            map(QuantifiedType::parse, Self::Quantified),
        ))(x)
    }
}

// TODO control backtracking over parens/mapping?
// (A) > B
// (A)
/// [`tff_non_atomic_type`](http://tptp.org/TPTP/SyntaxBNF.html#tff_non_atomic_type)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum NonAtomicType<'a> {
    Mapping(MappingType<'a>),
    Quantified(QuantifiedType<'a>),
    #[display(fmt = "({})", _0)]
    Parenthesised(Box<NonAtomicType<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for NonAtomicType<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(MappingType::parse, Self::Mapping),
            map(QuantifiedType::parse, Self::Quantified),
            map(map(parens, Box::new), Self::Parenthesised),
        ))(x)
    }
}

/// [`tff_top_level_type`](http://tptp.org/TPTP/SyntaxBNF.html#tff_top_level_type)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum TopLevelType<'a> {
    Atomic(Box<AtomicType<'a>>),
    NonAtomic(Box<NonAtomicType<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for TopLevelType<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(map(NonAtomicType::parse, Box::new), Self::NonAtomic),
            map(map(AtomicType::parse, Box::new), Self::Atomic),
        ))(x)
    }
}

/// [`tff_atom_typing`](http://tptp.org/TPTP/SyntaxBNF.html#tff_atom_typing)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AtomTyping<'a> {
    #[display(fmt = "{}:{}", _0, _1)]
    Typing(UntypedAtom<'a>, TopLevelType<'a>),
    #[display(fmt = "({})", _0)]
    Parenthesised(Box<AtomTyping<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for AtomTyping<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(
                pair(
                    UntypedAtom::parse,
                    preceded(
                        delimited(ignored, tag(":"), ignored),
                        TopLevelType::parse,
                    ),
                ),
                |(atom, typ)| Self::Typing(atom, typ),
            ),
            map(parens, |typing| Self::Parenthesised(Box::new(typing))),
        ))(x)
    }
}

/// [`tff_term`](http://tptp.org/TPTP/SyntaxBNF.html#tff_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Term<'a> {
    Logic(Box<LogicFormula<'a>>),
    Defined(DefinedTerm<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for Term<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(map(LogicFormula::parse, Box::new), Self::Logic),
            map(DefinedTerm::parse, Self::Defined),
        ))(x)
    }
}

/// [`tff_unitary_term`](http://tptp.org/TPTP/SyntaxBNF.html#tff_unitary_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitaryTerm<'a> {
    Atomic(AtomicFormula<'a>),
    Defined(DefinedTerm<'a>),
    Variable(common::Variable<'a>),
    #[display(fmt = "({})", _0)]
    Logic(Box<LogicFormula<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitaryTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(AtomicFormula::parse, Self::Atomic),
            map(DefinedTerm::parse, Self::Defined),
            map(common::Variable::parse, Self::Variable),
            map(map(parens, Box::new), Self::Logic),
        ))(x)
    }
}

/// [`tff_arguments`](http://tptp.org/TPTP/SyntaxBNF.html#tff-arguments)
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

/// [`tff_system_atomic`](http://tptp.org/TPTP/SyntaxBNF.html#tff_system_atomic)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum SystemAtomic<'a> {
    Constant(SystemConstant<'a>),
    #[display(fmt = "{}({})", _0, _1)]
    Function(SystemFunctor<'a>, Box<Arguments<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for SystemAtomic<'a> {
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

/// [`tff_plain_atomic`](http://tptp.org/TPTP/SyntaxBNF.html#tff_plain_atomic)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PlainAtomic<'a> {
    Constant(Constant<'a>),
    #[display(fmt = "{}({})", _0, _1)]
    Function(Functor<'a>, Box<Arguments<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for PlainAtomic<'a> {
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

/// [`tff_defined_plain`](http://tptp.org/TPTP/SyntaxBNF.html#tff_defined_plain)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum DefinedPlain<'a> {
    Constant(DefinedConstant<'a>),
    #[display(fmt = "{}({})", _0, _1)]
    Function(DefinedFunctor<'a>, Box<Arguments<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedPlain<'a> {
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

/// [`tff_defined_atomic`](http://tptp.org/TPTP/SyntaxBNF.html#tff_defined_atomic)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedAtomic<'a>(pub DefinedPlain<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedAtomic<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(DefinedPlain::parse, Self)(x)
    }
}

/// [`tff_atomic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_atomic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AtomicFormula<'a> {
    Plain(PlainAtomic<'a>),
    Defined(DefinedAtomic<'a>),
    System(SystemAtomic<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for AtomicFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(PlainAtomic::parse, Self::Plain),
            map(SystemAtomic::parse, Self::System),
            map(DefinedAtomic::parse, Self::Defined),
        ))(x)
    }
}

struct DefinedInfixTail<'a>(DefinedInfixPred, Box<UnitaryTerm<'a>>);

impl<'a> DefinedInfixTail<'a> {
    fn finish(self, left: UnitaryTerm<'a>) -> DefinedInfix<'a> {
        let left = Box::new(left);
        let op = self.0;
        let right = self.1;
        DefinedInfix { left, op, right }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedInfixTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                preceded(ignored, DefinedInfixPred::parse),
                preceded(ignored, map(UnitaryTerm::parse, Box::new)),
            ),
            |(op, right)| Self(op, right),
        )(x)
    }
}

/// [`tff_defined_infix`](http://tptp.org/TPTP/SyntaxBNF.html#tff_defined_infix)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}{}", left, op, right)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefinedInfix<'a> {
    pub left: Box<UnitaryTerm<'a>>,
    pub op: DefinedInfixPred,
    pub right: Box<UnitaryTerm<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for DefinedInfix<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(UnitaryTerm::parse, DefinedInfixTail::parse),
            |(left, tail)| tail.finish(left),
        )(x)
    }
}

struct InfixUnaryTail<'a>(InfixInequality, Box<UnitaryTerm<'a>>);

impl<'a> InfixUnaryTail<'a> {
    fn finish(self, left: UnitaryTerm<'a>) -> InfixUnary<'a> {
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
                preceded(ignored, InfixInequality::parse),
                preceded(ignored, map(UnitaryTerm::parse, Box::new)),
            ),
            |(op, right)| Self(op, right),
        )(x)
    }
}

/// [`tff_infix_unary`](http://tptp.org/TPTP/SyntaxBNF.html#tff_infix_unary)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}{}", left, op, right)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct InfixUnary<'a> {
    pub left: Box<UnitaryTerm<'a>>,
    pub op: InfixInequality,
    pub right: Box<UnitaryTerm<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for InfixUnary<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(UnitaryTerm::parse, InfixUnaryTail::parse),
            |(left, tail)| tail.finish(left),
        )(x)
    }
}

/// [`tff_prefix_unary`](http://tptp.org/TPTP/SyntaxBNF.html#tff_prefix_unary)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}{}", op, formula)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct PrefixUnary<'a> {
    pub op: UnaryConnective,
    pub formula: Box<PreunitFormula<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for PrefixUnary<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                UnaryConnective::parse,
                preceded(ignored, PreunitFormula::parse),
            ),
            |(op, formula)| Self {
                op,
                formula: Box::new(formula),
            },
        )(x)
    }
}

/// [`tff_unary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_unary_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnaryFormula<'a> {
    Prefix(PrefixUnary<'a>),
    Infix(InfixUnary<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnaryFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(PrefixUnary::parse, Self::Prefix),
            map(InfixUnary::parse, Self::Infix),
        ))(x)
    }
}

/// [`tff_preunit_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_preunit_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PreunitFormula<'a> {
    Unitary(UnitaryFormula<'a>),
    Prefix(PrefixUnary<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for PreunitFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(PrefixUnary::parse, Self::Prefix),
            map(UnitaryFormula::parse, Self::Unitary),
        ))(x)
    }
}

/// [`tff_quantified_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_quantified_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}[{}]:{}", quantifier, bound, formula)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct QuantifiedFormula<'a> {
    pub quantifier: fof::Quantifier,
    pub bound: VariableList<'a>,
    pub formula: Box<UnitFormula<'a>>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for QuantifiedFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            tuple((
                fof::Quantifier::parse,
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

/// [`tff_unitary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_unitary_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitaryFormula<'a> {
    Quantified(QuantifiedFormula<'a>),
    Atomic(AtomicFormula<'a>),
    Variable(common::Variable<'a>),
    #[display(fmt = "({})", _0)]
    Logic(Box<LogicFormula<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitaryFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(QuantifiedFormula::parse, Self::Quantified),
            map(AtomicFormula::parse, Self::Atomic),
            map(common::Variable::parse, Self::Variable),
            map(map(parens, Box::new), Self::Logic),
        ))(x)
    }
}

enum UnitFormulaTail<'a> {
    Equality(DefinedInfixTail<'a>),
    Inequality(InfixUnaryTail<'a>),
}

impl<'a> UnitFormulaTail<'a> {
    fn finish(self, left: UnitaryTerm<'a>) -> UnitFormula<'a> {
        match self {
            Self::Equality(tail) => {
                UnitFormula::DefinedInfix(tail.finish(left))
            }
            Self::Inequality(tail) => {
                UnitFormula::Unary(UnaryFormula::Infix(tail.finish(left)))
            }
        }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitFormulaTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(DefinedInfixTail::parse, Self::Equality),
            map(InfixUnaryTail::parse, Self::Inequality),
        ))(x)
    }
}

enum UnitaryTermOrFormula<'a> {
    Atomic(AtomicFormula<'a>),
    Variable(common::Variable<'a>),
    Logic(Box<LogicFormula<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitaryTermOrFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(AtomicFormula::parse, Self::Atomic),
            map(common::Variable::parse, Self::Variable),
            map(map(parens, Box::new), Self::Logic),
        ))(x)
    }
}

impl<'a> From<UnitaryTermOrFormula<'a>> for UnitaryTerm<'a> {
    fn from(unit: UnitaryTermOrFormula<'a>) -> Self {
        match unit {
            UnitaryTermOrFormula::Atomic(f) => Self::Atomic(f),
            UnitaryTermOrFormula::Variable(v) => Self::Variable(v),
            UnitaryTermOrFormula::Logic(f) => Self::Logic(f),
        }
    }
}

impl<'a> From<UnitaryTermOrFormula<'a>> for UnitaryFormula<'a> {
    fn from(unit: UnitaryTermOrFormula<'a>) -> Self {
        match unit {
            UnitaryTermOrFormula::Atomic(f) => Self::Atomic(f),
            UnitaryTermOrFormula::Variable(v) => Self::Variable(v),
            UnitaryTermOrFormula::Logic(f) => Self::Logic(f),
        }
    }
}

/// [`tff_unit_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_unit_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnitFormula<'a> {
    Unitary(UnitaryFormula<'a>),
    Unary(UnaryFormula<'a>),
    DefinedInfix(DefinedInfix<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for UnitFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(
                pair(UnitaryTermOrFormula::parse, opt(UnitFormulaTail::parse)),
                |(left, tail)| {
                    if let Some(tail) = tail {
                        tail.finish(left.into())
                    } else {
                        UnitFormula::Unitary(left.into())
                    }
                },
            ),
            map(
                map(QuantifiedFormula::parse, UnitaryFormula::Quantified),
                Self::Unitary,
            ),
            map(map(PrefixUnary::parse, UnaryFormula::Prefix), Self::Unary),
            map(
                pair(DefinedTerm::parse, UnitFormulaTail::parse),
                |(left, tail)| tail.finish(UnitaryTerm::Defined(left)),
            ),
        ))(x)
    }
}

fn assoc_tail<'a, E: Error<'a>>(
    sep: u8,
) -> impl Fn(&'a [u8]) -> Result<'a, GarbageFirstVec<UnitFormula<'a>>, E> {
    move |x| {
        let sep = &[sep];
        let (x, second) =
            preceded(tag(sep), preceded(ignored, UnitFormula::parse))(x)?;
        let mut result = GarbageFirstVec::default();
        result.push(second);
        fold_many0_once(
            preceded(
                delimited(ignored, tag(sep), ignored),
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

impl<'a, E: Error<'a>> Parse<'a, E> for OrTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(assoc_tail(b'|'), Self)(x)
    }
}

/// [`tff_or_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_or_formula)
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

struct AndTail<'a>(GarbageFirstVec<UnitFormula<'a>>);

impl<'a> AndTail<'a> {
    fn finish(self, left: UnitFormula<'a>) -> AndFormula<'a> {
        AndFormula(self.0.finish(left))
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for AndTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(assoc_tail(b'&'), Self)(x)
    }
}

/// [`tff_and_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_and_formula)
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

/// [`tff_binary_assoc`](http://tptp.org/TPTP/SyntaxBNF.html#tff_binary_assoc)
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

/// [`tff_binary_nonassoc`](http://tptp.org/TPTP/SyntaxBNF.html#tff_binary_nonassoc)
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

/// [`tff_binary_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_binary_formula)
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

/// [`tfx_logic_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tfx_logic_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum LogicFormula<'a> {
    Unary(UnaryFormula<'a>),
    Unitary(UnitaryFormula<'a>),
    Binary(BinaryFormula<'a>),
    DefinedInfix(DefinedInfix<'a>),
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
                    UnitFormula::DefinedInfix(i) => Self::DefinedInfix(i),
                },
            },
        )(x)
    }
}

/// [`tff_formula`](http://tptp.org/TPTP/SyntaxBNF.html#tff_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Formula<'a> {
    Logic(Box<LogicFormula<'a>>),
    AtomTyping(Box<AtomTyping<'a>>),
    // TODO subtypes?
}

impl<'a, E: Error<'a>> Parse<'a, E> for Formula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(map(AtomTyping::parse, Box::new), Self::AtomTyping),
            map(map(LogicFormula::parse, Box::new), Self::Logic),
        ))(x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    #[test]
    fn test_tfx_type_arguments() {
        check_size::<TypeArguments>();
        parse::<TypeArguments>(b"A\0");
        parse::<TypeArguments>(b"A , c\0");
    }

    #[test]
    fn test_tfx_atomic_type() {
        check_size::<AtomicType>();
        parse::<AtomicType>(b"type_constant\0");
        parse::<AtomicType>(b"$defined_type\0");
        parse::<AtomicType>(b"TypeVariable\0");
        parse::<AtomicType>(b"type_function (A, B)\0");
    }

    #[test]
    fn test_tfx_typed_variable() {
        check_size::<TypedVariable>();
        parse::<TypedVariable>(b"X : A\0");
    }

    #[test]
    fn test_tfx_variable() {
        check_size::<super::Variable>();
        parse::<super::Variable>(b"X\0");
        parse::<super::Variable>(b"X : A\0");
    }

    #[test]
    fn test_tfx_variable_list() {
        check_size::<VariableList>();
        parse::<VariableList>(b"X\0");
        parse::<VariableList>(b"X , Y : A , Z : c\0");
    }

    #[test]
    fn test_tfx_xprod_type() {
        check_size::<XprodType>();
        parse::<XprodType>(b"A * c\0");
        parse::<XprodType>(b"A * B * c\0");
    }

    #[test]
    fn test_tfx_unitary_type() {
        check_size::<UnitaryType>();
        parse::<UnitaryType>(b"A\0");
        parse::<UnitaryType>(b"( A * B )\0");
        parse::<UnitaryType>(b"( ( A * c ) * ( B * d ) )\0");
    }

    #[test]
    fn test_tfx_mapping_type() {
        check_size::<MappingType>();
        parse::<MappingType>(b"A > B\0");
        parse::<MappingType>(b"( A * c ) > B\0");
    }

    #[test]
    fn test_tfx_quantified_type() {
        check_size::<QuantifiedType>();
        parse::<QuantifiedType>(b"!> [ A : $tType , B ] : A\0");
    }

    #[test]
    fn test_tfx_monotype() {
        check_size::<Monotype>();
        parse::<Monotype>(b"A\0");
        parse::<Monotype>(b"( A > A )\0");
        parse::<Monotype>(b"!>[A]: A\0");
    }

    #[test]
    fn test_tfx_non_atomic_type() {
        check_size::<NonAtomicType>();
        parse::<NonAtomicType>(b"A > A\0");
        parse::<NonAtomicType>(b"!>[A]: A\0");
        parse::<NonAtomicType>(b"( A > A )\0");
    }

    #[test]
    fn test_tfx_top_level_type() {
        check_size::<TopLevelType>();
        parse::<TopLevelType>(b"A\0");
        parse::<TopLevelType>(b"A > A\0");
    }

    #[test]
    fn test_tfx_atom_typing() {
        check_size::<AtomTyping>();
        parse::<AtomTyping>(b"c : A\0");
        parse::<AtomTyping>(b"( c : A )\0");
    }

    #[test]
    fn test_tfx_term() {
        check_size::<Term>();
        parse::<Term>(b"$true\0");
        parse::<Term>(b"123\0");
    }

    #[test]
    fn test_tfx_unitary_term() {
        check_size::<UnitaryTerm>();
        parse::<UnitaryTerm>(b"p\0");
        parse::<UnitaryTerm>(b"123\0");
        parse::<UnitaryTerm>(b"X\0");
        parse::<UnitaryTerm>(b"( $true )\0");
    }

    #[test]
    fn test_tfx_arguments() {
        check_size::<Arguments>();
        parse::<Arguments>(b"$true\0");
        parse::<Arguments>(b"$true , $true\0");
    }

    #[test]
    fn test_tfx_system_atomic() {
        check_size::<SystemAtomic>();
        parse::<SystemAtomic>(b"$$system\0");
        parse::<SystemAtomic>(b"$$system ( $true )\0");
    }

    #[test]
    fn test_tfx_plain_atomic() {
        check_size::<PlainAtomic>();
        parse::<PlainAtomic>(b"c\0");
        parse::<PlainAtomic>(b"f ( $true )\0");
    }

    #[test]
    fn test_tfx_defined_plain() {
        check_size::<DefinedPlain>();
        parse::<DefinedPlain>(b"$defined\0");
        parse::<DefinedPlain>(b"$defined ( $true )\0");
    }

    #[test]
    fn test_tfx_defined_atomic() {
        check_size::<DefinedAtomic>();
        parse::<DefinedAtomic>(b"$defined\0");
    }

    #[test]
    fn test_tfx_atomic_formula() {
        check_size::<AtomicFormula>();
        parse::<AtomicFormula>(b"p\0");
        parse::<AtomicFormula>(b"$defined\0");
        parse::<AtomicFormula>(b"$$system\0");
    }

    #[test]
    fn test_tfx_defined_infix() {
        check_size::<DefinedInfix>();
        parse::<DefinedInfix>(b"X = Y\0");
    }

    #[test]
    fn test_tfx_infix_unary() {
        check_size::<InfixUnary>();
        parse::<InfixUnary>(b"X != Y\0");
    }

    #[test]
    fn test_tfx_prefix_unary() {
        check_size::<PrefixUnary>();
        parse::<PrefixUnary>(b"~ $true\0");
    }

    #[test]
    fn test_tfx_unary_formula() {
        check_size::<UnaryFormula>();
        parse::<UnaryFormula>(b"X != Y\0");
        parse::<UnaryFormula>(b"~ $true\0");
    }

    #[test]
    fn test_tfx_preunit_formula() {
        check_size::<PreunitFormula>();
        parse::<PreunitFormula>(b"$true\0");
        parse::<PreunitFormula>(b"~ $true\0");
    }

    #[test]
    fn test_tfx_quantified_formula() {
        check_size::<QuantifiedFormula>();
        parse::<QuantifiedFormula>(b"! [ X : A ] : $true\0");
    }

    #[test]
    fn test_tfx_unitary_formula() {
        check_size::<UnitaryFormula>();
        parse::<UnitaryFormula>(b"![X : A]: $true\0");
        parse::<UnitaryFormula>(b"$true\0");
        parse::<UnitaryFormula>(b"X\0");
        parse::<UnitaryFormula>(b"( $true )\0");
    }

    #[test]
    fn test_tfx_unit_formula() {
        check_size::<UnitFormula>();
        parse::<UnitFormula>(b"$true\0");
        parse::<UnitFormula>(b"~$true\0");
        parse::<UnitFormula>(b"X = Y\0");
    }

    #[test]
    fn test_tfx_binary_nonassoc() {
        check_size::<BinaryNonassoc>();
        parse::<BinaryNonassoc>(b"p => q\0");
    }

    #[test]
    fn test_tfx_or_formula() {
        check_size::<OrFormula>();
        parse::<OrFormula>(b"p | q | r\0");
    }

    #[test]
    fn test_tfx_and_formula() {
        check_size::<AndFormula>();
        parse::<AndFormula>(b"p & q\0");
    }

    #[test]
    fn test_tfx_binary_assoc() {
        check_size::<BinaryAssoc>();
        parse::<BinaryAssoc>(b"p | q | r\0");
        parse::<BinaryAssoc>(b"p & q\0");
    }

    #[test]
    fn test_tfx_binary_formula() {
        check_size::<BinaryFormula>();
        parse::<BinaryFormula>(b"p => q\0");
        parse::<BinaryFormula>(b"p | q | r\0");
    }

    #[test]
    fn test_tfx_logic_formula() {
        check_size::<LogicFormula>();
        parse::<LogicFormula>(b"$true\0");
        parse::<LogicFormula>(b"~ p\0");
        parse::<LogicFormula>(b"p => q\0");
        parse::<LogicFormula>(b"X = Y\0");
    }

    #[test]
    fn test_tfx_formula() {
        check_size::<Formula>();
        parse::<Formula>(b"$true\0");
        parse::<Formula>(b"c : A\0");
    }
}
