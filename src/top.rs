use alloc::boxed::Box;
use alloc::fmt;
use alloc::vec::Vec;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::combinator::{map, opt, value};
use nom::multi::separated_nonempty_list;
use nom::sequence::{delimited, pair, preceded, tuple};
#[cfg(feature = "serde")]
use serde::Serialize;

use crate::cnf;
use crate::common::*;
use crate::fof;
use crate::utils::fmt_list;
use crate::{Error, Parse, Result};

/// [`file_name`](http://tptp.org/TPTP/SyntaxBNF.html#file_name)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FileName<'a>(pub SingleQuoted<'a>);
impl_unit_anon_display! {FileName}

parser! {
    FileName,
    map(SingleQuoted::parse, Self)
}

/// [`name_list`](http://tptp.org/TPTP/SyntaxBNF.html#name_list)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct NameList<'a>(pub Vec<Name<'a>>);

impl<'a> fmt::Display for NameList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, ",", &self.0)
    }
}

parser! {
    NameList,
    map(
        separated_nonempty_list(
            tuple((ignored, tag(","), ignored)),
            Name::parse,
        ),
        Self,
    )
}

/// [`formula_role`](http://tptp.org/TPTP/SyntaxBNF.html#formula_role)
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum FormulaRole {
    Axiom,
    Hypothesis,
    Definition,
    Assumption,
    Lemma,
    Theorem,
    Corollary,
    Conjecture,
    NegatedConjecture,
    Plain,
    Type,
    FiDomain,
    FiFunctors,
    FiPredicates,
    Unknown,
}

impl fmt::Display for FormulaRole {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FormulaRole::*;
        match self {
            Axiom => write!(f, "axiom"),
            Hypothesis => write!(f, "hypothesis"),
            Definition => write!(f, "definition"),
            Assumption => write!(f, "assumption"),
            Lemma => write!(f, "lemma"),
            Theorem => write!(f, "theorem"),
            Corollary => write!(f, "corollary"),
            Conjecture => write!(f, "conjecture"),
            NegatedConjecture => write!(f, "negated_conjecture"),
            Plain => write!(f, "plain"),
            Type => write!(f, "type"),
            FiDomain => write!(f, "fi_domain"),
            FiFunctors => write!(f, "fi_functors"),
            FiPredicates => write!(f, "fi_predicates"),
            Unknown => write!(f, "unknown"),
        }
    }
}

parser_no_lifetime! {
    FormulaRole,
    alt((
        value(Self::Axiom, tag("axiom")),
        value(Self::Hypothesis, tag("hypothesis")),
        value(Self::Definition, tag("definition")),
        value(Self::Assumption, tag("assumption")),
        value(Self::Lemma, tag("lemma")),
        value(Self::Theorem, tag("theorem")),
        value(Self::Corollary, tag("corollary")),
        value(Self::Conjecture, tag("conjecture")),
        value(Self::NegatedConjecture, tag("negated_conjecture")),
        value(Self::Plain, tag("plain")),
        value(Self::Type, tag("type")),
        value(Self::FiDomain, tag("fi_domain")),
        value(Self::FiFunctors, tag("fi_functors")),
        value(Self::FiPredicates, tag("fi_predicates")),
        value(Self::Unknown, tag("unknown")),
    ))
}

/// [`formula_data`](http://tptp.org/TPTP/SyntaxBNF.html#formula_data)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum FormulaData<'a> {
    Fof(fof::Formula<'a>),
    Cnf(cnf::Formula<'a>),
    Fot(fof::Term<'a>),
}

impl<'a> fmt::Display for FormulaData<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FormulaData::*;
        match self {
            Fof(fof) => write!(f, "$fof({})", fof),
            Cnf(cnf) => write!(f, "$cnf({})", cnf),
            Fot(cnf) => write!(f, "$fot({})", cnf),
        }
    }
}

parser! {
    FormulaData,
    preceded(
        tag("$"),
        alt((
            map(
                delimited(
                    tuple((tag("fof"), ignored, tag("("), ignored)),
                    fof::Formula::parse,
                    tuple((ignored, tag(")"))),
                ),
                Self::Fof,
            ),
            map(
                delimited(
                    tuple((tag("cnf"), ignored, tag("("), ignored)),
                    cnf::Formula::parse,
                    tuple((ignored, tag(")"))),
                ),
                Self::Cnf,
            ),
            map(
                delimited(
                    tuple((tag("fot"), ignored, tag("("), ignored)),
                    fof::Term::parse,
                    tuple((ignored, tag(")"))),
                ),
                Self::Fot,
            ),
        )),
    )
}

/// [`general_function`](http://tptp.org/TPTP/SyntaxBNF.html#general_function)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct GeneralFunction<'a> {
    pub word: AtomicWord<'a>,
    pub terms: GeneralTerms<'a>,
}

impl<'a> fmt::Display for GeneralFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.word, self.terms)
    }
}

fn general_function_tail<'a, E: Error<'a>>(
    x: &'a [u8],
) -> Result<GeneralTerms, E> {
    delimited(
        delimited(ignored, tag("("), ignored),
        GeneralTerms::parse,
        preceded(ignored, tag(")")),
    )(x)
}

parser! {
    GeneralFunction,
    map(
        pair(AtomicWord::parse, general_function_tail),
        |(word, terms)| Self { word, terms },
    )
}

/// [`general_data`](http://tptp.org/TPTP/SyntaxBNF.html#general_data)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum GeneralData<'a> {
    Atomic(AtomicWord<'a>),
    Function(Box<GeneralFunction<'a>>),
    Variable(Variable<'a>),
    Number(Number<'a>),
    DistinctObject(DistinctObject<'a>),
    Formula(Box<FormulaData<'a>>),
}
impl_enum_anon_display!(
    GeneralData,
    Atomic,
    Function,
    Variable,
    Number,
    DistinctObject,
    Formula
);

parser! {
    GeneralData,
    alt((
        map(
            pair(AtomicWord::parse, opt(general_function_tail)),
            |(word, tail)| {
                if let Some(terms) = tail {
                    let f = GeneralFunction { word, terms };
                    Self::Function(Box::new(f))
                } else {
                    Self::Atomic(word)
                }
            },
        ),
        map(Variable::parse, Self::Variable),
        map(Number::parse, Self::Number),
        map(DistinctObject::parse, Self::DistinctObject),
        map(map(FormulaData::parse, Box::new), Self::Formula),
    ))
}

/// [`general_terms`](http://tptp.org/TPTP/SyntaxBNF.html#general_terms)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct GeneralTerms<'a>(pub Vec<GeneralTerm<'a>>);

impl<'a> fmt::Display for GeneralTerms<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, ",", &self.0)
    }
}

parser! {
    GeneralTerms,
    map(
        separated_nonempty_list(
            delimited(ignored, tag(","), ignored),
            GeneralTerm::parse,
        ),
        Self,
    )
}

/// [`general_list`](http://tptp.org/TPTP/SyntaxBNF.html#general_list)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct GeneralList<'a>(pub Option<GeneralTerms<'a>>);

impl<'a> fmt::Display for GeneralList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            None => write!(f, "[]"),
            Some(terms) => write!(f, "[{}]", terms),
        }
    }
}

parser! {
    GeneralList,
    map(
        delimited(
            pair(tag("["), ignored),
            opt(GeneralTerms::parse),
            pair(ignored, tag("]")),
        ),
        Self,
    )
}

/// [`general_term`](http://tptp.org/TPTP/SyntaxBNF.html#general_term)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum GeneralTerm<'a> {
    Data(GeneralData<'a>),
    Colon(GeneralData<'a>, Box<GeneralTerm<'a>>),
    List(GeneralList<'a>),
}

impl<'a> fmt::Display for GeneralTerm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::GeneralTerm::*;
        match self {
            Data(d) => write!(f, "{}", d),
            Colon(d, t) => write!(f, "{}:{}", d, t),
            List(l) => write!(f, "{}", l),
        }
    }
}

parser! {
    GeneralTerm,
    alt((
        map(GeneralList::parse, Self::List),
        map(
            pair(
                GeneralData::parse,
                opt(preceded(
                    delimited(ignored, tag(":"), ignored),
                    Self::parse,
                )),
            ),
            |(left, right)| {
                if let Some(right) = right {
                    Self::Colon(left, Box::new(right))
                } else {
                    Self::Data(left)
                }
            },
        ),
    ))
}

/// [`source`](http://tptp.org/TPTP/SyntaxBNF.html#source)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Source<'a>(pub GeneralTerm<'a>);
impl_unit_anon_display! {Source}

parser! {
    Source,
    map(GeneralTerm::parse, Self)
}

/// [`useful_info`](http://tptp.org/TPTP/SyntaxBNF.html#useful_info)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct UsefulInfo<'a>(pub GeneralList<'a>);
impl_unit_anon_display! {UsefulInfo}

parser! {
    UsefulInfo,
    map(GeneralList::parse, Self)
}

/// [`optional_info`](http://tptp.org/TPTP/SyntaxBNF.html#optional_info)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct OptionalInfo<'a>(pub Option<UsefulInfo<'a>>);

impl<'a> fmt::Display for OptionalInfo<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(ref info) => write!(f, ",{}", info),
            None => Ok(()),
        }
    }
}

parser! {
    OptionalInfo,
    map(
        opt(preceded(pair(tag(","), ignored), UsefulInfo::parse)),
        Self,
    )
}

/// [`annotations`](http://tptp.org/TPTP/SyntaxBNF.html#annotations)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Annotations<'a>(pub Option<Box<(Source<'a>, OptionalInfo<'a>)>>);

impl<'a> fmt::Display for Annotations<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Some(boxed) => write!(f, ",{}{}", &boxed.0, &boxed.1),
            None => Ok(()),
        }
    }
}

parser! {
    Annotations,
    map(
        opt(preceded(
            pair(tag(","), ignored),
            map(
                pair(Source::parse, preceded(ignored, OptionalInfo::parse)),
                Box::new
            )
        )),
        Self
    )
}

/// [`formula_selection`](http://tptp.org/TPTP/SyntaxBNF.html#formula_selection)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FormulaSelection<'a>(pub Option<NameList<'a>>);

impl<'a> fmt::Display for FormulaSelection<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Some(list) => write!(f, ",[{}]", list),
            None => Ok(()),
        }
    }
}

parser! {
    FormulaSelection,
    map(
        opt(delimited(
            tuple((tag(","), ignored, tag("["), ignored)),
            NameList::parse,
            tuple((ignored, tag("]"))),
        )),
        Self,
    )
}

/// [`include`](http://tptp.org/TPTP/SyntaxBNF.html#include)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Include<'a> {
    pub file_name: FileName<'a>,
    pub selection: FormulaSelection<'a>,
}

impl<'a> fmt::Display for Include<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "include({}{}).", self.file_name, self.selection)
    }
}

parser! {
    Include,
    map(
        delimited(
            tuple((tag("include"), ignored, tag("("), ignored)),
            pair(FileName::parse, preceded(ignored, FormulaSelection::parse)),
            tuple((ignored, tag(")"), ignored, tag("."))),
        ),
        |(file_name, selection)| Self {
            file_name,
            selection,
        },
    )
}

/// helper struct to share common fields - thanks to Michael Färber
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Annotated<'a, T> {
    pub name: Name<'a>,
    pub role: FormulaRole,
    pub formula: Box<T>,
    pub annotations: Annotations<'a>,
}

impl<'a, T: fmt::Display> fmt::Display for Annotated<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({},{},{}{}).",
            self.name, self.role, self.formula, self.annotations
        )
    }
}

impl<'a, E: Error<'a>, T: Parse<'a, E>> Parse<'a, E> for Annotated<'a, T> {
    fn parse(x: &'a [u8]) -> Result<'a, Self, E> {
        map(
            delimited(
                pair(tag("("), ignored),
                tuple((
                    Name::parse,
                    preceded(
                        delimited(ignored, tag(","), ignored),
                        FormulaRole::parse,
                    ),
                    preceded(
                        delimited(ignored, tag(","), ignored),
                        map(T::parse, Box::new),
                    ),
                    preceded(ignored, Annotations::parse),
                )),
                pair(ignored, tag(")")),
            ),
            |(name, role, formula, annotations)| Self {
                name,
                role,
                formula,
                annotations,
            },
        )(x)
    }
}

/// [`fof_annotated`](http://tptp.org/TPTP/SyntaxBNF.html#fof_annotated)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FofAnnotated<'a>(pub Annotated<'a, fof::Formula<'a>>);

impl<'a> fmt::Display for FofAnnotated<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fof{}", self.0)
    }
}

parser! {
    FofAnnotated,
    delimited(
        pair(tag("fof"), ignored),
        map(Annotated::parse, Self),
        pair(ignored, tag(".")),
    )
}

/// [`cnf_annotated`](http://tptp.org/TPTP/SyntaxBNF.html#cnf_annotated)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct CnfAnnotated<'a>(pub Annotated<'a, cnf::Formula<'a>>);

impl<'a> fmt::Display for CnfAnnotated<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "cnf{}", self.0)
    }
}

parser! {
    CnfAnnotated,
    delimited(
        pair(tag("cnf"), ignored),
        map(Annotated::parse, Self),
        pair(ignored, tag(".")),
    )
}

/// [`annotated_formula`](http://tptp.org/TPTP/SyntaxBNF.html#annotated_formula)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AnnotatedFormula<'a> {
    Fof(FofAnnotated<'a>),
    Cnf(CnfAnnotated<'a>),
}
impl_enum_anon_display! {AnnotatedFormula, Fof, Cnf}

parser! {
    AnnotatedFormula,
    alt((
        map(FofAnnotated::parse, Self::Fof),
        map(CnfAnnotated::parse, Self::Cnf),
    ))
}

/// [`TPTP_input`](http://tptp.org/TPTP/SyntaxBNF.html#TPTP_input)
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum TPTPInput<'a> {
    Annotated(Box<AnnotatedFormula<'a>>),
    Include(Box<Include<'a>>),
}
impl_enum_anon_display! {TPTPInput, Annotated, Include}

parser! {
    TPTPInput,
    alt((
        map(map(AnnotatedFormula::parse, Box::new), Self::Annotated),
        map(map(Include::parse, Box::new), Self::Include),
    ))
}
