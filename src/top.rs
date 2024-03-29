use alloc::boxed::Box;
use alloc::fmt;
use alloc::vec::Vec;
use derive_more::Display;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::combinator::{map, opt};
use nom::multi::separated_list1;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
#[cfg(feature = "serde")]
use serde::Serialize;

use crate::cnf;
use crate::common::*;
use crate::fof;
use crate::tfx;
use crate::utils::Separated;
use crate::{Error, Parse, Result};

/// [`file_name`](http://tptp.org/TPTP/SyntaxBNF.html#file_name)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FileName<'a>(pub SingleQuoted<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for FileName<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(SingleQuoted::parse, Self)(x)
    }
}

/// [`name_list`](http://tptp.org/TPTP/SyntaxBNF.html#name_list)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated(',', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct NameList<'a>(pub Vec<Name<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for NameList<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            separated_list1(
                delimited(ignored, tag(","), ignored),
                Name::parse,
            ),
            Self,
        )(x)
    }
}

/// [`formula_role`](http://tptp.org/TPTP/SyntaxBNF.html#formula_role)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FormulaRole<'a>(pub LowerWord<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for FormulaRole<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(LowerWord::parse, FormulaRole)(x)
    }
}

/// [`formula_data`](http://tptp.org/TPTP/SyntaxBNF.html#formula_data)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum FormulaData<'a> {
    #[display(fmt = "$tff({})", _0)]
    Tfx(tfx::Formula<'a>),
    #[display(fmt = "$fof({})", _0)]
    Fof(fof::Formula<'a>),
    #[display(fmt = "$cnf({})", _0)]
    Cnf(cnf::Formula<'a>),
    #[display(fmt = "$fot({})", _0)]
    Fot(fof::Term<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for FormulaData<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        preceded(
            tag("$"),
            alt((
                preceded(
                    tag("tff"),
                    map(preceded(ignored, parens), Self::Tfx),
                ),
                preceded(
                    tag("fof"),
                    map(preceded(ignored, parens), Self::Fof),
                ),
                preceded(
                    tag("cnf"),
                    map(preceded(ignored, parens), Self::Cnf),
                ),
                preceded(
                    tag("fot"),
                    map(preceded(ignored, parens), Self::Fot),
                ),
            )),
        )(x)
    }
}

struct GeneralFunctionTail<'a>(GeneralTerms<'a>);

impl<'a> GeneralFunctionTail<'a> {
    fn finish(self, word: AtomicWord<'a>) -> GeneralFunction<'a> {
        let terms = self.0;
        GeneralFunction { word, terms }
    }
}

impl<'a, E: Error<'a>> Parse<'a, E> for GeneralFunctionTail<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(parens, Self)(x)
    }
}

/// [`general_function`](http://tptp.org/TPTP/SyntaxBNF.html#general_function)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}({})", word, terms)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct GeneralFunction<'a> {
    pub word: AtomicWord<'a>,
    pub terms: GeneralTerms<'a>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for GeneralFunction<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            pair(
                AtomicWord::parse,
                preceded(ignored, GeneralFunctionTail::parse),
            ),
            |(word, tail)| tail.finish(word),
        )(x)
    }
}

/// [`general_data`](http://tptp.org/TPTP/SyntaxBNF.html#general_data)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum GeneralData<'a> {
    Atomic(AtomicWord<'a>),
    Function(Box<GeneralFunction<'a>>),
    Variable(Variable<'a>),
    Number(Number<'a>),
    DistinctObject(DistinctObject<'a>),
    Formula(Box<FormulaData<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for GeneralData<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(
                pair(
                    AtomicWord::parse,
                    opt(preceded(ignored, GeneralFunctionTail::parse)),
                ),
                |(word, tail)| {
                    if let Some(tail) = tail {
                        Self::Function(Box::new(tail.finish(word)))
                    } else {
                        Self::Atomic(word)
                    }
                },
            ),
            map(Variable::parse, Self::Variable),
            map(Number::parse, Self::Number),
            map(DistinctObject::parse, Self::DistinctObject),
            map(map(FormulaData::parse, Box::new), Self::Formula),
        ))(x)
    }
}

/// [`general_terms`](http://tptp.org/TPTP/SyntaxBNF.html#general_terms)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "{}", "Separated(',', _0)")]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct GeneralTerms<'a>(pub Vec<GeneralTerm<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for GeneralTerms<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            separated_list1(
                delimited(ignored, tag(","), ignored),
                GeneralTerm::parse,
            ),
            Self,
        )(x)
    }
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

impl<'a, E: Error<'a>> Parse<'a, E> for GeneralList<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            delimited(
                tag("["),
                delimited(ignored, opt(GeneralTerms::parse), ignored),
                tag("]"),
            ),
            Self,
        )(x)
    }
}

/// [`general_term`](http://tptp.org/TPTP/SyntaxBNF.html#general_term)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum GeneralTerm<'a> {
    Data(GeneralData<'a>),
    #[display(fmt = "{}:{}", _0, _1)]
    Colon(GeneralData<'a>, Box<GeneralTerm<'a>>),
    List(GeneralList<'a>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for GeneralTerm<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(GeneralList::parse, Self::List),
            map(
                pair(
                    GeneralData::parse,
                    opt(preceded(
                        pair(ignored, tag(":")),
                        preceded(ignored, Self::parse),
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
        ))(x)
    }
}

/// [`source`](http://tptp.org/TPTP/SyntaxBNF.html#source)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Source<'a>(pub GeneralTerm<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for Source<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(GeneralTerm::parse, Self)(x)
    }
}

/// [`useful_info`](http://tptp.org/TPTP/SyntaxBNF.html#useful_info)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct UsefulInfo<'a>(pub GeneralList<'a>);

impl<'a, E: Error<'a>> Parse<'a, E> for UsefulInfo<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(GeneralList::parse, Self)(x)
    }
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

impl<'a, E: Error<'a>> Parse<'a, E> for OptionalInfo<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            opt(preceded(tag(","), preceded(ignored, UsefulInfo::parse))),
            Self,
        )(x)
    }
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

impl<'a, E: Error<'a>> Parse<'a, E> for Annotations<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            opt(preceded(
                pair(tag(","), ignored),
                map(
                    pair(
                        Source::parse,
                        map(
                            opt(preceded(ignored, OptionalInfo::parse)),
                            |info| info.unwrap_or(OptionalInfo(None)),
                        ),
                    ),
                    Box::new,
                ),
            )),
            Self,
        )(x)
    }
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

impl<'a, E: Error<'a>> Parse<'a, E> for FormulaSelection<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            opt(preceded(tag(","), delimited(ignored, brackets, ignored))),
            Self,
        )(x)
    }
}

/// [`include`](http://tptp.org/TPTP/SyntaxBNF.html#include)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "include({}{}).", file_name, selection)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Include<'a> {
    pub file_name: FileName<'a>,
    pub selection: FormulaSelection<'a>,
}

impl<'a, E: Error<'a>> Parse<'a, E> for Include<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        map(
            preceded(
                tag("include"),
                terminated(
                    delimited(
                        ignored,
                        delimited(
                            tag("("),
                            delimited(
                                ignored,
                                pair(
                                    FileName::parse,
                                    preceded(ignored, FormulaSelection::parse),
                                ),
                                ignored,
                            ),
                            tag(")"),
                        ),
                        ignored,
                    ),
                    tag("."),
                ),
            ),
            |(file_name, selection)| Self {
                file_name,
                selection,
            },
        )(x)
    }
}

/// helper struct to share common fields - thanks to Michael Färber
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "({},{},{}{}).", name, role, formula, annotations)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Annotated<'a, T> {
    pub name: Name<'a>,
    pub role: FormulaRole<'a>,
    pub formula: Box<T>,
    pub annotations: Annotations<'a>,
}

impl<'a, E: Error<'a>, T: Parse<'a, E>> Parse<'a, E> for Annotated<'a, T> {
    fn parse(x: &'a [u8]) -> Result<'a, Self, E> {
        map(
            preceded(
                tag("("),
                terminated(
                    delimited(
                        ignored,
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
                        ignored,
                    ),
                    tuple((tag(")"), ignored, tag("."))),
                ),
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

/// [`tff_annotated`](http://tptp.org/TPTP/SyntaxBNF.html#tff_annotated)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "tff{}", _0)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TfxAnnotated<'a>(pub Annotated<'a, tfx::Formula<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for TfxAnnotated<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        preceded(tag("tff"), preceded(ignored, map(Annotated::parse, Self)))(x)
    }
}

/// [`fof_annotated`](http://tptp.org/TPTP/SyntaxBNF.html#fof_annotated)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "fof{}", _0)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FofAnnotated<'a>(pub Annotated<'a, fof::Formula<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for FofAnnotated<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        preceded(tag("fof"), preceded(ignored, map(Annotated::parse, Self)))(x)
    }
}

/// [`cnf_annotated`](http://tptp.org/TPTP/SyntaxBNF.html#cnf_annotated)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[display(fmt = "cnf{}", _0)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct CnfAnnotated<'a>(pub Annotated<'a, cnf::Formula<'a>>);

impl<'a, E: Error<'a>> Parse<'a, E> for CnfAnnotated<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        preceded(tag("cnf"), preceded(ignored, map(Annotated::parse, Self)))(x)
    }
}

/// [`annotated_formula`](http://tptp.org/TPTP/SyntaxBNF.html#annotated_formula)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum AnnotatedFormula<'a> {
    Tfx(Box<TfxAnnotated<'a>>),
    Fof(Box<FofAnnotated<'a>>),
    Cnf(Box<CnfAnnotated<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for AnnotatedFormula<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(map(TfxAnnotated::parse, Box::new), Self::Tfx),
            map(map(FofAnnotated::parse, Box::new), Self::Fof),
            map(map(CnfAnnotated::parse, Box::new), Self::Cnf),
        ))(x)
    }
}

/// [`TPTP_input`](http://tptp.org/TPTP/SyntaxBNF.html#TPTP_input)
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum TPTPInput<'a> {
    Annotated(Box<AnnotatedFormula<'a>>),
    Include(Box<Include<'a>>),
}

impl<'a, E: Error<'a>> Parse<'a, E> for TPTPInput<'a> {
    fn parse(x: &'a [u8]) -> Result<Self, E> {
        alt((
            map(map(AnnotatedFormula::parse, Box::new), Self::Annotated),
            map(map(Include::parse, Box::new), Self::Include),
        ))(x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    #[test]
    fn test_formula_role() {
        check_size::<FormulaRole>();
        parse_snapshot!(FormulaRole, b"axiom\0");
        parse_snapshot!(FormulaRole, b"conjecture\0");
    }

    #[test]
    fn test_general_terms() {
        check_size::<GeneralTerms>();
        parse_snapshot!(GeneralTerms, b"X\0");
        parse_snapshot!(GeneralTerms, b"X , Y\0");
    }

    #[test]
    fn test_general_list() {
        check_size::<GeneralList>();
        parse_snapshot!(GeneralList, b"[ ]\0");
        parse_snapshot!(GeneralList, b"[ X ]\0");
        parse_snapshot!(GeneralList, b"[ X , Y ]\0");
    }

    #[test]
    fn test_general_function() {
        check_size::<GeneralFunction>();
        parse_snapshot!(GeneralFunction, b"atomic ( X )\0");
    }

    #[test]
    fn test_formula_data() {
        check_size::<FormulaData>();
        parse_snapshot!(FormulaData, b"$tff ( p )\0");
        parse_snapshot!(FormulaData, b"$fof ( p )\0");
        parse_snapshot!(FormulaData, b"$cnf ( p )\0");
        parse_snapshot!(FormulaData, b"$fot ( t )\0");
    }

    #[test]
    fn test_general_data() {
        check_size::<GeneralData>();
        parse_snapshot!(GeneralData, b"c\0");
        parse_snapshot!(GeneralData, b"X\0");
        parse_snapshot!(GeneralData, b"atomic ( X )\0");
        parse_snapshot!(GeneralData, b"$fof ( p )\0");
        parse_snapshot!(GeneralData, b"123\0");
        parse_snapshot!(GeneralData, b"\"distinct object\"\0");
    }

    #[test]
    fn test_general_term() {
        check_size::<GeneralTerm>();
        parse_snapshot!(GeneralTerm, b"[ X , Y ]\0");
        parse_snapshot!(GeneralTerm, b"$fof ( p )\0");
        parse_snapshot!(GeneralTerm, b"X : Y\0");
    }

    #[test]
    fn test_useful_info() {
        check_size::<UsefulInfo>();
        parse_snapshot!(UsefulInfo, b"[ X , Y ]\0");
    }

    #[test]
    fn test_optional_info() {
        check_size::<OptionalInfo>();
        parse_snapshot!(OptionalInfo, b"\0");
        parse_snapshot!(OptionalInfo, b", [ X , Y ]\0");
    }

    #[test]
    fn test_annotations() {
        check_size::<Annotations>();
        parse_snapshot!(Annotations, b"\0");
        parse_snapshot!(Annotations, b", c\0");
        parse_snapshot!(Annotations, b", c , [X]\0");
    }

    #[test]
    fn test_tfx_annotated() {
        check_size::<TfxAnnotated>();
        parse_snapshot!(TfxAnnotated, b"tff ( test , axiom , $true ) .\0");
        parse_snapshot!(
            TfxAnnotated,
            b"tff ( test , axiom , $true , unknown ) .\0"
        );
        parse_snapshot!(
            TfxAnnotated,
            b"tff ( test , axiom , $true , unknown , [] ) .\0"
        );
    }

    #[test]
    fn test_fof_annotated() {
        check_size::<FofAnnotated>();
        parse_snapshot!(FofAnnotated, b"fof ( test , axiom , $true ) .\0");
        parse_snapshot!(
            FofAnnotated,
            b"fof ( test , axiom , $true , unknown ) .\0"
        );
        parse_snapshot!(
            FofAnnotated,
            b"fof ( test , axiom , $true , unknown , [] ) .\0"
        );
    }

    #[test]
    fn test_cnf_annotated() {
        check_size::<CnfAnnotated>();
        parse_snapshot!(CnfAnnotated, b"cnf ( test , axiom , $true ) .\0");
        parse_snapshot!(
            CnfAnnotated,
            b"cnf ( test , axiom , $true , unknown ) .\0"
        );
        parse_snapshot!(
            CnfAnnotated,
            b"cnf ( test , axiom , $true , unknown , [] ) .\0"
        );
    }

    #[test]
    fn test_annotated_formula() {
        check_size::<AnnotatedFormula>();
        parse_snapshot!(AnnotatedFormula, b"tff ( test , axiom , $true ) .\0");
        parse_snapshot!(AnnotatedFormula, b"fof ( test , axiom , $true ) .\0");
        parse_snapshot!(AnnotatedFormula, b"cnf ( test , axiom , $true ) .\0");
    }

    #[test]
    fn test_file_name() {
        check_size::<FileName>();
        parse_snapshot!(FileName, b"'test'\0");
    }

    #[test]
    fn test_name_list() {
        check_size::<NameList>();
        parse_snapshot!(NameList, b"name , 'name' , 123\0");
        parse_snapshot!(NameList, b"name\0");
    }

    #[test]
    fn test_formula_selection() {
        check_size::<FormulaSelection>();
        parse_snapshot!(FormulaSelection, b", [ name , 'name' , 123 ]\0");
        parse_snapshot!(FormulaSelection, b", [ name ]\0");
    }

    #[test]
    fn test_include() {
        check_size::<Include>();
        parse_snapshot!(Include, b"include ( 'test' ) .\0");
        parse_snapshot!(Include, b"include ( 'test', [ test ] ) .\0");
    }

    #[test]
    fn test_tptp_input() {
        check_size::<TPTPInput>();
        parse_snapshot!(TPTPInput, b"include ( 'test' ) .\0");
        parse_snapshot!(TPTPInput, b"tff ( test , axiom , $true ) .\0");
        parse_snapshot!(TPTPInput, b"fof ( test , axiom , $true ) .\0");
        parse_snapshot!(TPTPInput, b"cnf ( test , axiom , $true ) .\0");
    }

    // https://github.com/MichaelRawson/tptp/issues/2
    // with thanks to @skbaek
    #[test]
    fn test_large_annotations() {
        parse_snapshot!(TPTPInput, b"cnf(c_0_137, negated_conjecture, $false, inference(cn,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[c_0_134, c_0_95]), c_0_97]), c_0_99]), c_0_101]), c_0_103]), c_0_105]), c_0_107]), c_0_109]), c_0_111]), c_0_113]), c_0_115]), c_0_117]), c_0_119]), c_0_121]), c_0_123]), c_0_125]), c_0_127]), c_0_129]), c_0_131]), c_0_133]), c_0_135])])).\0");
    }

    // reported by Michael Färber
    #[test]
    #[should_panic]
    fn test_cnf_trailing() {
        parse_snapshot!(TPTPInput, b"cnf(classical, conjecture, p(X) | ~p(X)");
    }
}
