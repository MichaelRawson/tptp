use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::str;
use alloc::vec;
use alloc::vec::Vec;
use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_until, take_while, take_while1};
use nom::character::complete::{digit0, multispace1, one_of};
use nom::combinator::{iterator, map, opt, recognize, value, ParserIterator};
use nom::error::ParseError;
use nom::multi::{many0, many1, separated_nonempty_list};
use nom::sequence::{delimited, pair, preceded, tuple};

use crate::syntax::*;

type ParseResult<'a, T, E> = nom::IResult<&'a [u8], T, E>;

unsafe fn to_str(bytes: &[u8]) -> &str {
    str::from_utf8_unchecked(bytes)
}

pub fn is_lower_alpha(c: u8) -> bool {
    c >= b'a' && c <= b'z'
}

pub fn is_upper_alpha(c: u8) -> bool {
    c >= b'A' && c <= b'Z'
}

pub fn is_alphanumeric(c: u8) -> bool {
    (c >= b'a' && c <= b'z')
        || (c >= b'A' && c <= b'Z')
        || (c >= b'0' && c <= b'9')
        || (c == b'_')
}

pub fn is_visible(c: u8) -> bool {
    c >= b' ' && c <= b'~'
}

pub fn is_sq_char(c: u8) -> bool {
    is_visible(c) && c != b'\'' && c != b'\\'
}

pub fn whitespace<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(), E> {
    value((), multispace1)(x)
}

pub fn comment_line<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(), E> {
    value((), tuple((tag("%"), take_until("\n"), tag("\n"))))(x)
}

pub fn comment_block<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(), E> {
    value((), tuple((tag("/*"), take_until("*/"), tag("*/"))))(x)
}

/// zero or more `whitespace`, `comment_line`, or `comment_block`
pub fn ignored<'a, E: ParseError<&'a [u8]>>(x: &'a [u8]) -> ParseResult<(), E> {
    value((), many0(alt((whitespace, comment_line, comment_block))))(x)
}

pub fn lower_alpha<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<&[u8], E> {
    take_while1(is_lower_alpha)(x)
}

pub fn upper_alpha<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<&[u8], E> {
    take_while1(is_upper_alpha)(x)
}

pub fn alphanumeric<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<&[u8], E> {
    take_while(is_alphanumeric)(x)
}

pub fn lower_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<LowerWord, E> {
    map(recognize(preceded(lower_alpha, alphanumeric)), |w| {
        LowerWord(Cow::Borrowed(unsafe { to_str(w) }))
    })(x)
}

pub fn upper_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<UpperWord, E> {
    map(recognize(preceded(upper_alpha, alphanumeric)), |w| {
        UpperWord(Cow::Borrowed(unsafe { to_str(w) }))
    })(x)
}

pub fn single_quoted<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<SingleQuoted, E> {
    map(
        delimited(
            tag("'"),
            escaped(take_while1(is_sq_char), '\\', one_of("\\'")),
            tag("'"),
        ),
        |w| SingleQuoted(Cow::Borrowed(unsafe { to_str(w) })),
    )(x)
}

pub fn atomic_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<AtomicWord, E> {
    alt((
        map(lower_word, AtomicWord::Lower),
        map(single_quoted, AtomicWord::SingleQuoted),
    ))(x)
}

pub fn dollar_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<DollarWord, E> {
    map(preceded(tag("$"), lower_word), DollarWord)(x)
}

pub fn integer<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Integer, E> {
    map(
        recognize(preceded(
            opt(one_of("+-")),
            alt((tag("0"), preceded(one_of("123456789"), digit0))),
        )),
        |w| Integer(Cow::Borrowed(unsafe { to_str(w) })),
    )(x)
}

pub fn name<'a, E: ParseError<&'a [u8]>>(x: &'a [u8]) -> ParseResult<Name, E> {
    alt((
        map(atomic_word, Name::AtomicWord),
        map(integer, Name::Integer),
    ))(x)
}

pub fn variable<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Variable, E> {
    map(upper_word, Variable)(x)
}

pub fn atomic_defined_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<AtomicDefinedWord, E> {
    map(dollar_word, AtomicDefinedWord)(x)
}

pub fn defined_functor<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<DefinedFunctor, E> {
    map(atomic_defined_word, DefinedFunctor)(x)
}

pub fn defined_constant<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<DefinedConstant, E> {
    map(defined_functor, DefinedConstant)(x)
}

pub fn functor<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Functor, E> {
    map(atomic_word, Functor)(x)
}

pub fn fof_arguments<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofArguments, E> {
    map(
        delimited(
            tag("("),
            separated_nonempty_list(
                tag(","),
                delimited(ignored, fof_term, ignored),
            ),
            tag(")"),
        ),
        FofArguments,
    )(x)
}

pub fn fof_plain_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofPlainTerm, E> {
    map(
        pair(functor, preceded(ignored, opt(fof_arguments))),
        |(f, args)| match args {
            None => FofPlainTerm::Constant(f),
            Some(args) => FofPlainTerm::Function(f, args),
        },
    )(x)
}

pub fn fof_defined_plain_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofDefinedPlainTerm, E> {
    map(defined_constant, FofDefinedPlainTerm)(x)
}

pub fn fof_defined_atomic_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofDefinedAtomicTerm, E> {
    map(fof_defined_plain_term, FofDefinedAtomicTerm)(x)
}

pub fn fof_defined_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofDefinedTerm, E> {
    map(fof_defined_atomic_term, FofDefinedTerm)(x)
}

pub fn fof_function_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFunctionTerm, E> {
    alt((
        map(fof_plain_term, FofFunctionTerm::Plain),
        map(fof_defined_term, FofFunctionTerm::Defined),
    ))(x)
}

pub fn fof_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofTerm, E> {
    alt((
        map(variable, FofTerm::Variable),
        map(fof_function_term, FofTerm::Function),
    ))(x)
}

pub fn infix_equality<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<InfixEquality, E> {
    value(InfixEquality, tag("="))(x)
}

pub fn infix_inequality<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<InfixInequality, E> {
    value(InfixInequality, tag("!="))(x)
}

pub fn nonassoc_connective<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<NonassocConnective, E> {
    alt((
        value(NonassocConnective::LRImplies, tag("=>")),
        value(NonassocConnective::Equivalent, tag("<=>")),
        value(NonassocConnective::RLImplies, tag("<=")),
        value(NonassocConnective::NotEquivalent, tag("<~>")),
        value(NonassocConnective::NotAnd, tag("~&")),
        value(NonassocConnective::NotOr, tag("~|")),
    ))(x)
}

pub fn unary_connective<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<UnaryConnective, E> {
    value(UnaryConnective, tag("~"))(x)
}

pub fn fof_quantifier<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofQuantifier, E> {
    alt((
        value(FofQuantifier::Forall, tag("!")),
        value(FofQuantifier::Exists, tag("?")),
    ))(x)
}

pub fn fof_plain_atomic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofPlainAtomicFormula, E> {
    map(fof_plain_term, FofPlainAtomicFormula)(x)
}

pub fn defined_infix_pred<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<DefinedInfixPred, E> {
    map(infix_equality, DefinedInfixPred)(x)
}

pub fn fof_defined_plain_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofDefinedPlainFormula, E> {
    map(fof_defined_plain_term, FofDefinedPlainFormula)(x)
}

pub fn fof_defined_infix_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofDefinedInfixFormula, E> {
    map(
        tuple((
            fof_term,
            delimited(ignored, defined_infix_pred, ignored),
            fof_term,
        )),
        |(left, op, right)| FofDefinedInfixFormula { left, op, right },
    )(x)
}

pub fn fof_defined_atomic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofDefinedAtomicFormula, E> {
    alt((
        map(fof_defined_infix_formula, FofDefinedAtomicFormula::Infix),
        map(fof_defined_plain_formula, FofDefinedAtomicFormula::Plain),
    ))(x)
}

pub fn fof_atomic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofAtomicFormula, E> {
    // avoid re-parsing `fof_plain_term`
    fn plain_term_maybe_infix<'a, E: ParseError<&'a [u8]>>(
        x: &'a [u8],
    ) -> ParseResult<(FofPlainTerm, Option<(DefinedInfixPred, FofTerm)>), E>
    {
        pair(
            fof_plain_term,
            opt(pair(
                delimited(ignored, defined_infix_pred, ignored),
                fof_term,
            )),
        )(x)
    }

    alt((
        map(plain_term_maybe_infix, |(left, rest)| match rest {
            Some((op, right)) => FofAtomicFormula::Defined(
                FofDefinedAtomicFormula::Infix(FofDefinedInfixFormula {
                    left: FofTerm::Function(FofFunctionTerm::Plain(left)),
                    op,
                    right,
                }),
            ),
            None => FofAtomicFormula::Plain(FofPlainAtomicFormula(left)),
        }),
        map(fof_defined_atomic_formula, FofAtomicFormula::Defined),
    ))(x)
}

pub fn fof_variable_list<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofVariableList, E> {
    map(
        separated_nonempty_list(tuple((ignored, tag(","), ignored)), variable),
        FofVariableList,
    )(x)
}

pub fn fof_quantified_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofQuantifiedFormula, E> {
    map(
        tuple((
            fof_quantifier,
            delimited(
                tuple((ignored, tag("["), ignored)),
                fof_variable_list,
                tuple((ignored, tag("]"), ignored, tag(":"), ignored)),
            ),
            fof_unit_formula,
        )),
        |(quantifier, bound, formula)| FofQuantifiedFormula {
            quantifier,
            bound,
            formula,
        },
    )(x)
}

pub fn fof_infix_unary<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofInfixUnary, E> {
    map(
        tuple((
            fof_term,
            delimited(ignored, infix_inequality, ignored),
            fof_term,
        )),
        |(left, op, right)| FofInfixUnary { left, op, right },
    )(x)
}

pub fn fof_unary_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofUnaryFormula, E> {
    alt((
        map(
            pair(unary_connective, preceded(ignored, fof_unit_formula)),
            |(c, f)| FofUnaryFormula::Unary(c, f),
        ),
        map(fof_infix_unary, FofUnaryFormula::InfixUnary),
    ))(x)
}

pub fn fof_unitary_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofUnitaryFormula, E> {
    alt((
        map(fof_quantified_formula, FofUnitaryFormula::Quantified),
        map(
            delimited(
                pair(tag("("), ignored),
                fof_logic_formula,
                pair(ignored, tag(")")),
            ),
            FofUnitaryFormula::Parenthesised,
        ),
        map(fof_atomic_formula, FofUnitaryFormula::Atomic),
    ))(x)
}

pub fn fof_unit_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofUnitFormula, E> {
    enum Op {
        Equals(DefinedInfixPred),
        NotEquals(InfixInequality),
    }
    use Op::*;

    fn op<'a, E: ParseError<&'a [u8]>>(x: &'a [u8]) -> ParseResult<Op, E> {
        alt((
            map(defined_infix_pred, Equals),
            map(infix_inequality, NotEquals),
        ))(x)
    }

    // avoid re-parsing `fof_plain_term`
    fn plain_term_maybe_infix<'a, E: ParseError<&'a [u8]>>(
        x: &'a [u8],
    ) -> ParseResult<(FofPlainTerm, Option<(Op, FofTerm)>), E> {
        pair(
            fof_plain_term,
            opt(pair(delimited(ignored, op, ignored), fof_term)),
        )(x)
    }

    alt((
        map(plain_term_maybe_infix, |(left, rest)| match rest {
            Some((Equals(op), right)) => FofUnitFormula::Unitary(Box::new(
                FofUnitaryFormula::Atomic(FofAtomicFormula::Defined(
                    FofDefinedAtomicFormula::Infix(FofDefinedInfixFormula {
                        left: FofTerm::Function(FofFunctionTerm::Plain(left)),
                        op,
                        right,
                    }),
                )),
            )),
            Some((NotEquals(op), right)) => FofUnitFormula::Unary(Box::new(
                FofUnaryFormula::InfixUnary(FofInfixUnary {
                    left: FofTerm::Function(FofFunctionTerm::Plain(left)),
                    op,
                    right,
                }),
            )),
            None => {
                FofUnitFormula::Unitary(Box::new(FofUnitaryFormula::Atomic(
                    FofAtomicFormula::Plain(FofPlainAtomicFormula(left)),
                )))
            }
        }),
        map(fof_unary_formula, |f| FofUnitFormula::Unary(Box::new(f))),
        map(fof_unitary_formula, |f| {
            FofUnitFormula::Unitary(Box::new(f))
        }),
    ))(x)
}

fn fof_nonassoc_suffix<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(NonassocConnective, FofUnitFormula), E> {
    pair(
        delimited(ignored, nonassoc_connective, ignored),
        fof_unit_formula,
    )(x)
}

pub fn fof_binary_nonassoc<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofBinaryNonassoc, E> {
    map(
        pair(fof_unit_formula, fof_nonassoc_suffix),
        |(left, (op, right))| FofBinaryNonassoc { left, op, right },
    )(x)
}

fn fof_or_suffix<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Vec<FofUnitFormula<'a>>, E> {
    many1(preceded(
        delimited(ignored, tag("|"), ignored),
        fof_unit_formula,
    ))(x)
}

pub fn fof_or_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofOrFormula, E> {
    map(pair(fof_unit_formula, fof_or_suffix), |(first, rest)| {
        let mut formulae = vec![first];
        formulae.extend(rest);
        FofOrFormula(formulae)
    })(x)
}

fn fof_and_suffix<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Vec<FofUnitFormula<'a>>, E> {
    many1(preceded(
        delimited(ignored, tag("&"), ignored),
        fof_unit_formula,
    ))(x)
}

pub fn fof_and_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofAndFormula, E> {
    map(pair(fof_unit_formula, fof_and_suffix), |(first, rest)| {
        let mut formulae = vec![first];
        formulae.extend(rest);
        FofAndFormula(formulae)
    })(x)
}

enum FofAssocSuffix<'a> {
    Or(Vec<FofUnitFormula<'a>>),
    And(Vec<FofUnitFormula<'a>>),
}

fn fof_assoc_suffix<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofAssocSuffix, E> {
    alt((
        map(fof_or_suffix, FofAssocSuffix::Or),
        map(fof_and_suffix, FofAssocSuffix::And),
    ))(x)
}

pub fn fof_binary_assoc<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofBinaryAssoc, E> {
    // avoid re-parsing `fof_unit_formula`
    map(
        pair(fof_unit_formula, fof_assoc_suffix),
        |(left, rest)| match rest {
            FofAssocSuffix::Or(rest) => {
                let mut formulae = vec![left];
                formulae.extend(rest);
                FofBinaryAssoc::Or(FofOrFormula(formulae))
            }
            FofAssocSuffix::And(rest) => {
                let mut formulae = vec![left];
                formulae.extend(rest);
                FofBinaryAssoc::And(FofAndFormula(formulae))
            }
        },
    )(x)
}

enum FofBinarySuffix<'a> {
    Assoc(FofAssocSuffix<'a>),
    Nonassoc(NonassocConnective, FofUnitFormula<'a>),
}

fn fof_binary_suffix<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofBinarySuffix, E> {
    alt((
        map(fof_assoc_suffix, FofBinarySuffix::Assoc),
        map(fof_nonassoc_suffix, |(op, right)| {
            FofBinarySuffix::Nonassoc(op, right)
        }),
    ))(x)
}

pub fn fof_binary_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofBinaryFormula, E> {
    // avoid re-parsing `fof_unit_formula`
    map(
        pair(fof_unit_formula, fof_binary_suffix),
        |(left, suffix)| match suffix {
            FofBinarySuffix::Assoc(FofAssocSuffix::Or(rest)) => {
                let mut formulae = vec![left];
                formulae.extend(rest);
                FofBinaryFormula::Assoc(FofBinaryAssoc::Or(FofOrFormula(
                    formulae,
                )))
            }
            FofBinarySuffix::Assoc(FofAssocSuffix::And(rest)) => {
                let mut formulae = vec![left];
                formulae.extend(rest);
                FofBinaryFormula::Assoc(FofBinaryAssoc::And(FofAndFormula(
                    formulae,
                )))
            }
            FofBinarySuffix::Nonassoc(op, right) => {
                FofBinaryFormula::Nonassoc(FofBinaryNonassoc {
                    left,
                    op,
                    right,
                })
            }
        },
    )(x)
}

pub fn fof_logic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofLogicFormula, E> {
    map(
        pair(fof_unit_formula, opt(fof_binary_suffix)),
        |(left, suffix)| {
            if let Some(suffix) = suffix {
                FofLogicFormula::Binary(match suffix {
                    FofBinarySuffix::Assoc(FofAssocSuffix::Or(rest)) => {
                        let mut formulae = vec![left];
                        formulae.extend(rest);
                        FofBinaryFormula::Assoc(FofBinaryAssoc::Or(
                            FofOrFormula(formulae),
                        ))
                    }
                    FofBinarySuffix::Assoc(FofAssocSuffix::And(rest)) => {
                        let mut formulae = vec![left];
                        formulae.extend(rest);
                        FofBinaryFormula::Assoc(FofBinaryAssoc::And(
                            FofAndFormula(formulae),
                        ))
                    }
                    FofBinarySuffix::Nonassoc(op, right) => {
                        FofBinaryFormula::Nonassoc(FofBinaryNonassoc {
                            left,
                            op,
                            right,
                        })
                    }
                })
            } else {
                match left {
                    FofUnitFormula::Unary(u) => FofLogicFormula::Unary(u),
                    FofUnitFormula::Unitary(u) => FofLogicFormula::Unitary(u),
                }
            }
        },
    )(x)
}

pub fn fof_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    map(fof_logic_formula, FofFormula)(x)
}

pub fn literal<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Literal, E> {
    alt((
        map(
            preceded(pair(tag("~"), ignored), fof_atomic_formula),
            Literal::NegatedAtomic,
        ),
        map(fof_infix_unary, Literal::Infix),
        map(fof_atomic_formula, Literal::Atomic),
    ))(x)
}

pub fn disjunction<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Disjunction, E> {
    map(
        separated_nonempty_list(tuple((ignored, tag("|"), ignored)), literal),
        Disjunction,
    )(x)
}

pub fn cnf_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<CnfFormula, E> {
    alt((
        map(
            delimited(
                pair(tag("("), ignored),
                disjunction,
                pair(ignored, tag(")")),
            ),
            CnfFormula::Parenthesised,
        ),
        map(disjunction, CnfFormula::Disjunction),
    ))(x)
}

pub fn formula_role<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FormulaRole, E> {
    alt((
        value(FormulaRole::Axiom, tag("axiom")),
        value(FormulaRole::Hypothesis, tag("hypothesis")),
        value(FormulaRole::Definition, tag("definition")),
        value(FormulaRole::Assumption, tag("assumption")),
        value(FormulaRole::Lemma, tag("lemma")),
        value(FormulaRole::Theorem, tag("theorem")),
        value(FormulaRole::Corollary, tag("corollary")),
        value(FormulaRole::Conjecture, tag("conjecture")),
        value(FormulaRole::NegatedConjecture, tag("negated_conjecture")),
        value(FormulaRole::Plain, tag("plain")),
        value(FormulaRole::Unknown, tag("unknown")),
    ))(x)
}

pub fn general_terms<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralTerms, E> {
    map(
        separated_nonempty_list(
            delimited(ignored, tag(","), ignored),
            general_term,
        ),
        GeneralTerms,
    )(x)
}

pub fn general_list<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralList, E> {
    map(
        delimited(
            pair(tag("["), ignored),
            opt(general_terms),
            pair(ignored, tag("]")),
        ),
        GeneralList,
    )(x)
}

pub fn general_function<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralFunction, E> {
    map(
        pair(
            atomic_word,
            delimited(
                tuple((ignored, tag("("), ignored)),
                general_terms,
                tuple((ignored, tag(")"))),
            ),
        ),
        |(word, terms)| GeneralFunction { word, terms },
    )(x)
}

pub fn formula_data<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FormulaData, E> {
    preceded(
        tag("$"),
        alt((
            map(
                delimited(
                    tuple((tag("fof"), ignored, tag("("), ignored)),
                    fof_formula,
                    tuple((ignored, tag(")"))),
                ),
                FormulaData::Fof,
            ),
            map(
                delimited(
                    tuple((tag("cnf"), ignored, tag("("), ignored)),
                    cnf_formula,
                    tuple((ignored, tag(")"))),
                ),
                FormulaData::Cnf,
            ),
        )),
    )(x)
}

pub fn general_data<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralData, E> {
    alt((
        map(general_function, GeneralData::Function),
        map(atomic_word, GeneralData::Atomic),
        map(variable, GeneralData::Variable),
        map(formula_data, GeneralData::Formula),
    ))(x)
}

pub fn general_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralTerm, E> {
    alt((
        map(general_list, GeneralTerm::List),
        map(
            pair(
                general_data,
                preceded(tuple((ignored, tag(":"), ignored)), general_term),
            ),
            |(d, f)| GeneralTerm::Colon(d, Box::new(f)),
        ),
        map(general_data, GeneralTerm::Data),
    ))(x)
}

pub fn source<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Source, E> {
    map(general_term, Source)(x)
}

pub fn useful_info<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<UsefulInfo, E> {
    map(general_list, UsefulInfo)(x)
}

pub fn optional_info<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<OptionalInfo, E> {
    map(
        opt(preceded(pair(tag(","), ignored), useful_info)),
        OptionalInfo,
    )(x)
}

pub fn annotations<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Annotations, E> {
    map(
        opt(preceded(
            pair(tag(","), ignored),
            pair(source, preceded(ignored, optional_info)),
        )),
        Annotations,
    )(x)
}

pub fn fof_annotated<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofAnnotated, E> {
    map(
        delimited(
            tuple((tag("fof"), ignored, tag("("), ignored)),
            tuple((
                name,
                preceded(delimited(ignored, tag(","), ignored), formula_role),
                preceded(delimited(ignored, tag(","), ignored), fof_formula),
                preceded(ignored, annotations),
            )),
            tuple((ignored, tag(")"), ignored, tag("."))),
        ),
        |(name, role, formula, annotations)| FofAnnotated {
            name,
            role,
            formula,
            annotations,
        },
    )(x)
}

pub fn cnf_annotated<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<CnfAnnotated, E> {
    map(
        delimited(
            tuple((tag("cnf"), ignored, tag("("), ignored)),
            tuple((
                name,
                preceded(delimited(ignored, tag(","), ignored), formula_role),
                preceded(delimited(ignored, tag(","), ignored), cnf_formula),
                preceded(ignored, annotations),
            )),
            tuple((ignored, tag(")"), ignored, tag("."))),
        ),
        |(name, role, formula, annotations)| CnfAnnotated {
            name,
            role,
            formula,
            annotations,
        },
    )(x)
}

pub fn annotated_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<AnnotatedFormula, E> {
    alt((
        map(fof_annotated, AnnotatedFormula::Fof),
        map(cnf_annotated, AnnotatedFormula::Cnf),
    ))(x)
}

pub fn file_name<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FileName, E> {
    map(single_quoted, FileName)(x)
}

pub fn name_list<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<NameList, E> {
    map(
        separated_nonempty_list(tuple((ignored, tag(","), ignored)), name),
        NameList,
    )(x)
}

pub fn formula_selection<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FormulaSelection, E> {
    map(
        opt(delimited(
            tuple((tag(","), ignored, tag("["), ignored)),
            name_list,
            tuple((ignored, tag("]"))),
        )),
        FormulaSelection,
    )(x)
}

pub fn include<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Include, E> {
    map(
        delimited(
            tuple((tag("include"), ignored, tag("("), ignored)),
            pair(file_name, preceded(ignored, formula_selection)),
            tuple((ignored, tag(")"), ignored, tag("."))),
        ),
        |(file_name, selection)| Include {
            file_name,
            selection,
        },
    )(x)
}

pub fn tptp_input<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<TPTPInput, E> {
    alt((
        map(annotated_formula, |x| TPTPInput::Annotated(Box::new(x))),
        map(include, TPTPInput::Include),
    ))(x)
}

/// iterator returning `tptp_input`, separated with `ignored`.
///
/// Convenience function not in TPTP BNF.
/// Call `.finish()` to check for error conditions and get remaining input.
pub fn tptp_input_iterator<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParserIterator<&[u8], E, impl Fn(&'a [u8]) -> ParseResult<TPTPInput, E>> {
    iterator(x, delimited(ignored, tptp_input, ignored))
}
