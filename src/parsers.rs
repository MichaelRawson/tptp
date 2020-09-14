use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::str;
use alloc::vec;
use alloc::vec::Vec;
use nom::branch::alt;
use nom::bytes::streaming::{
    escaped, tag, take_until, take_while, take_while1,
};
use nom::character::complete::multispace1;
use nom::character::streaming::{
    digit0, digit1, line_ending, not_line_ending, one_of,
};
use nom::combinator::{cut, map, opt, peek, recognize, value};
use nom::error::ParseError;
use nom::multi::{fold_many0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

use crate::syntax::*;

type ParseResult<'a, T, E> = nom::IResult<&'a [u8], T, E>;

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

pub fn is_visible(c: u8) -> bool {
    c >= b' ' && c <= b'~'
}

pub fn is_sq_char(c: u8) -> bool {
    is_visible(c) && c != b'\'' && c != b'\\'
}

pub fn is_do_char(c: u8) -> bool {
    is_visible(c) && c != b'"' && c != b'\\'
}

pub fn whitespace<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(), E> {
    value((), multispace1)(x)
}

pub fn comment_line<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(), E> {
    preceded(
        tag("%"),
        cut(terminated(value((), not_line_ending), line_ending)),
    )(x)
}

pub fn comment_block<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(), E> {
    preceded(
        tag("/*"),
        cut(terminated(value((), take_until("*/")), tag("*/"))),
    )(x)
}

fn single_ignored<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(), E> {
    alt((whitespace, comment_line, comment_block))(x)
}

/// zero or more `whitespace`, `comment_line`, or `comment_block`
pub fn ignored<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(), E> {
    fold_many0(single_ignored, (), |_, _| ())(x)
}

fn lower_alpha1<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<&[u8], E> {
    take_while1(is_lower_alpha)(x)
}

fn upper_alpha1<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<&[u8], E> {
    take_while1(is_upper_alpha)(x)
}

fn alphanumeric<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<&[u8], E> {
    take_while(is_alphanumeric)(x)
}

pub fn lower_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<LowerWord, E> {
    map(recognize(preceded(lower_alpha1, cut(alphanumeric))), |w| {
        LowerWord(Cow::Borrowed(to_str(w)))
    })(x)
}

pub fn upper_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<UpperWord, E> {
    map(recognize(preceded(upper_alpha1, cut(alphanumeric))), |w| {
        UpperWord(Cow::Borrowed(to_str(w)))
    })(x)
}

pub fn single_quoted<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<SingleQuoted, E> {
    map(
        preceded(
            tag("'"),
            cut(terminated(
                escaped(take_while1(is_sq_char), '\\', one_of("\\'")),
                tag("'"),
            )),
        ),
        |w| SingleQuoted(Cow::Borrowed(to_str(w))),
    )(x)
}

pub fn distinct_object<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<DistinctObject, E> {
    map(
        preceded(
            tag("\""),
            cut(terminated(
                escaped(take_while1(is_do_char), '\\', one_of("\\\"")),
                tag("\""),
            )),
        ),
        |w| DistinctObject(Cow::Borrowed(to_str(w))),
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
    map(preceded(tag("$"), cut(lower_word)), DollarWord)(x)
}

pub fn dollar_dollar_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<DollarDollarWord, E> {
    map(preceded(tag("$$"), cut(lower_word)), DollarDollarWord)(x)
}

pub fn integer<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Integer, E> {
    map(
        recognize(preceded(
            opt(one_of("+-")),
            alt((tag("0"), preceded(one_of("123456789"), digit0))),
        )),
        |w| Integer(Cow::Borrowed(to_str(w))),
    )(x)
}

pub fn rational<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Rational, E> {
    map(
        recognize(tuple((integer, tag(b"/"), one_of("123456789"), digit0))),
        |w| Rational(Cow::Borrowed(to_str(w))),
    )(x)
}

pub fn real<'a, E: ParseError<&'a [u8]>>(x: &'a [u8]) -> ParseResult<Real, E> {
    fn exp_integer<'a, E: ParseError<&'a [u8]>>(
        x: &'a [u8],
    ) -> ParseResult<(), E> {
        value((), preceded(opt(one_of("+-")), digit1))(x)
    }

    map(
        recognize(tuple((
            integer,
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
        |w| Real(Cow::Borrowed(to_str(w))),
    )(x)
}

pub fn number<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Number, E> {
    alt((
        map(real, Number::Real),
        map(rational, Number::Rational),
        map(integer, Number::Integer),
    ))(x)
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

pub fn atomic_system_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<AtomicSystemWord, E> {
    map(dollar_dollar_word, AtomicSystemWord)(x)
}

pub fn atomic_defined_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<AtomicDefinedWord, E> {
    map(dollar_word, AtomicDefinedWord)(x)
}

pub fn system_functor<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<SystemFunctor, E> {
    map(atomic_system_word, SystemFunctor)(x)
}

pub fn system_constant<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<SystemConstant, E> {
    map(system_functor, SystemConstant)(x)
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

pub fn defined_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<DefinedTerm, E> {
    alt((
        map(number, DefinedTerm::Number),
        map(distinct_object, DefinedTerm::Distinct),
    ))(x)
}

pub fn functor<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Functor, E> {
    map(atomic_word, Functor)(x)
}

pub fn constant<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Constant, E> {
    map(functor, Constant)(x)
}

pub fn fof_arguments<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofArguments, E> {
    map(
        delimited(
            tag("("),
            separated_list1(tag(","), delimited(ignored, fof_term, ignored)),
            tag(")"),
        ),
        FofArguments,
    )(x)
}

pub fn fof_system_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofSystemTerm, E> {
    map(
        pair(system_functor, opt(preceded(ignored, fof_arguments))),
        |(f, args)| match args {
            None => FofSystemTerm::Constant(SystemConstant(f)),
            Some(args) => FofSystemTerm::Function(f, args),
        },
    )(x)
}

pub fn fof_plain_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofPlainTerm, E> {
    map(
        pair(functor, opt(preceded(ignored, fof_arguments))),
        |(f, args)| match args {
            None => FofPlainTerm::Constant(Constant(f)),
            Some(args) => FofPlainTerm::Function(f, Box::new(args)),
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
    alt((
        map(defined_term, FofDefinedTerm::Defined),
        map(fof_defined_atomic_term, FofDefinedTerm::Atomic),
    ))(x)
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
        map(fof_function_term, |f| FofTerm::Function(Box::new(f))),
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

fn fof_defined_infix_formula_tail<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(DefinedInfixPred, FofTerm), E> {
    pair(
        preceded(ignored, defined_infix_pred),
        preceded(ignored, fof_term),
    )(x)
}

pub fn fof_defined_infix_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofDefinedInfixFormula, E> {
    map(
        pair(fof_term, fof_defined_infix_formula_tail),
        |(left, (op, right))| {
            let left = Box::new(left);
            let right = Box::new(right);
            FofDefinedInfixFormula { left, op, right }
        },
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

pub fn fof_system_atomic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofSystemAtomicFormula, E> {
    map(fof_system_term, FofSystemAtomicFormula)(x)
}

pub fn fof_atomic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofAtomicFormula, E> {
    alt((
        map(
            pair(fof_plain_term, opt(fof_defined_infix_formula_tail)),
            |(left, possible_right)| match possible_right {
                Some((op, right)) => {
                    let left = Box::new(FofTerm::Function(Box::new(
                        FofFunctionTerm::Plain(left),
                    )));
                    let right = Box::new(right);
                    let infix = FofDefinedInfixFormula { left, op, right };
                    let defined = FofDefinedAtomicFormula::Infix(infix);
                    FofAtomicFormula::Defined(Box::new(defined))
                }
                None => FofAtomicFormula::Plain(Box::new(
                    FofPlainAtomicFormula(left),
                )),
            },
        ),
        map(fof_system_atomic_formula, |f| {
            FofAtomicFormula::System(Box::new(f))
        }),
        map(fof_defined_atomic_formula, |f| {
            FofAtomicFormula::Defined(Box::new(f))
        }),
    ))(x)
}

pub fn fof_variable_list<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofVariableList, E> {
    map(
        separated_list1(tuple((ignored, tag(","), ignored)), variable),
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
            map(fof_unit_formula, Box::new),
        )),
        |(quantifier, bound, formula)| FofQuantifiedFormula {
            quantifier,
            bound,
            formula,
        },
    )(x)
}

fn fof_infix_unary_tail<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<(InfixInequality, FofTerm), E> {
    pair(
        preceded(ignored, infix_inequality),
        preceded(ignored, fof_term),
    )(x)
}

pub fn fof_infix_unary<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofInfixUnary, E> {
    map(
        pair(fof_term, fof_infix_unary_tail),
        |(left, (op, right))| {
            let left = Box::new(left);
            let right = Box::new(right);
            FofInfixUnary { left, op, right }
        },
    )(x)
}

pub fn fof_unary_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofUnaryFormula, E> {
    alt((
        map(
            pair(unary_connective, preceded(ignored, fof_unit_formula)),
            |(c, f)| FofUnaryFormula::Unary(c, Box::new(f)),
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
                map(fof_logic_formula, Box::new),
                pair(ignored, tag(")")),
            ),
            FofUnitaryFormula::Parenthesised,
        ),
        map(fof_atomic_formula, FofUnitaryFormula::Atomic),
    ))(x)
}

enum FofUnitFormulaTail<'a> {
    Equal(DefinedInfixPred, Box<FofTerm<'a>>),
    NotEqual(InfixInequality, Box<FofTerm<'a>>),
}

fn fof_unit_formula_tail<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofUnitFormulaTail, E> {
    preceded(
        ignored,
        alt((
            map(
                pair(defined_infix_pred, preceded(ignored, fof_term)),
                |(op, right)| FofUnitFormulaTail::Equal(op, Box::new(right)),
            ),
            map(
                pair(infix_inequality, preceded(ignored, fof_term)),
                |(op, right)| {
                    FofUnitFormulaTail::NotEqual(op, Box::new(right))
                },
            ),
        )),
    )(x)
}

pub fn fof_unit_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofUnitFormula, E> {
    alt((
        map(
            pair(fof_plain_term, opt(fof_unit_formula_tail)),
            |(left, rest)| match rest {
                Some(rest) => {
                    let left = Box::new(FofTerm::Function(Box::new(
                        FofFunctionTerm::Plain(left),
                    )));
                    match rest {
                        FofUnitFormulaTail::Equal(op, right) => {
                            let infix =
                                FofDefinedInfixFormula { left, op, right };
                            let defined =
                                FofDefinedAtomicFormula::Infix(infix);
                            let atomic =
                                FofAtomicFormula::Defined(Box::new(defined));
                            let unitary = FofUnitaryFormula::Atomic(atomic);
                            FofUnitFormula::Unitary(unitary)
                        }
                        FofUnitFormulaTail::NotEqual(op, right) => {
                            let infix = FofInfixUnary { left, op, right };
                            let unary = FofUnaryFormula::InfixUnary(infix);
                            FofUnitFormula::Unary(unary)
                        }
                    }
                }
                None => {
                    let plain = FofPlainAtomicFormula(left);
                    let atomic = FofAtomicFormula::Plain(Box::new(plain));
                    let unitary = FofUnitaryFormula::Atomic(atomic);
                    FofUnitFormula::Unitary(unitary)
                }
            },
        ),
        map(fof_unary_formula, FofUnitFormula::Unary),
        map(fof_unitary_formula, FofUnitFormula::Unitary),
    ))(x)
}

fn fof_binary_nonassoc_impl<'a, E: ParseError<&'a [u8]>>(
    left: FofUnitFormula<'a>,
    x: &'a [u8],
) -> ParseResult<'a, FofBinaryNonassoc<'a>, E> {
    let (x, (op, right)) =
        pair(nonassoc_connective, preceded(ignored, fof_unit_formula))(x)?;
    let left = Box::new(left);
    let right = Box::new(right);
    Ok((x, FofBinaryNonassoc { left, op, right }))
}

pub fn fof_binary_nonassoc<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofBinaryNonassoc, E> {
    let (x, left) = terminated(fof_unit_formula, ignored)(x)?;
    fof_binary_nonassoc_impl(left, x)
}

fn fof_assoc_impl<'a, E: ParseError<&'a [u8]>>(
    first: FofUnitFormula<'a>,
    sep: &'static [u8],
    x: &'a [u8],
) -> ParseResult<'a, Vec<FofUnitFormula<'a>>, E> {
    let (x, second) = preceded(pair(tag(sep), ignored), fof_unit_formula)(x)?;
    fold_many0(
        preceded(tuple((ignored, tag(sep), ignored)), fof_unit_formula),
        vec![first, second],
        |mut result, next| {
            result.push(next);
            result
        },
    )(x)
}

fn fof_or_impl<'a, E: ParseError<&'a [u8]>>(
    first: FofUnitFormula<'a>,
    x: &'a [u8],
) -> ParseResult<'a, FofOrFormula<'a>, E> {
    let (x, formulae) = fof_assoc_impl(first, b"|", x)?;
    Ok((x, FofOrFormula(formulae)))
}

pub fn fof_or_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofOrFormula, E> {
    let (x, first) = terminated(fof_unit_formula, ignored)(x)?;
    fof_or_impl(first, x)
}

fn fof_and_impl<'a, E: ParseError<&'a [u8]>>(
    first: FofUnitFormula<'a>,
    x: &'a [u8],
) -> ParseResult<'a, FofAndFormula<'a>, E> {
    let (x, formulae) = fof_assoc_impl(first, b"&", x)?;
    Ok((x, FofAndFormula(formulae)))
}

pub fn fof_and_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofAndFormula, E> {
    let (x, first) = terminated(fof_unit_formula, ignored)(x)?;
    fof_and_impl(first, x)
}

fn fof_binary_assoc_impl<'a, E: ParseError<&'a [u8]>>(
    first: FofUnitFormula<'a>,
    sep: char,
    x: &'a [u8],
) -> ParseResult<'a, FofBinaryAssoc<'a>, E> {
    if sep == '|' {
        let (x, or) = fof_or_impl(first, x)?;
        Ok((x, FofBinaryAssoc::Or(or)))
    } else {
        let (x, and) = fof_and_impl(first, x)?;
        Ok((x, FofBinaryAssoc::And(and)))
    }
}

pub fn fof_binary_assoc<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofBinaryAssoc, E> {
    let (x, (first, sep)) =
        pair(fof_unit_formula, preceded(ignored, peek(one_of("&|"))))(x)?;
    fof_binary_assoc_impl(first, sep, x)
}

fn fof_binary_formula_impl<'a, E: ParseError<&'a [u8]>>(
    first: FofUnitFormula<'a>,
    sep: char,
    x: &'a [u8],
) -> ParseResult<'a, FofBinaryFormula<'a>, E> {
    match sep {
        '&' | '|' => {
            let (x, assoc) = fof_binary_assoc_impl(first, sep, x)?;
            Ok((x, FofBinaryFormula::Assoc(assoc)))
        }
        _ => {
            let (x, nonassoc) = fof_binary_nonassoc_impl(first, x)?;
            Ok((x, FofBinaryFormula::Nonassoc(Box::new(nonassoc))))
        }
    }
}

pub fn fof_binary_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofBinaryFormula, E> {
    let (x, (first, sep)) =
        pair(fof_unit_formula, preceded(ignored, peek(one_of("&|~<="))))(x)?;
    fof_binary_formula_impl(first, sep, x)
}

pub fn fof_logic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofLogicFormula, E> {
    let (x_after_first, first) = fof_unit_formula(x)?;
    let (x_after_ignored, _) = ignored(x_after_first)?;
    let (_, sep) = peek(opt(one_of("&|~<=")))(x_after_ignored)?;
    match sep {
        Some(sep) => {
            let (x, binary) =
                fof_binary_formula_impl(first, sep, x_after_ignored)?;
            Ok((x, FofLogicFormula::Binary(binary)))
        }
        None => {
            let result = match first {
                FofUnitFormula::Unary(u) => FofLogicFormula::Unary(u),
                FofUnitFormula::Unitary(u) => FofLogicFormula::Unitary(u),
            };
            Ok((x_after_first, result))
        }
    }
}

pub fn fof_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    map(fof_logic_formula, FofFormula)(x)
}

enum LiteralTail<'a> {
    Equal(DefinedInfixPred, Box<FofTerm<'a>>),
    NotEqual(InfixInequality, Box<FofTerm<'a>>),
}

fn literal_tail<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<LiteralTail<'a>, E> {
    preceded(
        ignored,
        alt((
            map(
                pair(defined_infix_pred, preceded(ignored, fof_term)),
                |(op, right)| LiteralTail::Equal(op, Box::new(right)),
            ),
            map(
                pair(infix_inequality, preceded(ignored, fof_term)),
                |(op, right)| LiteralTail::NotEqual(op, Box::new(right)),
            ),
        )),
    )(x)
}

pub fn literal<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Literal, E> {
    alt((
        map(
            preceded(pair(tag("~"), ignored), fof_atomic_formula),
            Literal::NegatedAtomic,
        ),
        map(
            pair(fof_plain_term, opt(literal_tail)),
            |(left, rest)| match rest {
                Some(rest) => {
                    let left = Box::new(FofTerm::Function(Box::new(
                        FofFunctionTerm::Plain(left),
                    )));
                    match rest {
                        LiteralTail::Equal(op, right) => {
                            let infix =
                                FofDefinedInfixFormula { left, op, right };
                            let defined =
                                FofDefinedAtomicFormula::Infix(infix);
                            let atomic =
                                FofAtomicFormula::Defined(Box::new(defined));
                            Literal::Atomic(atomic)
                        }
                        LiteralTail::NotEqual(op, right) => {
                            let infix = FofInfixUnary { left, op, right };
                            Literal::Infix(infix)
                        }
                    }
                }
                None => {
                    let plain = FofPlainAtomicFormula(left);
                    let atomic = FofAtomicFormula::Plain(Box::new(plain));
                    Literal::Atomic(atomic)
                }
            },
        ),
        map(pair(fof_term, literal_tail), |(left, rest)| {
            let left = Box::new(left);
            match rest {
                LiteralTail::Equal(op, right) => {
                    let infix = FofDefinedInfixFormula { left, op, right };
                    let defined = FofDefinedAtomicFormula::Infix(infix);
                    let atomic = FofAtomicFormula::Defined(Box::new(defined));
                    Literal::Atomic(atomic)
                }
                LiteralTail::NotEqual(op, right) => {
                    let infix = FofInfixUnary { left, op, right };
                    Literal::Infix(infix)
                }
            }
        }),
        map(fof_defined_atomic_formula, |f| {
            Literal::Atomic(FofAtomicFormula::Defined(Box::new(f)))
        }),
    ))(x)
}

pub fn disjunction<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Disjunction, E> {
    map(
        separated_list1(tuple((ignored, tag("|"), ignored)), literal),
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
        value(FormulaRole::Type, tag("type")),
        value(FormulaRole::FiDomain, tag("fi_domain")),
        value(FormulaRole::FiFunctors, tag("fi_functors")),
        value(FormulaRole::FiPredicates, tag("fi_predicates")),
        value(FormulaRole::Unknown, tag("unknown")),
    ))(x)
}

pub fn general_terms<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralTerms, E> {
    map(
        separated_list1(delimited(ignored, tag(","), ignored), general_term),
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

fn general_function_tail<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralTerms, E> {
    delimited(
        delimited(ignored, tag("("), ignored),
        general_terms,
        preceded(ignored, tag(")")),
    )(x)
}

pub fn general_function<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralFunction, E> {
    map(pair(atomic_word, general_function_tail), |(word, terms)| {
        GeneralFunction { word, terms }
    })(x)
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
            map(
                delimited(
                    tuple((tag("fot"), ignored, tag("("), ignored)),
                    fof_term,
                    tuple((ignored, tag(")"))),
                ),
                FormulaData::Fot,
            ),
        )),
    )(x)
}

pub fn general_data<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<GeneralData, E> {
    alt((
        map(
            pair(atomic_word, opt(general_function_tail)),
            |(word, tail)| {
                if let Some(terms) = tail {
                    let f = GeneralFunction { word, terms };
                    GeneralData::Function(Box::new(f))
                } else {
                    GeneralData::Atomic(word)
                }
            },
        ),
        map(variable, GeneralData::Variable),
        map(number, GeneralData::Number),
        map(distinct_object, GeneralData::DistinctObject),
        map(formula_data, |f| GeneralData::Formula(Box::new(f))),
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
                opt(preceded(
                    delimited(ignored, tag(":"), ignored),
                    general_term,
                )),
            ),
            |(left, right)| {
                let left = Box::new(left);
                if let Some(right) = right {
                    GeneralTerm::Colon(left, Box::new(right))
                } else {
                    GeneralTerm::Data(left)
                }
            },
        ),
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
                map(name, Box::new),
                preceded(delimited(ignored, tag(","), ignored), formula_role),
                map(
                    preceded(
                        delimited(ignored, tag(","), ignored),
                        fof_formula,
                    ),
                    Box::new,
                ),
                map(preceded(ignored, annotations), Box::new),
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
                map(name, Box::new),
                preceded(delimited(ignored, tag(","), ignored), formula_role),
                map(
                    preceded(
                        delimited(ignored, tag(","), ignored),
                        cnf_formula,
                    ),
                    Box::new,
                ),
                map(preceded(ignored, annotations), Box::new),
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
        separated_list1(tuple((ignored, tag(","), ignored)), name),
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
        map(annotated_formula, TPTPInput::Annotated),
        map(include, TPTPInput::Include),
    ))(x)
}

/// iterator returning `tptp_input`, separated with `ignored`.
///
/// Convenience structure not in TPTP BNF.
pub struct TPTPIterator<'a, E> {
    pub remaining: &'a [u8],
    _phantom: core::marker::PhantomData<E>,
}

impl<'a, E> TPTPIterator<'a, E> {
    pub fn new(remaining: &'a [u8]) -> Self {
        let _phantom = core::marker::PhantomData;
        Self {
            remaining,
            _phantom,
        }
    }
}

impl<'a, E: ParseError<&'a [u8]>> Iterator for TPTPIterator<'a, E> {
    type Item = Result<TPTPInput<'a>, E>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match single_ignored::<E>(self.remaining) {
                Ok((remaining, ())) => {
                    self.remaining = remaining;
                }
                Err(nom::Err::Incomplete(_)) => {
                    return None;
                }
                Err(_) => {
                    break;
                }
            }
        }

        match tptp_input::<E>(self.remaining) {
            Ok((remaining, input)) => {
                self.remaining = remaining;
                Some(Ok(input))
            }
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                Some(Err(e))
            }
            Err(nom::Err::Incomplete(_)) => None,
        }
    }
}
