use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_until, take_while, take_while1};
use nom::character::complete::{digit0, multispace1, one_of};
use nom::combinator::{map, map_opt, opt, recognize, value};
use nom::error::ParseError;
use nom::multi::{many0, separated_list, separated_nonempty_list};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use std::borrow::Cow;
use std::str;

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
) -> ParseResult<&[u8], E> {
    multispace1(x)
}

pub fn comment_line<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<&[u8], E> {
    recognize(tuple((tag("%"), take_until("\n"), tag("\n"))))(x)
}

pub fn comment_block<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<&[u8], E> {
    recognize(tuple((tag("/*"), take_until("*/"), tag("*/"))))(x)
}

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
) -> ParseResult<Cow<str>, E> {
    map(recognize(preceded(lower_alpha, alphanumeric)), |w| {
        Cow::Borrowed(unsafe { to_str(w) })
    })(x)
}

pub fn upper_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Cow<str>, E> {
    map(recognize(preceded(upper_alpha, alphanumeric)), |w| {
        Cow::Borrowed(unsafe { to_str(w) })
    })(x)
}

pub fn single_quoted<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Cow<str>, E> {
    map(
        recognize(delimited(
            tag("'"),
            escaped(take_while1(is_sq_char), '\\', one_of("\\'")),
            tag("'"),
        )),
        |w| Cow::Borrowed(unsafe { to_str(w) }),
    )(x)
}

pub fn atomic_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Cow<str>, E> {
    alt((lower_word, single_quoted))(x)
}

pub fn dollar_word<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Cow<str>, E> {
    preceded(tag("$"), lower_word)(x)
}

pub fn integer<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Cow<str>, E> {
    map(
        recognize(preceded(
            opt(one_of("+-")),
            alt((tag("0"), preceded(one_of("123456789"), digit0))),
        )),
        |w| Cow::Borrowed(unsafe { to_str(w) }),
    )(x)
}

pub fn name<'a, E: ParseError<&'a [u8]>>(x: &'a [u8]) -> ParseResult<Name, E> {
    alt((
        map(lower_word, Name::LowerWord),
        map(integer, Name::Integer),
        map(single_quoted, Name::SingleQuoted),
    ))(x)
}

pub fn name_list<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Vec<Name>, E> {
    delimited(
        pair(tag("["), ignored),
        separated_list(tuple((ignored, tag(","), ignored)), name),
        pair(ignored, tag("]")),
    )(x)
}

pub fn fof_arguments<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Vec<FofTerm>, E> {
    delimited(
        pair(tag("("), ignored),
        separated_list(tuple((ignored, tag(","), ignored)), fof_term),
        pair(ignored, tag(")")),
    )(x)
}

pub fn variable<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Variable, E> {
    map(upper_word, Variable)(x)
}

fn variable_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofTerm, E> {
    map(variable, FofTerm::Variable)(x)
}

pub fn fof_plain_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofTerm, E> {
    map(
        pair(name, preceded(ignored, opt(fof_arguments))),
        |(name, args)| match args {
            Some(args) => FofTerm::Functor(name, args),
            None => FofTerm::Constant(name),
        },
    )(x)
}

pub fn fof_function_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofTerm, E> {
    fof_plain_term(x)
}

pub fn fof_term<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofTerm, E> {
    alt((fof_function_term, variable_term))(x)
}

pub fn fof_defined_atomic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    map_opt(dollar_word, |w| match w.as_ref() {
        "true" => Some(FofFormula::Boolean(true)),
        "false" => Some(FofFormula::Boolean(false)),
        _ => None,
    })(x)
}

pub fn infix_equality<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<InfixEquality, E> {
    alt((
        value(InfixEquality::Equal, tag("=")),
        value(InfixEquality::NotEqual, tag("!=")),
    ))(x)
}

pub fn fof_atomic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    alt((
        map(
            tuple((
                variable_term,
                preceded(ignored, infix_equality),
                preceded(ignored, fof_term),
            )),
            |(left, op, right)| FofFormula::Infix(op, left, right),
        ),
        map(
            pair(
                fof_function_term,
                opt(pair(
                    preceded(ignored, infix_equality),
                    preceded(ignored, fof_term),
                )),
            ),
            |(left, rest)| {
                if let Some((op, right)) = rest {
                    FofFormula::Infix(op, left, right)
                } else {
                    match left {
                        FofTerm::Constant(name) => {
                            FofFormula::Proposition(name)
                        }
                        FofTerm::Functor(name, args) => {
                            FofFormula::Predicate(name, args)
                        }
                        _ => unreachable!(),
                    }
                }
            },
        ),
        fof_defined_atomic_formula,
    ))(x)
}

pub fn fof_quantified_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    map(
        tuple((
            alt((
                value(FofQuantifier::Forall, tag("!")),
                value(FofQuantifier::Exists, tag("?")),
            )),
            preceded(
                tuple((ignored, tag("["), ignored)),
                separated_nonempty_list(
                    tuple((ignored, tag(","), ignored)),
                    variable,
                ),
            ),
            preceded(
                tuple((ignored, tag("]"), ignored, tag(":"), ignored)),
                fof_unit_formula,
            ),
        )),
        |(quantifier, bound, formula)| {
            FofFormula::Quantified(quantifier, bound, Box::new(formula))
        },
    )(x)
}

pub fn fof_unitary_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    alt((
        delimited(
            pair(tag("("), ignored),
            fof_logic_formula,
            pair(ignored, tag(")")),
        ),
        fof_atomic_formula,
        fof_quantified_formula,
    ))(x)
}

pub fn fof_unary_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    map(
        preceded(pair(tag("~"), ignored), fof_unit_formula),
        |formula| FofFormula::Unary(UnaryConnective::Not, Box::new(formula)),
    )(x)
}

pub fn fof_unit_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    alt((fof_unary_formula, fof_unitary_formula))(x)
}

pub fn nonassoc_connective<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<NonAssocConnective, E> {
    alt((
        value(NonAssocConnective::LRImplies, tag("=>")),
        value(NonAssocConnective::Equivalent, tag("<=>")),
        value(NonAssocConnective::RLImplies, tag("<=")),
        value(NonAssocConnective::NotEquivalent, tag("<~>")),
        value(NonAssocConnective::NotAnd, tag("~&")),
        value(NonAssocConnective::NotOr, tag("~|")),
    ))(x)
}

pub fn assoc_connective<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<AssocConnective, E> {
    alt((
        value(AssocConnective::And, tag("&")),
        value(AssocConnective::Or, tag("|")),
    ))(x)
}

pub fn fof_logic_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    let (x, first) = alt((fof_unary_formula, fof_unit_formula))(x)?;
    if let Ok((x, op)) = delimited::<_, _, _, _, E, _, _, _>(
        ignored,
        nonassoc_connective,
        ignored,
    )(x)
    {
        let (x, second) = fof_unit_formula(x)?;
        Ok((
            x,
            FofFormula::NonAssoc(op, Box::new(first), Box::new(second)),
        ))
    } else if let Ok((x, op)) =
        delimited::<_, _, _, _, E, _, _, _>(ignored, assoc_connective, ignored)(
            x,
        )
    {
        let sep = tag(match op {
            AssocConnective::And => "&",
            AssocConnective::Or => "|",
        });
        let mut subformulae = vec![first];
        let (x, rest) = separated_nonempty_list(
            delimited(ignored, sep, ignored),
            fof_unit_formula,
        )(x)?;
        subformulae.extend(rest);
        Ok((x, FofFormula::Assoc(op, subformulae)))
    } else {
        Ok((x, first))
    }
}

pub fn fof_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<FofFormula, E> {
    fof_logic_formula(x)
}

pub fn literal<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Literal, E> {
    alt((
        map(
            preceded(pair(tag("~"), ignored), fof_atomic_formula),
            Literal::NegatedLiteral,
        ),
        map(fof_atomic_formula, |l| match l {
            FofFormula::Infix(_, _, _) => Literal::EqualityLiteral(l),
            _ => Literal::Literal(l),
        }),
    ))(x)
}

pub fn disjunction<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Vec<Literal>, E> {
    separated_nonempty_list(tuple((ignored, tag("|"), ignored)), literal)(x)
}

pub fn cnf_formula<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<CnfFormula, E> {
    map(
        alt((
            delimited(
                pair(tag("("), ignored),
                disjunction,
                pair(ignored, tag(")")),
            ),
            disjunction,
        )),
        CnfFormula,
    )(x)
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

pub fn unknown_source<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Source, E> {
    value(Source::Unknown, tag("unknown"))(x)
}

pub fn dag_source<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<DagSource, E> {
    alt((
        map(
            pair(
                preceded(
                    tuple((tag("inference"), ignored, tag("("), ignored)),
                    atomic_word,
                ),
                delimited(
                    tuple((
                        ignored,
                        tag(","),
                        ignored,
                        tag("["),
                        ignored,
                        tag("]"),
                        ignored,
                        tag(","),
                        ignored,
                    )),
                    sources,
                    pair(ignored, tag(")")),
                ),
            ),
            |(rule, parents)| DagSource::Inference(rule, parents),
        ),
        map(name, DagSource::Name),
    ))(x)
}

pub fn external_source<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<ExternalSource, E> {
    map(
        delimited(
            tuple((tag("file"), ignored, tag("("), ignored)),
            pair(
                single_quoted,
                opt(preceded(tuple((ignored, tag(","), ignored)), name)),
            ),
            pair(ignored, tag(")")),
        ),
        |(file, info)| ExternalSource::File(file, info),
    )(x)
}

pub fn sources<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Vec<Source>, E> {
    delimited(
        pair(tag("["), ignored),
        separated_list(tuple((ignored, tag(","), ignored)), source),
        pair(ignored, tag("]")),
    )(x)
}

pub fn source<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Source, E> {
    alt((
        unknown_source,
        map(external_source, Source::External),
        map(dag_source, Source::Dag),
        map(sources, Source::Sources),
    ))(x)
}

pub fn annotations<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Annotations, E> {
    map(source, |source| Annotations { source })(x)
}

pub fn include_path<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Included, E> {
    map(
        delimited(
            tag("'"),
            escaped(take_while1(is_sq_char), '\\', one_of("\\'")),
            tag("'"),
        ),
        |w| Included(unsafe { to_str(w) }.into()),
    )(x)
}

pub fn include<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Statement, E> {
    map(
        delimited(
            tuple((tag("include"), ignored, tag("("), ignored)),
            pair(
                include_path,
                opt(preceded(tuple((ignored, tag(","), ignored)), name_list)),
            ),
            pair(ignored, tag(")")),
        ),
        |(included, selection)| Statement::Include(included, selection),
    )(x)
}

pub fn fof_annotated<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Statement, E> {
    map(
        delimited(
            tuple((tag("fof"), ignored, tag("("), ignored)),
            tuple((
                name,
                preceded(tuple((ignored, tag(","), ignored)), formula_role),
                preceded(tuple((ignored, tag(","), ignored)), fof_formula),
                opt(preceded(tuple((ignored, tag(","), ignored)), annotations)),
            )),
            pair(ignored, tag(")")),
        ),
        |(name, role, formula, annotations)| {
            Statement::Fof(name, role, formula, annotations)
        },
    )(x)
}

pub fn cnf_annotated<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Statement, E> {
    map(
        delimited(
            tuple((tag("cnf"), ignored, tag("("), ignored)),
            tuple((
                name,
                preceded(tuple((ignored, tag(","), ignored)), formula_role),
                preceded(tuple((ignored, tag(","), ignored)), cnf_formula),
                opt(preceded(tuple((ignored, tag(","), ignored)), annotations)),
            )),
            pair(ignored, tag(")")),
        ),
        |(name, role, formula, annotations)| {
            Statement::Cnf(name, role, formula, annotations)
        },
    )(x)
}

pub fn tptp_input<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Statement, E> {
    terminated(
        alt((include, fof_annotated, cnf_annotated)),
        pair(ignored, tag(".")),
    )(x)
}

pub fn tptp_input_or_eof<'a, E: ParseError<&'a [u8]>>(
    x: &'a [u8],
) -> ParseResult<Option<Statement>, E> {
    if x.is_empty() {
        Ok((x, None))
    } else {
        map(delimited(ignored, tptp_input, ignored), Some)(x)
    }
}
