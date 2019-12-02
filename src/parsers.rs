use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_until, take_while, take_while1};
use nom::character::complete::{digit0, multispace1, one_of};
use nom::combinator::{map, opt, recognize, value};
use nom::multi::{many0, separated_list, separated_nonempty_list};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::Err::Failure;
use std::borrow::Cow;
use std::str;

use crate::syntax::*;

pub type Parsed<'a, T> = nom::IResult<&'a [u8], T, ()>;

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

pub fn whitespace(x: &[u8]) -> Parsed<&[u8]> {
    multispace1(x)
}

pub fn comment_line(x: &[u8]) -> Parsed<&[u8]> {
    recognize(tuple((tag("%"), take_until("\n"), tag("\n"))))(x)
}

pub fn comment_block(x: &[u8]) -> Parsed<&[u8]> {
    recognize(tuple((tag("/*"), take_until("*/"), tag("*/"))))(x)
}

pub fn ignored(x: &[u8]) -> Parsed<()> {
    value((), many0(alt((whitespace, comment_line, comment_block))))(x)
}

pub fn lower_alpha(x: &[u8]) -> Parsed<&[u8]> {
    take_while1(is_lower_alpha)(x)
}

pub fn upper_alpha(x: &[u8]) -> Parsed<&[u8]> {
    take_while1(is_upper_alpha)(x)
}

pub fn alphanumeric(x: &[u8]) -> Parsed<&[u8]> {
    take_while(is_alphanumeric)(x)
}

pub fn lower_word(x: &[u8]) -> Parsed<Cow<str>> {
    map(recognize(preceded(lower_alpha, alphanumeric)), |w| {
        Cow::Borrowed(unsafe { to_str(w) })
    })(x)
}

pub fn upper_word(x: &[u8]) -> Parsed<Cow<str>> {
    map(recognize(preceded(upper_alpha, alphanumeric)), |w| {
        Cow::Borrowed(unsafe { to_str(w) })
    })(x)
}

pub fn single_quoted(x: &[u8]) -> Parsed<Cow<str>> {
    map(
        recognize(delimited(
            tag("'"),
            escaped(take_while1(is_sq_char), '\\', one_of("\\'")),
            tag("'"),
        )),
        |w| Cow::Borrowed(unsafe { to_str(w) }),
    )(x)
}

pub fn atomic_word(x: &[u8]) -> Parsed<Cow<str>> {
    alt((lower_word, single_quoted))(x)
}

pub fn integer(x: &[u8]) -> Parsed<Cow<str>> {
    map(
        recognize(preceded(
            opt(one_of("+-")),
            alt((tag("0"), preceded(one_of("123456789"), digit0))),
        )),
        |w| Cow::Borrowed(unsafe { to_str(w) }),
    )(x)
}

pub fn name(x: &[u8]) -> Parsed<Name> {
    alt((
        map(lower_word, Name::LowerWord),
        map(integer, Name::Integer),
        map(single_quoted, Name::SingleQuoted),
    ))(x)
}

pub fn name_list(x: &[u8]) -> Parsed<Vec<Name>> {
    delimited(
        pair(tag("["), ignored),
        separated_list(tuple((ignored, tag(","), ignored)), name),
        pair(ignored, tag("]")),
    )(x)
}

pub fn fof_arguments(x: &[u8]) -> Parsed<Vec<FofTerm>> {
    delimited(
        pair(tag("("), ignored),
        separated_list(tuple((ignored, tag(","), ignored)), fof_term),
        pair(ignored, tag(")")),
    )(x)
}

pub fn variable(x: &[u8]) -> Parsed<Variable> {
    map(upper_word, Variable)(x)
}

pub fn fof_plain_term(x: &[u8]) -> Parsed<FofTerm> {
    map(
        pair(name, preceded(ignored, opt(fof_arguments))),
        |(name, args)| match args {
            Some(args) => FofTerm::Functor(name, args),
            None => FofTerm::Constant(name),
        },
    )(x)
}

pub fn fof_function_term(x: &[u8]) -> Parsed<FofTerm> {
    fof_plain_term(x)
}

pub fn fof_term(x: &[u8]) -> Parsed<FofTerm> {
    alt((fof_function_term, map(variable, FofTerm::Variable)))(x)
}

pub fn fof_defined_atomic_formula(x: &[u8]) -> Parsed<FofFormula> {
    alt((
        value(FofFormula::Boolean(true), tag("$true")),
        value(FofFormula::Boolean(false), tag("$false")),
    ))(x)
}

pub fn infix_equality(x: &[u8]) -> Parsed<InfixEquality> {
    alt((
        value(InfixEquality::Equal, tag("=")),
        value(InfixEquality::NotEqual, tag("!=")),
    ))(x)
}

pub fn fof_atomic_formula(x: &[u8]) -> Parsed<FofFormula> {
    fn predicate_or_infix(x: &[u8]) -> Parsed<FofFormula> {
        let (x, (term, rest)) = pair(
            fof_term,
            opt(pair(
                preceded(ignored, infix_equality),
                preceded(ignored, fof_term),
            )),
        )(x)?;
        let result = match (term, rest) {
            (left, Some((op, right))) => Ok(FofFormula::Infix(op, left, right)),
            (FofTerm::Constant(name), None) => {
                Ok(FofFormula::Proposition(name))
            }
            (FofTerm::Functor(name, args), None) => {
                Ok(FofFormula::Predicate(name, args))
            }
            (FofTerm::Variable(_), None) => Err(Failure(())),
        }?;
        Ok((x, result))
    }

    alt((fof_defined_atomic_formula, predicate_or_infix))(x)
}

pub fn fof_quantified_formula(x: &[u8]) -> Parsed<FofFormula> {
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

pub fn fof_unitary_formula(x: &[u8]) -> Parsed<FofFormula> {
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

pub fn fof_unary_formula(x: &[u8]) -> Parsed<FofFormula> {
    map(
        preceded(pair(tag("~"), ignored), fof_unit_formula),
        |formula| FofFormula::Unary(UnaryConnective::Not, Box::new(formula)),
    )(x)
}

pub fn fof_unit_formula(x: &[u8]) -> Parsed<FofFormula> {
    alt((fof_unary_formula, fof_unitary_formula))(x)
}

pub fn nonassoc_connective(x: &[u8]) -> Parsed<NonAssocConnective> {
    alt((
        value(NonAssocConnective::LRImplies, tag("=>")),
        value(NonAssocConnective::Equivalent, tag("<=>")),
        value(NonAssocConnective::RLImplies, tag("<=")),
        value(NonAssocConnective::NotEquivalent, tag("<~>")),
        value(NonAssocConnective::NotAnd, tag("~&")),
        value(NonAssocConnective::NotOr, tag("~|")),
    ))(x)
}

pub fn assoc_connective(x: &[u8]) -> Parsed<AssocConnective> {
    alt((
        value(AssocConnective::And, tag("&")),
        value(AssocConnective::Or, tag("|")),
    ))(x)
}

pub fn fof_logic_formula(x: &[u8]) -> Parsed<FofFormula> {
    let (x, first) =
        terminated(alt((fof_unary_formula, fof_unit_formula)), ignored)(x)?;

    if let Ok((x, op)) = nonassoc_connective(x) {
        let (x, second) = preceded(ignored, fof_unit_formula)(x)?;
        Ok((
            x,
            FofFormula::NonAssoc(op, Box::new(first), Box::new(second)),
        ))
    } else if let Ok((x, op)) = assoc_connective(x) {
        let (mut x, second) = preceded(ignored, fof_unit_formula)(x)?;
        let mut formulae = vec![first, second];
        while let Ok((after, (check_op, formula))) = pair(
            preceded(ignored, assoc_connective),
            preceded(ignored, fof_unit_formula),
        )(x)
        {
            if op == check_op {
                formulae.push(formula);
                x = after;
            } else {
                return Err(Failure(()));
            }
        }

        Ok((x, FofFormula::Assoc(op, formulae)))
    } else {
        Ok((x, first))
    }
}

pub fn fof_formula(x: &[u8]) -> Parsed<FofFormula> {
    fof_logic_formula(x)
}

pub fn literal(x: &[u8]) -> Parsed<Literal> {
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

pub fn disjunction(x: &[u8]) -> Parsed<Vec<Literal>> {
    separated_nonempty_list(tuple((ignored, tag("|"), ignored)), literal)(x)
}

pub fn cnf_formula(x: &[u8]) -> Parsed<CnfFormula> {
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

pub fn formula_role(x: &[u8]) -> Parsed<FormulaRole> {
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

pub fn unknown_source(x: &[u8]) -> Parsed<Source> {
    value(Source::Unknown, tag("unknown"))(x)
}

pub fn dag_source(x: &[u8]) -> Parsed<DagSource> {
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

pub fn external_source(x: &[u8]) -> Parsed<ExternalSource> {
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

pub fn sources(x: &[u8]) -> Parsed<Vec<Source>> {
    delimited(
        pair(tag("["), ignored),
        separated_list(tuple((ignored, tag(","), ignored)), source),
        pair(ignored, tag("]")),
    )(x)
}

pub fn source(x: &[u8]) -> Parsed<Source> {
    alt((
        unknown_source,
        map(external_source, Source::External),
        map(dag_source, Source::Dag),
        map(sources, Source::Sources),
    ))(x)
}

pub fn annotations(x: &[u8]) -> Parsed<Annotations> {
    map(source, |source| Annotations { source })(x)
}

pub fn include_path(x: &[u8]) -> Parsed<Included> {
    map(
        delimited(
            tag("'"),
            escaped(take_while1(is_sq_char), '\\', one_of("\\'")),
            tag("'"),
        ),
        |w| Included(unsafe { to_str(w) }.into()),
    )(x)
}

pub fn include(x: &[u8]) -> Parsed<Statement> {
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

pub fn fof_annotated(x: &[u8]) -> Parsed<Statement> {
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

pub fn cnf_annotated(x: &[u8]) -> Parsed<Statement> {
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

pub fn tptp_input(x: &[u8]) -> Parsed<Statement> {
    terminated(
        alt((include, fof_annotated, cnf_annotated)),
        pair(ignored, tag(".")),
    )(x)
}

pub fn tptp_input_or_eof(x: &[u8]) -> Parsed<Option<Statement>> {
    if x.is_empty() {
        Ok((x, None))
    } else {
        map(delimited(ignored, tptp_input, ignored), Some)(x)
    }
}
