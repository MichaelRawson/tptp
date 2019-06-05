use crate::syntax::Statement;

pub(in crate) mod parsers {
    pub use nom::types::CompleteByteSlice as Input;
    use nom::*;
    use std::borrow::Cow;
    use std::str;

    use crate::syntax::*;

    unsafe fn to_str(bytes: &[u8]) -> &str {
        str::from_utf8_unchecked(bytes)
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

    fn is_visible(c: u8) -> bool {
        c >= b' ' && c <= b'~'
    }

    fn is_sq_char(c: u8) -> bool {
        is_visible(c) && c != b'\'' && c != b'\\'
    }

    named!(pub whitespace<Input, ()>, value!((), multispace));

    named!(pub comment_line<Input, ()>, value!((),
        preceded!(char!('%'), take_until_and_consume!("\n"))
    ));

    named!(pub comment_block<Input, ()>, value!((),
        preceded!(tag!("/*"), take_until_and_consume!("*/"))
    ));

    named!(pub ignored<Input, ()>, value!((),
        many0!(alt!(whitespace | comment_line | comment_block))
    ));

    named!(pub lower_alpha<Input, Input>, take_while1!(is_lower_alpha));

    named!(pub upper_alpha<Input, Input>, take_while1!(is_upper_alpha));

    named!(pub alphanumeric<Input, Input>, take_while!(is_alphanumeric));

    named!(pub upper_word<Input, Cow<str>>, map!(
        recognize!(preceded!(upper_alpha, alphanumeric)),
        |w| Cow::Borrowed(unsafe {to_str(&w)})
    ));

    named!(pub dollar_word<Input, &str>, map!(
        recognize!(preceded!(char!('$'), lower_alpha)),
        |w| unsafe {to_str(&w)}
    ));

    named!(pub lower_word<Input, Cow<str>>, map!(
        recognize!(preceded!(lower_alpha, alphanumeric)),
        |w| Cow::Borrowed(unsafe {to_str(&w)})
    ));

    named!(pub single_quoted<Input, Cow<str>>, map!(
        recognize!(delimited!(
            char!('\''),
            escaped!(take_while1!(is_sq_char), '\\', one_of!("\\'")),
            char!('\'')
        )),
        |w| Cow::Borrowed(unsafe {to_str(&w)})
    ));

    named!(pub atomic_word<Input, Cow<str>>, alt!(lower_word | single_quoted));

    named!(pub integer<Input, Cow<str>>, map!(
        recognize!(preceded!(opt!(one_of!("+-")), alt!(
            tag!("0") |
            preceded!(one_of!("123456789"), digit0)
        ))),
        |w| Cow::Borrowed(unsafe {to_str(&w)})
    ));

    named!(pub name<Input, Name>, alt!(
        map!(lower_word, Name::LowerWord) |
        map!(integer, Name::Integer) |
        map!(single_quoted, Name::SingleQuoted)
    ));

    named!(pub name_list<Input, Vec<Name>>, do_parse!(
        char!('[') >>
        ignored >>
        names: separated_list!(
            delimited!(ignored, char!(','), ignored),
            name
        ) >>
        ignored >>
        char!(']') >>
        (names)
    ));

    named!(pub fof_arguments<Input, Vec<FofTerm>>, do_parse!(
        char!('(') >>
        ignored >>
        args: opt!(separated_list!(
            delimited!(ignored, char!(','), ignored),
            fof_term
        )) >>
        ignored >>
        char!(')') >>
        (args.unwrap_or_default())
    ));

    named!(pub variable<Input, Variable>, map!(upper_word, Variable));

    named!(pub fof_plain_term<Input, FofTerm>, do_parse!(
        name: name >>
        ignored >>
        arguments: opt!(fof_arguments) >>
        (FofTerm::Functor(name, arguments.unwrap_or_default()))
    ));

    named!(pub fof_function_term<Input, FofTerm>, alt!(fof_plain_term));

    named!(pub fof_term<Input, FofTerm>, alt!(fof_function_term | map!(variable, FofTerm::Variable)));

    named!(pub fof_defined_atomic_formula<Input, FofFormula>, switch!(dollar_word,
        "$true" => value!(FofFormula::Boolean(true)) |
        "$false" => value!(FofFormula::Boolean(false))
    ));

    named!(pub infix_equality<Input, InfixEquality>, alt!(
        value!(InfixEquality::Equal, tag!("=")) |
        value!(InfixEquality::NotEqual, tag!("!="))
    ));

    named!(pub fof_atomic_formula<Input, FofFormula>, alt!(
        fof_defined_atomic_formula |
        do_parse!(
            left: fof_plain_term >>
            rest: opt!(do_parse!(
                ignored >>
                op: infix_equality >>
                ignored >>
                right: fof_term >>
                (op, right)
            )) >>
            (match rest {
                Some((op, right)) => FofFormula::Infix(op, left, right),
                None => match left {
                    FofTerm::Functor(name, args) => FofFormula::Predicate(name, args),
                    _ => unreachable!()
                }
            })
        ) |
        do_parse!(
            left: fof_term >>
            ignored >>
            op: infix_equality >>
            ignored >>
            right: fof_term >>
            (FofFormula::Infix(op, left, right))
        )
    ));

    named!(pub fof_quantified_formula<Input, FofFormula>, do_parse!(
        quantifier: alt!(
            value!(FofQuantifier::Forall, char!('!')) |
            value!(FofQuantifier::Exists, char!('?'))
        ) >>
        ignored >>
        char!('[') >>
        ignored >>
        bound: separated_nonempty_list!(
            delimited!(ignored, char!(','), ignored),
            variable
        ) >>
        ignored >>
        char!(']') >>
        ignored >>
        char!(':') >>
        ignored >>
        formula: fof_unit_formula >>
        (FofFormula::Quantified(quantifier, bound, Box::new(formula)))
    ));

    named!(pub fof_unitary_formula<Input, FofFormula>, alt!(
        delimited!(
            terminated!(char!('('), ignored),
            fof_logic_formula,
            preceded!(ignored, char!(')'))
        ) |
        fof_atomic_formula |
        fof_quantified_formula
    ));

    named!(pub fof_unary_formula<Input, FofFormula>, do_parse!(
        char!('~') >>
        ignored >>
        formula: fof_unit_formula >>
        (FofFormula::Unary(UnaryConnective::Not, Box::new(formula)))
    ));

    named!(pub fof_unit_formula<Input, FofFormula>, alt!(fof_unary_formula | fof_unitary_formula));

    named!(pub nonassoc_connective<Input, NonAssocConnective>, alt!(
        value!(NonAssocConnective::LRImplies, tag!("=>")) |
        value!(NonAssocConnective::Equivalent, tag!("<=>")) |
        value!(NonAssocConnective::RLImplies, tag!("<=")) |
        value!(NonAssocConnective::NotEquivalent, tag!("<~>")) |
        value!(NonAssocConnective::NotAnd, tag!("~&")) |
        value!(NonAssocConnective::NotOr, tag!("~|"))
    ));

    named!(pub assoc_connective<Input, AssocConnective>, alt!(
        value!(AssocConnective::And, char!('&')) |
        value!(AssocConnective::Or, char!('|'))
    ));

    named_args!(fof_binary_nonassoc<'a>(left: FofFormula<'a>)<Input<'a>, FofFormula<'a>>, do_parse!(
        op: nonassoc_connective >>
        ignored >>
        right: fof_unit_formula >>
        (FofFormula::NonAssoc(op, Box::new(left), Box::new(right)))
    ));

    named_args!(fof_binary_assoc<'a>(first: FofFormula<'a>)<Input<'a>, FofFormula<'a>>, do_parse!(
        op: assoc_connective >>
        op_char: value!(match op {
            AssocConnective::And => '&',
            AssocConnective::Or => '|',
        }) >>
        ignored >>
        second: fof_unit_formula >>
        list: fold_many0!(
            preceded!(
                delimited!(ignored, char!(op_char), ignored),
                fof_unit_formula
            ),
            vec![first, second],
            |mut list: Vec<_>, item| {list.push(item); list}
        ) >>
        (FofFormula::Assoc(op, list))
    ));

    pub fn fof_logic_formula(input: Input) -> IResult<Input, FofFormula> {
        let (after_first_input, first) =
            fof_unary_formula(input).or_else(|_| fof_unit_formula(input))?;

        let (input, _) = ignored(after_first_input)?;

        if nonassoc_connective(input).is_ok() {
            fof_binary_nonassoc(input, first)
        } else if assoc_connective(input).is_ok() {
            fof_binary_assoc(input, first)
        } else {
            Ok((after_first_input, first))
        }
    }

    named!(pub fof_formula<Input, FofFormula>, alt!(fof_logic_formula));

    named!(pub literal<Input, CnfLiteral>, alt!(
        do_parse!(
            char!('~') >>
            ignored >>
            formula: fof_atomic_formula >>
            (CnfLiteral::NegatedLiteral(formula))
        ) |
        map!(fof_atomic_formula, CnfLiteral::Literal)
    ));

    named!(pub disjunction<Input, Vec<CnfLiteral> >, separated_nonempty_list!(
        delimited!(ignored, char!('|'), ignored),
        literal
    ));

    named!(pub cnf_formula<Input, CnfFormula>, map!(
        alt!(
            delimited!(
                char!('('),
                delimited!(ignored, disjunction, ignored),
                char!(')')
            ) |
            disjunction
        ),
        CnfFormula
    ));

    named!(pub formula_role<Input, FormulaRole>, alt!(
        value!(FormulaRole::Axiom, tag!("axiom")) |
        value!(FormulaRole::Hypothesis, tag!("hypothesis")) |
        value!(FormulaRole::Definition, tag!("definition")) |
        value!(FormulaRole::Assumption, tag!("assumption")) |
        value!(FormulaRole::Lemma, tag!("lemma")) |
        value!(FormulaRole::Theorem, tag!("theorem")) |
        value!(FormulaRole::Corollary, tag!("corollary")) |
        value!(FormulaRole::Conjecture, tag!("conjecture")) |
        value!(FormulaRole::NegatedConjecture, tag!("negated_conjecture")) |
        value!(FormulaRole::Plain, tag!("plain")) |
        value!(FormulaRole::Unknown, tag!("unknown"))
    ));

    named!(pub unknown_source<Input, Source>, value!(Source::Unknown, tag!("unknown")));

    named!(pub dag_source<Input, DagSource>, alt!(
        do_parse!(
            tag!("inference") >>
            ignored >>
            char!('(') >>
            ignored >>
            rule: atomic_word >>
            ignored >>
            char!(',') >>
            ignored >>
            char!('[') >>
            ignored >>
            char!(']') >>
            ignored >>
            char!(',') >>
            ignored >>
            parents: sources >>
            ignored >>
            char!(')') >>
            (DagSource::Inference(rule, parents))
        ) |
        map!(name, DagSource::Name)
    ));

    named!(pub external_source<Input, ExternalSource>, do_parse!(
        tag!("file") >>
        ignored >>
        char!('(') >>
        ignored >>
        file: single_quoted >>
        info: opt!(do_parse!(
            ignored >>
            char!(',') >>
            ignored >>
            name: name >>
            (name)
        )) >>
        ignored >>
        char!(')') >>
        (ExternalSource::File(file, info))
    ));

    named!(pub sources<Input, Vec<Source> >, do_parse!(
        char!('[') >>
        ignored >>
        sources: separated_list!(
            delimited!(ignored, char!(','), ignored),
            source
        ) >>
        ignored >>
        char!(']') >>
        (sources)
    ));

    named!(pub source<Input, Source>, alt!(
        unknown_source |
        map!(external_source, Source::External) |
        map!(dag_source, Source::Dag) |
        map!(sources, Source::Sources)
    ));

    named!(pub annotations<Input, Annotations>, map!(
        source,
        |source| Annotations {
            source
        }
    ));

    named!(pub include_path<Input, Included>, map!(
        delimited!(
            char!('\''),
            escaped!(take_while1!(is_sq_char), '\\', one_of!("\\'")),
            char!('\'')
        ),
        |w| Included(unsafe {to_str(&w)})
    ));

    named!(pub include<Input, Statement>, do_parse!(
        tag!("include") >>
        ignored >>
        char!('(') >>
        ignored >>
        included: include_path >>
        selection: opt!(do_parse!(
            ignored >>
            char!(',') >>
            ignored >>
            names: name_list >>
            (names)
        )) >>
        ignored >>
        char!(')') >>
        (Statement::Include(included, selection))
    ));

    named!(pub fof_annotated<Input, Statement>, do_parse!(
        tag!("fof") >>
        ignored >>
        char!('(') >>
        ignored >>
        name: name >>
        ignored >>
        char!(',') >>
        ignored >>
        role: formula_role >>
        ignored >>
        char!(',') >>
        ignored >>
        formula: fof_formula >>
        annotations: opt!(do_parse!(
            ignored >>
            char!(',') >>
            ignored >>
            annotations: annotations >>
            (annotations)
        )) >>
        ignored >>
        char!(')') >>
        (Statement::Fof(name, role, formula, annotations))
    ));

    named!(pub cnf_annotated<Input, Statement>, do_parse!(
        tag!("cnf") >>
        ignored >>
        char!('(') >>
        ignored >>
        name: name >>
        ignored >>
        char!(',') >>
        ignored >>
        role: formula_role >>
        ignored >>
        char!(',') >>
        ignored >>
        formula: cnf_formula >>
        annotations: opt!(do_parse!(
            ignored >>
            char!(',') >>
            ignored >>
            annotations: annotations >>
            (annotations)
        )) >>
        ignored >>
        char!(')') >>
        (Statement::Cnf(name, role, formula, annotations))
    ));

    named!(pub tptp_input<Input, Statement>, do_parse!(
        statement: alt!(include | fof_annotated | cnf_annotated) >>
        ignored >>
        char!('.') >>
        (statement)
    ));

    named!(pub tptp_input_or_eof<Input, Option<Statement> >, alt!(
        map!(tptp_input, Some) |
        value!(None, eof!())
    ));

    pub fn parse_step(input: Input) -> Result<(Input, Option<Statement>), Input> {
        let (input, _) = ignored(input).expect("ignored cannot fail");
        tptp_input_or_eof(input).map_err(|_| input)
    }
}

/// A syntax error (or an unsupported feature) occurred
#[derive(Debug)]
pub struct SyntaxError<'a> {
    /// The slice at the beginning of the erroneous statement
    pub position: &'a [u8],
}

struct Statements<'a> {
    start: parsers::Input<'a>,
}

impl<'a> Iterator for Statements<'a> {
    type Item = Result<Statement<'a>, SyntaxError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match parsers::parse_step(self.start) {
            Ok((input, statement)) => {
                self.start = input;
                statement.map(Ok)
            }
            Err(input) => {
                self.start = input;
                Some(Err(SyntaxError { position: &input }))
            }
        }
    }
}

/// Parse a byte slice, returning an iterator over the statements within.
///
/// In order to parse larger files, memory-mapped files may be employed.
/// A syntax error in the stream will re-occur indefinitely.
pub fn parse(
    bytes: &[u8],
) -> impl Iterator<Item = Result<Statement, SyntaxError>> {
    Statements {
        start: parsers::Input(bytes),
    }
}
