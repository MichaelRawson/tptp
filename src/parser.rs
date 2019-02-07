use nom::types::CompleteByteSlice as Input;
use nom::*;
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

named_attr!(#[inline], whitespace<Input, ()>, value!((), multispace));

named_attr!(#[inline], comment_line<Input, ()>, value!((),
    preceded!(char!('%'), take_until_and_consume!("\n"))
));

named_attr!(#[inline], comment_block<Input, ()>, value!((),
    preceded!(tag!("/*"), take_until_and_consume!("*/"))
));

named_attr!(#[inline], ignored<Input, ()>, value!((),
    many0!(alt!(whitespace | comment_line | comment_block))
));

named_attr!(#[inline], lower_alpha<Input, Input>, take_while1!(is_lower_alpha));

named_attr!(#[inline], upper_alpha<Input, Input>, take_while1!(is_upper_alpha));

named_attr!(#[inline], alphanumeric<Input, Input>, take_while!(is_alphanumeric));

named!(upper_word<Input, &str>, map!(
    recognize!(preceded!(upper_alpha, alphanumeric)),
    |w| unsafe {to_str(&w)}
));

named!(dollar_word<Input, &str>, map!(
    recognize!(preceded!(char!('$'), lower_alpha)),
    |w| unsafe {to_str(&w)}
));

named!(lower_word<Input, &str>, map!(
    recognize!(preceded!(lower_alpha, alphanumeric)),
    |w| unsafe {to_str(&w)}
));

named!(single_quoted<Input, &str>, map!(
    recognize!(delimited!(
        char!('\''),
        escaped!(take_while1!(is_sq_char), '\\', one_of!("\\'")),
        char!('\'')
    )),
    |w| unsafe {to_str(&w)}
));

named_attr!(#[inline], atomic_word<Input, &str>, alt!(lower_word | single_quoted));

named!(integer<Input, &str>, map!(
    recognize!(preceded!(opt!(one_of!("+-")), alt!(
        tag!("0") |
        preceded!(one_of!("123456789"), digit0)
    ))),
    |w| unsafe {to_str(&w)}
));

named!(name<Input, Name>, alt!(
    map!(lower_word, Name::LowerWord) |
    map!(integer, Name::Integer) |
    map!(single_quoted, Name::SingleQuoted)
));

named!(name_list<Input, Vec<Name>>, do_parse!(
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

named!(fof_arguments<Input, Vec<FofTerm>>, do_parse!(
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

named_attr!(#[inline], variable<Input, FofTerm>, map!(upper_word, FofTerm::Variable));

named!(fof_plain_term<Input, FofTerm>, do_parse!(
    name: name >>
    ignored >>
    arguments: opt!(fof_arguments) >>
    (FofTerm::Functor(name, arguments.unwrap_or_default()))
));

named_attr!(#[inline], fof_function_term<Input, FofTerm>, alt!(fof_plain_term));

named_attr!(#[inline], fof_term<Input, FofTerm>, alt!(fof_function_term | variable));

named!(fof_defined_atomic_formula<Input, FofFormula>, switch!(dollar_word,
    "$true" => value!(FofFormula::Boolean(true)) |
    "$false" => value!(FofFormula::Boolean(false))
));

named_attr!(#[inline], infix_equality<Input, InfixEquality>, alt!(
    value!(InfixEquality::Equal, tag!("=")) |
    value!(InfixEquality::NotEqual, tag!("!="))
));

named!(fof_atomic_formula<Input, FofFormula>, alt!(
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

named!(fof_quantified_formula<Input, FofFormula>, do_parse!(
    quantifier: alt!(
        value!(FofQuantifier::Forall, char!('!')) |
        value!(FofQuantifier::Exists, char!('?'))
    ) >>
    ignored >>
    char!('[') >>
    ignored >>
    bound: separated_nonempty_list!(
        delimited!(ignored, char!(','), ignored),
        upper_word
    ) >>
    ignored >>
    char!(']') >>
    ignored >>
    char!(':') >>
    ignored >>
    formula: fof_unit_formula >>
    (FofFormula::Quantified(quantifier, bound, Box::new(formula)))
));

named!(fof_unitary_formula<Input, FofFormula>, alt!(
    delimited!(
        terminated!(char!('('), ignored),
        fof_logic_formula,
        preceded!(ignored, char!(')'))
    ) |
    fof_atomic_formula |
    fof_quantified_formula
));

named!(fof_unary_formula<Input, FofFormula>, do_parse!(
    char!('~') >>
    ignored >>
    formula: fof_unit_formula >>
    (FofFormula::Unary(UnaryConnective::Not, Box::new(formula)))
));

named_attr!(#[inline], fof_unit_formula<Input, FofFormula>, alt!(fof_unary_formula | fof_unitary_formula));

named!(nonassoc_connective<Input, NonAssocConnective>, alt!(
    value!(NonAssocConnective::LRImplies, tag!("=>")) |
    value!(NonAssocConnective::Equivalent, tag!("<=>")) |
    value!(NonAssocConnective::RLImplies, tag!("<=")) |
    value!(NonAssocConnective::NotEquivalent, tag!("<~>")) |
    value!(NonAssocConnective::NotAnd, tag!("~&")) |
    value!(NonAssocConnective::NotOr, tag!("~|"))
));

named_attr!(#[inline], assoc_connective<Input, AssocConnective>, alt!(
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

fn fof_logic_formula(input: Input) -> IResult<Input, FofFormula> {
    if let Ok((input, formula)) = fof_unary_formula(input) {
        return Ok((input, formula));
    }

    let (after_first_input, first) = fof_unit_formula(input)?;
    let (input, _) = ignored(after_first_input)?;

    if nonassoc_connective(input).is_ok() {
        fof_binary_nonassoc(input, first)
    } else if assoc_connective(input).is_ok() {
        fof_binary_assoc(input, first)
    } else {
        Ok((after_first_input, first))
    }
}

named_attr!(#[inline], fof_formula<Input, FofFormula>, alt!(fof_logic_formula));

named!(literal<Input, CnfLiteral>, alt!(
    do_parse!(
        char!('~') >>
        ignored >>
        formula: fof_atomic_formula >>
        (CnfLiteral::NegatedLiteral(formula))
    ) |
    map!(fof_atomic_formula, CnfLiteral::Literal)
));

named!(disjunction<Input, Vec<CnfLiteral> >, separated_nonempty_list!(
    delimited!(ignored, char!('|'), ignored),
    literal
));

named!(cnf_formula<Input, CnfFormula>, map!(
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

named!(formula_role<Input, FormulaRole>, alt!(
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

named!(unknown_source<Input, Source>, value!(Source::Unknown, tag!("unknown")));

named!(dag_source<Input, DagSource>, alt!(
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

named!(external_source<Input, ExternalSource>, do_parse!(
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

named!(sources<Input, Vec<Source> >, do_parse!(
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

named!(source<Input, Source>, alt!(
    unknown_source |
    map!(external_source, Source::External) |
    map!(dag_source, Source::Dag) |
    map!(sources, Source::Sources)
));

named!(annotations<Input, Annotations>, map!(
    source,
    |source| Annotations {
        source
    }
));

named!(include_path<Input, Included>, map!(
    delimited!(
        char!('\''),
        escaped!(take_while1!(is_sq_char), '\\', one_of!("\\'")),
        char!('\'')
    ),
    |w| Included(unsafe {to_str(&w)})
));

named!(include<Input, Statement>, do_parse!(
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

named!(fof_annotated<Input, Statement>, do_parse!(
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

named!(cnf_annotated<Input, Statement>, do_parse!(
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

named!(tptp_input<Input, Statement>, do_parse!(
    statement: alt!(include | fof_annotated | cnf_annotated) >>
    ignored >>
    char!('.') >>
    (statement)
));

named!(tptp_input_or_eof<Input, Option<Statement> >, alt!(
    map!(tptp_input, Some) |
    value!(None, eof!())
));

fn parse_step(input: Input) -> Result<(Input, Option<Statement>), Input> {
    let (input, _) = ignored(input).expect("ignored cannot fail");
    tptp_input_or_eof(input).map_err(|_| input)
}

/// A syntax error (or an unsupported feature) occurred
#[derive(Debug)]
pub struct SyntaxError<'a> {
    /// The slice at the beginning of the erroneous statement
    pub position: &'a [u8],
}

struct Statements<'a> {
    start: Input<'a>,
}

impl<'a> Iterator for Statements<'a> {
    type Item = Result<Statement<'a>, SyntaxError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match parse_step(self.start) {
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
        start: Input(bytes),
    }
}

#[cfg(test)]
macro_rules! parses {
    ($parser:expr, $input:expr, $output:expr) => {
        assert_eq!($parser(Input($input)), Ok((Input(b""), $output)))
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_whitespace() {
        parses!(whitespace, b"  \t\n", ());
    }

    #[test]
    fn test_comment_line() {
        parses!(comment_line, b"% a comment\n", ());
    }

    #[test]
    fn test_comment_block() {
        parses!(comment_block, b"/* a\n block * / comment\n*/", ());
    }

    #[test]
    fn test_ignored() {
        parses!(ignored, b"", ());
        parses!(ignored, b"   %test\n  /* test\ntest */  ", ());
    }

    #[test]
    fn test_nonassoc_connective() {
        parses!(nonassoc_connective, b"<=>", NonAssocConnective::Equivalent);
        parses!(nonassoc_connective, b"<=", NonAssocConnective::RLImplies);
    }

    #[test]
    fn test_upper_word() {
        parses!(upper_word, b"X", "X");
        parses!(upper_word, b"Aa123", "Aa123");
    }

    #[test]
    fn test_lower_word() {
        parses!(lower_word, b"x", "x");
        parses!(lower_word, b"aA123", "aA123");
    }

    #[test]
    fn test_dollar_word() {
        parses!(dollar_word, b"$test", "$test");
    }

    #[test]
    fn test_integer() {
        parses!(integer, b"0", "0");
        parses!(integer, b"123", "123");
        parses!(integer, b"-123", "-123");
    }

    #[test]
    fn test_atomic_word() {
        parses!(atomic_word, b"x", "x");
        parses!(single_quoted, b"'single quoted'", "'single quoted'");
    }

    #[test]
    fn test_single_quoted() {
        parses!(single_quoted, b"'single quoted'", "'single quoted'");
        parses!(single_quoted, b"'\\'\\\\'", "'\\'\\\\'");
    }

    #[test]
    fn test_name() {
        parses!(name, b"lower_word2", Name::LowerWord("lower_word2"));
        parses!(
            name,
            b"'single quoted'",
            Name::SingleQuoted("'single quoted'")
        );
        parses!(name, b"123", Name::Integer("123"));
    }

    #[test]
    fn test_name_list() {
        parses!(
            name_list,
            b"[ name , 'name' , 123 ]",
            vec![
                Name::LowerWord("name"),
                Name::SingleQuoted("'name'"),
                Name::Integer("123")
            ]
        );
    }

    #[test]
    fn test_variable() {
        parses!(variable, b"X", FofTerm::Variable("X"));
    }

    #[test]
    fn test_fof_plain_term() {
        parses!(
            fof_plain_term,
            b"c",
            FofTerm::Functor(Name::LowerWord("c"), vec![])
        );
        parses!(
            fof_plain_term,
            b"f ( )",
            FofTerm::Functor(Name::LowerWord("f"), vec![])
        );
        parses!(
            fof_plain_term,
            b"f ( X )",
            FofTerm::Functor(
                Name::LowerWord("f"),
                vec![FofTerm::Variable("X")]
            )
        );
        parses!(
            fof_plain_term,
            b"f ( X, g ( Y ) )",
            FofTerm::Functor(
                Name::LowerWord("f"),
                vec![
                    FofTerm::Variable("X"),
                    FofTerm::Functor(
                        Name::LowerWord("g"),
                        vec![FofTerm::Variable("Y")]
                    )
                ]
            )
        );
    }

    #[test]
    fn test_fof_function_term() {
        parses!(
            fof_function_term,
            b"f(X)",
            FofTerm::Functor(
                Name::LowerWord("f"),
                vec![FofTerm::Variable("X")]
            )
        );
    }

    #[test]
    fn test_fof_term() {
        parses!(
            fof_term,
            b"f(X)",
            FofTerm::Functor(
                Name::LowerWord("f"),
                vec![FofTerm::Variable("X")]
            )
        );
        parses!(fof_term, b"X", FofTerm::Variable("X"));
    }

    #[test]
    fn test_fof_defined_atomic_formula() {
        parses!(
            fof_defined_atomic_formula,
            b"$true",
            FofFormula::Boolean(true)
        );
        parses!(
            fof_defined_atomic_formula,
            b"$false",
            FofFormula::Boolean(false)
        );
    }

    #[test]
    fn test_infix_equality() {
        parses!(infix_equality, b"=", InfixEquality::Equal);
        parses!(infix_equality, b"!=", InfixEquality::NotEqual);
    }

    #[test]
    fn test_fof_atomic_formula() {
        parses!(fof_atomic_formula, b"$true", FofFormula::Boolean(true));
        parses!(
            fof_atomic_formula,
            b"f(X) = Y",
            FofFormula::Infix(
                InfixEquality::Equal,
                FofTerm::Functor(
                    Name::LowerWord("f"),
                    vec![FofTerm::Variable("X")]
                ),
                FofTerm::Variable("Y")
            )
        );
        parses!(
            fof_atomic_formula,
            b"p(X)",
            FofFormula::Predicate(
                Name::LowerWord("p"),
                vec![FofTerm::Variable("X")]
            )
        );
        parses!(
            fof_atomic_formula,
            b"X != Y",
            FofFormula::Infix(
                InfixEquality::NotEqual,
                FofTerm::Variable("X"),
                FofTerm::Variable("Y")
            )
        );
    }

    #[test]
    fn test_fof_quantified_formula() {
        parses!(
            fof_quantified_formula,
            b"! [ X ] : $true",
            FofFormula::Quantified(
                FofQuantifier::Forall,
                vec!["X"],
                Box::new(FofFormula::Boolean(true))
            )
        );
        parses!(
            fof_quantified_formula,
            b"? [ X , Y, Z ] : $true",
            FofFormula::Quantified(
                FofQuantifier::Exists,
                vec!["X", "Y", "Z"],
                Box::new(FofFormula::Boolean(true))
            )
        );
    }

    #[test]
    fn test_fof_unitary_formula() {
        parses!(fof_unitary_formula, b"( $true )", FofFormula::Boolean(true));
        parses!(fof_unitary_formula, b"$true", FofFormula::Boolean(true));
        parses!(
            fof_unitary_formula,
            b"![X]: $true",
            FofFormula::Quantified(
                FofQuantifier::Forall,
                vec!["X"],
                Box::new(FofFormula::Boolean(true))
            )
        );
    }

    #[test]
    fn test_fof_unary_formula() {
        parses!(
            fof_unary_formula,
            b"~ $true",
            FofFormula::Unary(
                UnaryConnective::Not,
                Box::new(FofFormula::Boolean(true))
            )
        );
    }

    #[test]
    fn test_fof_unit_formula() {
        parses!(fof_unit_formula, b"($true)", FofFormula::Boolean(true));
        parses!(
            fof_unit_formula,
            b"~$true",
            FofFormula::Unary(
                UnaryConnective::Not,
                Box::new(FofFormula::Boolean(true))
            )
        );
    }

    #[test]
    fn test_fof_logic_formula() {
        let p = FofFormula::Predicate(Name::LowerWord("p"), vec![]);
        let q = FofFormula::Predicate(Name::LowerWord("q"), vec![]);
        let r = FofFormula::Predicate(Name::LowerWord("r"), vec![]);

        parses!(
            fof_logic_formula,
            b"~p",
            FofFormula::Unary(UnaryConnective::Not, Box::new(p.clone()))
        );
        parses!(
            fof_logic_formula,
            b"p => q",
            FofFormula::NonAssoc(
                NonAssocConnective::LRImplies,
                Box::new(p.clone()),
                Box::new(q.clone())
            )
        );
        parses!(
            fof_logic_formula,
            b"p & q",
            FofFormula::Assoc(AssocConnective::And, vec![p.clone(), q.clone()])
        );
        parses!(
            fof_logic_formula,
            b"p | q | r",
            FofFormula::Assoc(
                AssocConnective::Or,
                vec![p.clone(), q.clone(), r.clone()]
            )
        );
        parses!(fof_logic_formula, b"p", p.clone());
    }

    #[test]
    fn test_literal() {
        let p = FofFormula::Predicate(Name::LowerWord("p"), vec![]);

        parses!(literal, b"p", CnfLiteral::Literal(p.clone()));
        parses!(literal, b"~ p", CnfLiteral::NegatedLiteral(p));
    }

    #[test]
    fn test_disjunction() {
        let p = CnfLiteral::Literal(FofFormula::Predicate(
            Name::LowerWord("p"),
            vec![],
        ));
        let q = CnfLiteral::NegatedLiteral(FofFormula::Predicate(
            Name::LowerWord("q"),
            vec![],
        ));
        let r = CnfLiteral::Literal(FofFormula::Predicate(
            Name::LowerWord("r"),
            vec![],
        ));

        parses!(disjunction, b"p", vec![p.clone()]);
        parses!(disjunction, b"p | ~q", vec![p.clone(), q.clone()]);
        parses!(
            disjunction,
            b"p | ~q | r",
            vec![p.clone(), q.clone(), r.clone()]
        );
    }

    #[test]
    fn test_cnf_formula() {
        let p = CnfLiteral::Literal(FofFormula::Predicate(
            Name::LowerWord("p"),
            vec![],
        ));

        parses!(cnf_formula, b"p", CnfFormula(vec![p.clone()]));
        parses!(cnf_formula, b"( p )", CnfFormula(vec![p.clone()]));
    }

    #[test]
    fn test_role() {
        parses!(formula_role, b"axiom", FormulaRole::Axiom);
        parses!(formula_role, b"conjecture", FormulaRole::Conjecture);
    }

    #[test]
    fn test_unknown_source() {
        parses!(unknown_source, b"unknown", Source::Unknown);
    }

    #[test]
    fn test_dag_source() {
        parses!(
            dag_source,
            b"name",
            DagSource::Name(Name::LowerWord("name"))
        );
        parses!(
            dag_source,
            b"inference ( deduction , [ ] , [] )",
            DagSource::Inference("deduction", vec![])
        );
    }

    #[test]
    fn test_external_source() {
        parses!(
            external_source,
            b"file ( 'file' )",
            ExternalSource::File("'file'", None)
        );
        parses!(
            external_source,
            b"file ( 'file' , name )",
            ExternalSource::File("'file'", Some(Name::LowerWord("name")))
        );
    }

    #[test]
    fn test_sources() {
        parses!(sources, b"[ ]", vec![]);
        parses!(sources, b"[ unknown ]", vec![Source::Unknown]);
        parses!(
            sources,
            b"[ unknown , unknown ]",
            vec![Source::Unknown, Source::Unknown]
        );
    }

    #[test]
    fn test_source() {
        parses!(source, b"unknown", Source::Unknown);
        parses!(
            source,
            b"file ( 'file' )",
            Source::External(ExternalSource::File("'file'", None))
        );
        parses!(
            source,
            b"name",
            Source::Dag(DagSource::Name(Name::LowerWord("name")))
        );
        parses!(
            source,
            b"[ unknown ]",
            Source::Sources(vec![Source::Unknown])
        );
    }

    #[test]
    fn test_annotations() {
        parses!(
            annotations,
            b"unknown",
            Annotations {
                source: Source::Unknown
            }
        );
    }

    #[test]
    fn test_include_path() {
        parses!(include_path, b"'test'", Included("test"));
        parses!(include_path, b"'\\\\\\''", Included("\\\\\\'"));
    }

    #[test]
    fn test_include() {
        parses!(
            include,
            b"include ( 'test' )",
            Statement::Include(Included("test"), None)
        );
        parses!(
            include,
            b"include( 'test', [ test ])",
            Statement::Include(
                Included("test"),
                Some(vec![Name::LowerWord("test")])
            )
        );
    }

    #[test]
    fn test_fof_annotated() {
        parses!(
            fof_annotated,
            b"fof ( test , axiom , $true )",
            Statement::Fof(
                Name::LowerWord("test"),
                FormulaRole::Axiom,
                FofFormula::Boolean(true),
                None
            )
        );
        parses!(
            fof_annotated,
            b"fof ( test , axiom , $true , unknown )",
            Statement::Fof(
                Name::LowerWord("test"),
                FormulaRole::Axiom,
                FofFormula::Boolean(true),
                Some(Annotations {
                    source: Source::Unknown
                })
            )
        );
    }

    #[test]
    fn test_cnf_annotated() {
        parses!(
            cnf_annotated,
            b"cnf ( test , axiom , $true )",
            Statement::Cnf(
                Name::LowerWord("test"),
                FormulaRole::Axiom,
                CnfFormula(vec![CnfLiteral::Literal(FofFormula::Boolean(
                    true
                ))]),
                None
            )
        );
        parses!(
            cnf_annotated,
            b"cnf ( test , axiom , $true , unknown )",
            Statement::Cnf(
                Name::LowerWord("test"),
                FormulaRole::Axiom,
                CnfFormula(vec![CnfLiteral::Literal(FofFormula::Boolean(
                    true
                ))]),
                Some(Annotations {
                    source: Source::Unknown
                })
            )
        );
    }

    #[test]
    fn test_tptp_input() {
        parses!(
            tptp_input,
            b"include ( 'test' ) .",
            Statement::Include(Included("test"), None)
        );
        parses!(
            tptp_input,
            b"fof ( test , axiom , $true ) .",
            Statement::Fof(
                Name::LowerWord("test"),
                FormulaRole::Axiom,
                FofFormula::Boolean(true),
                None
            )
        );
        parses!(
            tptp_input,
            b"cnf ( test , axiom , $true ) .",
            Statement::Cnf(
                Name::LowerWord("test"),
                FormulaRole::Axiom,
                CnfFormula(vec![CnfLiteral::Literal(FofFormula::Boolean(
                    true
                ))]),
                None
            )
        );
    }

    #[test]
    fn test_tptp_input_or_eof() {
        parses!(
            tptp_input_or_eof,
            b"include('test').",
            Some(Statement::Include(Included("test"), None))
        );
        parses!(tptp_input_or_eof, b"", None);
    }
}
