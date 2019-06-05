use crate::parser::parsers::*;
use crate::syntax::*;

macro_rules! parses {
    ($parser:expr, $input:expr, $output:expr) => {
        assert_eq!($parser(Input($input)), Ok((Input(b""), $output)))
    };
}

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
    parses!(upper_word, b"X", "X".into());
    parses!(upper_word, b"Aa123", "Aa123".into());
}

#[test]
fn test_lower_word() {
    parses!(lower_word, b"x", "x".into());
    parses!(lower_word, b"aA123", "aA123".into());
}

#[test]
fn test_dollar_word() {
    parses!(dollar_word, b"$test", "$test".into());
}

#[test]
fn test_integer() {
    parses!(integer, b"0", "0".into());
    parses!(integer, b"123", "123".into());
    parses!(integer, b"-123", "-123".into());
}

#[test]
fn test_atomic_word() {
    parses!(atomic_word, b"x", "x".into());
    parses!(single_quoted, b"'single quoted'", "'single quoted'".into());
}

#[test]
fn test_single_quoted() {
    parses!(single_quoted, b"'single quoted'", "'single quoted'".into());
    parses!(single_quoted, b"'\\'\\\\'", "'\\'\\\\'".into());
}

#[test]
fn test_name() {
    parses!(name, b"lower_word2", Name::LowerWord("lower_word2".into()));
    parses!(
        name,
        b"'single quoted'",
        Name::SingleQuoted("'single quoted'".into())
    );
    parses!(name, b"123", Name::Integer("123".into()));
}

#[test]
fn test_name_list() {
    parses!(
        name_list,
        b"[ name , 'name' , 123 ]",
        vec![
            Name::LowerWord("name".into()),
            Name::SingleQuoted("'name'".into()),
            Name::Integer("123".into())
        ]
    );
}

#[test]
fn test_variable() {
    parses!(variable, b"X", "X".into());
}

#[test]
fn test_fof_plain_term() {
    parses!(
        fof_plain_term,
        b"c",
        FofTerm::Functor(Name::LowerWord("c".into()), vec![])
    );
    parses!(
        fof_plain_term,
        b"f ( )",
        FofTerm::Functor(Name::LowerWord("f".into()), vec![])
    );
    parses!(
        fof_plain_term,
        b"f ( X )",
        FofTerm::Functor(
            Name::LowerWord("f".into()),
            vec![FofTerm::Variable("X".into())]
        )
    );
    parses!(
        fof_plain_term,
        b"f ( X, g ( Y ) )",
        FofTerm::Functor(
            Name::LowerWord("f".into()),
            vec![
                FofTerm::Variable("X".into()),
                FofTerm::Functor(
                    Name::LowerWord("g".into()),
                    vec![FofTerm::Variable("Y".into())]
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
            Name::LowerWord("f".into()),
            vec![FofTerm::Variable("X".into())]
        )
    );
}

#[test]
fn test_fof_term() {
    parses!(
        fof_term,
        b"f(X)",
        FofTerm::Functor(
            Name::LowerWord("f".into()),
            vec![FofTerm::Variable("X".into())]
        )
    );
    parses!(fof_term, b"X", FofTerm::Variable("X".into()));
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
                Name::LowerWord("f".into()),
                vec![FofTerm::Variable("X".into())]
            ),
            FofTerm::Variable("Y".into())
        )
    );
    parses!(
        fof_atomic_formula,
        b"p(X)",
        FofFormula::Predicate(
            Name::LowerWord("p".into()),
            vec![FofTerm::Variable("X".into())]
        )
    );
    parses!(
        fof_atomic_formula,
        b"X != Y",
        FofFormula::Infix(
            InfixEquality::NotEqual,
            FofTerm::Variable("X".into()),
            FofTerm::Variable("Y".into())
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
            vec!["X".into()],
            Box::new(FofFormula::Boolean(true))
        )
    );
    parses!(
        fof_quantified_formula,
        b"? [ X , Y, Z ] : $true",
        FofFormula::Quantified(
            FofQuantifier::Exists,
            vec!["X".into(), "Y".into(), "Z".into()],
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
            vec!["X".into()],
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
    let p = FofFormula::Predicate(Name::LowerWord("p".into()), vec![]);
    let q = FofFormula::Predicate(Name::LowerWord("q".into()), vec![]);
    let r = FofFormula::Predicate(Name::LowerWord("r".into()), vec![]);

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
    parses!(
        fof_logic_formula,
        b"~p => q",
        FofFormula::NonAssoc(
            NonAssocConnective::LRImplies,
            Box::new(FofFormula::Unary(
                UnaryConnective::Not,
                Box::new(p.clone())
            )),
            Box::new(q.clone())
        )
    );
}

#[test]
fn test_literal() {
    let p = FofFormula::Predicate(Name::LowerWord("p".into()), vec![]);

    parses!(literal, b"p", CnfLiteral::Literal(p.clone()));
    parses!(literal, b"~ p", CnfLiteral::NegatedLiteral(p));
}

#[test]
fn test_disjunction() {
    let p = CnfLiteral::Literal(FofFormula::Predicate(
        Name::LowerWord("p".into()),
        vec![],
    ));
    let q = CnfLiteral::NegatedLiteral(FofFormula::Predicate(
        Name::LowerWord("q".into()),
        vec![],
    ));
    let r = CnfLiteral::Literal(FofFormula::Predicate(
        Name::LowerWord("r".into()),
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
        Name::LowerWord("p".into()),
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
        DagSource::Name(Name::LowerWord("name".into()))
    );
    parses!(
        dag_source,
        b"inference ( deduction , [ ] , [] )",
        DagSource::Inference("deduction".into(), vec![])
    );
}

#[test]
fn test_external_source() {
    parses!(
        external_source,
        b"file ( 'file' )",
        ExternalSource::File("'file'".into(), None)
    );
    parses!(
        external_source,
        b"file ( 'file' , name )",
        ExternalSource::File(
            "'file'".into(),
            Some(Name::LowerWord("name".into()))
        )
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
        Source::External(ExternalSource::File("'file'".into(), None))
    );
    parses!(
        source,
        b"name",
        Source::Dag(DagSource::Name(Name::LowerWord("name".into())))
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
    parses!(include_path, b"'test'", Included("test".into()));
    parses!(include_path, b"'\\\\\\''", Included("\\\\\\'"));
}

#[test]
fn test_include() {
    parses!(
        include,
        b"include ( 'test' )",
        Statement::Include(Included("test".into()), None)
    );
    parses!(
        include,
        b"include( 'test', [ test ])",
        Statement::Include(
            Included("test".into()),
            Some(vec![Name::LowerWord("test".into())])
        )
    );
}

#[test]
fn test_fof_annotated() {
    parses!(
        fof_annotated,
        b"fof ( test , axiom , $true )",
        Statement::Fof(
            Name::LowerWord("test".into()),
            FormulaRole::Axiom,
            FofFormula::Boolean(true),
            None
        )
    );
    parses!(
        fof_annotated,
        b"fof ( test , axiom , $true , unknown )",
        Statement::Fof(
            Name::LowerWord("test".into()),
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
            Name::LowerWord("test".into()),
            FormulaRole::Axiom,
            CnfFormula(vec![CnfLiteral::Literal(FofFormula::Boolean(true))]),
            None
        )
    );
    parses!(
        cnf_annotated,
        b"cnf ( test , axiom , $true , unknown )",
        Statement::Cnf(
            Name::LowerWord("test".into()),
            FormulaRole::Axiom,
            CnfFormula(vec![CnfLiteral::Literal(FofFormula::Boolean(true))]),
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
        Statement::Include(Included("test".into()), None)
    );
    parses!(
        tptp_input,
        b"fof ( test , axiom , $true ) .",
        Statement::Fof(
            Name::LowerWord("test".into()),
            FormulaRole::Axiom,
            FofFormula::Boolean(true),
            None
        )
    );
    parses!(
        tptp_input,
        b"cnf ( test , axiom , $true ) .",
        Statement::Cnf(
            Name::LowerWord("test".into()),
            FormulaRole::Axiom,
            CnfFormula(vec![CnfLiteral::Literal(FofFormula::Boolean(true))]),
            None
        )
    );
}

#[test]
fn test_tptp_input_or_eof() {
    parses!(
        tptp_input_or_eof,
        b"include('test').",
        Some(Statement::Include(Included("test".into()), None))
    );
    parses!(tptp_input_or_eof, b"", None);
}
