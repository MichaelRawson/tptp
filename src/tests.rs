use insta::assert_debug_snapshot;

use crate::parsers::*;

macro_rules! check_parse {
    ($parser:expr, $input:expr) => {
        let result: nom::IResult<_, _, ()> = $parser($input);
        assert!(result.is_ok(), "parse error");
        let (remaining, parsed) = result.unwrap();
        assert!(remaining.is_empty(), "parsed, but bytes remaining");
        assert_debug_snapshot!(parsed);
    };
}

#[test]
fn test_whitespace() {
    check_parse!(whitespace, b" \t\n");
}

#[test]
fn test_comment_line() {
    check_parse!(comment_line, b"% a comment\n");
}

#[test]
fn test_comment_block() {
    check_parse!(comment_block, b"/* a\n block * / comment\n*/");
}

#[test]
fn test_ignored() {
    check_parse!(ignored, b"");
    check_parse!(ignored, b"   %test\n  /* test\ntest */  ");
}

#[test]
fn test_upper_word() {
    check_parse!(upper_word, b"X");
    check_parse!(upper_word, b"Aa123");
}

#[test]
fn test_lower_word() {
    check_parse!(lower_word, b"x");
    check_parse!(lower_word, b"aA123");
}

#[test]
fn test_single_quoted() {
    check_parse!(single_quoted, b"'single quoted'");
    check_parse!(single_quoted, b"'\\'\\\\'");
}

#[test]
fn test_atomic_word() {
    check_parse!(atomic_word, b"x");
    check_parse!(single_quoted, b"'single quoted'");
}

#[test]
fn test_integer() {
    check_parse!(integer, b"0");
    check_parse!(integer, b"123");
    check_parse!(integer, b"-123");
}

#[test]
fn test_name() {
    check_parse!(name, b"lower_word2");
    check_parse!(name, b"'single quoted'");
    check_parse!(name, b"123");
}

#[test]
fn test_name_list() {
    check_parse!(name_list, b"[ name , 'name' , 123 ]");
}

#[test]
fn test_variable() {
    check_parse!(variable, b"X");
}

#[test]
fn test_fof_plain_term() {
    check_parse!(fof_plain_term, b"c");
    check_parse!(fof_plain_term, b"f ( )");
    check_parse!(fof_plain_term, b"f ( X )");
    check_parse!(fof_plain_term, b"f ( X, g ( Y ) )");
}

#[test]
fn test_fof_function_term() {
    check_parse!(fof_function_term, b"f(X)");
}

#[test]
fn test_fof_term() {
    check_parse!(fof_term, b"f(X)");
    check_parse!(fof_term, b"X");
}

#[test]
fn test_fof_defined_atomic_formula() {
    check_parse!(fof_defined_atomic_formula, b"$true");
    check_parse!(fof_defined_atomic_formula, b"$false");
}

#[test]
fn test_infix_equality() {
    check_parse!(infix_equality, b"=");
    check_parse!(infix_equality, b"!=");
}

#[test]
fn test_fof_atomic_formula() {
    check_parse!(fof_atomic_formula, b"$true");
    check_parse!(fof_atomic_formula, b"f(X) = Y");
    check_parse!(fof_atomic_formula, b"p(X)");
    check_parse!(fof_atomic_formula, b"X != Y");
}

#[test]
fn test_fof_quantified_formula() {
    check_parse!(fof_quantified_formula, b"! [ X ] : $true");
    check_parse!(fof_quantified_formula, b"? [ X , Y, Z ] : $true");
}

#[test]
fn test_fof_unitary_formula() {
    check_parse!(fof_unitary_formula, b"( $true )");
    check_parse!(fof_unitary_formula, b"$true");
    check_parse!(fof_unitary_formula, b"![X]: $true");
}

#[test]
fn test_fof_unary_formula() {
    check_parse!(fof_unary_formula, b"~ $true");
}

#[test]
fn test_fof_unit_formula() {
    check_parse!(fof_unit_formula, b"($true)");
    check_parse!(fof_unit_formula, b"~$true");
}

#[test]
fn test_nonassoc_connective() {
    check_parse!(nonassoc_connective, b"<=");
    check_parse!(nonassoc_connective, b"<=>");
    check_parse!(nonassoc_connective, b"=>");
    check_parse!(nonassoc_connective, b"<~>");
    check_parse!(nonassoc_connective, b"~&");
    check_parse!(nonassoc_connective, b"~|");
}

#[test]
fn test_assoc_connective() {
    check_parse!(assoc_connective, b"&");
    check_parse!(assoc_connective, b"|");
}

#[test]
fn test_fof_logic_formula() {
    check_parse!(fof_logic_formula, b"~p");
    check_parse!(fof_logic_formula, b"p => q");
    check_parse!(fof_logic_formula, b"p & q");
    check_parse!(fof_logic_formula, b"p | q | r");
    check_parse!(fof_logic_formula, b"p");
    check_parse!(fof_logic_formula, b"~p => q");
}

#[test]
fn test_literal() {
    check_parse!(literal, b"p");
    check_parse!(literal, b"~ p");
    check_parse!(literal, b"f(X) = c");
}

#[test]
fn test_disjunction() {
    check_parse!(disjunction, b"p");
    check_parse!(disjunction, b"p | ~q");
    check_parse!(disjunction, b"p | ~q | r");
}

#[test]
fn test_cnf_formula() {
    check_parse!(cnf_formula, b"p");
    check_parse!(cnf_formula, b"( p )");
}

#[test]
fn test_role() {
    check_parse!(formula_role, b"axiom");
    check_parse!(formula_role, b"conjecture");
}

#[test]
fn test_unknown_source() {
    check_parse!(unknown_source, b"unknown");
}

#[test]
fn test_dag_source() {
    check_parse!(dag_source, b"name");
    check_parse!(dag_source, b"inference ( deduction , [ ] , [] )");
}

#[test]
fn test_external_source() {
    check_parse!(external_source, b"file ( 'file' )");
    check_parse!(external_source, b"file ( 'file' , name )");
}

#[test]
fn test_sources() {
    check_parse!(sources, b"[ ]");
    check_parse!(sources, b"[ unknown ]");
    check_parse!(sources, b"[ unknown , unknown ]");
}

#[test]
fn test_source() {
    check_parse!(source, b"unknown");
    check_parse!(source, b"file ( 'file' )");
    check_parse!(source, b"name");
    check_parse!(source, b"[ unknown ]");
}

#[test]
fn test_annotations() {
    check_parse!(annotations, b"unknown");
}

#[test]
fn test_include_path() {
    check_parse!(include_path, b"'test'");
    check_parse!(include_path, b"'\\\\\\''");
}

#[test]
fn test_include() {
    check_parse!(include, b"include ( 'test' )");
    check_parse!(include, b"include( 'test', [ test ])");
}

#[test]
fn test_fof_annotated() {
    check_parse!(fof_annotated, b"fof ( test , axiom , $true )");
    check_parse!(fof_annotated, b"fof ( test , axiom , $true , unknown )");
}

#[test]
fn test_cnf_annotated() {
    check_parse!(cnf_annotated, b"cnf ( test , axiom , $true )");
    check_parse!(cnf_annotated, b"cnf ( test , axiom , $true , unknown )");
}

#[test]
fn test_tptp_input() {
    check_parse!(tptp_input, b"include ( 'test' ) .");
    check_parse!(tptp_input, b"fof ( test , axiom , $true ) .");
    check_parse!(tptp_input, b"cnf ( test , axiom , $true ) .");
}

#[test]
fn test_tptp_input_or_eof() {
    check_parse!(tptp_input_or_eof, b"include('test').");
    check_parse!(tptp_input_or_eof, b"");
}
