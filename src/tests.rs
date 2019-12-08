use insta::{assert_debug_snapshot, assert_display_snapshot};
use std::fmt;

use crate::parsers::*;

fn do_parse<'a, P, T: 'a>(parser: P, input: &'a [u8]) -> T
where
    P: FnOnce(&'a [u8]) -> nom::IResult<&[u8], T, ()>,
{
    let result = parser(input);
    assert!(result.is_ok(), "parse error");
    let (remaining, parsed) = result.unwrap();
    assert!(remaining.is_empty(), "parsed, but bytes remaining");
    parsed
}

fn parse_nodisplay<'a, P, T: 'a>(parser: P, input: &'a [u8])
where
    P: FnOnce(&'a [u8]) -> nom::IResult<&[u8], T, ()>,
    T: fmt::Debug,
{
    let parsed = do_parse(parser, input);
    assert_debug_snapshot!(parsed);
}

fn parse<'a, P, T: 'a>(parser: P, input: &'a [u8])
where
    P: FnOnce(&'a [u8]) -> nom::IResult<&[u8], T, ()>,
    T: fmt::Debug + fmt::Display,
{
    let parsed = do_parse(parser, input);
    assert_debug_snapshot!(parsed);
    assert_display_snapshot!(parsed);
}

#[test]
fn test_whitespace() {
    parse_nodisplay(whitespace, b" \t\n");
}

#[test]
fn test_comment_line() {
    parse_nodisplay(comment_line, b"% a comment\n");
}

#[test]
fn test_comment_block() {
    parse_nodisplay(comment_block, b"/* a\n block * / comment\n*/");
}

#[test]
fn test_ignored() {
    parse_nodisplay(ignored, b"");
    parse_nodisplay(ignored, b"   %test\n  /* test\ntest */  ");
}

#[test]
fn test_upper_word() {
    parse(upper_word, b"X");
    parse(upper_word, b"Aa123");
}

#[test]
fn test_lower_word() {
    parse(lower_word, b"x");
    parse(lower_word, b"aA123");
}

#[test]
fn test_dollar_word() {
    parse(dollar_word, b"$dollar");
}

#[test]
fn test_single_quoted() {
    parse(single_quoted, b"'single quoted'");
    parse(single_quoted, b"'\\'\\\\'");
}

#[test]
fn test_atomic_word() {
    parse(atomic_word, b"x");
    parse(atomic_word, b"'single quoted'");
}

#[test]
fn test_integer() {
    parse(integer, b"0");
    parse(integer, b"123");
    parse(integer, b"-123");
}

#[test]
fn test_name() {
    parse(name, b"lower_word2");
    parse(name, b"'single quoted'");
    parse(name, b"123");
}

#[test]
fn test_variable() {
    parse(variable, b"X");
}

#[test]
fn test_atomic_defiend_word() {
    parse(atomic_defined_word, b"$atomic");
}

#[test]
fn test_defined_functor() {
    parse(defined_functor, b"$defined_functor");
}

#[test]
fn test_defined_constant() {
    parse(defined_constant, b"$defined_constant");
}

#[test]
fn test_functor() {
    parse(functor, b"functor");
}

#[test]
fn test_fof_arguments() {
    parse(fof_arguments, b"( c )");
    parse(fof_arguments, b"( X )");
    parse(fof_arguments, b"( X, f ( X ) )");
}

#[test]
fn test_fof_plain_term() {
    parse(fof_plain_term, b"c");
    parse(fof_plain_term, b"f ( X )");
    parse(fof_plain_term, b"f ( X, g ( Y ) )");
}

#[test]
fn test_fof_defined_plain_term() {
    parse(fof_defined_plain_term, b"$defined_plain_term");
}

#[test]
fn test_fof_defined_atomic_term() {
    parse(fof_defined_atomic_term, b"$defined_atomic_term");
}

#[test]
fn test_fof_defined_term() {
    parse(fof_defined_term, b"$defined_term");
}

#[test]
fn test_fof_function_term() {
    parse(fof_function_term, b"f(X)");
    parse(fof_function_term, b"$defined");
}

#[test]
fn test_fof_term() {
    parse(fof_term, b"f(X)");
    parse(fof_term, b"X");
}

#[test]
fn test_fof_plain_atomic_formula() {
    parse(fof_plain_atomic_formula, b"f ( X, g ( Y ) )");
}

#[test]
fn test_fof_defined_plain_formula() {
    parse(fof_defined_plain_formula, b"$defined_plain_formula");
}

#[test]
fn test_infix_equality() {
    parse(infix_equality, b"=");
}

#[test]
fn test_infix_inequality() {
    parse(infix_inequality, b"!=");
}

#[test]
fn test_unary_connective() {
    parse(unary_connective, b"~");
}

#[test]
fn test_nonassoc_connective() {
    parse(nonassoc_connective, b"<=");
    parse(nonassoc_connective, b"<=>");
    parse(nonassoc_connective, b"=>");
    parse(nonassoc_connective, b"<~>");
    parse(nonassoc_connective, b"~&");
    parse(nonassoc_connective, b"~|");
}

#[test]
fn test_fof_quantifier() {
    parse(fof_quantifier, b"!");
    parse(fof_quantifier, b"?");
}

#[test]
fn test_defined_infix_pred() {
    parse(defined_infix_pred, b"=");
}

#[test]
fn test_fof_defined_infix_formula() {
    parse(fof_defined_infix_formula, b"f(X) = c");
}

#[test]
fn test_fof_defined_atomic_formula() {
    parse(fof_defined_atomic_formula, b"$true");
    parse(fof_defined_atomic_formula, b"$false");
    parse(fof_defined_atomic_formula, b"f(X) = c");
}

#[test]
fn test_fof_atomic_formula() {
    parse(fof_atomic_formula, b"$true");
    parse(fof_atomic_formula, b"f(X) = Y");
    parse(fof_atomic_formula, b"p(X)");
}

#[test]
fn test_fof_variable_list() {
    parse(fof_variable_list, b"X");
    parse(fof_variable_list, b"X , Y");
    parse(fof_variable_list, b"X , Y , Z");
}

#[test]
fn test_fof_quantified_formula() {
    parse(fof_quantified_formula, b"! [ X ] : $true");
    parse(fof_quantified_formula, b"? [ X , Y , Z ] : $true");
}

#[test]
fn test_fof_infix_unary() {
    parse(fof_infix_unary, b"f(X) != c");
}

#[test]
fn test_fof_unary_formula() {
    parse(fof_unary_formula, b"~ $true");
    parse(fof_unary_formula, b"f(X) != c");
}

#[test]
fn test_fof_unitary_formula() {
    parse(fof_unitary_formula, b"( $true )");
    parse(fof_unitary_formula, b"$true");
    parse(fof_unitary_formula, b"![X]: $true");
}

#[test]
fn test_fof_unit_formula() {
    parse(fof_unit_formula, b"($true)");
    parse(fof_unit_formula, b"~$true");
}

#[test]
fn test_fof_binary_nonassoc() {
    parse(fof_binary_nonassoc, b"p => q");
    parse(fof_binary_nonassoc, b"p ~| q");
}

#[test]
fn test_fof_or_formula() {
    parse(fof_or_formula, b"p | q | r");
}

#[test]
fn test_fof_and_formula() {
    parse(fof_and_formula, b"p & q");
}

#[test]
fn test_fof_binary_assoc() {
    parse(fof_binary_assoc, b"p | q | r");
    parse(fof_binary_assoc, b"p & q");
}

#[test]
fn test_fof_logic_formula() {
    parse(fof_logic_formula, b"~p");
    parse(fof_logic_formula, b"p => q");
    parse(fof_logic_formula, b"p & q");
    parse(fof_logic_formula, b"p | q | r");
    parse(fof_logic_formula, b"p");
    parse(fof_logic_formula, b"~p => q");
}

#[test]
fn test_fof_formula() {
    parse(fof_formula, b"p");
    parse(fof_formula, b"~~p");
    parse(fof_formula, b"(p)");
    parse(fof_formula, b"$true|$false");
    parse(fof_formula, b"(![X,Y,Z]:?[Q]:Q!=p(A))&p&(q=>r)");
}

#[test]
fn test_literal() {
    parse(literal, b"p");
    parse(literal, b"~ p");
    parse(literal, b"f(X) = c");
}

#[test]
fn test_disjunction() {
    parse(disjunction, b"p");
    parse(disjunction, b"p | ~q");
    parse(disjunction, b"p | ~q | r");
}

#[test]
fn test_cnf_formula() {
    parse(cnf_formula, b"p");
    parse(cnf_formula, b"( p )");
    parse(cnf_formula, b"p | ~q");
}

#[test]
fn test_role() {
    parse(formula_role, b"axiom");
    parse(formula_role, b"conjecture");
}

#[test]
fn test_general_terms() {
    parse(general_terms, b"X");
    parse(general_terms, b"X , Y");
}

#[test]
fn test_general_list() {
    parse(general_list, b"[ ]");
    parse(general_list, b"[ X ]");
    parse(general_list, b"[ X , Y ]");
}

#[test]
fn test_general_function() {
    parse(general_function, b"atomic ( X )");
}

#[test]
fn test_formula_data() {
    parse(formula_data, b"$fof ( p )");
    parse(formula_data, b"$cnf ( p )");
}

#[test]
fn test_general_data() {
    parse(general_data, b"c");
    parse(general_data, b"X");
    parse(general_data, b"atomic ( X )");
    parse(general_data, b"$fof ( p )");
}

#[test]
fn test_general_term() {
    parse(general_term, b"[ X , Y ]");
    parse(general_term, b"$fof ( p )");
    parse(general_term, b"X : Y");
}

#[test]
fn test_useful_info() {
    parse(useful_info, b"[ X , Y ]");
}

#[test]
fn test_optional_info() {
    parse(optional_info, b"");
    parse(optional_info, b", [ X , Y ]");
}

#[test]
fn test_annotations() {
    parse(annotations, b"");
    parse(annotations, b", c");
    parse(annotations, b", c , [X]");
}

#[test]
fn test_fof_annotated() {
    parse(fof_annotated, b"fof ( test , axiom , $true ) .");
    parse(fof_annotated, b"fof ( test , axiom , $true , unknown ) .");
    parse(
        fof_annotated,
        b"fof ( test , axiom , $true , unknown , [] ) .",
    );
}

#[test]
fn test_cnf_annotated() {
    parse(cnf_annotated, b"cnf ( test , axiom , $true ) .");
    parse(cnf_annotated, b"cnf ( test , axiom , $true , unknown ) .");
    parse(
        cnf_annotated,
        b"cnf ( test , axiom , $true , unknown , [] ) .",
    );
}

#[test]
fn test_annotated_formula() {
    parse(annotated_formula, b"fof ( test , axiom , $true ) .");
    parse(annotated_formula, b"cnf ( test , axiom , $true ) .");
}

#[test]
fn test_file_name() {
    parse(file_name, b"'test'");
}

#[test]
fn test_name_list() {
    parse(name_list, b"name , 'name' , 123");
    parse(name_list, b"name");
}

#[test]
fn test_formula_selection() {
    parse(formula_selection, b", [ name , 'name' , 123 ]");
    parse(formula_selection, b", [ name ]");
}

#[test]
fn test_include() {
    parse(include, b"include ( 'test' ) .");
    parse(include, b"include( 'test', [ test ] ) .");
}

#[test]
fn test_tptp_input() {
    parse(tptp_input, b"include ( 'test' ) .");
    parse(tptp_input, b"fof ( test , axiom , $true ) .");
    parse(tptp_input, b"cnf ( test , axiom , $true ) .");
}

#[test]
fn test_ignored_then_tptp_input() {
    parse_nodisplay(ignored_then_tptp_input, b" include('test').");
    parse_nodisplay(ignored_then_tptp_input, b" include('test').");
    parse_nodisplay(ignored_then_tptp_input, b" ");
    parse_nodisplay(ignored_then_tptp_input, b"");
}
