use alloc::fmt;
use alloc::format;
use insta::{assert_debug_snapshot, assert_display_snapshot};

use crate::parsers::*;

fn do_parse<'a, P, T: 'a>(parser: P, input: &'a [u8]) -> T
where
    P: FnOnce(&'a [u8]) -> nom::IResult<&[u8], T, ()>,
{
    match parser(input) {
        Ok((b"\0", result)) => result,
        Ok((_, _)) => panic!("parsed, but bytes remaining"),
        Err(_) => panic!("parse error"),
    }
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
    parse_nodisplay(whitespace, b" \t\n\0");
}

#[test]
fn test_comment_line() {
    parse_nodisplay(comment_line, b"% a comment\n\0");
}

#[test]
fn test_comment_block() {
    parse_nodisplay(comment_block, b"/* a\n block * / comment\n*/\0");
}

#[test]
fn test_ignored() {
    parse_nodisplay(ignored, b"\0");
    parse_nodisplay(ignored, b"   %test\n  /* test\ntest */  \0");
}

#[test]
fn test_upper_word() {
    parse(upper_word, b"X\0");
    parse(upper_word, b"Aa123\0");
}

#[test]
fn test_lower_word() {
    parse(lower_word, b"x\0");
    parse(lower_word, b"aA123\0");
}

#[test]
fn test_dollar_word() {
    parse(dollar_word, b"$dollar\0");
}

#[test]
fn test_dollar_dollar_word() {
    parse(dollar_dollar_word, b"$$dollar\0");
}

#[test]
fn test_single_quoted() {
    parse(single_quoted, b"'single quoted'\0");
    parse(single_quoted, b"'\\'\\\\'\0");
}

#[test]
fn test_distinct_object() {
    parse(distinct_object, b"\"distinct object\"\0");
    parse(distinct_object, b"\"\\\"\\\\\"\0");
}

#[test]
fn test_atomic_word() {
    parse(atomic_word, b"x\0");
    parse(atomic_word, b"'single quoted'\0");
}

#[test]
fn test_integer() {
    parse(integer, b"0\0");
    parse(integer, b"123\0");
    parse(integer, b"-123\0");
}

#[test]
fn test_rational() {
    parse(rational, b"0/1\0");
    parse(rational, b"123/456\0");
    parse(rational, b"-123/456\0");
}

#[test]
fn test_real() {
    parse(real, b"0.0\0");
    parse(real, b"1E0\0");
    parse(real, b"-1.23E-456\0");
    parse(real, b"1e-06\0");
}

#[test]
fn test_number() {
    parse(number, b"-123\0");
    parse(number, b"-123/456\0");
    parse(number, b"-1.23E-456\0");
}

#[test]
fn test_name() {
    parse(name, b"lower_word2\0");
    parse(name, b"'single quoted'\0");
    parse(name, b"123\0");
}

#[test]
fn test_variable() {
    parse(variable, b"X\0");
}

#[test]
fn test_atomic_system_word() {
    parse(atomic_system_word, b"$$atomic\0");
}

#[test]
fn test_atomic_defined_word() {
    parse(atomic_defined_word, b"$atomic\0");
}

#[test]
fn test_system_functor() {
    parse(system_functor, b"$$system_functor\0");
}

#[test]
fn test_system_constant() {
    parse(system_constant, b"$$system_constant\0");
}

#[test]
fn test_defined_functor() {
    parse(defined_functor, b"$defined_functor\0");
}

#[test]
fn test_defined_constant() {
    parse(defined_constant, b"$defined_constant\0");
}

#[test]
fn test_defined_term() {
    parse(defined_term, b"-123\0");
    parse(defined_term, b"\"distinct object\"\0");
}

#[test]
fn test_functor() {
    parse(functor, b"functor\0");
}

#[test]
fn test_constant() {
    parse(constant, b"constant\0");
}

#[test]
fn test_untyped_atom() {
    parse(untyped_atom, b"constant\0");
    parse(untyped_atom, b"$$system_constant\0");
}

#[test]
fn test_fof_arguments() {
    parse(fof_arguments, b"( c )\0");
    parse(fof_arguments, b"( X )\0");
    parse(fof_arguments, b"( X, f ( X ) )\0");
}

#[test]
fn test_fof_plain_term() {
    parse(fof_plain_term, b"c\0");
    parse(fof_plain_term, b"f ( X )\0");
    parse(fof_plain_term, b"f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_system_term() {
    parse(fof_system_term, b"$$c\0");
    parse(fof_system_term, b"$$f ( X )\0");
    parse(fof_system_term, b"$$f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_defined_plain_term() {
    parse(fof_defined_plain_term, b"$defined_plain_term\0");
}

#[test]
fn test_fof_defined_atomic_term() {
    parse(fof_defined_atomic_term, b"$defined_atomic_term\0");
}

#[test]
fn test_fof_defined_term() {
    parse(fof_defined_term, b"$defined_term\0");
    parse(fof_defined_term, b"-123\0");
}

#[test]
fn test_fof_function_term() {
    parse(fof_function_term, b"f(X)\0");
    parse(fof_function_term, b"$defined\0");
}

#[test]
fn test_fof_term() {
    parse(fof_term, b"f(X)\0");
    parse(fof_term, b"X\0");
}

#[test]
fn test_fof_plain_atomic_formula() {
    parse(fof_plain_atomic_formula, b"f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_defined_plain_formula() {
    parse(fof_defined_plain_formula, b"$defined_plain_formula\0");
}

#[test]
fn test_infix_equality() {
    parse(infix_equality, b"=\0");
}

#[test]
fn test_infix_inequality() {
    parse(infix_inequality, b"!=\0");
}

#[test]
fn test_unary_connective() {
    parse(unary_connective, b"~\0");
}

#[test]
fn test_nonassoc_connective() {
    parse(nonassoc_connective, b"<=\0");
    parse(nonassoc_connective, b"<=>\0");
    parse(nonassoc_connective, b"=>\0");
    parse(nonassoc_connective, b"<~>\0");
    parse(nonassoc_connective, b"~&\0");
    parse(nonassoc_connective, b"~|\0");
}

#[test]
fn test_fof_quantifier() {
    parse(fof_quantifier, b"!\0");
    parse(fof_quantifier, b"?\0");
}

#[test]
fn test_defined_infix_pred() {
    parse(defined_infix_pred, b"=\0");
}

#[test]
fn test_fof_defined_infix_formula() {
    parse(fof_defined_infix_formula, b"f(X) = c\0");
}

#[test]
fn test_fof_defined_atomic_formula() {
    parse(fof_defined_atomic_formula, b"$true\0");
    parse(fof_defined_atomic_formula, b"$false\0");
    parse(fof_defined_atomic_formula, b"f(X) = c\0");
}

#[test]
fn test_fof_system_atomic_formula() {
    parse(fof_system_atomic_formula, b"$$system\0");
}

#[test]
fn test_fof_atomic_formula() {
    parse(fof_atomic_formula, b"$true\0");
    parse(fof_atomic_formula, b"f(X) = Y\0");
    parse(fof_atomic_formula, b"p(X)\0");
    parse(fof_atomic_formula, b"$$system\0");
}

#[test]
fn test_fof_variable_list() {
    parse(fof_variable_list, b"X\0");
    parse(fof_variable_list, b"X , Y\0");
    parse(fof_variable_list, b"X , Y , Z\0");
}

#[test]
fn test_fof_quantified_formula() {
    parse(fof_quantified_formula, b"! [ X ] : $true\0");
    parse(fof_quantified_formula, b"? [ X , Y , Z ] : $true\0");
}

#[test]
fn test_fof_infix_unary() {
    parse(fof_infix_unary, b"f(X) != c\0");
}

#[test]
fn test_fof_unary_formula() {
    parse(fof_unary_formula, b"~ $true\0");
    parse(fof_unary_formula, b"f(X) != c\0");
}

#[test]
fn test_fof_unitary_formula() {
    parse(fof_unitary_formula, b"( $true )\0");
    parse(fof_unitary_formula, b"$true\0");
    parse(fof_unitary_formula, b"![X]: $true\0");
}

#[test]
fn test_fof_unit_formula() {
    parse(fof_unit_formula, b"($true)\0");
    parse(fof_unit_formula, b"~$true\0");
}

#[test]
fn test_fof_binary_nonassoc() {
    parse(fof_binary_nonassoc, b"p => q\0");
    parse(fof_binary_nonassoc, b"p ~| q\0");
}

#[test]
fn test_fof_or_formula() {
    parse(fof_or_formula, b"p | q | r\0");
}

#[test]
fn test_fof_and_formula() {
    parse(fof_and_formula, b"p & q\0");
}

#[test]
fn test_fof_binary_assoc() {
    parse(fof_binary_assoc, b"p | q | r\0");
    parse(fof_binary_assoc, b"p & q\0");
}

#[test]
fn test_fof_logic_formula() {
    parse(fof_logic_formula, b"~p\0");
    parse(fof_logic_formula, b"p => q\0");
    parse(fof_logic_formula, b"p & q\0");
    parse(fof_logic_formula, b"p | q | r\0");
    parse(fof_logic_formula, b"p\0");
    parse(fof_logic_formula, b"~p => q\0");
}

#[test]
fn test_fof_formula() {
    parse(fof_formula, b"p\0");
    parse(fof_formula, b"~~p\0");
    parse(fof_formula, b"(p)\0");
    parse(fof_formula, b"$true|$false\0");
    parse(fof_formula, b"(![X,Y,Z]:?[Q]:Q!=p(A))&p&(q=>r)\0");
}

#[test]
fn test_literal() {
    parse(literal, b"p\0");
    parse(literal, b"~ p\0");
    parse(literal, b"f(X) = c\0");
}

#[test]
fn test_disjunction() {
    parse(disjunction, b"p\0");
    parse(disjunction, b"p | ~q\0");
    parse(disjunction, b"p | ~q | r\0");
}

#[test]
fn test_cnf_formula() {
    parse(cnf_formula, b"p\0");
    parse(cnf_formula, b"( p )\0");
    parse(cnf_formula, b"p | ~q\0");
}

#[test]
fn test_thf_plain_atomic() {
    parse(thf_plain_atomic, b"c\0");
}

#[test]
fn test_thf_defined_atomic() {
    parse(thf_defined_atomic, b"$defined_constant\0");
}

#[test]
fn test_thf_atomic_formula() {
    parse(thf_atomic_formula, b"c\0");
    parse(thf_atomic_formula, b"$defined_constant\0");
}

#[test]
fn test_thf_unitary_term() {
    parse(thf_unitary_term, b"c\0");
    parse(thf_unitary_term, b"X\0");
    parse(thf_unitary_term, b"( p )\0");
}

#[test]
fn test_thf_defined_infix() {
    parse(thf_defined_infix, b"c = d\0");
}

#[test]
fn test_thf_unitary_formula() {
    parse(thf_unitary_term, b"c\0");
    parse(thf_unitary_term, b"X\0");
    parse(thf_unitary_term, b"( p )\0");
}

#[test]
fn test_thf_infix_unary() {
    parse(thf_infix_unary, b"c != d\0");
}

#[test]
fn test_thf_unary_formula() {
    parse(thf_unary_formula, b"c != d\0");
}

#[test]
fn test_thf_unit_formula() {
    parse(thf_unit_formula, b"c = d\0");
    parse(thf_unit_formula, b"c != d\0");
    parse(thf_unit_formula, b"c\0");
}

#[test]
fn test_thf_or_formula() {
    parse(thf_or_formula, b"c | d\0");
    parse(thf_or_formula, b"c | d | e\0");
}

#[test]
fn test_thf_apply_formula() {
    parse(thf_apply_formula, b"c @ d\0");
    parse(thf_apply_formula, b"c @ d @ e\0");
}

#[test]
fn test_thf_binary_assoc() {
    parse(thf_binary_assoc, b"c | d\0");
    parse(thf_apply_formula, b"c @ d\0");
}

#[test]
fn test_thf_unitary_type() {
    parse(thf_unitary_type, b"c\0");
}

#[test]
fn test_thf_apply_type() {
    parse(thf_apply_type, b"c @ d\0");
}

#[test]
fn test_thf_mapping_type() {
    parse(thf_mapping_type, b"c > d\0");
    parse(thf_mapping_type, b"c > d > e\0");
}

#[test]
fn test_thf_top_level_type() {
    parse(thf_top_level_type, b"c\0");
    parse(thf_top_level_type, b"c > d\0");
    parse(thf_top_level_type, b"c @ d\0");
}

#[test]
fn test_thf_atom_typing() {
    parse(thf_atom_typing, b"c : t\0");
    parse(thf_atom_typing, b"( c : t )\0");
}

#[test]
fn test_thf_binary_type() {
    parse(thf_binary_type, b"c > d\0");
}

#[test]
fn test_thf_binary_formula() {
    parse(thf_binary_formula, b"c | d\0");
    parse(thf_binary_formula, b"c > d\0");
}

#[test]
fn test_thf_logic_formula() {
    parse(thf_logic_formula, b"c | d\0");
    parse(thf_logic_formula, b"c != d\0");
    parse(thf_logic_formula, b"c = d\0");
    parse(thf_logic_formula, b"c\0");
}

#[test]
fn test_thf_formula() {
    parse(thf_formula, b"c\0");
    parse(thf_formula, b"c : t\0");
}

#[test]
fn test_role() {
    parse(formula_role, b"axiom\0");
    parse(formula_role, b"conjecture\0");
}

#[test]
fn test_general_terms() {
    parse(general_terms, b"X\0");
    parse(general_terms, b"X , Y\0");
}

#[test]
fn test_general_list() {
    parse(general_list, b"[ ]\0");
    parse(general_list, b"[ X ]\0");
    parse(general_list, b"[ X , Y ]\0");
}

#[test]
fn test_general_function() {
    parse(general_function, b"atomic ( X )\0");
}

#[test]
fn test_formula_data() {
    parse(formula_data, b"$fof ( p )\0");
    parse(formula_data, b"$cnf ( p )\0");
    parse(formula_data, b"$fot ( t )\0");
}

#[test]
fn test_general_data() {
    parse(general_data, b"c\0");
    parse(general_data, b"X\0");
    parse(general_data, b"atomic ( X )\0");
    parse(general_data, b"$fof ( p )\0");
    parse(general_data, b"123\0");
    parse(general_data, b"\"distinct object\"\0");
}

#[test]
fn test_general_term() {
    parse(general_term, b"[ X , Y ]\0");
    parse(general_term, b"$fof ( p )\0");
    parse(general_term, b"X : Y\0");
}

#[test]
fn test_useful_info() {
    parse(useful_info, b"[ X , Y ]\0");
}

#[test]
fn test_optional_info() {
    parse(optional_info, b"\0");
    parse(optional_info, b", [ X , Y ]\0");
}

#[test]
fn test_annotations() {
    parse(annotations, b"\0");
    parse(annotations, b", c\0");
    parse(annotations, b", c , [X]\0");
}

#[test]
fn test_fof_annotated() {
    parse(fof_annotated, b"fof ( test , axiom , $true ) .\0");
    parse(fof_annotated, b"fof ( test , axiom , $true , unknown ) .\0");
    parse(
        fof_annotated,
        b"fof ( test , axiom , $true , unknown , [] ) .\0",
    );
}

#[test]
fn test_cnf_annotated() {
    parse(cnf_annotated, b"cnf ( test , axiom , $true ) .\0");
    parse(cnf_annotated, b"cnf ( test , axiom , $true , unknown ) .\0");
    parse(
        cnf_annotated,
        b"cnf ( test , axiom , $true , unknown , [] ) .\0",
    );
}

#[test]
fn test_annotated_formula() {
    parse(annotated_formula, b"fof ( test , axiom , $true ) .\0");
    parse(annotated_formula, b"cnf ( test , axiom , $true ) .\0");
}

#[test]
fn test_file_name() {
    parse(file_name, b"'test'\0");
}

#[test]
fn test_name_list() {
    parse(name_list, b"name , 'name' , 123\0");
    parse(name_list, b"name\0");
}

#[test]
fn test_formula_selection() {
    parse(formula_selection, b", [ name , 'name' , 123 ]\0");
    parse(formula_selection, b", [ name ]\0");
}

#[test]
fn test_include() {
    parse(include, b"include ( 'test' ) .\0");
    parse(include, b"include( 'test', [ test ] ) .\0");
}

#[test]
fn test_tptp_input() {
    parse(tptp_input, b"include ( 'test' ) .\0");
    parse(tptp_input, b"fof ( test , axiom , $true ) .\0");
    parse(tptp_input, b"cnf ( test , axiom , $true ) .\0");
}
