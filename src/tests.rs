use alloc::fmt;
use alloc::format;
use insta::{assert_debug_snapshot, assert_display_snapshot};

use crate::parsers::*;
use crate::syntax::*;

fn check_size<T>() {
    assert!(core::mem::size_of::<T>() <= 64);
}

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
fn test_lower_word() {
    check_size::<LowerWord>();
    parse(lower_word, b"x\0");
    parse(lower_word, b"aA123\0");
}

#[test]
fn test_upper_word() {
    check_size::<UpperWord>();
    parse(upper_word, b"X\0");
    parse(upper_word, b"Aa123\0");
}

#[test]
fn test_dollar_word() {
    check_size::<DollarWord>();
    parse(dollar_word, b"$dollar\0");
}

#[test]
fn test_dollar_dollar_word() {
    check_size::<DollarDollarWord>();
    parse(dollar_dollar_word, b"$$dollar\0");
}

#[test]
fn test_single_quoted() {
    check_size::<SingleQuoted>();
    parse(single_quoted, b"'single quoted'\0");
    parse(single_quoted, b"'\\'\\\\'\0");
}

#[test]
fn test_distinct_object() {
    check_size::<DistinctObject>();
    parse(distinct_object, b"\"distinct object\"\0");
    parse(distinct_object, b"\"\\\"\\\\\"\0");
}

#[test]
fn test_atomic_word() {
    check_size::<AtomicWord>();
    parse(atomic_word, b"x\0");
    parse(atomic_word, b"'single quoted'\0");
}

#[test]
fn test_integer() {
    check_size::<Integer>();
    parse(integer, b"0\0");
    parse(integer, b"123\0");
    parse(integer, b"-123\0");
}

#[test]
fn test_rational() {
    check_size::<Rational>();
    parse(rational, b"0/1\0");
    parse(rational, b"123/456\0");
    parse(rational, b"-123/456\0");
}

#[test]
fn test_real() {
    check_size::<Real>();
    parse(real, b"0.0\0");
    parse(real, b"1E0\0");
    parse(real, b"-1.23E-456\0");
    parse(real, b"1e-06\0");
}

#[test]
fn test_number() {
    check_size::<Number>();
    parse(number, b"-123\0");
    parse(number, b"-123/456\0");
    parse(number, b"-1.23E-456\0");
}

#[test]
fn test_name() {
    check_size::<Name>();
    parse(name, b"lower_word2\0");
    parse(name, b"'single quoted'\0");
    parse(name, b"123\0");
}

#[test]
fn test_variable() {
    check_size::<Variable>();
    parse(variable, b"X\0");
}

#[test]
fn test_atomic_system_word() {
    check_size::<AtomicSystemWord>();
    parse(atomic_system_word, b"$$atomic\0");
}

#[test]
fn test_atomic_defined_word() {
    check_size::<AtomicDefinedWord>();
    parse(atomic_defined_word, b"$atomic\0");
}

#[test]
fn test_system_functor() {
    check_size::<SystemFunctor>();
    parse(system_functor, b"$$system_functor\0");
}

#[test]
fn test_system_constant() {
    check_size::<SystemConstant>();
    parse(system_constant, b"$$system_constant\0");
}

#[test]
fn test_defined_functor() {
    check_size::<DefinedFunctor>();
    parse(defined_functor, b"$defined_functor\0");
}

#[test]
fn test_defined_constant() {
    check_size::<DefinedConstant>();
    parse(defined_constant, b"$defined_constant\0");
}

#[test]
fn test_defined_term() {
    check_size::<DefinedTerm>();
    parse(defined_term, b"-123\0");
    parse(defined_term, b"\"distinct object\"\0");
}

#[test]
fn test_functor() {
    check_size::<Functor>();
    parse(functor, b"functor\0");
}

#[test]
fn test_constant() {
    check_size::<Constant>();
    parse(constant, b"constant\0");
}

#[test]
fn test_fof_arguments() {
    check_size::<FofArguments>();
    parse(fof_arguments, b"( c )\0");
    parse(fof_arguments, b"( X )\0");
    parse(fof_arguments, b"( X, f ( X ) )\0");
}

#[test]
fn test_fof_plain_term() {
    check_size::<FofPlainTerm>();
    parse(fof_plain_term, b"c\0");
    parse(fof_plain_term, b"f ( X )\0");
    parse(fof_plain_term, b"f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_system_term() {
    check_size::<FofSystemTerm>();
    parse(fof_system_term, b"$$c\0");
    parse(fof_system_term, b"$$f ( X )\0");
    parse(fof_system_term, b"$$f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_defined_plain_term() {
    check_size::<FofDefinedPlainTerm>();
    parse(fof_defined_plain_term, b"$defined_plain_term\0");
}

#[test]
fn test_fof_defined_atomic_term() {
    check_size::<FofDefinedAtomicTerm>();
    parse(fof_defined_atomic_term, b"$defined_atomic_term\0");
}

#[test]
fn test_fof_defined_term() {
    check_size::<FofDefinedTerm>();
    parse(fof_defined_term, b"$defined_term\0");
    parse(fof_defined_term, b"-123\0");
}

#[test]
fn test_fof_function_term() {
    check_size::<FofFunctionTerm>();
    parse(fof_function_term, b"f(X)\0");
    parse(fof_function_term, b"$defined\0");
}

#[test]
fn test_fof_term() {
    check_size::<FofTerm>();
    parse(fof_term, b"f(X)\0");
    parse(fof_term, b"X\0");
}

#[test]
fn test_fof_plain_atomic_formula() {
    check_size::<FofPlainAtomicFormula>();
    parse(fof_plain_atomic_formula, b"f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_defined_plain_formula() {
    check_size::<FofDefinedPlainFormula>();
    parse(fof_defined_plain_formula, b"$defined_plain_formula\0");
}

#[test]
fn test_infix_equality() {
    check_size::<InfixEquality>();
    parse(infix_equality, b"=\0");
}

#[test]
fn test_infix_inequality() {
    check_size::<InfixInequality>();
    parse(infix_inequality, b"!=\0");
}

#[test]
fn test_unary_connective() {
    check_size::<UnaryConnective>();
    parse(unary_connective, b"~\0");
}

#[test]
fn test_nonassoc_connective() {
    check_size::<NonassocConnective>();
    parse(nonassoc_connective, b"<=\0");
    parse(nonassoc_connective, b"<=>\0");
    parse(nonassoc_connective, b"=>\0");
    parse(nonassoc_connective, b"<~>\0");
    parse(nonassoc_connective, b"~&\0");
    parse(nonassoc_connective, b"~|\0");
}

#[test]
fn test_fof_quantifier() {
    check_size::<FofQuantifier>();
    parse(fof_quantifier, b"!\0");
    parse(fof_quantifier, b"?\0");
}

#[test]
fn test_defined_infix_pred() {
    check_size::<DefinedInfixPred>();
    parse(defined_infix_pred, b"=\0");
}

#[test]
fn test_fof_defined_infix_formula() {
    check_size::<FofDefinedInfixFormula>();
    parse(fof_defined_infix_formula, b"f(X) = c\0");
}

#[test]
fn test_fof_defined_atomic_formula() {
    check_size::<FofDefinedAtomicFormula>();
    parse(fof_defined_atomic_formula, b"$true\0");
    parse(fof_defined_atomic_formula, b"$false\0");
    parse(fof_defined_atomic_formula, b"f(X) = c\0");
}

#[test]
fn test_fof_system_atomic_formula() {
    check_size::<FofSystemAtomicFormula>();
    parse(fof_system_atomic_formula, b"$$system\0");
}

#[test]
fn test_fof_atomic_formula() {
    check_size::<FofAtomicFormula>();
    parse(fof_atomic_formula, b"$true\0");
    parse(fof_atomic_formula, b"f(X) = Y\0");
    parse(fof_atomic_formula, b"p(X)\0");
    parse(fof_atomic_formula, b"$$system\0");
}

#[test]
fn test_fof_variable_list() {
    check_size::<FofVariableList>();
    parse(fof_variable_list, b"X\0");
    parse(fof_variable_list, b"X , Y\0");
    parse(fof_variable_list, b"X , Y , Z\0");
}

#[test]
fn test_fof_quantified_formula() {
    check_size::<FofQuantifiedFormula>();
    parse(fof_quantified_formula, b"! [ X ] : $true\0");
    parse(fof_quantified_formula, b"? [ X , Y , Z ] : $true\0");
}

#[test]
fn test_fof_infix_unary() {
    check_size::<FofInfixUnary>();
    parse(fof_infix_unary, b"f(X) != c\0");
}

#[test]
fn test_fof_unary_formula() {
    check_size::<FofUnaryFormula>();
    parse(fof_unary_formula, b"~ $true\0");
    parse(fof_unary_formula, b"f(X) != c\0");
}

#[test]
fn test_fof_unitary_formula() {
    check_size::<FofUnitaryFormula>();
    parse(fof_unitary_formula, b"( $true )\0");
    parse(fof_unitary_formula, b"$true\0");
    parse(fof_unitary_formula, b"![X]: $true\0");
}

#[test]
fn test_fof_unit_formula() {
    check_size::<FofUnitFormula>();
    parse(fof_unit_formula, b"($true)\0");
    parse(fof_unit_formula, b"~$true\0");
}

#[test]
fn test_fof_binary_nonassoc() {
    check_size::<FofBinaryNonassoc>();
    parse(fof_binary_nonassoc, b"p => q\0");
    parse(fof_binary_nonassoc, b"p ~| q\0");
}

#[test]
fn test_fof_or_formula() {
    check_size::<FofOrFormula>();
    parse(fof_or_formula, b"p | q | r\0");
}

#[test]
fn test_fof_and_formula() {
    check_size::<FofAndFormula>();
    parse(fof_and_formula, b"p & q\0");
}

#[test]
fn test_fof_binary_assoc() {
    check_size::<FofBinaryAssoc>();
    parse(fof_binary_assoc, b"p | q | r\0");
    parse(fof_binary_assoc, b"p & q\0");
}

#[test]
fn test_fof_logic_formula() {
    check_size::<FofLogicFormula>();
    parse(fof_logic_formula, b"~p\0");
    parse(fof_logic_formula, b"p => q\0");
    parse(fof_logic_formula, b"p & q\0");
    parse(fof_logic_formula, b"p | q | r\0");
    parse(fof_logic_formula, b"p\0");
    parse(fof_logic_formula, b"~p => q\0");
}

#[test]
fn test_fof_formula() {
    check_size::<FofFormula>();
    parse(fof_formula, b"p\0");
    parse(fof_formula, b"~~p\0");
    parse(fof_formula, b"(p)\0");
    parse(fof_formula, b"$true|$false\0");
    parse(fof_formula, b"(![X,Y,Z]:?[Q]:Q!=p(A))&p&(q=>r)\0");
}

#[test]
fn test_literal() {
    check_size::<Literal>();
    parse(literal, b"p\0");
    parse(literal, b"~ p\0");
    parse(literal, b"f(X) = c\0");
}

#[test]
fn test_disjunction() {
    check_size::<Disjunction>();
    parse(disjunction, b"p\0");
    parse(disjunction, b"p | ~q\0");
    parse(disjunction, b"p | ~q | r\0");
}

#[test]
fn test_cnf_formula() {
    check_size::<CnfFormula>();
    parse(cnf_formula, b"p\0");
    parse(cnf_formula, b"( p )\0");
    parse(cnf_formula, b"p | ~q\0");
}

#[test]
fn test_role() {
    check_size::<FormulaRole>();
    parse(formula_role, b"axiom\0");
    parse(formula_role, b"conjecture\0");
}

#[test]
fn test_general_terms() {
    check_size::<GeneralTerms>();
    parse(general_terms, b"X\0");
    parse(general_terms, b"X , Y\0");
}

#[test]
fn test_general_list() {
    check_size::<GeneralList>();
    parse(general_list, b"[ ]\0");
    parse(general_list, b"[ X ]\0");
    parse(general_list, b"[ X , Y ]\0");
}

#[test]
fn test_general_function() {
    check_size::<GeneralFunction>();
    parse(general_function, b"atomic ( X )\0");
}

#[test]
fn test_formula_data() {
    check_size::<FormulaData>();
    parse(formula_data, b"$fof ( p )\0");
    parse(formula_data, b"$cnf ( p )\0");
    parse(formula_data, b"$fot ( t )\0");
}

#[test]
fn test_general_data() {
    check_size::<GeneralData>();
    parse(general_data, b"c\0");
    parse(general_data, b"X\0");
    parse(general_data, b"atomic ( X )\0");
    parse(general_data, b"$fof ( p )\0");
    parse(general_data, b"123\0");
    parse(general_data, b"\"distinct object\"\0");
}

#[test]
fn test_general_term() {
    check_size::<GeneralTerm>();
    parse(general_term, b"[ X , Y ]\0");
    parse(general_term, b"$fof ( p )\0");
    parse(general_term, b"X : Y\0");
}

#[test]
fn test_useful_info() {
    check_size::<UsefulInfo>();
    parse(useful_info, b"[ X , Y ]\0");
}

#[test]
fn test_optional_info() {
    check_size::<OptionalInfo>();
    parse(optional_info, b"\0");
    parse(optional_info, b", [ X , Y ]\0");
}

#[test]
fn test_annotations() {
    check_size::<Annotations>();
    parse(annotations, b"\0");
    parse(annotations, b", c\0");
    parse(annotations, b", c , [X]\0");
}

#[test]
fn test_fof_annotated() {
    check_size::<FofAnnotated>();
    parse(fof_annotated, b"fof ( test , axiom , $true ) .\0");
    parse(fof_annotated, b"fof ( test , axiom , $true , unknown ) .\0");
    parse(
        fof_annotated,
        b"fof ( test , axiom , $true , unknown , [] ) .\0",
    );
}

#[test]
fn test_cnf_annotated() {
    check_size::<CnfAnnotated>();
    parse(cnf_annotated, b"cnf ( test , axiom , $true ) .\0");
    parse(cnf_annotated, b"cnf ( test , axiom , $true , unknown ) .\0");
    parse(
        cnf_annotated,
        b"cnf ( test , axiom , $true , unknown , [] ) .\0",
    );
}

#[test]
fn test_annotated_formula() {
    check_size::<AnnotatedFormula>();
    parse(annotated_formula, b"fof ( test , axiom , $true ) .\0");
    parse(annotated_formula, b"cnf ( test , axiom , $true ) .\0");
}

#[test]
fn test_file_name() {
    check_size::<FileName>();
    parse(file_name, b"'test'\0");
}

#[test]
fn test_name_list() {
    check_size::<NameList>();
    parse(name_list, b"name , 'name' , 123\0");
    parse(name_list, b"name\0");
}

#[test]
fn test_formula_selection() {
    check_size::<FormulaSelection>();
    parse(formula_selection, b", [ name , 'name' , 123 ]\0");
    parse(formula_selection, b", [ name ]\0");
}

#[test]
fn test_include() {
    check_size::<Include>();
    parse(include, b"include ( 'test' ) .\0");
    parse(include, b"include ( 'test', [ test ] ) .\0");
}

#[test]
fn test_tptp_input() {
    check_size::<TPTPInput>();
    parse(tptp_input, b"include ( 'test' ) .\0");
    parse(tptp_input, b"fof ( test , axiom , $true ) .\0");
    parse(tptp_input, b"cnf ( test , axiom , $true ) .\0");
}

// https://github.com/MichaelRawson/tptp/issues/2
// with thanks to @skbaek
#[test]
fn test_large_annotations() {
    parse(
        tptp_input,
        b"cnf(c_0_137, negated_conjecture, $false, inference(cn,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[c_0_134, c_0_95]), c_0_97]), c_0_99]), c_0_101]), c_0_103]), c_0_105]), c_0_107]), c_0_109]), c_0_111]), c_0_113]), c_0_115]), c_0_117]), c_0_119]), c_0_121]), c_0_123]), c_0_125]), c_0_127]), c_0_129]), c_0_131]), c_0_133]), c_0_135])])).\0"
    );
}
