use alloc::fmt;
use alloc::format;
use insta::{assert_debug_snapshot, assert_display_snapshot};

use crate::cnf;
use crate::common::*;
use crate::fof;
use crate::top::*;
use crate::{Parse, Result};

fn check_size<T>() {
    assert!(core::mem::size_of::<T>() <= 64);
}

fn check_parse<'a, P, T: 'a>(parser: P, input: &'a [u8]) -> T
where
    P: FnOnce(&'a [u8]) -> Result<T, ()>,
{
    match parser(input) {
        Ok((b"\0", result)) => result,
        Ok((_, _)) => panic!("parsed, but bytes remaining"),
        Err(_) => panic!("parse error"),
    }
}

fn parse_unit<'a, P, T: 'a>(parser: P, input: &'a [u8])
where
    P: FnOnce(&'a [u8]) -> Result<T, ()>,
    T: fmt::Debug,
{
    let parsed = check_parse(parser, input);
    assert_debug_snapshot!(parsed);
}

fn parse_fn<'a, P, T: 'a>(parser: P, input: &'a [u8])
where
    P: FnOnce(&'a [u8]) -> Result<T, ()>,
    T: fmt::Debug + fmt::Display,
{
    let parsed = check_parse(parser, input);
    assert_debug_snapshot!(parsed);
    assert_display_snapshot!(parsed);
}

fn parse<'a, T: 'a>(input: &'a [u8])
where
    T: Parse<'a, ()> + fmt::Debug + fmt::Display,
{
    parse_fn(T::parse, input);
}

#[test]
fn test_whitespace() {
    parse_unit(whitespace, b" \t\n\0");
}

#[test]
fn test_comment_line() {
    parse_unit(comment_line, b"% a comment\n\0");
}

#[test]
fn test_comment_block() {
    parse_unit(comment_block, b"/* a\n block * / comment\n*/\0");
}

#[test]
fn test_ignored() {
    parse_unit(ignored, b"\0");
    parse_unit(ignored, b"   %test\n  /* test\ntest */  \0");
}

#[test]
fn test_lower_word() {
    check_size::<LowerWord>();
    parse::<LowerWord>(b"x\0");
    parse::<LowerWord>(b"aA123\0");
}

#[test]
fn test_upper_word() {
    check_size::<UpperWord>();
    parse::<UpperWord>(b"X\0");
    parse::<UpperWord>(b"Aa123\0");
}

#[test]
fn test_single_quoted() {
    check_size::<SingleQuoted>();
    parse::<SingleQuoted>(b"'single quoted'\0");
    parse::<SingleQuoted>(b"'\\'\\\\'\0");
}

#[test]
fn test_dollar_word() {
    check_size::<DollarWord>();
    parse::<DollarWord>(b"$dollar\0");
}

#[test]
fn test_dollar_dollar_word() {
    check_size::<DollarDollarWord>();
    parse::<DollarDollarWord>(b"$$dollar\0");
}

#[test]
fn test_distinct_object() {
    check_size::<DistinctObject>();
    parse::<DistinctObject>(b"\"distinct object\"\0");
    parse::<DistinctObject>(b"\"\\\"\\\\\"\0");
}

#[test]
fn test_atomic_word() {
    check_size::<AtomicWord>();
    parse::<AtomicWord>(b"x\0");
    parse::<AtomicWord>(b"'single quoted'\0");
}

#[test]
fn test_integer() {
    check_size::<Integer>();
    parse::<Integer>(b"0\0");
    parse::<Integer>(b"123\0");
    parse::<Integer>(b"-123\0");
}

#[test]
fn test_rational() {
    check_size::<Rational>();
    parse::<Rational>(b"0/1\0");
    parse::<Rational>(b"123/456\0");
    parse::<Rational>(b"-123/456\0");
}

#[test]
fn test_real() {
    check_size::<Real>();
    parse::<Real>(b"0.0\0");
    parse::<Real>(b"1E0\0");
    parse::<Real>(b"-1.23E-456\0");
    parse::<Real>(b"1e-06\0");
}

#[test]
fn test_number() {
    check_size::<Number>();
    parse::<Number>(b"-123\0");
    parse::<Number>(b"-123/456\0");
    parse::<Number>(b"-1.23E-456\0");
}

#[test]
fn test_name() {
    check_size::<Name>();
    parse::<Name>(b"lower_word2\0");
    parse::<Name>(b"'single quoted'\0");
    parse::<Name>(b"123\0");
}

#[test]
fn test_variable() {
    check_size::<Variable>();
    parse::<Variable>(b"X\0");
}

#[test]
fn test_atomic_system_word() {
    check_size::<AtomicSystemWord>();
    parse::<AtomicSystemWord>(b"$$atomic\0");
}

#[test]
fn test_atomic_defined_word() {
    check_size::<AtomicDefinedWord>();
    parse::<AtomicDefinedWord>(b"$atomic\0");
}

#[test]
fn test_system_functor() {
    check_size::<SystemFunctor>();
    parse::<SystemFunctor>(b"$$system_functor\0");
}

#[test]
fn test_system_constant() {
    check_size::<SystemConstant>();
    parse::<SystemConstant>(b"$$system_constant\0");
}

#[test]
fn test_defined_functor() {
    check_size::<DefinedFunctor>();
    parse::<DefinedFunctor>(b"$defined_functor\0");
}

#[test]
fn test_defined_constant() {
    check_size::<DefinedConstant>();
    parse::<DefinedConstant>(b"$defined_constant\0");
}

#[test]
fn test_defined_term() {
    check_size::<DefinedTerm>();
    parse::<DefinedTerm>(b"-123\0");
    parse::<DefinedTerm>(b"\"distinct object\"\0");
}

#[test]
fn test_functor() {
    check_size::<Functor>();
    parse::<Functor>(b"functor\0");
}

#[test]
fn test_constant() {
    check_size::<Constant>();
    parse::<Constant>(b"constant\0");
}

#[test]
fn test_fof_arguments() {
    check_size::<fof::Arguments>();
    parse::<fof::Arguments>(b"( c )\0");
    parse::<fof::Arguments>(b"( X )\0");
    parse::<fof::Arguments>(b"( X, f ( X ) )\0");
}

#[test]
fn test_fof_plain_term() {
    check_size::<fof::PlainTerm>();
    parse::<fof::PlainTerm>(b"c\0");
    parse::<fof::PlainTerm>(b"f ( X )\0");
    parse::<fof::PlainTerm>(b"f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_system_term() {
    check_size::<fof::SystemTerm>();
    parse::<fof::SystemTerm>(b"$$c\0");
    parse::<fof::SystemTerm>(b"$$f ( X )\0");
    parse::<fof::SystemTerm>(b"$$f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_defined_plain_term() {
    check_size::<fof::DefinedPlainTerm>();
    parse::<fof::DefinedPlainTerm>(b"$defined_plain_term\0");
}

#[test]
fn test_fof_defined_atomic_term() {
    check_size::<fof::DefinedAtomicTerm>();
    parse::<fof::DefinedAtomicTerm>(b"$defined_atomic_term\0");
}

#[test]
fn test_fof_defined_term() {
    check_size::<fof::DefinedTerm>();
    parse::<fof::DefinedTerm>(b"$defined_term\0");
    parse::<fof::DefinedTerm>(b"-123\0");
}

#[test]
fn test_fof_function_term() {
    check_size::<fof::FunctionTerm>();
    parse::<fof::FunctionTerm>(b"f(X)\0");
    parse::<fof::FunctionTerm>(b"$defined\0");
}

#[test]
fn test_fof_term() {
    check_size::<fof::Term>();
    parse::<fof::Term>(b"f(X)\0");
    parse::<fof::Term>(b"X\0");
}

#[test]
fn test_fof_plain_atomic_formula() {
    check_size::<fof::PlainAtomicFormula>();
    parse::<fof::PlainAtomicFormula>(b"f ( X, g ( Y ) )\0");
}

#[test]
fn test_fof_defined_plain_formula() {
    check_size::<fof::DefinedPlainFormula>();
    parse::<fof::DefinedPlainFormula>(b"$defined_plain_formula\0");
}

#[test]
fn test_infix_equality() {
    check_size::<InfixEquality>();
    parse::<InfixEquality>(b"=\0");
}

#[test]
fn test_infix_inequality() {
    check_size::<InfixInequality>();
    parse::<InfixInequality>(b"!=\0");
}

#[test]
fn test_unary_connective() {
    check_size::<UnaryConnective>();
    parse::<UnaryConnective>(b"~\0");
}

#[test]
fn test_nonassoc_connective() {
    check_size::<NonassocConnective>();
    parse::<NonassocConnective>(b"<=\0");
    parse::<NonassocConnective>(b"<=>\0");
    parse::<NonassocConnective>(b"=>\0");
    parse::<NonassocConnective>(b"<~>\0");
    parse::<NonassocConnective>(b"~&\0");
    parse::<NonassocConnective>(b"~|\0");
}

#[test]
fn test_assoc_connective() {
    check_size::<AssocConnective>();
    parse::<AssocConnective>(b"&\0");
    parse::<AssocConnective>(b"|\0");
}

#[test]
fn test_fof_quantifier() {
    check_size::<fof::Quantifier>();
    parse::<fof::Quantifier>(b"!\0");
    parse::<fof::Quantifier>(b"?\0");
}

#[test]
fn test_defined_infix_pred() {
    check_size::<DefinedInfixPred>();
    parse::<DefinedInfixPred>(b"=\0");
}

#[test]
fn test_fof_defined_infix_formula() {
    check_size::<fof::DefinedInfixFormula>();
    parse::<fof::DefinedInfixFormula>(b"f(X) = c\0");
}

#[test]
fn test_fof_defined_atomic_formula() {
    check_size::<fof::DefinedAtomicFormula>();
    parse::<fof::DefinedAtomicFormula>(b"$true\0");
    parse::<fof::DefinedAtomicFormula>(b"$false\0");
    parse::<fof::DefinedAtomicFormula>(b"f(X) = c\0");
}

#[test]
fn test_fof_system_atomic_formula() {
    check_size::<fof::SystemAtomicFormula>();
    parse::<fof::SystemAtomicFormula>(b"$$system\0");
}

#[test]
fn test_fof_atomic_formula() {
    check_size::<fof::AtomicFormula>();
    parse::<fof::AtomicFormula>(b"$true\0");
    parse::<fof::AtomicFormula>(b"f(X) = Y\0");
    parse::<fof::AtomicFormula>(b"p(X)\0");
    parse::<fof::AtomicFormula>(b"$$system\0");
}

#[test]
fn test_fof_variable_list() {
    check_size::<fof::VariableList>();
    parse::<fof::VariableList>(b"X\0");
    parse::<fof::VariableList>(b"X , Y\0");
    parse::<fof::VariableList>(b"X , Y , Z\0");
}

#[test]
fn test_fof_quantified_formula() {
    check_size::<fof::QuantifiedFormula>();
    parse::<fof::QuantifiedFormula>(b"! [ X ] : $true\0");
    parse::<fof::QuantifiedFormula>(b"? [ X , Y , Z ] : $true\0");
}

#[test]
fn test_fof_infix_unary() {
    check_size::<fof::InfixUnary>();
    parse::<fof::InfixUnary>(b"f(X) != c\0");
}

#[test]
fn test_fof_unary_formula() {
    check_size::<fof::UnaryFormula>();
    parse::<fof::UnaryFormula>(b"~ $true\0");
    parse::<fof::UnaryFormula>(b"f(X) != c\0");
}

#[test]
fn test_fof_unitary_formula() {
    check_size::<fof::UnitaryFormula>();
    parse::<fof::UnitaryFormula>(b"( $true )\0");
    parse::<fof::UnitaryFormula>(b"$true\0");
    parse::<fof::UnitaryFormula>(b"![X]: $true\0");
}

#[test]
fn test_fof_unit_formula() {
    check_size::<fof::UnitFormula>();
    parse::<fof::UnitFormula>(b"($true)\0");
    parse::<fof::UnitFormula>(b"~$true\0");
}

#[test]
fn test_fof_binary_nonassoc() {
    check_size::<fof::BinaryNonassoc>();
    parse::<fof::BinaryNonassoc>(b"p => q\0");
    parse::<fof::BinaryNonassoc>(b"p ~| q\0");
}

#[test]
fn test_fof_or_formula() {
    check_size::<fof::OrFormula>();
    parse::<fof::OrFormula>(b"p | q | r\0");
}

#[test]
fn test_fof_and_formula() {
    check_size::<fof::AndFormula>();
    parse::<fof::AndFormula>(b"p & q\0");
}

#[test]
fn test_fof_binary_assoc() {
    check_size::<fof::BinaryAssoc>();
    parse::<fof::BinaryAssoc>(b"p | q | r\0");
    parse::<fof::BinaryAssoc>(b"p & q\0");
}

#[test]
fn test_fof_logic_formula() {
    check_size::<fof::LogicFormula>();
    parse::<fof::LogicFormula>(b"~p\0");
    parse::<fof::LogicFormula>(b"p => q\0");
    parse::<fof::LogicFormula>(b"p & q\0");
    parse::<fof::LogicFormula>(b"p | q | r\0");
    parse::<fof::LogicFormula>(b"p\0");
    parse::<fof::LogicFormula>(b"~p => q\0");
}

#[test]
fn test_fof_formula() {
    check_size::<fof::Formula>();
    parse::<fof::Formula>(b"p\0");
    parse::<fof::Formula>(b"~~p\0");
    parse::<fof::Formula>(b"(p)\0");
    parse::<fof::Formula>(b"$true|$false\0");
    parse::<fof::Formula>(b"(![X,Y,Z]:?[Q]:Q!=p(A))&p&(q=>r)\0");
}

#[test]
fn test_literal() {
    check_size::<cnf::Literal>();
    parse::<cnf::Literal>(b"p\0");
    parse::<cnf::Literal>(b"~ p\0");
    parse::<cnf::Literal>(b"f(X) = c\0");
}

#[test]
fn test_disjunction() {
    check_size::<cnf::Disjunction>();
    parse::<cnf::Disjunction>(b"p\0");
    parse::<cnf::Disjunction>(b"p | ~q\0");
    parse::<cnf::Disjunction>(b"p | ~q | r\0");
}

#[test]
fn test_cnf_formula() {
    check_size::<cnf::Formula>();
    parse::<cnf::Formula>(b"p\0");
    parse::<cnf::Formula>(b"( p )\0");
    parse::<cnf::Formula>(b"p | ~q\0");
}

#[test]
fn test_role() {
    check_size::<FormulaRole>();
    parse::<FormulaRole>(b"axiom\0");
    parse::<FormulaRole>(b"conjecture\0");
}

#[test]
fn test_general_terms() {
    check_size::<GeneralTerms>();
    parse::<GeneralTerms>(b"X\0");
    parse::<GeneralTerms>(b"X , Y\0");
}

#[test]
fn test_general_list() {
    check_size::<GeneralList>();
    parse::<GeneralList>(b"[ ]\0");
    parse::<GeneralList>(b"[ X ]\0");
    parse::<GeneralList>(b"[ X , Y ]\0");
}

#[test]
fn test_general_function() {
    check_size::<GeneralFunction>();
    parse::<GeneralFunction>(b"atomic ( X )\0");
}

#[test]
fn test_formula_data() {
    check_size::<FormulaData>();
    parse::<FormulaData>(b"$fof ( p )\0");
    parse::<FormulaData>(b"$cnf ( p )\0");
    parse::<FormulaData>(b"$fot ( t )\0");
}

#[test]
fn test_general_data() {
    check_size::<GeneralData>();
    parse::<GeneralData>(b"c\0");
    parse::<GeneralData>(b"X\0");
    parse::<GeneralData>(b"atomic ( X )\0");
    parse::<GeneralData>(b"$fof ( p )\0");
    parse::<GeneralData>(b"123\0");
    parse::<GeneralData>(b"\"distinct object\"\0");
}

#[test]
fn test_general_term() {
    check_size::<GeneralTerm>();
    parse::<GeneralTerm>(b"[ X , Y ]\0");
    parse::<GeneralTerm>(b"$fof ( p )\0");
    parse::<GeneralTerm>(b"X : Y\0");
}

#[test]
fn test_useful_info() {
    check_size::<UsefulInfo>();
    parse::<UsefulInfo>(b"[ X , Y ]\0");
}

#[test]
fn test_optional_info() {
    check_size::<OptionalInfo>();
    parse::<OptionalInfo>(b"\0");
    parse::<OptionalInfo>(b", [ X , Y ]\0");
}

#[test]
fn test_annotations() {
    check_size::<Annotations>();
    parse::<Annotations>(b"\0");
    parse::<Annotations>(b", c\0");
    parse::<Annotations>(b", c , [X]\0");
}

#[test]
fn test_fof_annotated() {
    check_size::<FofAnnotated>();
    parse::<FofAnnotated>(b"fof ( test , axiom , $true ) .\0");
    parse::<FofAnnotated>(b"fof ( test , axiom , $true , unknown ) .\0");
    parse::<FofAnnotated>(b"fof ( test , axiom , $true , unknown , [] ) .\0");
}

#[test]
fn test_cnf_annotated() {
    check_size::<CnfAnnotated>();
    parse::<CnfAnnotated>(b"cnf ( test , axiom , $true ) .\0");
    parse::<CnfAnnotated>(b"cnf ( test , axiom , $true , unknown ) .\0");
    parse::<CnfAnnotated>(b"cnf ( test , axiom , $true , unknown , [] ) .\0")
}

#[test]
fn test_annotated_formula() {
    check_size::<AnnotatedFormula>();
    parse::<AnnotatedFormula>(b"fof ( test , axiom , $true ) .\0");
    parse::<AnnotatedFormula>(b"cnf ( test , axiom , $true ) .\0");
}

#[test]
fn test_file_name() {
    check_size::<FileName>();
    parse::<FileName>(b"'test'\0");
}

#[test]
fn test_name_list() {
    check_size::<NameList>();
    parse::<NameList>(b"name , 'name' , 123\0");
    parse::<NameList>(b"name\0");
}

#[test]
fn test_formula_selection() {
    check_size::<FormulaSelection>();
    parse::<FormulaSelection>(b", [ name , 'name' , 123 ]\0");
    parse::<FormulaSelection>(b", [ name ]\0");
}

#[test]
fn test_include() {
    check_size::<Include>();
    parse::<Include>(b"include ( 'test' ) .\0");
    parse::<Include>(b"include ( 'test', [ test ] ) .\0");
}

#[test]
fn test_tptp_input() {
    check_size::<TPTPInput>();
    parse::<TPTPInput>(b"include ( 'test' ) .\0");
    parse::<TPTPInput>(b"fof ( test , axiom , $true ) .\0");
    parse::<TPTPInput>(b"cnf ( test , axiom , $true ) .\0");
}

// https://github.com/MichaelRawson/tptp/issues/2
// with thanks to @skbaek
#[test]
fn test_large_annotations() {
    parse::<TPTPInput>(
        b"cnf(c_0_137, negated_conjecture, $false, inference(cn,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[inference(rw,[status(thm)],[c_0_134, c_0_95]), c_0_97]), c_0_99]), c_0_101]), c_0_103]), c_0_105]), c_0_107]), c_0_109]), c_0_111]), c_0_113]), c_0_115]), c_0_117]), c_0_119]), c_0_121]), c_0_123]), c_0_125]), c_0_127]), c_0_129]), c_0_131]), c_0_133]), c_0_135])])).\0"
    );
}
