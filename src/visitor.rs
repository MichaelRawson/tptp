use crate::cnf;
use crate::common::*;
use crate::fof;
use crate::top::*;

/// [visitor pattern](https://en.wikipedia.org/wiki/Visitor_pattern) trait
///
/// This can make traversing parsed syntax easier.
/// Default method implementations visit child structures in parsing order.
pub trait Visitor<'a> {
    fn visit_lower_word(&mut self, _lower_word: &LowerWord<'a>) {}

    fn visit_upper_word(&mut self, _upper_word: &UpperWord<'a>) {}

    fn visit_single_quoted(&mut self, _single_quoted: &SingleQuoted<'a>) {}

    fn visit_distinct_object(
        &mut self,
        _distinct_object: &DistinctObject<'a>,
    ) {
    }

    fn visit_atomic_word(&mut self, atomic_word: &AtomicWord<'a>) {
        match atomic_word {
            AtomicWord::Lower(lower_word) => self.visit_lower_word(lower_word),
            AtomicWord::SingleQuoted(single_quoted) => {
                self.visit_single_quoted(single_quoted)
            }
        }
    }

    fn visit_integer(&mut self, _integer: &Integer<'a>) {}

    fn visit_rational(&mut self, _rational: &Rational<'a>) {}

    fn visit_real(&mut self, _real: &Real<'a>) {}

    fn visit_name(&mut self, name: &Name<'a>) {
        match name {
            Name::AtomicWord(atomic_word) => {
                self.visit_atomic_word(atomic_word)
            }
            Name::Integer(integer) => self.visit_integer(integer),
        }
    }

    fn visit_variable(&mut self, variable: &Variable<'a>) {
        self.visit_upper_word(&variable.0);
    }

    fn visit_functor(&mut self, functor: &Functor<'a>) {
        self.visit_atomic_word(&functor.0);
    }

    fn visit_constant(&mut self, constant: &Constant<'a>) {
        self.visit_functor(&constant.0);
    }

    fn visit_dollar_word(&mut self, dollar_word: &DollarWord<'a>) {
        self.visit_lower_word(&dollar_word.0);
    }

    fn visit_dollar_dollar_word(
        &mut self,
        dollar_dollar_word: &DollarDollarWord<'a>,
    ) {
        self.visit_lower_word(&dollar_dollar_word.0);
    }

    fn visit_number(&mut self, number: &Number<'a>) {
        match number {
            Number::Integer(integer) => self.visit_integer(integer),
            Number::Rational(rational) => self.visit_rational(rational),
            Number::Real(real) => self.visit_real(real),
        }
    }

    fn visit_atomic_defined_word(
        &mut self,
        atomic_defined_word: &AtomicDefinedWord<'a>,
    ) {
        self.visit_dollar_word(&atomic_defined_word.0);
    }

    fn visit_atomic_system_word(
        &mut self,
        atomic_system_word: &AtomicSystemWord<'a>,
    ) {
        self.visit_dollar_dollar_word(&atomic_system_word.0);
    }

    fn visit_system_functor(&mut self, system_functor: &SystemFunctor<'a>) {
        self.visit_atomic_system_word(&system_functor.0);
    }

    fn visit_system_constant(&mut self, system_constant: &SystemConstant<'a>) {
        self.visit_system_functor(&system_constant.0);
    }

    fn visit_defined_functor(&mut self, defined_functor: &DefinedFunctor<'a>) {
        self.visit_atomic_defined_word(&defined_functor.0);
    }

    fn visit_defined_constant(
        &mut self,
        defined_constant: &DefinedConstant<'a>,
    ) {
        self.visit_defined_functor(&defined_constant.0);
    }

    fn visit_defined_term(&mut self, defined_term: &DefinedTerm<'a>) {
        match defined_term {
            DefinedTerm::Number(number) => self.visit_number(number),
            DefinedTerm::Distinct(distinct) => {
                self.visit_distinct_object(distinct)
            }
        }
    }

    fn visit_fof_arguments(&mut self, fof_arguments: &fof::Arguments<'a>) {
        for fof_term in &*fof_arguments.0 {
            self.visit_fof_term(fof_term);
        }
    }

    fn visit_fof_system_term(
        &mut self,
        fof_system_term: &fof::SystemTerm<'a>,
    ) {
        match fof_system_term {
            fof::SystemTerm::Constant(constant) => {
                self.visit_system_constant(constant)
            }
            fof::SystemTerm::Function(functor, fof_arguments) => {
                self.visit_system_functor(functor);
                self.visit_fof_arguments(fof_arguments);
            }
        }
    }

    fn visit_fof_plain_term(&mut self, fof_plain_term: &fof::PlainTerm<'a>) {
        match fof_plain_term {
            fof::PlainTerm::Constant(constant) => {
                self.visit_constant(constant)
            }
            fof::PlainTerm::Function(functor, fof_arguments) => {
                self.visit_functor(functor);
                self.visit_fof_arguments(fof_arguments);
            }
        }
    }

    fn visit_fof_defined_plain_term(
        &mut self,
        fof_defined_plain_term: &fof::DefinedPlainTerm<'a>,
    ) {
        match fof_defined_plain_term {
            fof::DefinedPlainTerm::Constant(constant) => {
                self.visit_defined_constant(constant)
            }
            fof::DefinedPlainTerm::Function(functor, fof_arguments) => {
                self.visit_defined_functor(functor);
                self.visit_fof_arguments(fof_arguments);
            }
        }
    }

    fn visit_fof_defined_atomic_term(
        &mut self,
        fof_defined_atomic_term: &fof::DefinedAtomicTerm<'a>,
    ) {
        self.visit_fof_defined_plain_term(&fof_defined_atomic_term.0);
    }

    fn visit_fof_defined_term(
        &mut self,
        fof_defined_term: &fof::DefinedTerm<'a>,
    ) {
        match fof_defined_term {
            fof::DefinedTerm::Defined(ref defined) => {
                self.visit_defined_term(defined)
            }
            fof::DefinedTerm::Atomic(ref atomic) => {
                self.visit_fof_defined_atomic_term(atomic)
            }
        }
    }

    fn visit_fof_function_term(
        &mut self,
        fof_function_term: &fof::FunctionTerm<'a>,
    ) {
        match fof_function_term {
            fof::FunctionTerm::Plain(fof_plain_term) => {
                self.visit_fof_plain_term(fof_plain_term)
            }
            fof::FunctionTerm::Defined(fof_defined_term) => {
                self.visit_fof_defined_term(fof_defined_term)
            }
            fof::FunctionTerm::System(fof_system_term) => {
                self.visit_fof_system_term(fof_system_term)
            }
        }
    }

    fn visit_fof_term(&mut self, fof_term: &fof::Term<'a>) {
        match fof_term {
            fof::Term::Function(fof_function_term) => {
                self.visit_fof_function_term(fof_function_term)
            }
            fof::Term::Variable(variable) => self.visit_variable(variable),
        }
    }

    fn visit_infix_equality(&mut self, _infix_equality: InfixEquality) {}

    fn visit_defined_infix_pred(
        &mut self,
        defined_infix_pred: DefinedInfixPred,
    ) {
        self.visit_infix_equality(defined_infix_pred.0);
    }

    fn visit_infix_inequality(&mut self, _infix_inequality: InfixInequality) {}

    fn visit_fof_quantifier(&mut self, _fof_quantifier: fof::Quantifier) {}

    fn visit_unary_connective(&mut self, _unary_connective: UnaryConnective) {}

    fn visit_nonassoc_connective(
        &mut self,
        _nonassoc_connective: NonassocConnective,
    ) {
    }

    fn visit_fof_system_atomic_formula(
        &mut self,
        fof_system_atomic_formula: &fof::SystemAtomicFormula<'a>,
    ) {
        self.visit_fof_system_term(&fof_system_atomic_formula.0);
    }

    fn visit_fof_plain_atomic_formula(
        &mut self,
        fof_plain_atomic_formula: &fof::PlainAtomicFormula<'a>,
    ) {
        self.visit_fof_plain_term(&fof_plain_atomic_formula.0);
    }

    fn visit_fof_defined_plain_formula(
        &mut self,
        fof_defined_plain_formula: &fof::DefinedPlainFormula<'a>,
    ) {
        self.visit_fof_defined_plain_term(&fof_defined_plain_formula.0);
    }

    fn visit_fof_defined_infix_formula(
        &mut self,
        fof_defined_infix_formula: &fof::DefinedInfixFormula<'a>,
    ) {
        self.visit_fof_term(&fof_defined_infix_formula.left);
        self.visit_defined_infix_pred(fof_defined_infix_formula.op);
        self.visit_fof_term(&fof_defined_infix_formula.right);
    }

    fn visit_fof_defined_atomic_formula(
        &mut self,
        fof_defined_atomic_formula: &fof::DefinedAtomicFormula<'a>,
    ) {
        match fof_defined_atomic_formula {
            fof::DefinedAtomicFormula::Plain(fof_defined_plain_formula) => {
                self.visit_fof_defined_plain_formula(fof_defined_plain_formula)
            }
            fof::DefinedAtomicFormula::Infix(fof_defined_infix_formula) => {
                self.visit_fof_defined_infix_formula(fof_defined_infix_formula)
            }
        }
    }

    fn visit_fof_atomic_formula(
        &mut self,
        fof_atomic_formula: &fof::AtomicFormula<'a>,
    ) {
        match fof_atomic_formula {
            fof::AtomicFormula::Plain(fof_plain_atomic_formula) => {
                self.visit_fof_plain_atomic_formula(fof_plain_atomic_formula)
            }
            fof::AtomicFormula::Defined(fof_defined_atomic_formula) => self
                .visit_fof_defined_atomic_formula(fof_defined_atomic_formula),
            fof::AtomicFormula::System(fof_system_atomic_formula) => {
                self.visit_fof_system_atomic_formula(fof_system_atomic_formula)
            }
        }
    }

    fn visit_fof_infix_unary(
        &mut self,
        fof_infix_unary: &fof::InfixUnary<'a>,
    ) {
        self.visit_fof_term(&fof_infix_unary.left);
        self.visit_infix_inequality(fof_infix_unary.op);
        self.visit_fof_term(&fof_infix_unary.right);
    }

    fn visit_fof_binary_nonassoc(
        &mut self,
        fof_binary_nonassoc: &fof::BinaryNonassoc<'a>,
    ) {
        self.visit_fof_unit_formula(&fof_binary_nonassoc.left);
        self.visit_nonassoc_connective(fof_binary_nonassoc.op);
        self.visit_fof_unit_formula(&fof_binary_nonassoc.right);
    }

    fn visit_fof_or_formula(&mut self, fof_or_formula: &fof::OrFormula<'a>) {
        for fof_unit_formula in &*fof_or_formula.0 {
            self.visit_fof_unit_formula(fof_unit_formula);
        }
    }

    fn visit_fof_and_formula(
        &mut self,
        fof_and_formula: &fof::AndFormula<'a>,
    ) {
        for fof_unit_formula in &*fof_and_formula.0 {
            self.visit_fof_unit_formula(fof_unit_formula);
        }
    }

    fn visit_fof_binary_assoc(
        &mut self,
        fof_binary_assoc: &fof::BinaryAssoc<'a>,
    ) {
        match fof_binary_assoc {
            fof::BinaryAssoc::Or(fof_or_formula) => {
                self.visit_fof_or_formula(fof_or_formula)
            }
            fof::BinaryAssoc::And(fof_and_formula) => {
                self.visit_fof_and_formula(fof_and_formula)
            }
        }
    }

    fn visit_fof_binary_formula(
        &mut self,
        fof_binary_formula: &fof::BinaryFormula<'a>,
    ) {
        match fof_binary_formula {
            fof::BinaryFormula::Nonassoc(fof_binary_nonassoc) => {
                self.visit_fof_binary_nonassoc(fof_binary_nonassoc)
            }
            fof::BinaryFormula::Assoc(ref fof_binary_assoc) => {
                self.visit_fof_binary_assoc(fof_binary_assoc)
            }
        }
    }

    fn visit_fof_unary_formula(
        &mut self,
        fof_unary_formula: &fof::UnaryFormula<'a>,
    ) {
        match fof_unary_formula {
            fof::UnaryFormula::Unary(unary_connective, fof_unit_formula) => {
                self.visit_unary_connective(*unary_connective);
                self.visit_fof_unit_formula(fof_unit_formula);
            }
            fof::UnaryFormula::InfixUnary(ref fof_infix_unary) => {
                self.visit_fof_infix_unary(fof_infix_unary)
            }
        }
    }

    fn visit_fof_variable_list(
        &mut self,
        fof_variable_list: &fof::VariableList<'a>,
    ) {
        for variable in &*fof_variable_list.0 {
            self.visit_variable(variable);
        }
    }

    fn visit_fof_unit_formula(
        &mut self,
        fof_unit_formula: &fof::UnitFormula<'a>,
    ) {
        match fof_unit_formula {
            fof::UnitFormula::Unitary(fof_unitary_formula) => {
                self.visit_fof_unitary_formula(fof_unitary_formula)
            }
            fof::UnitFormula::Unary(fof_unary_formula) => {
                self.visit_fof_unary_formula(fof_unary_formula)
            }
        }
    }

    fn visit_fof_quantified_formula(
        &mut self,
        fof_quantified_formula: &fof::QuantifiedFormula<'a>,
    ) {
        self.visit_fof_quantifier(fof_quantified_formula.quantifier);
        self.visit_fof_variable_list(&fof_quantified_formula.bound);
        self.visit_fof_unit_formula(&fof_quantified_formula.formula);
    }

    fn visit_fof_unitary_formula(
        &mut self,
        fof_unitary_formula: &fof::UnitaryFormula<'a>,
    ) {
        match fof_unitary_formula {
            fof::UnitaryFormula::Quantified(ref fof_quantified_formula) => {
                self.visit_fof_quantified_formula(fof_quantified_formula)
            }
            fof::UnitaryFormula::Atomic(ref fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            fof::UnitaryFormula::Parenthesised(fof_logic_formula) => {
                self.visit_fof_logic_formula(fof_logic_formula)
            }
        }
    }

    fn visit_fof_logic_formula(
        &mut self,
        fof_logic_formula: &fof::LogicFormula<'a>,
    ) {
        match fof_logic_formula {
            fof::LogicFormula::Binary(ref fof_binary_formula) => {
                self.visit_fof_binary_formula(fof_binary_formula)
            }
            fof::LogicFormula::Unary(ref fof_unary_formula) => {
                self.visit_fof_unary_formula(fof_unary_formula)
            }
            fof::LogicFormula::Unitary(ref fof_unitary_formula) => {
                self.visit_fof_unitary_formula(fof_unitary_formula)
            }
        }
    }

    fn visit_fof_formula(&mut self, fof_formula: &fof::Formula<'a>) {
        self.visit_fof_logic_formula(&fof_formula.0);
    }

    fn visit_literal(&mut self, literal: &cnf::Literal<'a>) {
        match literal {
            cnf::Literal::Atomic(ref fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            cnf::Literal::NegatedAtomic(ref fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            cnf::Literal::Infix(ref fof_infix_unary) => {
                self.visit_fof_infix_unary(fof_infix_unary)
            }
        }
    }

    fn visit_disjunction(&mut self, disjunction: &cnf::Disjunction<'a>) {
        for literal in &*disjunction.0 {
            self.visit_literal(literal);
        }
    }

    fn visit_cnf_formula(&mut self, cnf_formula: &cnf::Formula<'a>) {
        match cnf_formula {
            cnf::Formula::Disjunction(disjunction) => {
                self.visit_disjunction(disjunction)
            }
            cnf::Formula::Parenthesised(disjunction) => {
                self.visit_disjunction(disjunction)
            }
        }
    }

    fn visit_formula_role(&mut self, formula_role: &FormulaRole<'a>) {
        self.visit_lower_word(&formula_role.0);
    }

    fn visit_general_terms(&mut self, general_terms: &GeneralTerms<'a>) {
        for general_term in &*general_terms.0 {
            self.visit_general_term(general_term);
        }
    }

    fn visit_general_list(&mut self, general_list: &GeneralList<'a>) {
        if let Some(ref general_terms) = general_list.0 {
            self.visit_general_terms(general_terms)
        }
    }

    fn visit_general_function(
        &mut self,
        general_function: &GeneralFunction<'a>,
    ) {
        self.visit_atomic_word(&general_function.word);
        self.visit_general_terms(&general_function.terms);
    }

    fn visit_formula_data(&mut self, formula_data: &FormulaData<'a>) {
        match formula_data {
            FormulaData::Fof(fof_formula) => {
                self.visit_fof_formula(fof_formula)
            }
            FormulaData::Cnf(cnf_formula) => {
                self.visit_cnf_formula(cnf_formula)
            }
            FormulaData::Fot(fof_term) => self.visit_fof_term(fof_term),
        }
    }

    fn visit_general_data(&mut self, general_data: &GeneralData<'a>) {
        match general_data {
            GeneralData::Atomic(atomic_word) => {
                self.visit_atomic_word(atomic_word)
            }
            GeneralData::Function(general_function) => {
                self.visit_general_function(general_function)
            }
            GeneralData::Variable(variable) => self.visit_variable(variable),
            GeneralData::Number(number) => self.visit_number(number),
            GeneralData::DistinctObject(distinct_object) => {
                self.visit_distinct_object(distinct_object)
            }
            GeneralData::Formula(formula_data) => {
                self.visit_formula_data(formula_data)
            }
        }
    }

    fn visit_general_term(&mut self, general_term: &GeneralTerm<'a>) {
        match general_term {
            GeneralTerm::Data(general_data) => {
                self.visit_general_data(general_data)
            }
            GeneralTerm::Colon(general_data, general_term) => {
                self.visit_general_data(general_data);
                self.visit_general_term(general_term);
            }
            GeneralTerm::List(general_list) => {
                self.visit_general_list(general_list)
            }
        }
    }

    fn visit_source(&mut self, source: &Source<'a>) {
        self.visit_general_term(&source.0);
    }

    fn visit_useful_info(&mut self, useful_info: &UsefulInfo<'a>) {
        self.visit_general_list(&useful_info.0);
    }

    fn visit_optional_info(&mut self, optional_info: &OptionalInfo<'a>) {
        if let Some(ref useful_info) = optional_info.0 {
            self.visit_useful_info(useful_info)
        }
    }

    fn visit_annotations(&mut self, annotations: &Annotations<'a>) {
        if let Some(boxed) = &annotations.0 {
            self.visit_source(&boxed.0);
            self.visit_optional_info(&boxed.1);
        }
    }

    fn visit_fof_annotated(&mut self, fof_annotated: &FofAnnotated<'a>) {
        self.visit_name(&fof_annotated.0.name);
        self.visit_formula_role(&fof_annotated.0.role);
        self.visit_fof_formula(&fof_annotated.0.formula);
        self.visit_annotations(&fof_annotated.0.annotations);
    }

    fn visit_cnf_annotated(&mut self, cnf_annotated: &CnfAnnotated<'a>) {
        self.visit_name(&cnf_annotated.0.name);
        self.visit_formula_role(&cnf_annotated.0.role);
        self.visit_cnf_formula(&cnf_annotated.0.formula);
        self.visit_annotations(&cnf_annotated.0.annotations);
    }

    fn visit_annotated_formula(&mut self, annotated: &AnnotatedFormula<'a>) {
        match annotated {
            AnnotatedFormula::Fof(fof_annotated) => {
                self.visit_fof_annotated(fof_annotated)
            }
            AnnotatedFormula::Cnf(cnf_annotated) => {
                self.visit_cnf_annotated(cnf_annotated)
            }
        }
    }

    fn visit_name_list(&mut self, name_list: &NameList<'a>) {
        for name in &*name_list.0 {
            self.visit_name(name);
        }
    }

    fn visit_formula_selection(&mut self, selection: &FormulaSelection<'a>) {
        if let Some(ref name_list) = selection.0 {
            self.visit_name_list(name_list);
        }
    }

    fn visit_file_name(&mut self, file_name: &FileName<'a>) {
        self.visit_single_quoted(&file_name.0);
    }

    fn visit_include(&mut self, include: &Include<'a>) {
        self.visit_file_name(&include.file_name);
        self.visit_formula_selection(&include.selection);
    }

    fn visit_tptp_input(&mut self, input: &TPTPInput<'a>) {
        match input {
            TPTPInput::Annotated(annotated) => {
                self.visit_annotated_formula(annotated)
            }
            TPTPInput::Include(include) => self.visit_include(include),
        }
    }
}
