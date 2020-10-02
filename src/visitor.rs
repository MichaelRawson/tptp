use crate::syntax::*;

/// [visitor pattern](https://en.wikipedia.org/wiki/Visitor_pattern) trait
///
/// This can make traversing the TPTP AST easier.
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

    fn visit_fof_arguments(&mut self, fof_arguments: &FofArguments<'a>) {
        for fof_term in &fof_arguments.0 {
            self.visit_fof_term(fof_term);
        }
    }

    fn visit_fof_system_term(&mut self, fof_system_term: &FofSystemTerm<'a>) {
        match fof_system_term {
            FofSystemTerm::Constant(constant) => {
                self.visit_system_constant(constant)
            }
            FofSystemTerm::Function(functor, fof_arguments) => {
                self.visit_system_functor(functor);
                self.visit_fof_arguments(fof_arguments);
            }
        }
    }

    fn visit_fof_plain_term(&mut self, fof_plain_term: &FofPlainTerm<'a>) {
        match fof_plain_term {
            FofPlainTerm::Constant(constant) => self.visit_constant(constant),
            FofPlainTerm::Function(functor, fof_arguments) => {
                self.visit_functor(functor);
                self.visit_fof_arguments(&fof_arguments);
            }
        }
    }

    fn visit_fof_defined_plain_term(
        &mut self,
        fof_defined_plain_term: &FofDefinedPlainTerm<'a>,
    ) {
        self.visit_defined_constant(&fof_defined_plain_term.0);
    }

    fn visit_fof_defined_atomic_term(
        &mut self,
        fof_defined_atomic_term: &FofDefinedAtomicTerm<'a>,
    ) {
        self.visit_fof_defined_plain_term(&fof_defined_atomic_term.0);
    }

    fn visit_fof_defined_term(
        &mut self,
        fof_defined_term: &FofDefinedTerm<'a>,
    ) {
        match fof_defined_term {
            FofDefinedTerm::Defined(ref defined) => {
                self.visit_defined_term(defined)
            }
            FofDefinedTerm::Atomic(ref atomic) => {
                self.visit_fof_defined_atomic_term(atomic)
            }
        }
    }

    fn visit_fof_function_term(
        &mut self,
        fof_function_term: &FofFunctionTerm<'a>,
    ) {
        match fof_function_term {
            FofFunctionTerm::Plain(fof_plain_term) => {
                self.visit_fof_plain_term(fof_plain_term)
            }
            FofFunctionTerm::Defined(fof_defined_term) => {
                self.visit_fof_defined_term(fof_defined_term)
            }
        }
    }

    fn visit_fof_term(&mut self, fof_term: &FofTerm<'a>) {
        match fof_term {
            FofTerm::Function(fof_function_term) => {
                self.visit_fof_function_term(&fof_function_term)
            }
            FofTerm::Variable(variable) => self.visit_variable(variable),
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

    fn visit_fof_quantifier(&mut self, _fof_quantifier: FofQuantifier) {}

    fn visit_unary_connective(&mut self, _unary_connective: UnaryConnective) {}

    fn visit_nonassoc_connective(
        &mut self,
        _nonassoc_connective: NonassocConnective,
    ) {
    }

    fn visit_fof_system_atomic_formula(
        &mut self,
        fof_system_atomic_formula: &FofSystemAtomicFormula<'a>,
    ) {
        self.visit_fof_system_term(&fof_system_atomic_formula.0);
    }

    fn visit_fof_plain_atomic_formula(
        &mut self,
        fof_plain_atomic_formula: &FofPlainAtomicFormula<'a>,
    ) {
        self.visit_fof_plain_term(&fof_plain_atomic_formula.0);
    }

    fn visit_fof_defined_plain_formula(
        &mut self,
        fof_defined_plain_formula: &FofDefinedPlainFormula<'a>,
    ) {
        self.visit_fof_defined_plain_term(&fof_defined_plain_formula.0);
    }

    fn visit_fof_defined_infix_formula(
        &mut self,
        fof_defined_infix_formula: &FofDefinedInfixFormula<'a>,
    ) {
        self.visit_fof_term(&fof_defined_infix_formula.left);
        self.visit_defined_infix_pred(fof_defined_infix_formula.op);
        self.visit_fof_term(&fof_defined_infix_formula.right);
    }

    fn visit_fof_defined_atomic_formula(
        &mut self,
        fof_defined_atomic_formula: &FofDefinedAtomicFormula<'a>,
    ) {
        match fof_defined_atomic_formula {
            FofDefinedAtomicFormula::Plain(fof_defined_plain_formula) => {
                self.visit_fof_defined_plain_formula(fof_defined_plain_formula)
            }
            FofDefinedAtomicFormula::Infix(fof_defined_infix_formula) => {
                self.visit_fof_defined_infix_formula(fof_defined_infix_formula)
            }
        }
    }

    fn visit_fof_atomic_formula(
        &mut self,
        fof_atomic_formula: &FofAtomicFormula<'a>,
    ) {
        match fof_atomic_formula {
            FofAtomicFormula::Plain(fof_plain_atomic_formula) => {
                self.visit_fof_plain_atomic_formula(&fof_plain_atomic_formula)
            }
            FofAtomicFormula::Defined(fof_defined_atomic_formula) => self
                .visit_fof_defined_atomic_formula(&fof_defined_atomic_formula),
            FofAtomicFormula::System(fof_system_atomic_formula) => self
                .visit_fof_system_atomic_formula(&fof_system_atomic_formula),
        }
    }

    fn visit_fof_infix_unary(&mut self, fof_infix_unary: &FofInfixUnary<'a>) {
        self.visit_fof_term(&fof_infix_unary.left);
        self.visit_infix_inequality(fof_infix_unary.op);
        self.visit_fof_term(&fof_infix_unary.right);
    }

    fn visit_fof_binary_nonassoc(
        &mut self,
        fof_binary_nonassoc: &FofBinaryNonassoc<'a>,
    ) {
        self.visit_fof_unit_formula(&fof_binary_nonassoc.left);
        self.visit_nonassoc_connective(fof_binary_nonassoc.op);
        self.visit_fof_unit_formula(&fof_binary_nonassoc.right);
    }

    fn visit_fof_or_formula(&mut self, fof_or_formula: &FofOrFormula<'a>) {
        for fof_unit_formula in &fof_or_formula.0 {
            self.visit_fof_unit_formula(fof_unit_formula);
        }
    }

    fn visit_fof_and_formula(&mut self, fof_and_formula: &FofAndFormula<'a>) {
        for fof_unit_formula in &fof_and_formula.0 {
            self.visit_fof_unit_formula(fof_unit_formula);
        }
    }

    fn visit_fof_binary_assoc(
        &mut self,
        fof_binary_assoc: &FofBinaryAssoc<'a>,
    ) {
        match fof_binary_assoc {
            FofBinaryAssoc::Or(fof_or_formula) => {
                self.visit_fof_or_formula(fof_or_formula)
            }
            FofBinaryAssoc::And(fof_and_formula) => {
                self.visit_fof_and_formula(fof_and_formula)
            }
        }
    }

    fn visit_fof_binary_formula(
        &mut self,
        fof_binary_formula: &FofBinaryFormula<'a>,
    ) {
        match fof_binary_formula {
            FofBinaryFormula::Nonassoc(fof_binary_nonassoc) => {
                self.visit_fof_binary_nonassoc(&fof_binary_nonassoc)
            }
            FofBinaryFormula::Assoc(ref fof_binary_assoc) => {
                self.visit_fof_binary_assoc(fof_binary_assoc)
            }
        }
    }

    fn visit_fof_unary_formula(
        &mut self,
        fof_unary_formula: &FofUnaryFormula<'a>,
    ) {
        match fof_unary_formula {
            FofUnaryFormula::Unary(unary_connective, fof_unit_formula) => {
                self.visit_unary_connective(*unary_connective);
                self.visit_fof_unit_formula(&fof_unit_formula);
            }
            FofUnaryFormula::InfixUnary(ref fof_infix_unary) => {
                self.visit_fof_infix_unary(fof_infix_unary)
            }
        }
    }

    fn visit_fof_variable_list(
        &mut self,
        fof_variable_list: &FofVariableList<'a>,
    ) {
        for variable in &fof_variable_list.0 {
            self.visit_variable(variable);
        }
    }

    fn visit_fof_unit_formula(
        &mut self,
        fof_unit_formula: &FofUnitFormula<'a>,
    ) {
        match fof_unit_formula {
            FofUnitFormula::Unitary(fof_unitary_formula) => {
                self.visit_fof_unitary_formula(fof_unitary_formula)
            }
            FofUnitFormula::Unary(fof_unary_formula) => {
                self.visit_fof_unary_formula(fof_unary_formula)
            }
        }
    }

    fn visit_fof_quantified_formula(
        &mut self,
        fof_quantified_formula: &FofQuantifiedFormula<'a>,
    ) {
        self.visit_fof_quantifier(fof_quantified_formula.quantifier);
        self.visit_fof_variable_list(&fof_quantified_formula.bound);
        self.visit_fof_unit_formula(&fof_quantified_formula.formula);
    }

    fn visit_fof_unitary_formula(
        &mut self,
        fof_unitary_formula: &FofUnitaryFormula<'a>,
    ) {
        match fof_unitary_formula {
            FofUnitaryFormula::Quantified(ref fof_quantified_formula) => {
                self.visit_fof_quantified_formula(fof_quantified_formula)
            }
            FofUnitaryFormula::Atomic(ref fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            FofUnitaryFormula::Parenthesised(fof_logic_formula) => {
                self.visit_fof_logic_formula(&fof_logic_formula)
            }
        }
    }

    fn visit_fof_logic_formula(
        &mut self,
        fof_logic_formula: &FofLogicFormula<'a>,
    ) {
        match fof_logic_formula {
            FofLogicFormula::Binary(ref fof_binary_formula) => {
                self.visit_fof_binary_formula(fof_binary_formula)
            }
            FofLogicFormula::Unary(ref fof_unary_formula) => {
                self.visit_fof_unary_formula(fof_unary_formula)
            }
            FofLogicFormula::Unitary(ref fof_unitary_formula) => {
                self.visit_fof_unitary_formula(fof_unitary_formula)
            }
        }
    }

    fn visit_fof_formula(&mut self, fof_formula: &FofFormula<'a>) {
        self.visit_fof_logic_formula(&fof_formula.0);
    }

    fn visit_literal(&mut self, literal: &Literal<'a>) {
        match literal {
            Literal::Atomic(ref fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            Literal::NegatedAtomic(ref fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            Literal::Infix(ref fof_infix_unary) => {
                self.visit_fof_infix_unary(fof_infix_unary)
            }
        }
    }

    fn visit_disjunction(&mut self, disjunction: &Disjunction<'a>) {
        for literal in &disjunction.0 {
            self.visit_literal(literal);
        }
    }

    fn visit_cnf_formula(&mut self, cnf_formula: &CnfFormula<'a>) {
        match cnf_formula {
            CnfFormula::Disjunction(disjunction) => {
                self.visit_disjunction(disjunction)
            }
            CnfFormula::Parenthesised(disjunction) => {
                self.visit_disjunction(disjunction)
            }
        }
    }

    fn visit_formula_role(&mut self, _formula_role: FormulaRole) {}

    fn visit_general_terms(&mut self, general_terms: &GeneralTerms<'a>) {
        for general_term in &general_terms.0 {
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
                self.visit_general_data(&general_data)
            }
            GeneralTerm::Colon(general_data, general_term) => {
                self.visit_general_data(&general_data);
                self.visit_general_term(&general_term);
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
        if let Some((ref source, ref optional_info)) = annotations.0 {
            self.visit_source(source);
            self.visit_optional_info(optional_info);
        }
    }

    fn visit_fof_annotated(&mut self, fof_annotated: &FofAnnotated<'a>) {
        self.visit_name(&fof_annotated.name);
        self.visit_formula_role(fof_annotated.role);
        self.visit_fof_formula(&fof_annotated.formula);
        self.visit_annotations(&fof_annotated.annotations);
    }

    fn visit_cnf_annotated(&mut self, cnf_annotated: &CnfAnnotated<'a>) {
        self.visit_name(&cnf_annotated.name);
        self.visit_formula_role(cnf_annotated.role);
        self.visit_cnf_formula(&cnf_annotated.formula);
        self.visit_annotations(&cnf_annotated.annotations);
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
        for name in &name_list.0 {
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
