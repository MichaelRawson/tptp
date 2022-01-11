use crate::cnf;
use crate::common::*;
use crate::fof;
use crate::tfx;
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

    fn visit_type_functor(&mut self, type_functor: &TypeFunctor<'a>) {
        self.visit_atomic_word(&type_functor.0);
    }

    fn visit_constant(&mut self, constant: &Constant<'a>) {
        self.visit_functor(&constant.0);
    }

    fn visit_type_constant(&mut self, type_constant: &TypeConstant<'a>) {
        self.visit_type_functor(&type_constant.0);
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

    fn visit_defined_type(&mut self, defined_type: &DefinedType<'a>) {
        self.visit_atomic_defined_word(&defined_type.0);
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

    fn visit_untyped_atom(&mut self, untyped_atom: &UntypedAtom<'a>) {
        match untyped_atom {
            UntypedAtom::Constant(c) => self.visit_constant(c),
            UntypedAtom::System(c) => self.visit_system_constant(c),
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

    fn visit_unary_connective(&mut self, _unary_connective: UnaryConnective) {}

    fn visit_nonassoc_connective(
        &mut self,
        _nonassoc_connective: NonassocConnective,
    ) {
    }

    fn visit_tfx_type_arguments(
        &mut self,
        tfx_type_arguments: &tfx::TypeArguments<'a>,
    ) {
        for t in &tfx_type_arguments.0 {
            self.visit_tfx_atomic_type(t);
        }
    }

    fn visit_tfx_atomic_type(
        &mut self,
        tfx_atomic_type: &tfx::AtomicType<'a>,
    ) {
        match tfx_atomic_type {
            tfx::AtomicType::Constant(c) => self.visit_type_constant(c),
            tfx::AtomicType::Defined(d) => self.visit_defined_type(d),
            tfx::AtomicType::Variable(v) => self.visit_variable(v),
            tfx::AtomicType::Function(f, args) => {
                self.visit_type_functor(f);
                self.visit_tfx_type_arguments(args);
            }
        }
    }

    fn visit_tfx_xprod_type(&mut self, tfx_xprod_type: &tfx::XprodType<'a>) {
        for t in &tfx_xprod_type.0 {
            self.visit_tfx_unitary_type(t);
        }
    }

    fn visit_tfx_typed_variable(
        &mut self,
        tfx_typed_variable: &tfx::TypedVariable<'a>,
    ) {
        self.visit_variable(&tfx_typed_variable.variable);
        self.visit_tfx_atomic_type(&tfx_typed_variable.typ);
    }

    fn visit_tfx_variable(&mut self, tfx_variable: &tfx::Variable<'a>) {
        match tfx_variable {
            tfx::Variable::Typed(t) => self.visit_tfx_typed_variable(t),
            tfx::Variable::Untyped(u) => self.visit_variable(u),
        }
    }

    fn visit_tfx_variable_list(
        &mut self,
        tfx_variable_list: &tfx::VariableList<'a>,
    ) {
        for v in &tfx_variable_list.0 {
            self.visit_tfx_variable(v);
        }
    }

    fn visit_tfx_unitary_type(
        &mut self,
        tfx_unitary_type: &tfx::UnitaryType<'a>,
    ) {
        match tfx_unitary_type {
            tfx::UnitaryType::Atomic(a) => self.visit_tfx_atomic_type(a),
            tfx::UnitaryType::Product(p) => self.visit_tfx_xprod_type(p),
        }
    }

    fn visit_tfx_mapping_type(
        &mut self,
        tfx_mapping_type: &tfx::MappingType<'a>,
    ) {
        self.visit_tfx_unitary_type(&tfx_mapping_type.domain);
        self.visit_tfx_atomic_type(&tfx_mapping_type.range);
    }

    fn visit_tfx_monotype(&mut self, tfx_monotype: &tfx::Monotype<'a>) {
        match tfx_monotype {
            tfx::Monotype::Atomic(a) => self.visit_tfx_atomic_type(a),
            tfx::Monotype::Mapping(m) => self.visit_tfx_mapping_type(m),
            tfx::Monotype::Quantified(q) => self.visit_tfx_quantified_type(q),
        }
    }

    fn visit_tfx_quantified_type(
        &mut self,
        tfx_quantified_type: &tfx::QuantifiedType<'a>,
    ) {
        self.visit_tfx_variable_list(&tfx_quantified_type.bound);
        self.visit_tfx_monotype(&tfx_quantified_type.typ);
    }

    fn visit_tfx_non_atomic_type(
        &mut self,
        tfx_non_atomic_type: &tfx::NonAtomicType<'a>,
    ) {
        match tfx_non_atomic_type {
            tfx::NonAtomicType::Mapping(m) => self.visit_tfx_mapping_type(m),
            tfx::NonAtomicType::Quantified(q) => {
                self.visit_tfx_quantified_type(q)
            }
            tfx::NonAtomicType::Parenthesised(n) => {
                self.visit_tfx_non_atomic_type(n)
            }
        }
    }

    fn visit_tfx_toplevel_type(
        &mut self,
        tfx_toplevel_type: &tfx::TopLevelType<'a>,
    ) {
        match tfx_toplevel_type {
            tfx::TopLevelType::Atomic(a) => self.visit_tfx_atomic_type(a),
            tfx::TopLevelType::NonAtomic(n) => {
                self.visit_tfx_non_atomic_type(n)
            }
        }
    }

    fn visit_tfx_atom_typing(
        &mut self,
        tfx_atom_typing: &tfx::AtomTyping<'a>,
    ) {
        match tfx_atom_typing {
            tfx::AtomTyping::Typing(a, t) => {
                self.visit_untyped_atom(a);
                self.visit_tfx_toplevel_type(t);
            }
            tfx::AtomTyping::Parenthesised(t) => self.visit_tfx_atom_typing(t),
        }
    }

    fn visit_tfx_term(&mut self, tfx_term: &tfx::Term<'a>) {
        match tfx_term {
            tfx::Term::Logic(f) => self.visit_tfx_logic_formula(f),
            tfx::Term::Defined(d) => self.visit_defined_term(d),
        }
    }

    fn visit_tfx_unitary_term(
        &mut self,
        tfx_unitary_term: &tfx::UnitaryTerm<'a>,
    ) {
        match tfx_unitary_term {
            tfx::UnitaryTerm::Atomic(a) => self.visit_tfx_atomic_formula(a),
            tfx::UnitaryTerm::Defined(d) => self.visit_defined_term(d),
            tfx::UnitaryTerm::Variable(v) => self.visit_variable(v),
            tfx::UnitaryTerm::Logic(f) => self.visit_tfx_logic_formula(f),
        }
    }

    fn visit_tfx_arguments(&mut self, tfx_arguments: &tfx::Arguments<'a>) {
        for t in &tfx_arguments.0 {
            self.visit_tfx_term(t);
        }
    }

    fn visit_tfx_plain_atomic(
        &mut self,
        tfx_plain_atomic: &tfx::PlainAtomic<'a>,
    ) {
        match tfx_plain_atomic {
            tfx::PlainAtomic::Constant(c) => self.visit_constant(c),
            tfx::PlainAtomic::Function(f, args) => {
                self.visit_functor(f);
                self.visit_tfx_arguments(args);
            }
        }
    }

    fn visit_tfx_system_atomic(
        &mut self,
        tfx_system_atomic: &tfx::SystemAtomic<'a>,
    ) {
        match tfx_system_atomic {
            tfx::SystemAtomic::Constant(c) => self.visit_system_constant(c),
            tfx::SystemAtomic::Function(f, args) => {
                self.visit_system_functor(f);
                self.visit_tfx_arguments(args);
            }
        }
    }

    fn visit_tfx_defined_plain(
        &mut self,
        tfx_defined_plain: &tfx::DefinedPlain<'a>,
    ) {
        match tfx_defined_plain {
            tfx::DefinedPlain::Constant(c) => self.visit_defined_constant(c),
            tfx::DefinedPlain::Function(f, args) => {
                self.visit_defined_functor(f);
                self.visit_tfx_arguments(args);
            }
        }
    }

    fn visit_tfx_defined_atomic(
        &mut self,
        tfx_defined_atomic: &tfx::DefinedAtomic<'a>,
    ) {
        self.visit_tfx_defined_plain(&tfx_defined_atomic.0);
    }

    fn visit_tfx_atomic_formula(
        &mut self,
        tfx_atomic_formula: &tfx::AtomicFormula<'a>,
    ) {
        match tfx_atomic_formula {
            tfx::AtomicFormula::Plain(p) => self.visit_tfx_plain_atomic(p),
            tfx::AtomicFormula::Defined(d) => self.visit_tfx_defined_atomic(d),
            tfx::AtomicFormula::System(s) => self.visit_tfx_system_atomic(s),
        }
    }

    fn visit_tfx_preunit_formula(
        &mut self,
        tfx_preunit_formula: &tfx::PreunitFormula<'a>,
    ) {
        match tfx_preunit_formula {
            tfx::PreunitFormula::Unitary(u) => {
                self.visit_tfx_unitary_formula(u)
            }
            tfx::PreunitFormula::Prefix(p) => self.visit_tfx_prefix_unary(p),
        }
    }

    fn visit_tfx_prefix_unary(
        &mut self,
        tfx_prefix_unary: &tfx::PrefixUnary<'a>,
    ) {
        self.visit_unary_connective(tfx_prefix_unary.op);
        self.visit_tfx_preunit_formula(&tfx_prefix_unary.formula);
    }

    fn visit_tfx_infix_unary(
        &mut self,
        tfx_infix_unary: &tfx::InfixUnary<'a>,
    ) {
        self.visit_tfx_unitary_term(&tfx_infix_unary.left);
        self.visit_infix_inequality(tfx_infix_unary.op);
        self.visit_tfx_unitary_term(&tfx_infix_unary.right);
    }

    fn visit_tfx_defined_infix(
        &mut self,
        tfx_defined_infix: &tfx::DefinedInfix<'a>,
    ) {
        self.visit_tfx_unitary_term(&tfx_defined_infix.left);
        self.visit_defined_infix_pred(tfx_defined_infix.op);
        self.visit_tfx_unitary_term(&tfx_defined_infix.right);
    }

    fn visit_tfx_unary_formula(
        &mut self,
        tfx_unary_formula: &tfx::UnaryFormula<'a>,
    ) {
        match tfx_unary_formula {
            tfx::UnaryFormula::Prefix(p) => self.visit_tfx_prefix_unary(p),
            tfx::UnaryFormula::Infix(i) => self.visit_tfx_infix_unary(i),
        }
    }

    fn visit_tfx_quantified_formula(
        &mut self,
        tfx_quantified_formula: &tfx::QuantifiedFormula<'a>,
    ) {
        self.visit_fof_quantifier(tfx_quantified_formula.quantifier);
        self.visit_tfx_variable_list(&tfx_quantified_formula.bound);
        self.visit_tfx_unit_formula(&tfx_quantified_formula.formula);
    }

    fn visit_tfx_unit_formula(
        &mut self,
        tfx_unit_formula: &tfx::UnitFormula<'a>,
    ) {
        match tfx_unit_formula {
            tfx::UnitFormula::Unitary(u) => self.visit_tfx_unitary_formula(u),
            tfx::UnitFormula::Unary(u) => self.visit_tfx_unary_formula(u),
            tfx::UnitFormula::DefinedInfix(d) => {
                self.visit_tfx_defined_infix(d)
            }
        }
    }

    fn visit_tfx_unitary_formula(
        &mut self,
        tfx_unitary_formula: &tfx::UnitaryFormula<'a>,
    ) {
        match tfx_unitary_formula {
            tfx::UnitaryFormula::Quantified(q) => {
                self.visit_tfx_quantified_formula(q)
            }
            tfx::UnitaryFormula::Atomic(a) => self.visit_tfx_atomic_formula(a),
            tfx::UnitaryFormula::Variable(v) => self.visit_variable(v),
            tfx::UnitaryFormula::Logic(f) => self.visit_tfx_logic_formula(f),
        }
    }

    fn visit_tfx_or_formula(&mut self, tfx_or_formula: &tfx::OrFormula<'a>) {
        for f in &tfx_or_formula.0 {
            self.visit_tfx_unit_formula(f);
        }
    }

    fn visit_tfx_and_formula(
        &mut self,
        tfx_and_formula: &tfx::AndFormula<'a>,
    ) {
        for f in &tfx_and_formula.0 {
            self.visit_tfx_unit_formula(f);
        }
    }

    fn visit_tfx_binary_assoc(
        &mut self,
        tfx_binary_assoc: &tfx::BinaryAssoc<'a>,
    ) {
        match tfx_binary_assoc {
            tfx::BinaryAssoc::Or(or) => self.visit_tfx_or_formula(or),
            tfx::BinaryAssoc::And(and) => self.visit_tfx_and_formula(and),
        }
    }

    fn visit_tfx_binary_nonassoc(
        &mut self,
        tfx_binary_nonassoc: &tfx::BinaryNonassoc<'a>,
    ) {
        self.visit_tfx_unit_formula(&tfx_binary_nonassoc.left);
        self.visit_nonassoc_connective(tfx_binary_nonassoc.op);
        self.visit_tfx_unit_formula(&tfx_binary_nonassoc.right);
    }

    fn visit_tfx_binary_formula(
        &mut self,
        tfx_binary_formula: &tfx::BinaryFormula<'a>,
    ) {
        match tfx_binary_formula {
            tfx::BinaryFormula::Assoc(a) => self.visit_tfx_binary_assoc(a),
            tfx::BinaryFormula::Nonassoc(n) => {
                self.visit_tfx_binary_nonassoc(n)
            }
        }
    }

    fn visit_tfx_logic_formula(
        &mut self,
        tfx_logic_formula: &tfx::LogicFormula<'a>,
    ) {
        match tfx_logic_formula {
            tfx::LogicFormula::Unary(u) => self.visit_tfx_unary_formula(u),
            tfx::LogicFormula::Unitary(u) => self.visit_tfx_unitary_formula(u),
            tfx::LogicFormula::Binary(b) => self.visit_tfx_binary_formula(b),
            tfx::LogicFormula::DefinedInfix(d) => {
                self.visit_tfx_defined_infix(d)
            }
        }
    }

    fn visit_tfx_formula(&mut self, tfx_formula: &tfx::Formula<'a>) {
        match tfx_formula {
            tfx::Formula::Logic(f) => self.visit_tfx_logic_formula(f),
            tfx::Formula::AtomTyping(t) => self.visit_tfx_atom_typing(t),
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

    fn visit_fof_quantifier(&mut self, _fof_quantifier: fof::Quantifier) {}

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
            FormulaData::Tfx(tfx_formula) => {
                self.visit_tfx_formula(tfx_formula)
            }
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

    fn visit_tfx_annotated(&mut self, tfx_annotated: &TfxAnnotated<'a>) {
        self.visit_name(&tfx_annotated.0.name);
        self.visit_formula_role(&tfx_annotated.0.role);
        self.visit_tfx_formula(&tfx_annotated.0.formula);
        self.visit_annotations(&tfx_annotated.0.annotations);
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
            AnnotatedFormula::Tfx(tfx_annotated) => {
                self.visit_tfx_annotated(tfx_annotated)
            }
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
