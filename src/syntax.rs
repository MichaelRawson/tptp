use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::fmt;
use alloc::vec::Vec;
use derive_more::{AsRef, Display, From};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

fn fmt_list<T: fmt::Display>(
    f: &mut fmt::Formatter,
    sep: &'static str,
    args: &[T],
) -> fmt::Result {
    if args.is_empty() {
        return Ok(());
    }

    let mut args = args.iter();
    write!(f, "{}", args.next().unwrap())?;
    for arg in args {
        write!(f, "{}{}", sep, arg)?;
    }
    Ok(())
}

/// `integer`
#[derive(AsRef, Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Integer<'a>(pub Cow<'a, str>);

/// `lower_word`
#[derive(AsRef, Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LowerWord<'a>(pub Cow<'a, str>);

/// `upper_word`
#[derive(AsRef, Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct UpperWord<'a>(pub Cow<'a, str>);

/// `dollar_word`
#[derive(AsRef, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DollarWord<'a>(pub LowerWord<'a>);

impl<'a> fmt::Display for DollarWord<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

/// `single_quoted`
#[derive(AsRef, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct SingleQuoted<'a>(pub Cow<'a, str>);

impl<'a> fmt::Display for SingleQuoted<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

/// `atomic_word`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum AtomicWord<'a> {
    Lower(LowerWord<'a>),
    SingleQuoted(SingleQuoted<'a>),
}

/// `name`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Name<'a> {
    AtomicWord(AtomicWord<'a>),
    Integer(Integer<'a>),
}

/// `variable`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Variable<'a>(pub UpperWord<'a>);

/// `functor`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Functor<'a>(pub AtomicWord<'a>);

/// `fof_arguments`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofArguments<'a>(pub Vec<FofTerm<'a>>);

impl<'a> fmt::Display for FofArguments<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(f, "(")?;
        fmt_list(f, ",", &self.0)?;
        write!(f, ")")
    }
}

/// `fof_plain_term`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofPlainTerm<'a> {
    /// `constant`
    Constant(Functor<'a>),
    /// `functor`, `fof_arguments`
    Function(Functor<'a>, FofArguments<'a>),
}

impl<'a> fmt::Display for FofPlainTerm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofPlainTerm::*;
        match self {
            Constant(c) => write!(f, "{}", c),
            Function(name, args) => write!(f, "{}{}", name, args),
        }
    }
}

/// `atomic_defined_word`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct AtomicDefinedWord<'a>(pub DollarWord<'a>);

/// `defined_functor`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DefinedFunctor<'a>(pub AtomicDefinedWord<'a>);

/// `defined_constant`
#[derive(
    AsRef, Clone, Display, From, Debug, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DefinedConstant<'a>(pub DefinedFunctor<'a>);

/// `fof_defined_plain_term`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofDefinedPlainTerm<'a>(pub DefinedConstant<'a>);

/// `fof_defined_atomic_term`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofDefinedAtomicTerm<'a>(pub FofDefinedPlainTerm<'a>);

/// `fof_defined_term`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofDefinedTerm<'a>(pub FofDefinedAtomicTerm<'a>);

/// `fof_function_term`
#[derive(Clone, Debug, From, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofFunctionTerm<'a> {
    /// `fof_plain_term`
    Plain(FofPlainTerm<'a>),
    /// `fof_defined_term`
    Defined(FofDefinedTerm<'a>),
}

/// `fof_term`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofTerm<'a> {
    /// `fof_function_term`,
    Function(FofFunctionTerm<'a>),
    /// `variable`
    Variable(Variable<'a>),
}

/// `unary_connective`
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct UnaryConnective;

impl fmt::Display for UnaryConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "~")
    }
}

/// `infix_equality`
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct InfixEquality;

impl fmt::Display for InfixEquality {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "=")
    }
}

/// `infix_inequality`
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct InfixInequality;

impl fmt::Display for InfixInequality {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "!=")
    }
}

/// `nonassoc_connective`
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum NonassocConnective {
    /// `=>`
    LRImplies,
    /// `<=`
    RLImplies,
    /// `<=>`
    Equivalent,
    /// `<~>`
    NotEquivalent,
    /// `~|`
    NotOr,
    /// `~&`
    NotAnd,
}

impl fmt::Display for NonassocConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::NonassocConnective::*;
        match self {
            LRImplies => write!(f, "=>"),
            RLImplies => write!(f, "<="),
            Equivalent => write!(f, "<=>"),
            NotEquivalent => write!(f, "<~>"),
            NotOr => write!(f, "~|"),
            NotAnd => write!(f, "~&"),
        }
    }
}

/// `assoc_connective`
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum AssocConnective {
    /// `&`
    And,
    /// `|`
    Or,
}

impl fmt::Display for AssocConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AssocConnective::*;
        match self {
            And => write!(f, "&"),
            Or => write!(f, "|"),
        }
    }
}

/// `fof_quantifier`
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofQuantifier {
    /// `!`
    Forall,
    /// `?`
    Exists,
}

impl fmt::Display for FofQuantifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofQuantifier::*;
        match self {
            Forall => write!(f, "!"),
            Exists => write!(f, "?"),
        }
    }
}

/// `fof_plain_atomic_formula`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofPlainAtomicFormula<'a>(pub FofPlainTerm<'a>);

/// `defined_infix_pred`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DefinedInfixPred(pub InfixEquality);

/// `fof_defined_infix_formula`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofDefinedInfixFormula<'a> {
    pub left: FofTerm<'a>,
    pub op: DefinedInfixPred,
    pub right: FofTerm<'a>,
}

impl<'a> fmt::Display for FofDefinedInfixFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.left, self.op, self.right)
    }
}

/// `fof_defined_plain_formula`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofDefinedPlainFormula<'a>(pub FofDefinedPlainTerm<'a>);

/// `fof_defined_atomic_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofDefinedAtomicFormula<'a> {
    Plain(FofDefinedPlainFormula<'a>),
    Infix(FofDefinedInfixFormula<'a>),
}

/// `fof_atomic_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofAtomicFormula<'a> {
    Plain(FofPlainAtomicFormula<'a>),
    Defined(FofDefinedAtomicFormula<'a>),
}

/// `fof_variable_list`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofVariableList<'a>(pub Vec<Variable<'a>>);

impl<'a> fmt::Display for FofVariableList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, ",", &self.0)
    }
}

/// `fof_quantified_formula`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofQuantifiedFormula<'a> {
    pub quantifier: FofQuantifier,
    pub bound: FofVariableList<'a>,
    pub formula: FofUnitFormula<'a>,
}

impl<'a> fmt::Display for FofQuantifiedFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]:{}", self.quantifier, self.bound, self.formula)
    }
}

/// `fof_infix_unary`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofInfixUnary<'a> {
    pub left: FofTerm<'a>,
    pub op: InfixInequality,
    pub right: FofTerm<'a>,
}

impl<'a> fmt::Display for FofInfixUnary<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.left, self.op, self.right)
    }
}

/// `fof_unary_formula`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofUnaryFormula<'a> {
    Unary(UnaryConnective, FofUnitFormula<'a>),
    InfixUnary(FofInfixUnary<'a>),
}

impl<'a> fmt::Display for FofUnaryFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofUnaryFormula::*;
        match self {
            Unary(connective, u) => write!(f, "{}{}", connective, u),
            InfixUnary(i) => write!(f, "{}", i),
        }
    }
}

/// `fof_unitary_formula`
#[derive(Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofUnitaryFormula<'a> {
    Quantified(FofQuantifiedFormula<'a>),
    Atomic(FofAtomicFormula<'a>),
    Parenthesised(FofLogicFormula<'a>),
}

impl<'a> fmt::Display for FofUnitaryFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofUnitaryFormula::*;
        match self {
            Quantified(q) => write!(f, "{}", q),
            Atomic(a) => write!(f, "{}", a),
            Parenthesised(p) => write!(f, "({})", p),
        }
    }
}

/// `fof_unit_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofUnitFormula<'a> {
    Unitary(Box<FofUnitaryFormula<'a>>),
    Unary(Box<FofUnaryFormula<'a>>),
}

/// `fof_or_formula`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofOrFormula<'a>(pub Vec<FofUnitFormula<'a>>);

impl<'a> fmt::Display for FofOrFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, "|", &self.0)
    }
}

/// `fof_and_formula`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofAndFormula<'a>(pub Vec<FofUnitFormula<'a>>);

impl<'a> fmt::Display for FofAndFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, "&", &self.0)
    }
}

/// `fof_binary_assoc`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofBinaryAssoc<'a> {
    Or(FofOrFormula<'a>),
    And(FofAndFormula<'a>),
}

/// `fof_binary_nonassoc`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofBinaryNonassoc<'a> {
    pub left: FofUnitFormula<'a>,
    pub op: NonassocConnective,
    pub right: FofUnitFormula<'a>,
}

impl<'a> fmt::Display for FofBinaryNonassoc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.left, self.op, self.right)
    }
}

/// `fof_binary_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofBinaryFormula<'a> {
    Nonassoc(FofBinaryNonassoc<'a>),
    Assoc(FofBinaryAssoc<'a>),
}

/// `fof_logic_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofLogicFormula<'a> {
    Binary(FofBinaryFormula<'a>),
    Unary(Box<FofUnaryFormula<'a>>),
    Unitary(Box<FofUnitaryFormula<'a>>),
}

/// `fof_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofFormula<'a>(pub FofLogicFormula<'a>);

/// `literal`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Literal<'a> {
    Atomic(FofAtomicFormula<'a>),
    NegatedAtomic(FofAtomicFormula<'a>),
    Infix(FofInfixUnary<'a>),
}

impl<'a> fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Literal::*;
        match self {
            Atomic(a) => write!(f, "{}", a),
            NegatedAtomic(n) => write!(f, "~{}", n),
            Infix(i) => write!(f, "{}", i),
        }
    }
}

/// `disjunction`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Disjunction<'a>(pub Vec<Literal<'a>>);

impl<'a> fmt::Display for Disjunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, "|", &self.0)
    }
}

/// `cnf_formula`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum CnfFormula<'a> {
    Disjunction(Disjunction<'a>),
    Parenthesised(Disjunction<'a>),
}

impl<'a> fmt::Display for CnfFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CnfFormula::*;
        match self {
            Disjunction(d) => write!(f, "{}", d),
            Parenthesised(d) => write!(f, "({})", d),
        }
    }
}

/// `formula_role`
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FormulaRole {
    Axiom,
    Hypothesis,
    Definition,
    Assumption,
    Lemma,
    Theorem,
    Corollary,
    Conjecture,
    NegatedConjecture,
    Plain,
    Unknown,
}

impl fmt::Display for FormulaRole {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FormulaRole::*;
        match self {
            Axiom => write!(f, "axiom"),
            Hypothesis => write!(f, "hypothesis"),
            Definition => write!(f, "definition"),
            Assumption => write!(f, "assumption"),
            Lemma => write!(f, "lemma"),
            Theorem => write!(f, "theorem"),
            Corollary => write!(f, "corollary"),
            Conjecture => write!(f, "conjecture"),
            NegatedConjecture => write!(f, "negated_conjecture"),
            Plain => write!(f, "plain"),
            Unknown => write!(f, "unknown"),
        }
    }
}

/// `formula_data`
#[derive(Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FormulaData<'a> {
    Fof(FofFormula<'a>),
    Cnf(CnfFormula<'a>),
}

impl<'a> fmt::Display for FormulaData<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FormulaData::*;
        match self {
            Fof(fof) => write!(f, "$fof({})", fof),
            Cnf(cnf) => write!(f, "$cnf({})", cnf),
        }
    }
}

/// `general_function`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct GeneralFunction<'a> {
    pub word: AtomicWord<'a>,
    pub terms: GeneralTerms<'a>,
}

impl<'a> fmt::Display for GeneralFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.word, self.terms)
    }
}

/// `general_data`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum GeneralData<'a> {
    Atomic(AtomicWord<'a>),
    Function(GeneralFunction<'a>),
    Variable(Variable<'a>),
    Formula(FormulaData<'a>),
}

/// `general_terms`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct GeneralTerms<'a>(pub Vec<GeneralTerm<'a>>);

impl<'a> fmt::Display for GeneralTerms<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, ",", &self.0)
    }
}

/// `general_list`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct GeneralList<'a>(pub Option<GeneralTerms<'a>>);

impl<'a> fmt::Display for GeneralList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            None => write!(f, "[]"),
            Some(terms) => write!(f, "[{}]", terms),
        }
    }
}

/// `general_term`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum GeneralTerm<'a> {
    Data(GeneralData<'a>),
    Colon(GeneralData<'a>, Box<GeneralTerm<'a>>),
    List(GeneralList<'a>),
}

impl<'a> fmt::Display for GeneralTerm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::GeneralTerm::*;
        match self {
            Data(d) => write!(f, "{}", d),
            Colon(d, t) => write!(f, "{}:{}", d, t),
            List(l) => write!(f, "{}", l),
        }
    }
}

/// `source`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Source<'a>(pub GeneralTerm<'a>);

/// `useful_info`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct UsefulInfo<'a>(pub GeneralList<'a>);

/// `optional_info`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct OptionalInfo<'a>(pub Option<UsefulInfo<'a>>);

impl<'a> fmt::Display for OptionalInfo<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(ref info) => write!(f, ",{}", info),
            None => Ok(()),
        }
    }
}

/// `annotations`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Annotations<'a>(pub Option<(Source<'a>, OptionalInfo<'a>)>);

impl<'a> fmt::Display for Annotations<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Some((source, info)) => write!(f, ",{}{}", source, info),
            None => Ok(()),
        }
    }
}

/// `fof_annotated`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofAnnotated<'a> {
    pub name: Name<'a>,
    pub role: FormulaRole,
    pub formula: FofFormula<'a>,
    pub annotations: Annotations<'a>,
}

impl<'a> fmt::Display for FofAnnotated<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fof({},{},{}{}).",
            self.name, self.role, self.formula, self.annotations
        )
    }
}

/// `cnf_annotated`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct CnfAnnotated<'a> {
    pub name: Name<'a>,
    pub role: FormulaRole,
    pub formula: CnfFormula<'a>,
    pub annotations: Annotations<'a>,
}

impl<'a> fmt::Display for CnfAnnotated<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "cnf({},{},{}{}).",
            self.name, self.role, self.formula, self.annotations
        )
    }
}

/// `annotated_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum AnnotatedFormula<'a> {
    Fof(FofAnnotated<'a>),
    Cnf(CnfAnnotated<'a>),
}

/// `file_name`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FileName<'a>(pub SingleQuoted<'a>);

/// `name_list`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct NameList<'a>(pub Vec<Name<'a>>);

impl<'a> fmt::Display for NameList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, ",", &self.0)
    }
}

/// `formula_selection`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FormulaSelection<'a>(pub Option<NameList<'a>>);

impl<'a> fmt::Display for FormulaSelection<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Some(list) => write!(f, ",[{}]", list),
            None => Ok(()),
        }
    }
}

/// `include`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Include<'a> {
    pub file_name: FileName<'a>,
    pub selection: FormulaSelection<'a>,
}

impl<'a> fmt::Display for Include<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "include({}{}).", self.file_name, self.selection)
    }
}

/// `TPTP_input`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum TPTPInput<'a> {
    Annotated(Box<AnnotatedFormula<'a>>),
    Include(Include<'a>),
}

/// [visitor pattern](https://en.wikipedia.org/wiki/Visitor_pattern) trait
///
/// This can make traversing the TPTP AST easier.
/// Default method implementations visit child structures in parsing order.
pub trait Visitor<'a> {
    fn visit_lower_word(&mut self, _lower_word: LowerWord<'a>) {}

    fn visit_upper_word(&mut self, _upper_word: UpperWord<'a>) {}

    fn visit_single_quoted(&mut self, _single_quoted: SingleQuoted<'a>) {}

    fn visit_atomic_word(&mut self, atomic_word: AtomicWord<'a>) {
        match atomic_word {
            AtomicWord::Lower(lower_word) => self.visit_lower_word(lower_word),
            AtomicWord::SingleQuoted(single_quoted) => {
                self.visit_single_quoted(single_quoted)
            }
        }
    }

    fn visit_integer(&mut self, _integer: Integer<'a>) {}

    fn visit_name(&mut self, name: Name<'a>) {
        match name {
            Name::AtomicWord(atomic_word) => {
                self.visit_atomic_word(atomic_word)
            }
            Name::Integer(integer) => self.visit_integer(integer),
        }
    }

    fn visit_variable(&mut self, variable: Variable<'a>) {
        self.visit_upper_word(variable.0);
    }

    fn visit_functor(&mut self, functor: Functor<'a>) {
        self.visit_atomic_word(functor.0);
    }

    fn visit_dollar_word(&mut self, dollar_word: DollarWord<'a>) {
        self.visit_lower_word(dollar_word.0);
    }

    fn visit_atomic_defined_word(
        &mut self,
        atomic_defined_word: AtomicDefinedWord<'a>,
    ) {
        self.visit_dollar_word(atomic_defined_word.0);
    }

    fn visit_defined_functor(&mut self, defined_functor: DefinedFunctor<'a>) {
        self.visit_atomic_defined_word(defined_functor.0);
    }

    fn visit_defined_constant(
        &mut self,
        defined_constant: DefinedConstant<'a>,
    ) {
        self.visit_defined_functor(defined_constant.0);
    }

    fn visit_fof_arguments(&mut self, fof_arguments: FofArguments<'a>) {
        for fof_term in fof_arguments.0 {
            self.visit_fof_term(fof_term);
        }
    }

    fn visit_fof_plain_term(&mut self, fof_plain_term: FofPlainTerm<'a>) {
        match fof_plain_term {
            FofPlainTerm::Constant(functor) => self.visit_functor(functor),
            FofPlainTerm::Function(functor, fof_arguments) => {
                self.visit_functor(functor);
                self.visit_fof_arguments(fof_arguments);
            }
        }
    }

    fn visit_fof_defined_plain_term(
        &mut self,
        fof_defined_plain_term: FofDefinedPlainTerm<'a>,
    ) {
        self.visit_defined_constant(fof_defined_plain_term.0);
    }

    fn visit_fof_defined_atomic_term(
        &mut self,
        fof_defined_atomic_term: FofDefinedAtomicTerm<'a>,
    ) {
        self.visit_fof_defined_plain_term(fof_defined_atomic_term.0);
    }

    fn visit_fof_defined_term(&mut self, fof_defined_term: FofDefinedTerm<'a>) {
        self.visit_fof_defined_atomic_term(fof_defined_term.0);
    }

    fn visit_fof_function_term(
        &mut self,
        fof_function_term: FofFunctionTerm<'a>,
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

    fn visit_fof_term(&mut self, fof_term: FofTerm<'a>) {
        match fof_term {
            FofTerm::Function(fof_function_term) => {
                self.visit_fof_function_term(fof_function_term)
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

    fn visit_fof_plain_atomic_formula(
        &mut self,
        fof_plain_atomic_formula: FofPlainAtomicFormula<'a>,
    ) {
        self.visit_fof_plain_term(fof_plain_atomic_formula.0);
    }

    fn visit_fof_defined_plain_formula(
        &mut self,
        fof_defined_plain_formula: FofDefinedPlainFormula<'a>,
    ) {
        self.visit_fof_defined_plain_term(fof_defined_plain_formula.0);
    }

    fn visit_fof_defined_infix_formula(
        &mut self,
        fof_defined_infix_formula: FofDefinedInfixFormula<'a>,
    ) {
        self.visit_fof_term(fof_defined_infix_formula.left);
        self.visit_defined_infix_pred(fof_defined_infix_formula.op);
        self.visit_fof_term(fof_defined_infix_formula.right);
    }

    fn visit_fof_defined_atomic_formula(
        &mut self,
        fof_defined_atomic_formula: FofDefinedAtomicFormula<'a>,
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
        fof_atomic_formula: FofAtomicFormula<'a>,
    ) {
        match fof_atomic_formula {
            FofAtomicFormula::Plain(fof_plain_atomic_formula) => {
                self.visit_fof_plain_atomic_formula(fof_plain_atomic_formula)
            }
            FofAtomicFormula::Defined(fof_defined_atomic_formula) => self
                .visit_fof_defined_atomic_formula(fof_defined_atomic_formula),
        }
    }

    fn visit_fof_infix_unary(&mut self, fof_infix_unary: FofInfixUnary<'a>) {
        self.visit_fof_term(fof_infix_unary.left);
        self.visit_infix_inequality(fof_infix_unary.op);
        self.visit_fof_term(fof_infix_unary.right);
    }

    fn visit_fof_binary_nonassoc(
        &mut self,
        fof_binary_nonassoc: FofBinaryNonassoc<'a>,
    ) {
        self.visit_fof_unit_formula(fof_binary_nonassoc.left);
        self.visit_nonassoc_connective(fof_binary_nonassoc.op);
        self.visit_fof_unit_formula(fof_binary_nonassoc.right);
    }

    fn visit_fof_or_formula(&mut self, fof_or_formula: FofOrFormula<'a>) {
        for fof_unit_formula in fof_or_formula.0 {
            self.visit_fof_unit_formula(fof_unit_formula);
        }
    }

    fn visit_fof_and_formula(&mut self, fof_and_formula: FofAndFormula<'a>) {
        for fof_unit_formula in fof_and_formula.0 {
            self.visit_fof_unit_formula(fof_unit_formula);
        }
    }

    fn visit_fof_binary_assoc(&mut self, fof_binary_assoc: FofBinaryAssoc<'a>) {
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
        fof_binary_formula: FofBinaryFormula<'a>,
    ) {
        match fof_binary_formula {
            FofBinaryFormula::Nonassoc(fof_binary_nonassoc) => {
                self.visit_fof_binary_nonassoc(fof_binary_nonassoc)
            }
            FofBinaryFormula::Assoc(fof_binary_assoc) => {
                self.visit_fof_binary_assoc(fof_binary_assoc)
            }
        }
    }

    fn visit_fof_unary_formula(
        &mut self,
        fof_unary_formula: FofUnaryFormula<'a>,
    ) {
        match fof_unary_formula {
            FofUnaryFormula::Unary(unary_connective, fof_unit_formula) => {
                self.visit_unary_connective(unary_connective);
                self.visit_fof_unit_formula(fof_unit_formula);
            }
            FofUnaryFormula::InfixUnary(fof_infix_unary) => {
                self.visit_fof_infix_unary(fof_infix_unary)
            }
        }
    }

    fn visit_fof_variable_list(
        &mut self,
        fof_variable_list: FofVariableList<'a>,
    ) {
        for variable in fof_variable_list.0 {
            self.visit_variable(variable);
        }
    }

    fn visit_fof_unit_formula(&mut self, fof_unit_formula: FofUnitFormula<'a>) {
        match fof_unit_formula {
            FofUnitFormula::Unitary(fof_unitary_formula) => {
                self.visit_fof_unitary_formula(*fof_unitary_formula)
            }
            FofUnitFormula::Unary(fof_unary_formula) => {
                self.visit_fof_unary_formula(*fof_unary_formula)
            }
        }
    }

    fn visit_fof_quantified_formula(
        &mut self,
        fof_quantified_formula: FofQuantifiedFormula<'a>,
    ) {
        self.visit_fof_quantifier(fof_quantified_formula.quantifier);
        self.visit_fof_variable_list(fof_quantified_formula.bound);
        self.visit_fof_unit_formula(fof_quantified_formula.formula);
    }

    fn visit_fof_unitary_formula(
        &mut self,
        fof_unitary_formula: FofUnitaryFormula<'a>,
    ) {
        match fof_unitary_formula {
            FofUnitaryFormula::Quantified(fof_quantified_formula) => {
                self.visit_fof_quantified_formula(fof_quantified_formula)
            }
            FofUnitaryFormula::Atomic(fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            FofUnitaryFormula::Parenthesised(fof_logic_formula) => {
                self.visit_fof_logic_formula(fof_logic_formula)
            }
        }
    }

    fn visit_fof_logic_formula(
        &mut self,
        fof_logic_formula: FofLogicFormula<'a>,
    ) {
        match fof_logic_formula {
            FofLogicFormula::Binary(fof_binary_formula) => {
                self.visit_fof_binary_formula(fof_binary_formula)
            }
            FofLogicFormula::Unary(fof_unary_formula) => {
                self.visit_fof_unary_formula(*fof_unary_formula)
            }
            FofLogicFormula::Unitary(fof_unitary_formula) => {
                self.visit_fof_unitary_formula(*fof_unitary_formula)
            }
        }
    }

    fn visit_fof_formula(&mut self, fof_formula: FofFormula<'a>) {
        self.visit_fof_logic_formula(fof_formula.0);
    }

    fn visit_literal(&mut self, literal: Literal<'a>) {
        match literal {
            Literal::Atomic(fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            Literal::NegatedAtomic(fof_atomic_formula) => {
                self.visit_fof_atomic_formula(fof_atomic_formula)
            }
            Literal::Infix(fof_infix_unary) => {
                self.visit_fof_infix_unary(fof_infix_unary)
            }
        }
    }

    fn visit_disjunction(&mut self, disjunction: Disjunction<'a>) {
        for literal in disjunction.0 {
            self.visit_literal(literal);
        }
    }

    fn visit_cnf_formula(&mut self, cnf_formula: CnfFormula<'a>) {
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

    fn visit_general_terms(&mut self, general_terms: GeneralTerms<'a>) {
        for general_term in general_terms.0 {
            self.visit_general_term(general_term);
        }
    }

    fn visit_general_list(&mut self, general_list: GeneralList<'a>) {
        if let Some(general_terms) = general_list.0 {
            self.visit_general_terms(general_terms)
        }
    }

    fn visit_general_function(
        &mut self,
        general_function: GeneralFunction<'a>,
    ) {
        self.visit_atomic_word(general_function.word);
        self.visit_general_terms(general_function.terms);
    }

    fn visit_formula_data(&mut self, formula_data: FormulaData<'a>) {
        match formula_data {
            FormulaData::Fof(fof_formula) => {
                self.visit_fof_formula(fof_formula)
            }
            FormulaData::Cnf(cnf_formula) => {
                self.visit_cnf_formula(cnf_formula)
            }
        }
    }

    fn visit_general_data(&mut self, general_data: GeneralData<'a>) {
        match general_data {
            GeneralData::Atomic(atomic_word) => {
                self.visit_atomic_word(atomic_word)
            }
            GeneralData::Function(general_function) => {
                self.visit_general_function(general_function)
            }
            GeneralData::Variable(variable) => self.visit_variable(variable),
            GeneralData::Formula(formula_data) => {
                self.visit_formula_data(formula_data)
            }
        }
    }

    fn visit_general_term(&mut self, general_term: GeneralTerm<'a>) {
        match general_term {
            GeneralTerm::Data(general_data) => {
                self.visit_general_data(general_data)
            }
            GeneralTerm::Colon(general_data, general_term) => {
                self.visit_general_data(general_data);
                self.visit_general_term(*general_term);
            }
            GeneralTerm::List(general_list) => {
                self.visit_general_list(general_list)
            }
        }
    }

    fn visit_source(&mut self, source: Source<'a>) {
        self.visit_general_term(source.0);
    }

    fn visit_useful_info(&mut self, useful_info: UsefulInfo<'a>) {
        self.visit_general_list(useful_info.0);
    }

    fn visit_optional_info(&mut self, optional_info: OptionalInfo<'a>) {
        if let Some(useful_info) = optional_info.0 {
            self.visit_useful_info(useful_info)
        }
    }

    fn visit_annotations(&mut self, annotations: Annotations<'a>) {
        if let Some((source, optional_info)) = annotations.0 {
            self.visit_source(source);
            self.visit_optional_info(optional_info);
        }
    }

    fn visit_fof_annotated(&mut self, fof_annotated: FofAnnotated<'a>) {
        self.visit_name(fof_annotated.name);
        self.visit_formula_role(fof_annotated.role);
        self.visit_fof_formula(fof_annotated.formula);
        self.visit_annotations(fof_annotated.annotations);
    }

    fn visit_cnf_annotated(&mut self, cnf_annotated: CnfAnnotated<'a>) {
        self.visit_name(cnf_annotated.name);
        self.visit_formula_role(cnf_annotated.role);
        self.visit_cnf_formula(cnf_annotated.formula);
        self.visit_annotations(cnf_annotated.annotations);
    }

    fn visit_annotated_formula(&mut self, annotated: AnnotatedFormula<'a>) {
        match annotated {
            AnnotatedFormula::Fof(fof_annotated) => {
                self.visit_fof_annotated(fof_annotated)
            }
            AnnotatedFormula::Cnf(cnf_annotated) => {
                self.visit_cnf_annotated(cnf_annotated)
            }
        }
    }

    fn visit_name_list(&mut self, name_list: NameList<'a>) {
        for name in name_list.0 {
            self.visit_name(name);
        }
    }

    fn visit_formula_selection(&mut self, selection: FormulaSelection<'a>) {
        if let Some(name_list) = selection.0 {
            self.visit_name_list(name_list);
        }
    }

    fn visit_file_name(&mut self, file_name: FileName<'a>) {
        self.visit_single_quoted(file_name.0);
    }

    fn visit_include(&mut self, include: Include<'a>) {
        self.visit_file_name(include.file_name);
        self.visit_formula_selection(include.selection);
    }

    fn visit_tptp_input(&mut self, input: TPTPInput<'a>) {
        match input {
            TPTPInput::Annotated(annotated) => {
                self.visit_annotated_formula(*annotated)
            }
            TPTPInput::Include(include) => self.visit_include(include),
        }
    }
}
