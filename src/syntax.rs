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
#[derive(
    AsRef, Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Integer<'a>(pub Cow<'a, str>);

/// `rational`
#[derive(
    AsRef, Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Rational<'a>(pub Cow<'a, str>);

/// `rational`
#[derive(
    AsRef, Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Real<'a>(pub Cow<'a, str>);

/// `lower_word`
#[derive(
    AsRef, Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LowerWord<'a>(pub Cow<'a, str>);

/// `upper_word`
#[derive(
    AsRef, Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
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

/// `dollar_dollar_word`
#[derive(AsRef, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DollarDollarWord<'a>(pub LowerWord<'a>);

impl<'a> fmt::Display for DollarDollarWord<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "$${}", self.0)
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

/// `distinct_object`
#[derive(AsRef, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DistinctObject<'a>(pub Cow<'a, str>);

impl<'a> fmt::Display for DistinctObject<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

/// `atomic_system_word`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct AtomicSystemWord<'a>(pub DollarDollarWord<'a>);

/// `system_functor`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct SystemFunctor<'a>(pub AtomicSystemWord<'a>);

/// `system_constant`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct SystemConstant<'a>(pub SystemFunctor<'a>);

/// `number`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Number<'a> {
    Integer(Integer<'a>),
    Rational(Rational<'a>),
    Real(Real<'a>),
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

/// `constant`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Constant<'a>(pub Functor<'a>);

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

/// `fof_system_term`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofSystemTerm<'a> {
    /// `system_constant`
    Constant(SystemConstant<'a>),
    /// `system_functor`, `fof_arguments`
    Function(SystemFunctor<'a>, FofArguments<'a>),
}

impl<'a> fmt::Display for FofSystemTerm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofSystemTerm::*;
        match self {
            Constant(c) => write!(f, "{}", c),
            Function(name, args) => write!(f, "{}{}", name, args),
        }
    }
}

/// `fof_plain_term`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofPlainTerm<'a> {
    /// `constant`
    Constant(Constant<'a>),
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

/// `defined_term`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum DefinedTerm<'a> {
    Number(Number<'a>),
    Distinct(DistinctObject<'a>),
}

/// `fof_defined_term`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofDefinedTerm<'a> {
    Defined(DefinedTerm<'a>),
    Atomic(FofDefinedAtomicTerm<'a>),
}

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

/// `fof_system_atomic_formula`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FofSystemAtomicFormula<'a>(pub FofSystemTerm<'a>);

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
    System(FofSystemAtomicFormula<'a>),
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

/// `thf_unitary_type`
#[derive(
    AsRef, Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash,
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ThfUnitaryType<'a>(pub ThfUnitaryFormula<'a>);

/// `thf_mapping_type`
#[derive(AsRef, Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ThfMappingType<'a>(pub Vec<ThfUnitaryType<'a>>);

impl<'a> fmt::Display for ThfMappingType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, ">", &self.0)
    }
}

/// `thf_binary_type`
#[derive(Clone, Debug, Display, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfBinaryType<'a> {
    Mapping(ThfMappingType<'a>),
}

/// `thf_plain_atomic`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfPlainAtomic<'a> {
    Constant(Constant<'a>),
}

/// `thf_unitary_term`
#[derive(Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfUnitaryTerm<'a> {
    Atomic(ThfAtomicFormula<'a>),
    Variable(Variable<'a>),
    Parenthesised(ThfLogicFormula<'a>),
}

impl<'a> fmt::Display for ThfUnitaryTerm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ThfUnitaryTerm::Atomic(atomic) => write!(f, "{}", atomic),
            ThfUnitaryTerm::Variable(variable) => write!(f, "{}", variable),
            ThfUnitaryTerm::Parenthesised(formula) => {
                write!(f, "({})", formula)
            }
        }
    }
}

/// `thf_defined_infix`
#[derive(Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ThfDefinedInfix<'a> {
    pub left: ThfUnitaryTerm<'a>,
    pub op: DefinedInfixPred,
    pub right: ThfUnitaryTerm<'a>,
}

impl<'a> fmt::Display for ThfDefinedInfix<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.left, self.op, self.right)
    }
}

/// `thf_defined_atomic_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfDefinedAtomic<'a> {
    Constant(DefinedConstant<'a>),
}

/// `thf_atomic_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfAtomicFormula<'a> {
    Plain(ThfPlainAtomic<'a>),
    Defined(ThfDefinedAtomic<'a>),
}

/// `thf_unitary_formula`
#[derive(Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfUnitaryFormula<'a> {
    Atomic(ThfAtomicFormula<'a>),
    Variable(Variable<'a>),
    Parenthesised(ThfLogicFormula<'a>),
}

impl<'a> fmt::Display for ThfUnitaryFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ThfUnitaryFormula::Atomic(atomic) => write!(f, "{}", atomic),
            ThfUnitaryFormula::Variable(variable) => write!(f, "{}", variable),
            ThfUnitaryFormula::Parenthesised(formula) => {
                write!(f, "({})", formula)
            }
        }
    }
}

/// `thf_infix_unary`
#[derive(Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ThfInfixUnary<'a> {
    pub left: ThfUnitaryTerm<'a>,
    pub op: InfixInequality,
    pub right: ThfUnitaryTerm<'a>,
}

impl<'a> fmt::Display for ThfInfixUnary<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.left, self.op, self.right)
    }
}

/// `thf_unary_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfUnaryFormula<'a> {
    Infix(ThfInfixUnary<'a>),
}

/// `thf_unit_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfUnitFormula<'a> {
    Unitary(ThfUnitaryFormula<'a>),
    Unary(ThfUnaryFormula<'a>),
    Infix(ThfDefinedInfix<'a>),
}

/// `thf_or_formula`
#[derive(Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ThfOrFormula<'a>(pub Vec<ThfUnitFormula<'a>>);

impl<'a> fmt::Display for ThfOrFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, "|", &self.0)
    }
}

/// `thf_apply_formula`
#[derive(Clone, Debug, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ThfApplyFormula<'a>(pub Vec<ThfUnitFormula<'a>>);

impl<'a> fmt::Display for ThfApplyFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_list(f, "@", &self.0)
    }
}

/// `thf_binary_assoc`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfBinaryAssoc<'a> {
    Or(ThfOrFormula<'a>),
    Apply(ThfApplyFormula<'a>),
}

/// `thf_binary_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfBinaryFormula<'a> {
    Assoc(ThfBinaryAssoc<'a>),
    Type(ThfBinaryType<'a>),
}

/// `thf_logic_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfLogicFormula<'a> {
    Unitary(Box<ThfUnitaryFormula<'a>>),
    Unary(Box<ThfUnaryFormula<'a>>),
    Binary(ThfBinaryFormula<'a>),
    DefinedInfix(Box<ThfDefinedInfix<'a>>),
}

/// `thf_formula`
#[derive(Clone, Debug, Display, From, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ThfFormula<'a> {
    Logic(ThfLogicFormula<'a>),
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
    Type,
    FiDomain,
    FiFunctors,
    FiPredicates,
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
            Type => write!(f, "type"),
            FiDomain => write!(f, "fi_domain"),
            FiFunctors => write!(f, "fi_functors"),
            FiPredicates => write!(f, "fi_predicates"),
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
    Fot(FofTerm<'a>),
}

impl<'a> fmt::Display for FormulaData<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FormulaData::*;
        match self {
            Fof(fof) => write!(f, "$fof({})", fof),
            Cnf(cnf) => write!(f, "$cnf({})", cnf),
            Fot(cnf) => write!(f, "$fot({})", cnf),
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
    Number(Number<'a>),
    DistinctObject(DistinctObject<'a>),
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
