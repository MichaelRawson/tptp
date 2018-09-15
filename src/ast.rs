use std::sync::Arc;

/// A variable name.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Bound(pub Arc<String>);

/// One of various types of TPTP identifiers.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Name {
    /// An alphanumeric token, like `propositional_fact2`.
    Word(Arc<String>),
    /// A 'quoted string'.
    Quoted(Arc<String>),
    /// Integral identifiers of arbitrary size.
    Integer(Arc<String>),
}

impl AsRef<String> for Name {
    fn as_ref(&self) -> &String {
        use self::Name::*;
        match self {
            Word(x) => x.as_ref(),
            Quoted(x) => x.as_ref(),
            Integer(x) => x.as_ref(),
        }
    }
}

/// A FOF term.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofTerm {
    /// A bound variable `X`.
    Variable(Bound),
    /// An application of a name to arguments, `f(t1, t2, ...)`.
    /// Constants `c` are treated as nullary functors `c()`.
    Functor(Name, Vec<Box<FofTerm>>),
}

/// A FOF formula, simplified where possible.
/// For example, `p <~> q` becomes `~(p <=> q)`.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofFormula {
    /// `$true`
    True,
    /// `$false`
    False,
    /// `t1 = t2`
    Equal(Box<FofTerm>, Box<FofTerm>),
    /// `p(t1, t2, ...)`
    Predicate(Name, Vec<Box<FofTerm>>),
    /// `~p`
    Not(Box<FofFormula>),
    /// `p1 & p2 & ...`
    And(Vec<Box<FofFormula>>),
    /// `p1 | p2 | ...`
    Or(Vec<Box<FofFormula>>),
    /// `p => q`
    Implies(Box<FofFormula>, Box<FofFormula>),
    /// `p <=> q`
    Equivalent(Box<FofFormula>, Box<FofFormula>),
    /// `![X1, X2, ...]: p`
    Forall(Vec<Bound>, Box<FofFormula>),
    /// `?[X1, X2, ...]: p`
    Exists(Vec<Bound>, Box<FofFormula>),
}

/// A TPTP formula role, such as `axiom` or `negated_conjecture`
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
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

/// A top-level TPTP statement, current `include` or `fof`.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Statement {
    Include(String),
    Fof(Name, FormulaRole, Box<FofFormula>),
}
