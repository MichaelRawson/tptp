#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::convert::AsRef;
use std::fmt;
use std::path::PathBuf;

/// One of various types of TPTP identifiers.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Name<'a> {
    /// An alphanumeric token, like `propositional_fact2`
    LowerWord(Cow<'a, str>),
    /// A `'quoted string'`, quotes included
    SingleQuoted(Cow<'a, str>),
    /// Integral identifiers of arbitrary size.
    Integer(Cow<'a, str>),
}

impl<'a> AsRef<str> for Name<'a> {
    fn as_ref(&self) -> &str {
        use self::Name::*;
        match self {
            LowerWord(name) => name,
            SingleQuoted(name) => name,
            Integer(name) => name,
        }
    }
}

impl<'a> fmt::Display for Name<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

/// A variable name, `X`
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Variable<'a>(pub Cow<'a, str>);

impl<'a> AsRef<str> for Variable<'a> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<'a> fmt::Display for Variable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

/// A FOF term.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofTerm<'a> {
    /// A bound variable
    Variable(Variable<'a>),
    /// A constant `c`
    Constant(Name<'a>),
    /// An application of a name to arguments, `f(t1, t2, ...)`.
    Functor(Name<'a>, Vec<FofTerm<'a>>),
}

fn fmt_args(f: &mut fmt::Formatter, args: &[FofTerm]) -> fmt::Result {
    if args.is_empty() {
        return Ok(());
    }

    write!(f, "(")?;
    let mut args = args.iter();
    write!(f, "{}", args.next().unwrap())?;
    for arg in args {
        write!(f, ",{}", arg)?;
    }
    write!(f, ")")
}

impl<'a> fmt::Display for FofTerm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofTerm::*;
        match self {
            Variable(x) => write!(f, "{}", x),
            Constant(c) => write!(f, "{}", c),
            Functor(name, args) => {
                write!(f, "{}", name)?;
                fmt_args(f, args)
            }
        }
    }
}

/// A unary operator on FOF formulae
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum UnaryConnective {
    /// `~p`
    Not,
}

impl fmt::Display for UnaryConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::UnaryConnective::*;
        match self {
            Not => write!(f, "~"),
        }
    }
}

/// An infix binary operator on FOF terms
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum InfixEquality {
    /// `t = s`
    Equal,
    /// `t != s`
    NotEqual,
}

impl fmt::Display for InfixEquality {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::InfixEquality::*;
        match self {
            Equal => write!(f, "="),
            NotEqual => write!(f, "!="),
        }
    }
}

/// A non-associative binary operator on FOF formulae
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum NonAssocConnective {
    /// `p => q`
    LRImplies,
    /// `p <= q`
    RLImplies,
    /// `p <=> q`
    Equivalent,
    /// `p <~> q`
    NotEquivalent,
    /// `p ~| q`
    NotOr,
    /// `p ~& q`
    NotAnd,
}

impl fmt::Display for NonAssocConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::NonAssocConnective::*;
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

/// An associative binary operator on FOF formulae
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum AssocConnective {
    /// `p1 & p2 & ...`
    And,
    /// `p1 | p2 | ...`
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

/// A FOF quantifier
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofQuantifier {
    /// `![X1, X2, ...]: p`
    Forall,
    /// `?[X1, X2, ...]: p`
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

/// A FOF formula
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum FofFormula<'a> {
    /// `$true`, `$false`
    Boolean(bool),
    /// `t1 $op t2`
    Infix(InfixEquality, FofTerm<'a>, FofTerm<'a>),
    /// `p`
    Proposition(Name<'a>),
    /// `p(t1, t2, ...)`
    Predicate(Name<'a>, Vec<FofTerm<'a>>),
    /// `$op(p)`
    Unary(UnaryConnective, Box<FofFormula<'a>>),
    /// `p $op q`
    NonAssoc(NonAssocConnective, Box<FofFormula<'a>>, Box<FofFormula<'a>>),
    /// `p1 $op p2 $op ...`
    Assoc(AssocConnective, Vec<FofFormula<'a>>),
    /// `$op[X1, X2, ...]: p`
    Quantified(FofQuantifier, Vec<Variable<'a>>, Box<FofFormula<'a>>),
}

impl<'a> fmt::Display for FofFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofFormula::*;
        match self {
            Boolean(b) => write!(f, "${}", b),
            Infix(op, left, right) => write!(f, "{}{}{}", left, op, right),
            Proposition(name) => write!(f, "{}", name),
            Predicate(name, args) => {
                write!(f, "{}", name)?;
                fmt_args(f, args)
            }
            Unary(op, sub) => write!(f, "{}({})", op, sub),
            NonAssoc(op, left, right) => write!(f, "({}{}{})", left, op, right),
            Assoc(op, args) => {
                if args.is_empty() {
                    panic!("displaying empty associative formula");
                }

                let mut args = args.iter();
                write!(f, "({}", args.next().unwrap())?;
                for arg in args {
                    write!(f, "{}{}", op, arg)?;
                }
                write!(f, ")")
            }
            Quantified(op, bound, sub) => {
                if bound.is_empty() {
                    panic!("displaying empty quantifier");
                }

                write!(f, "{}[", op)?;
                let mut bound = bound.iter();
                write!(f, "{}", bound.next().unwrap())?;
                for x in bound {
                    write!(f, ",{}", x)?;
                }
                write!(f, "]:{}", sub)
            }
        }
    }
}

/// A CNF literal
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum CnfLiteral<'a> {
    /// A literal, e.g. `p(X)`
    Literal(FofFormula<'a>),
    /// A negated literal, e.g. `~p(X)`
    NegatedLiteral(FofFormula<'a>),
}

impl<'a> fmt::Display for CnfLiteral<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CnfLiteral::*;
        match self {
            Literal(fof) => write!(f, "{}", fof),
            NegatedLiteral(fof) => write!(f, "~{}", fof),
        }
    }
}

/// A CNF formula
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct CnfFormula<'a>(pub Vec<CnfLiteral<'a>>);

impl<'a> fmt::Display for CnfFormula<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut literals = self.0.iter();
        write!(f, "{}", literals.next().unwrap())?;
        for literal in literals {
            write!(f, "|{}", literal)?;
        }
        Ok(())
    }
}

/// A TPTP formula role, such as `axiom` or `negated_conjecture`
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

/// DAG formula sources
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum DagSource<'a> {
    Name(Name<'a>),
    Inference(Cow<'a, str>, Vec<Source<'a>>),
}

impl<'a> fmt::Display for DagSource<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::DagSource::*;
        match self {
            Name(ref name) => write!(f, "{}", name),
            Inference(ref rule, ref parents) => {
                write!(f, "inference({},[],[", rule)?;
                if !parents.is_empty() {
                    let mut parents = parents.iter();
                    write!(f, "{}", parents.next().unwrap())?;
                    for parent in parents {
                        write!(f, ",{}", parent)?;
                    }
                }
                write!(f, "])")
            }
        }
    }
}

/// Internal formula sources
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum InternalSource {}

impl fmt::Display for InternalSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

/// External formula sources
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ExternalSource<'a> {
    File(Cow<'a, str>, Option<Name<'a>>),
}

impl<'a> fmt::Display for ExternalSource<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ExternalSource::*;
        match self {
            File(ref name, None) => write!(f, "file('{}')", name),
            File(ref name, Some(info)) => {
                write!(f, "file('{}',{})", name, info)
            }
        }
    }
}

/// Formula sources for use in annotations
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Source<'a> {
    /// `unknown`
    Unknown,
    /// A DAG source - either a name or an inference record
    Dag(DagSource<'a>),
    /// An internal source
    Internal(InternalSource),
    /// An external source
    External(ExternalSource<'a>),
    /// Multiple sources
    Sources(Vec<Source<'a>>),
}

impl<'a> fmt::Display for Source<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Source::*;
        match self {
            Unknown => write!(f, "unknown"),
            Dag(ref dag) => write!(f, "{}", dag),
            Internal(ref internal) => write!(f, "{}", internal),
            External(ref external) => write!(f, "{}", external),
            Sources(ref sources) => {
                write!(f, "[",)?;
                if !sources.is_empty() {
                    let mut sources = sources.iter();
                    write!(f, "{}", sources.next().unwrap())?;
                    for source in sources {
                        write!(f, ",{}", source)?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

/// Formula annotations
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Annotations<'a> {
    pub source: Source<'a>,
}

impl<'a> fmt::Display for Annotations<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.source)
    }
}

/// An include path, not including outer quotes
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Included<'a>(pub Cow<'a, str>);

impl<'a> fmt::Display for Included<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a> From<Included<'a>> for PathBuf {
    fn from(included: Included<'a>) -> Self {
        included.0.replace("\\\\", "\\").replace("\\'", "'").into()
    }
}

/// A top-level TPTP statement, currently `include`, `cnf`, or `fof`.
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Statement<'a> {
    Include(Included<'a>, Option<Vec<Name<'a>>>),
    Cnf(
        Name<'a>,
        FormulaRole,
        CnfFormula<'a>,
        Option<Annotations<'a>>,
    ),
    Fof(
        Name<'a>,
        FormulaRole,
        FofFormula<'a>,
        Option<Annotations<'a>>,
    ),
}

impl<'a> fmt::Display for Statement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Include(include, None) => write!(f, "include({}).", include),
            Include(include, Some(names)) => {
                write!(f, "include({},[", include)?;

                let mut names = names.iter();
                write!(f, "{}", names.next().unwrap())?;
                for name in names {
                    write!(f, ",{}", name)?;
                }

                write!(f, ").")
            }
            Cnf(name, role, formula, None) => {
                write!(f, "cnf({},{},{}).", name, role, formula)
            }
            Cnf(name, role, formula, Some(annotations)) => {
                write!(f, "cnf({},{},{},{}).", name, role, formula, annotations)
            }
            Fof(name, role, formula, None) => {
                write!(f, "fof({},{},{}).", name, role, formula)
            }
            Fof(name, role, formula, Some(annotations)) => {
                write!(f, "fof({},{},{},{}).", name, role, formula, annotations)
            }
        }
    }
}
