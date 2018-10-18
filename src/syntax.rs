use std::fmt;

/// One of various types of TPTP identifiers.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Name {
    /// An alphanumeric token, like `propositional_fact2`.
    Word(String),
    /// A 'quoted string'.
    Quoted(String),
    /// Integral identifiers of arbitrary size.
    Integer(String),
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Name::*;
        match self {
            Word(x) => write!(f, "{}", x),
            Quoted(x) => {
                let escaped = x.replace('\\', "\\\\").replace('\'', "\\'");
                write!(f, "'{}'", escaped)
            }
            Integer(x) => write!(f, "{}", x),
        }
    }
}

/// A FOF term.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofTerm {
    /// A bound variable `X`.
    Variable(String),
    /// An application of a name to arguments, `f(t1, t2, ...)`.
    /// Constants `c` are treated as nullary functors `c()`.
    Functor(Name, Vec<Box<FofTerm>>),
}

fn fmt_args(f: &mut fmt::Formatter, args: &[Box<FofTerm>]) -> fmt::Result {
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

impl fmt::Display for FofTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofTerm::*;
        match self {
            Variable(x) => write!(f, "{}", x),
            Functor(name, args) => {
                write!(f, "{}", name)?;
                fmt_args(f, args)
            }
        }
    }
}

/// A unary operator on FOF formulae
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofUnaryOp {
    /// `~p`
    Not,
}

impl fmt::Display for FofUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofUnaryOp::*;
        match self {
            Not => write!(f, "~"),
        }
    }
}

/// An infix binary operator on FOF terms
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofInfixOp {
    /// `t = s`
    Equal,
    /// `t != s`
    NotEqual,
}

impl fmt::Display for FofInfixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofInfixOp::*;
        match self {
            Equal => write!(f, "="),
            NotEqual => write!(f, "!="),
        }
    }
}

/// A non-associative binary operator on FOF formulae
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofNonAssocOp {
    /// `p => q`
    Implies,
    /// `p <=> q`
    Equivalent,
    /// `p <~> q`
    NotEquivalent,
}

impl fmt::Display for FofNonAssocOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofNonAssocOp::*;
        match self {
            Implies => write!(f, "=>"),
            Equivalent => write!(f, "<=>"),
            NotEquivalent => write!(f, "<~>"),
        }
    }
}

/// An associative binary operator on FOF formulae
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofAssocOp {
    /// `p1 & p2 & ...`
    And,
    /// `p1 | p2 | ...`
    Or,
}

impl fmt::Display for FofAssocOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofAssocOp::*;
        match self {
            And => write!(f, "&"),
            Or => write!(f, "|"),
        }
    }
}

/// A FOF quantifier
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
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
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofFormula {
    /// `$true`, `$false`
    Boolean(bool),
    /// `t1 $op t2`
    Infix(FofInfixOp, Box<FofTerm>, Box<FofTerm>),
    /// `p(t1, t2, ...)`
    Predicate(Name, Vec<Box<FofTerm>>),
    /// `$op(p)`
    Unary(FofUnaryOp, Box<FofFormula>),
    /// `(p $op q)`
    NonAssoc(FofNonAssocOp, Box<FofFormula>, Box<FofFormula>),
    /// `p1 $op p2 $op ...`
    Assoc(FofAssocOp, Vec<Box<FofFormula>>),
    /// `$op[X1, X2, ...]: p`
    Quantified(FofQuantifier, Vec<String>, Box<FofFormula>),
}

impl fmt::Display for FofFormula {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofFormula::*;
        match self {
            Boolean(b) => write!(f, "${}", b),
            Infix(op, left, right) => write!(f, "{}{}{}", left, op, right),
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

/// A top-level TPTP statement, currently `include` or `fof`.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Statement {
    Include(String),
    Fof(Name, FormulaRole, Box<FofFormula>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Include(include) => write!(f, "include('{}').", include),
            Fof(name, role, formula) => write!(f, "fof({},{},{}).", name, role, formula),
        }
    }
}
