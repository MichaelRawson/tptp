use std::fmt;

fn escape_single_quoted(quoted: &str) -> String {
    quoted.replace('\\', "\\\\").replace('\'', "\\'")
}

fn escape_double_quoted(quoted: &str) -> String {
    quoted.replace('\\', "\\\\").replace('"', "\\\"")
}

/// One of various types of TPTP identifiers.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Name {
    /// An atomic token, like `propositional_fact2` or `'quoted string'`.
    Atomic(String),
    /// Integral identifiers of arbitrary size.
    Integer(String),
}

fn alphanumeric(name: &str) -> bool {
    name.chars().all(|x| match x {
        'A'...'Z' | 'a'...'z' | '0'...'9' | '_' => true,
        _ => false,
    })
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Name::*;
        match self {
            Atomic(x) => if alphanumeric(&x) {
                write!(f, "{}", x)
            } else {
                write!(f, "'{}'", escape_single_quoted(x))
            },
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
    /// A distinct object, e.g. `"distinct"`
    DistinctObject(String),
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
            DistinctObject(name) => write!(f, "\"{}\"", escape_double_quoted(name)),
        }
    }
}

/// A unary operator on FOF formulae
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofUnaryConnective {
    /// `~p`
    Not,
}

impl fmt::Display for FofUnaryConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofUnaryConnective::*;
        match self {
            Not => write!(f, "~"),
        }
    }
}

/// An infix binary operator on FOF terms
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
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
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofNonAssocConnective {
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

impl fmt::Display for FofNonAssocConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofNonAssocConnective::*;
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
#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum FofAssocConnective {
    /// `p1 & p2 & ...`
    And,
    /// `p1 | p2 | ...`
    Or,
}

impl fmt::Display for FofAssocConnective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FofAssocConnective::*;
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
    Infix(InfixEquality, Box<FofTerm>, Box<FofTerm>),
    /// `p(t1, t2, ...)`
    Predicate(Name, Vec<Box<FofTerm>>),
    /// `$op(p)`
    Unary(FofUnaryConnective, Box<FofFormula>),
    /// `(p $op q)`
    NonAssoc(FofNonAssocConnective, Box<FofFormula>, Box<FofFormula>),
    /// `p1 $op p2 $op ...`
    Assoc(FofAssocConnective, Vec<Box<FofFormula>>),
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

/// A CNF literal
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum CnfLiteral {
    /// A literal, e.g. `p(X)`
    Literal(Box<FofFormula>),
    /// A negated literal, e.g. `~p(X)`
    NegatedLiteral(Box<FofFormula>),
}

impl fmt::Display for CnfLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CnfLiteral::*;
        match self {
            Literal(fof) => write!(f, "{}", fof),
            NegatedLiteral(fof) => write!(f, "~{}", fof),
        }
    }
}

/// A CNF formula
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct CnfFormula(pub Vec<CnfLiteral>);

impl fmt::Display for CnfFormula {
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

/// Formula sources for use in annotations
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Source {
    Unknown,
    Dag(Name),
    File(String, Option<Name>),
    Inference(Name, Vec<Source>),
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Source::*;
        match self {
            Unknown => write!(f, "unknown"),
            Dag(ref name) => write!(f, "{}", name),
            File(ref name, None) => write!(f, "file('{}')", escape_single_quoted(name)),
            File(ref name, Some(info)) => {
                write!(f, "file('{}',{})", escape_single_quoted(name), info)
            }
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

/// Formula annotations
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Annotations {
    pub source: Source,
}

impl fmt::Display for Annotations {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.source)
    }
}

/// A top-level TPTP statement, currently `include`, `cnf`, or `fof`.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Statement {
    Include(String, Option<Vec<Name>>),
    Cnf(Name, FormulaRole, CnfFormula, Option<Annotations>),
    Fof(Name, FormulaRole, Box<FofFormula>, Option<Annotations>),
}

impl Statement {
    /// Get the name of a non-`include` statement.
    pub fn name(&self) -> &Name {
        use self::Statement::*;
        match self {
            Include(_, _) => panic!("include statement has no name"),
            Cnf(ref name, _, _, _) => name,
            Fof(ref name, _, _, _) => name,
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Include(include, None) => write!(f, "include('{}').", escape_single_quoted(include)),
            Include(include, Some(names)) => {
                write!(f, "include('{}',[", escape_single_quoted(include))?;

                let mut names = names.iter();
                write!(f, "{}", names.next().unwrap())?;
                for name in names {
                    write!(f, ",{}", name)?;
                }

                write!(f, ").")
            }
            Cnf(name, role, formula, None) => write!(f, "cnf({},{},{}).", name, role, formula),
            Cnf(name, role, formula, Some(annotations)) => {
                write!(f, "cnf({},{},{},{}).", name, role, formula, annotations)
            }
            Fof(name, role, formula, None) => write!(f, "fof({},{},{}).", name, role, formula),
            Fof(name, role, formula, Some(annotations)) => {
                write!(f, "fof({},{},{},{}).", name, role, formula, annotations)
            }
        }
    }
}
