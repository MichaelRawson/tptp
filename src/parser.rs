use std::mem;

use error::Error;
use error::SyntacticError::*;
use position::Position;
use syntax::FofFormula::*;
use syntax::FofTerm::*;
use syntax::*;
use token::Token;
use token::Token::*;

/// A stream of statements, wrapping an underlying `Token` stream
///
/// Understands either `None` or `EOF` to mean end-of-file.
/// It is an error to continue iterating over a `Parser` which has previously produced an error.
#[derive(Debug)]
pub struct Parser<T: Iterator<Item = Result<(Position, Token), (Position, Error)>>> {
    stream: T,
    read: Token,
    error: bool,
    position: Position,
}

impl<T: Iterator<Item = Result<(Position, Token), (Position, Error)>>> Parser<T> {
    /// Create a new stream by wrapping another
    pub fn new(stream: T) -> Self {
        Parser {
            stream,
            read: EOF,
            error: false,
            position: Position::default(),
        }
    }

    fn token(&mut self) -> Result<Token, (Position, Error)> {
        {
            if self.read != EOF {
                return Ok(mem::replace(&mut self.read, EOF));
            }
        }
        let (position, token) = self
            .stream
            .next()
            .unwrap_or_else(|| Ok((self.position, Token::EOF)))?;
        self.position = position;
        Ok(token)
    }

    fn put_back(&mut self, token: Token) {
        self.read = token;
    }

    fn error<E: Into<Error>, X>(&mut self, error: E) -> Result<X, (Position, Error)> {
        self.error = true;
        Err((self.position, error.into()))
    }

    fn expect(&mut self, expected: &Token) -> Result<(), (Position, Error)> {
        let token = self.token()?;
        if &token == expected {
            Ok(())
        } else {
            self.error(UnexpectedToken(token))
        }
    }

    fn statement(&mut self) -> Result<Option<Statement>, (Position, Error)> {
        match self.token()? {
            EOF => Ok(None),
            Lower(w) => match w.as_ref() {
                "include" => self.include(),
                "cnf" => self.cnf(),
                "fof" => self.fof(),
                _ => self.error(UnsupportedDialect(w)),
            }.map(Some),
            t => self.error(UnexpectedToken(t)),
        }
    }

    fn include(&mut self) -> Result<Statement, (Position, Error)> {
        self.expect(&LParen)?;
        let name = match self.token()? {
            SingleQuoted(name) => name,
            token => self.error(UnexpectedToken(token))?,
        };

        let select = match self.token()? {
            Comma => Some(self.name_list()?),
            t => {
                self.put_back(t);
                None
            }
        };
        self.expect(&RParen)?;
        self.expect(&Period)?;

        Ok(Statement::Include(name, select))
    }

    fn annotations(&mut self) -> Result<Annotations, (Position, Error)> {
        let source = self.source()?;
        Ok(Annotations { source })
    }

    fn source(&mut self) -> Result<Source, (Position, Error)> {
        match self.token()? {
            Lower(w) => match w.as_ref() {
                "unknown" => Ok(Source::Unknown),
                "file" => self.file_source(),
                "inference" => self.inference_record(),
                _ => {
                    self.put_back(Lower(w));
                    self.dag_source()
                }
            },
            token => {
                self.put_back(token);
                self.dag_source()
            }
        }
    }

    fn file_source(&mut self) -> Result<Source, (Position, Error)> {
        self.expect(&LParen)?;

        let file = match self.token()? {
            SingleQuoted(file) => file,
            token => self.error(UnexpectedToken(token))?,
        };

        let info = match self.token()? {
            Comma => Some(self.name()?),
            t => {
                self.put_back(t);
                None
            }
        };

        self.expect(&RParen)?;
        Ok(Source::File(file, info))
    }

    fn inference_record(&mut self) -> Result<Source, (Position, Error)> {
        self.expect(&LParen)?;

        let rule = self.atomic_word()?;
        self.expect(&Comma)?;

        // TODO: support the "useful info" field
        self.expect(&LBrack)?;
        self.expect(&RBrack)?;
        self.expect(&Comma)?;

        self.expect(&LBrack)?;
        let mut parents = vec![];

        let token = self.token()?;
        if token != RBrack {
            self.put_back(token);
            parents.push(self.source()?);
            let mut token = self.token()?;
            while token != RBrack {
                self.put_back(token);
                self.expect(&Comma)?;
                parents.push(self.source()?);
                token = self.token()?;
            }
        }

        self.expect(&RParen)?;
        Ok(Source::Inference(rule, parents))
    }

    fn dag_source(&mut self) -> Result<Source, (Position, Error)> {
        let name = self.name()?;
        Ok(Source::Dag(name))
    }

    fn cnf(&mut self) -> Result<Statement, (Position, Error)> {
        self.expect(&LParen)?;
        let name = self.name()?;
        self.expect(&Comma)?;
        let role = self.formula_role()?;
        self.expect(&Comma)?;
        let formula = self.cnf_formula()?;

        let annotations = match self.token()? {
            Comma => Some(self.annotations()?),
            t => {
                self.put_back(t);
                None
            }
        };
        self.expect(&RParen)?;
        self.expect(&Period)?;

        Ok(Statement::Cnf(name, role, formula, annotations))
    }

    fn cnf_formula(&mut self) -> Result<CnfFormula, (Position, Error)> {
        let bracketed = match self.token()? {
            LParen => true,
            t => {
                self.put_back(t);
                false
            }
        };
        let mut literals = vec![self.cnf_literal()?];

        let mut token = self.token()?;
        while token == Pipe {
            literals.push(self.cnf_literal()?);
            token = self.token()?;
        }
        self.put_back(token);

        if bracketed {
            self.expect(&RParen)?;
        }

        Ok(CnfFormula(literals))
    }

    fn cnf_literal(&mut self) -> Result<CnfLiteral, (Position, Error)> {
        use syntax::CnfLiteral::*;
        match self.token()? {
            Tilde => {
                let fof = self.fof_atomic_formula()?;
                Ok(NegatedLiteral(fof))
            }
            t => {
                self.put_back(t);
                let fof = self.fof_atomic_formula()?;
                Ok(Literal(fof))
            }
        }
    }

    fn fof(&mut self) -> Result<Statement, (Position, Error)> {
        self.expect(&LParen)?;
        let name = self.name()?;
        self.expect(&Comma)?;
        let role = self.formula_role()?;
        self.expect(&Comma)?;
        let formula = self.fof_formula()?;

        let annotations = match self.token()? {
            Comma => Some(self.annotations()?),
            t => {
                self.put_back(t);
                None
            }
        };
        self.expect(&RParen)?;
        self.expect(&Period)?;

        Ok(Statement::Fof(name, role, formula, annotations))
    }

    fn formula_role(&mut self) -> Result<FormulaRole, (Position, Error)> {
        use syntax::FormulaRole::*;
        match self.token()? {
            Lower(role) => match role.as_ref() {
                "axiom" => Ok(Axiom),
                "hypothesis" => Ok(Hypothesis),
                "definition" => Ok(Definition),
                "assumption" => Ok(Assumption),
                "lemma" => Ok(Lemma),
                "theorem" => Ok(Theorem),
                "corollary" => Ok(Corollary),
                "conjecture" => Ok(Conjecture),
                "negated_conjecture" => Ok(NegatedConjecture),
                "plain" => Ok(Plain),
                "unknown" => Ok(Unknown),
                _ => self.error(UnknownRole(role)),
            },
            t => self.error(UnexpectedToken(t)),
        }
    }

    fn fof_formula(&mut self) -> Result<Box<FofFormula>, (Position, Error)> {
        self.fof_logic_formula()
    }

    fn fof_logic_formula(&mut self) -> Result<Box<FofFormula>, (Position, Error)> {
        let first = match self.token()? {
            Tilde => {
                self.put_back(Tilde);
                self.fof_unary_formula()?
            }
            t => {
                self.put_back(t);
                self.fof_unitary_formula()?
            }
        };

        let token = self.token()?;
        match token {
            Ampersand | Pipe => {
                self.put_back(token);
                self.fof_binary_assoc(first)
            }
            LeftArrow | RightArrow | BothArrow | TildeBothArrow | TildePipe | TildeAmpersand => {
                self.put_back(token);
                self.fof_binary_nonassoc(first)
            }
            _ => {
                self.put_back(token);
                Ok(first)
            }
        }
    }

    fn fof_binary_nonassoc(
        &mut self,
        left: Box<FofFormula>,
    ) -> Result<Box<FofFormula>, (Position, Error)> {
        use syntax::FofNonAssocConnective::*;
        let op = match self.token()? {
            LeftArrow => RLImplies,
            RightArrow => LRImplies,
            BothArrow => Equivalent,
            TildeBothArrow => NotEquivalent,
            TildePipe => NotOr,
            TildeAmpersand => NotAnd,
            _ => unreachable!(),
        };
        let right = self.fof_unit_formula()?;
        Ok(Box::new(NonAssoc(op, left, right)))
    }

    fn fof_binary_assoc(
        &mut self,
        first: Box<FofFormula>,
    ) -> Result<Box<FofFormula>, (Position, Error)> {
        let op = self.token()?;
        use syntax::FofAssocConnective::*;
        let constructor = match op {
            Ampersand => And,
            Pipe => Or,
            _ => unreachable!(),
        };
        let mut children = vec![first, self.fof_unit_formula()?];

        let mut token = self.token()?;
        while token == op {
            children.push(self.fof_unit_formula()?);
            token = self.token()?;
        }
        self.put_back(token);

        Ok(Box::new(Assoc(constructor, children)))
    }

    fn fof_unary_formula(&mut self) -> Result<Box<FofFormula>, (Position, Error)> {
        use syntax::FofUnaryConnective::*;
        match self.token()? {
            Tilde => Ok(Box::new(Unary(Not, self.fof_unit_formula()?))),
            _ => unreachable!(),
        }
    }

    fn fof_unitary_formula(&mut self) -> Result<Box<FofFormula>, (Position, Error)> {
        let token = self.token()?;
        match token {
            LParen => {
                let f = self.fof_logic_formula()?;
                self.expect(&RParen)?;
                Ok(f)
            }
            Lower(_) | SingleQuoted(_) | DoubleQuoted(_) | Integer(_) | Upper(_) | Defined(_) => {
                self.put_back(token);
                self.fof_atomic_formula()
            }
            Exclamation | Question => {
                self.put_back(token);
                self.fof_quantified_formula()
            }
            _ => self.error(UnexpectedToken(token)),
        }
    }

    fn fof_unit_formula(&mut self) -> Result<Box<FofFormula>, (Position, Error)> {
        match self.token()? {
            Tilde => {
                self.put_back(Tilde);
                self.fof_unary_formula()
            }
            t => {
                self.put_back(t);
                self.fof_unitary_formula()
            }
        }
    }

    fn fof_quantified_formula(&mut self) -> Result<Box<FofFormula>, (Position, Error)> {
        use syntax::FofQuantifier::*;
        let quantifier = match self.token()? {
            Exclamation => Forall,
            Question => Exists,
            _ => unreachable!(),
        };
        self.expect(&LBrack)?;

        let mut bound = vec![self.fof_bound()?];
        let mut token = self.token()?;
        while token == Comma {
            bound.push(self.fof_bound()?);
            token = self.token()?;
        }
        self.put_back(token);
        self.expect(&RBrack)?;
        self.expect(&Colon)?;

        let formula = self.fof_unit_formula()?;
        Ok(Box::new(Quantified(quantifier, bound, formula)))
    }

    fn fof_atomic_formula(&mut self) -> Result<Box<FofFormula>, (Position, Error)> {
        let token = self.token()?;
        match token {
            // either a predicate or an equality
            Lower(_) | SingleQuoted(_) => {
                self.put_back(token);
                let term = self.fof_term()?;
                let token = self.token()?;
                match token {
                    Equals | NotEquals => {
                        self.put_back(token);
                        self.fof_defined_infix_formula(term)
                    }
                    _ => {
                        self.put_back(token);
                        self.fof_plain_atomic_formula(*term)
                    }
                }
            }
            // definitely an equality
            Upper(_) | Integer(_) | DoubleQuoted(_) => {
                self.put_back(token);
                let term = self.fof_term()?;
                let token = self.token()?;
                match token {
                    Equals | NotEquals => {
                        self.put_back(token);
                        self.fof_defined_infix_formula(term)
                    }
                    _ => self.error(UnexpectedToken(token)),
                }
            }
            // definitely defined predicate
            Defined(_) => {
                self.put_back(token);
                self.fof_defined_atomic_formula()
            }
            _ => unreachable!(),
        }
    }

    fn fof_plain_atomic_formula(
        &mut self,
        term: FofTerm,
    ) -> Result<Box<FofFormula>, (Position, Error)> {
        Ok(match term {
            Functor(name, args) => Box::new(Predicate(name, args)),
            _ => unreachable!(),
        })
    }

    fn fof_defined_infix_formula(
        &mut self,
        left: Box<FofTerm>,
    ) -> Result<Box<FofFormula>, (Position, Error)> {
        use syntax::InfixEquality::*;
        let operator = match self.token()? {
            Equals => Equal,
            NotEquals => NotEqual,
            _ => unreachable!(),
        };
        let right = self.fof_term()?;
        Ok(Box::new(Infix(operator, left, right)))
    }

    fn fof_defined_atomic_formula(&mut self) -> Result<Box<FofFormula>, (Position, Error)> {
        match self.token()? {
            Defined(name) => match name.as_ref() {
                "true" => Ok(Box::new(Boolean(true))),
                "false" => Ok(Box::new(Boolean(false))),
                _ => self.error(UnknownDefined(name)),
            },
            _ => unreachable!(),
        }
    }

    fn fof_term(&mut self) -> Result<Box<FofTerm>, (Position, Error)> {
        let token = self.token()?;
        match token {
            Lower(_) | SingleQuoted(_) | DoubleQuoted(_) => {
                self.put_back(token);
                self.fof_function_term()
            }
            Upper(_) => {
                self.put_back(token);
                self.fof_variable()
            }
            _ => self.error(UnexpectedToken(token)),
        }
    }

    fn fof_function_term(&mut self) -> Result<Box<FofTerm>, (Position, Error)> {
        let token = self.token()?;
        match token {
            Lower(_) | SingleQuoted(_) => {
                self.put_back(token);
                self.fof_plain_term()
            }
            DoubleQuoted(_) => {
                self.put_back(token);
                self.fof_defined_term()
            }
            _ => unreachable!(),
        }
    }

    fn fof_plain_term(&mut self) -> Result<Box<FofTerm>, (Position, Error)> {
        let name = self.atomic_word()?;
        let token = self.token()?;
        match token {
            LParen => {
                let args = self.fof_arguments()?;
                self.expect(&RParen)?;
                Ok(Box::new(Functor(name, args)))
            }
            _ => {
                self.put_back(token);
                Ok(Box::new(Functor(name, vec![])))
            }
        }
    }

    fn fof_defined_term(&mut self) -> Result<Box<FofTerm>, (Position, Error)> {
        match self.token()? {
            DoubleQuoted(name) => Ok(Box::new(DistinctObject(name))),
            _ => unreachable!(),
        }
    }

    fn fof_variable(&mut self) -> Result<Box<FofTerm>, (Position, Error)> {
        Ok(Box::new(Variable(self.fof_bound()?)))
    }

    fn fof_arguments(&mut self) -> Result<Vec<Box<FofTerm>>, (Position, Error)> {
        let mut args = vec![self.fof_term()?];
        let mut token = self.token()?;
        while token == Comma {
            args.push(self.fof_term()?);
            token = self.token()?;
        }
        self.put_back(token);
        Ok(args)
    }

    fn fof_bound(&mut self) -> Result<String, (Position, Error)> {
        match self.token()? {
            Upper(x) => Ok(x),
            token => self.error(UnexpectedToken(token)),
        }
    }

    fn name_list(&mut self) -> Result<Vec<Name>, (Position, Error)> {
        self.expect(&LBrack)?;
        let mut names = vec![self.name()?];

        loop {
            match self.token()? {
                Comma => {
                    names.push(self.name()?);
                }
                RBrack => {
                    break;
                }
                t => {
                    return self.error(UnexpectedToken(t));
                }
            }
        }

        Ok(names)
    }

    fn name(&mut self) -> Result<Name, (Position, Error)> {
        match self.token()? {
            Lower(w) => Ok(Name::Atomic(w)),
            SingleQuoted(q) => Ok(Name::Atomic(q)),
            Integer(i) => Ok(Name::Integer(i)),
            t => self.error(UnexpectedToken(t)),
        }
    }

    fn atomic_word(&mut self) -> Result<Name, (Position, Error)> {
        match self.token()? {
            Lower(w) => Ok(Name::Atomic(w)),
            SingleQuoted(q) => Ok(Name::Atomic(q)),
            t => self.error(UnexpectedToken(t)),
        }
    }
}

impl<T: Iterator<Item = Result<(Position, Token), (Position, Error)>>> Iterator for Parser<T> {
    type Item = Result<Statement, (Position, Error)>;

    fn next(&mut self) -> Option<Self::Item> {
        assert!(!self.error, "parser used in error state");
        match self.statement() {
            Ok(Some(s)) => Some(Ok(s)),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}
