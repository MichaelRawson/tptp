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
pub struct Parser<T: Iterator<Item = Result<(Position, Token), Error>>> {
    stream: T,
    read: Token,
    error: bool,
    position: Position,
}

impl<T: Iterator<Item = Result<(Position, Token), Error>>> Parser<T> {
    /// Create a new stream by wrapping another
    pub fn new(stream: T) -> Self {
        Parser {
            stream,
            read: EOF,
            error: false,
            position: Position::default(),
        }
    }

    fn token(&mut self) -> Result<Token, Error> {
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

    fn error<E: Into<Error>, X>(&mut self, error: E) -> Result<X, Error> {
        self.error = true;
        Err(error.into())
    }

    fn expect(&mut self, expected: &Token) -> Result<(), Error> {
        let token = self.token()?;
        if &token == expected {
            Ok(())
        } else {
            self.error(UnexpectedToken(token))
        }
    }

    fn statement(&mut self) -> Result<Option<Statement>, Error> {
        let token = self.token()?;
        match token {
            EOF => Ok(None),
            Lower(w) => match w.as_ref() {
                "fof" => self.fof(),
                "include" => self.include(),
                _ => self.error(UnsupportedDialect(w)),
            }.map(Some),
            t => self.error(UnexpectedToken(t)),
        }
    }

    fn include(&mut self) -> Result<Statement, Error> {
        self.expect(&LParen)?;
        let name = match self.token()? {
            SingleQuoted(name) => Ok(name),
            token => self.error(UnexpectedToken(token)),
        }?;
        self.expect(&RParen)?;
        self.expect(&Period)?;

        Ok(Statement::Include(name))
    }

    fn fof(&mut self) -> Result<Statement, Error> {
        self.expect(&LParen)?;
        let name = self.name()?;
        self.expect(&Comma)?;
        let role = self.formula_role()?;
        self.expect(&Comma)?;
        let formula = self.fof_formula()?;
        self.expect(&RParen)?;
        self.expect(&Period)?;

        Ok(Statement::Fof(name, role, formula))
    }

    fn formula_role(&mut self) -> Result<FormulaRole, Error> {
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

    fn fof_formula(&mut self) -> Result<Box<FofFormula>, Error> {
        self.fof_logic_formula()
    }

    fn fof_logic_formula(&mut self) -> Result<Box<FofFormula>, Error> {
        let token = self.token()?;
        let first = match token {
            Tilde => {
                self.put_back(token);
                self.fof_unary_formula()
            }
            _ => {
                self.put_back(token);
                self.fof_unitary_formula()
            }
        }?;

        let token = self.token()?;
        match token {
            Ampersand | Pipe => {
                self.put_back(token);
                self.fof_binary_assoc(first)
            }
            LeftArrow | RightArrow | BothArrow | TildeBothArrow => {
                self.put_back(token);
                self.fof_binary_nonassoc(first)
            }
            _ => {
                self.put_back(token);
                Ok(first)
            }
        }
    }

    fn fof_binary_nonassoc(&mut self, left: Box<FofFormula>) -> Result<Box<FofFormula>, Error> {
        let op = self.token()?;
        let right = self.fof_unit_formula()?;

        use syntax::FofNonAssocOp::*;
        Ok(Box::new(match op {
            LeftArrow => NonAssoc(Implies, right, left),
            RightArrow => NonAssoc(Implies, left, right),
            BothArrow => NonAssoc(Equivalent, left, right),
            TildeBothArrow => NonAssoc(NotEquivalent, left, right),
            _ => unreachable!(),
        }))
    }

    fn fof_binary_assoc(&mut self, first: Box<FofFormula>) -> Result<Box<FofFormula>, Error> {
        let op = self.token()?;
        use syntax::FofAssocOp::*;
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

    fn fof_unary_formula(&mut self) -> Result<Box<FofFormula>, Error> {
        use syntax::FofUnaryOp::*;
        match self.token()? {
            Tilde => Ok(Box::new(Unary(Not, self.fof_unit_formula()?))),
            _ => unreachable!(),
        }
    }

    fn fof_unitary_formula(&mut self) -> Result<Box<FofFormula>, Error> {
        let token = self.token()?;
        match token {
            LParen => {
                let f = self.fof_logic_formula()?;
                self.expect(&RParen)?;
                Ok(f)
            }
            Lower(_) | SingleQuoted(_) | Integer(_) | Upper(_) | Defined(_) => {
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

    fn fof_unit_formula(&mut self) -> Result<Box<FofFormula>, Error> {
        let token = self.token()?;
        match token {
            Tilde => {
                self.put_back(token);
                self.fof_unary_formula()
            }
            _ => {
                self.put_back(token);
                self.fof_unitary_formula()
            }
        }
    }

    fn fof_quantified_formula(&mut self) -> Result<Box<FofFormula>, Error> {
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

    fn fof_atomic_formula(&mut self) -> Result<Box<FofFormula>, Error> {
        let token = self.token()?;
        match token {
            Lower(_) | SingleQuoted(_) | Integer(_) => {
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
            Upper(_) => {
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
            Defined(_) => {
                self.put_back(token);
                self.fof_defined_atomic_formula()
            }
            _ => unreachable!(),
        }
    }

    fn fof_plain_atomic_formula(&mut self, term: FofTerm) -> Result<Box<FofFormula>, Error> {
        Ok(match term {
            Functor(name, args) => Box::new(Predicate(name, args)),
            _ => unreachable!(),
        })
    }

    fn fof_defined_infix_formula(&mut self, left: Box<FofTerm>) -> Result<Box<FofFormula>, Error> {
        use syntax::FofInfixOp::*;
        let operator = match self.token()? {
            Equals => Equal,
            NotEquals => NotEqual,
            _ => unreachable!(),
        };
        let right = self.fof_term()?;
        Ok(Box::new(Infix(operator, left, right)))
    }

    fn fof_defined_atomic_formula(&mut self) -> Result<Box<FofFormula>, Error> {
        match self.token()? {
            Defined(name) => match name.as_ref() {
                "true" => Ok(Box::new(Boolean(true))),
                "false" => Ok(Box::new(Boolean(false))),
                _ => self.error(UnknownDefined(name)),
            },
            _ => unreachable!(),
        }
    }

    fn fof_term(&mut self) -> Result<Box<FofTerm>, Error> {
        let token = self.token()?;
        match token {
            Lower(_) | SingleQuoted(_) | Integer(_) => {
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

    fn fof_function_term(&mut self) -> Result<Box<FofTerm>, Error> {
        self.fof_plain_term()
    }

    fn fof_plain_term(&mut self) -> Result<Box<FofTerm>, Error> {
        let name = self.name()?;
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

    fn fof_variable(&mut self) -> Result<Box<FofTerm>, Error> {
        Ok(Box::new(Variable(self.fof_bound()?)))
    }

    fn fof_arguments(&mut self) -> Result<Vec<Box<FofTerm>>, Error> {
        let mut args = vec![self.fof_term()?];
        let mut token = self.token()?;
        while token == Comma {
            args.push(self.fof_term()?);
            token = self.token()?;
        }
        self.put_back(token);
        Ok(args)
    }

    fn fof_bound(&mut self) -> Result<String, Error> {
        match self.token()? {
            Upper(x) => Ok(x),
            token => self.error(UnexpectedToken(token)),
        }
    }

    fn name(&mut self) -> Result<Name, Error> {
        match self.token()? {
            Lower(w) => Ok(Name::Word(w)),
            SingleQuoted(q) => Ok(Name::Quoted(q)),
            Integer(i) => Ok(Name::Integer(i)),
            t => self.error(UnexpectedToken(t)),
        }
    }
}

impl<T: Iterator<Item = Result<(Position, Token), Error>>> Iterator for Parser<T> {
    type Item = Result<(Position, Statement), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        assert!(!self.error, "parser used in error state");
        match self.statement() {
            Ok(Some(s)) => Some(Ok((self.position, s))),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}
