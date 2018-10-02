use std::iter::Peekable;
use std::vec::Vec;

use super::ast::FofAssocOp::*;
use super::ast::FofFormula::*;
use super::ast::FofNonAssocOp::*;
use super::ast::FofQuantifier::*;
use super::ast::FofTerm::*;
use super::ast::FofUnaryOp::*;
use super::ast::*;
use super::error::Syntactic::*;
use super::error::*;
use super::lexical::Token;
use super::lexical::Token::*;
use super::position::*;

pub struct Parser<T>
where
    T: Iterator<Item = Result<(Position, Token)>>,
{
    stream: Peekable<T>,
    start: Position,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Result<(Position, Token)>>,
{
    pub fn new(stream: T) -> Self {
        let stream = stream.peekable();
        let start = Position::default();
        Parser { stream, start }
    }

    fn record_start(&mut self) {
        match self.stream.peek() {
            Some(Ok((position, _))) => {
                self.start = *position;
            }
            _ => {
                self.start = Position::default();
            }
        }
    }

    fn peek(&mut self) -> Option<Token> {
        match self.stream.peek() {
            Some(Ok((_, token))) => Some(token.clone()),
            _ => None,
        }
    }

    fn shift(&mut self) -> Result<()> {
        match self.stream.next() {
            Some(Err(e)) => Err(e),
            _ => Ok(()),
        }
    }

    fn error<Any>(&self, error: Syntactic) -> Result<Any> {
        Err(Reported::Syntactic(error))
    }

    fn unexpected<Any>(&mut self) -> Result<Any> {
        match self.stream.next() {
            Some(Ok((position, token))) => self.error(UnexpectedToken(position, token.clone())),
            Some(Err(e)) => Err(e),
            None => self.error(UnexpectedEnd),
        }
    }

    fn expect(&mut self, token: &Token) -> Result<()> {
        match self.peek() {
            Some(next) => if token == &next {
                self.shift()
            } else {
                self.unexpected()
            },
            None => self.unexpected(),
        }
    }

    fn peek_or_unexpected(&mut self) -> Result<Token> {
        match self.peek() {
            Some(next) => Ok(next),
            None => self.unexpected(),
        }
    }

    fn peek_position(&mut self) -> Position {
        match self.stream.peek() {
            Some(Ok((position, _))) => *position,
            _ => panic!("position requested when in an error state"),
        }
    }

    fn name(&mut self) -> Result<Name> {
        match self.peek() {
            Some(LowerWord(t)) => {
                self.shift()?;
                Ok(Name::Word(t))
            }
            Some(SingleQuoted(t)) => {
                self.shift()?;
                Ok(Name::Quoted(t))
            }
            Some(Integer(t)) => {
                self.shift()?;
                Ok(Name::Integer(t))
            }
            _ => self.unexpected(),
        }
    }

    fn formula_role(&mut self) -> Result<FormulaRole> {
        use self::FormulaRole::*;
        let result = match self.peek() {
            Some(LowerWord(role)) => match (*role).as_ref() {
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
                _ => {
                    let position = self.peek_position();
                    let role = (*role).clone();
                    let error = UnknownRole(position, role);
                    self.error(error)
                }
            },
            _ => self.unexpected(),
        }?;
        self.shift()?;
        Ok(result)
    }

    fn fof_bound(&mut self) -> Result<Bound> {
        let result = match self.peek_or_unexpected()? {
            UpperWord(x) => Ok(Bound(x.clone())),
            _ => self.unexpected(),
        }?;
        self.shift()?;
        Ok(result)
    }

    fn fof_arguments(&mut self) -> Result<Vec<Box<FofTerm>>> {
        let first = self.fof_term()?;
        let mut args = vec![first];
        while self.peek() == Some(Comma) {
            self.shift()?;
            args.push(self.fof_term()?);
        }
        Ok(args)
    }

    fn fof_variable(&mut self) -> Result<Box<FofTerm>> {
        let bound = self.fof_bound()?;
        Ok(Box::new(Variable(bound)))
    }

    fn fof_plain_term(&mut self) -> Result<Box<FofTerm>> {
        let name = self.name()?;

        match self.peek() {
            Some(LParen) => {
                self.shift()?;
                let args = self.fof_arguments()?;
                self.expect(&RParen)?;
                Ok(Box::new(Functor(name, args)))
            }
            _ => Ok(Box::new(Functor(name, vec![]))),
        }
    }

    fn fof_function_term(&mut self) -> Result<Box<FofTerm>> {
        self.fof_plain_term()
    }

    fn fof_term(&mut self) -> Result<Box<FofTerm>> {
        match self.peek_or_unexpected()? {
            LowerWord(_) | SingleQuoted(_) | Integer(_) => self.fof_function_term(),
            UpperWord(_) => self.fof_variable(),
            _ => self.unexpected(),
        }
    }

    fn fof_defined_atomic_formula(&mut self) -> Result<Box<FofFormula>> {
        let defined = self.peek().expect("called without defined operator");
        let result = match defined {
            Defined(name) => match (*name).as_ref() {
                "true" => Ok(Box::new(Boolean(true))),
                "false" => Ok(Box::new(Boolean(false))),
                _ => {
                    let position = self.peek_position();
                    let defined = (*name).clone();
                    let error = UnknownDefined(position, defined);
                    self.error(error)
                }
            },
            _ => panic!("bad defined term"),
        }?;
        self.shift()?;
        Ok(result)
    }

    fn fof_defined_infix_formula(&mut self, left: Box<FofTerm>) -> Result<Box<FofFormula>> {
        let op = self.peek().expect("called without operator");
        let invert = match op {
            Equals => false,
            NotEquals => true,
            _ => panic!("bad op"),
        };
        self.shift()?;

        let right = self.fof_term()?;
        let equal = Box::new(Equal(left, right));
        let result = if invert {
            Box::new(Unary(Not, equal))
        } else {
            equal
        };
        Ok(result)
    }

    fn fof_plain_atomic_formula(&mut self, term: FofTerm) -> Result<Box<FofFormula>> {
        Ok(match term {
            Functor(name, args) => Box::new(Predicate(name, args)),
            _ => panic!("bad term"),
        })
    }

    fn fof_atomic_formula(&mut self) -> Result<Box<FofFormula>> {
        match self.peek_or_unexpected()? {
            LowerWord(_) | SingleQuoted(_) | Integer(_) => {
                let term = self.fof_term()?;
                match self.peek_or_unexpected()? {
                    Equals | NotEquals => self.fof_defined_infix_formula(term),
                    _ => self.fof_plain_atomic_formula(*term),
                }
            }
            UpperWord(_) => {
                let term = self.fof_term()?;
                self.fof_defined_infix_formula(term)
            }
            Defined(_) => self.fof_defined_atomic_formula(),
            _ => self.unexpected(),
        }
    }

    fn fof_quantified_formula(&mut self) -> Result<Box<FofFormula>> {
        let op = self.peek().expect("called without quantifier");
        let quantifier = match op {
            Exclamation => Forall,
            Question => Exists,
            _ => panic!("bad quantifier"),
        };
        self.shift()?;
        self.expect(&LBrack)?;

        let first = self.fof_bound()?;
        let mut bound = vec![first];
        while self.peek() == Some(Comma) {
            self.shift()?;
            bound.push(self.fof_bound()?);
        }
        self.expect(&RBrack)?;
        self.expect(&Colon)?;

        let formula = self.fof_unit_formula()?;
        Ok(Box::new(Quantified(quantifier, bound, formula)))
    }

    fn fof_unary_formula(&mut self) -> Result<Box<FofFormula>> {
        match self.peek_or_unexpected()? {
            Tilde => {
                self.shift()?;
                let negated = self.fof_unit_formula()?;
                Ok(Box::new(Unary(Not, negated)))
            }
            _ => panic!("called with non-unary operator"),
        }
    }

    fn fof_unitary_formula(&mut self) -> Result<Box<FofFormula>> {
        match self.peek_or_unexpected()? {
            LParen => {
                self.shift()?;
                let bracketed = self.fof_logic_formula()?;
                self.expect(&RParen)?;
                Ok(bracketed)
            }
            LowerWord(_) | SingleQuoted(_) | Integer(_) | UpperWord(_) | Defined(_) => {
                self.fof_atomic_formula()
            }
            Exclamation | Question => self.fof_quantified_formula(),
            _ => self.unexpected(),
        }
    }

    fn fof_unit_formula(&mut self) -> Result<Box<FofFormula>> {
        match self.peek_or_unexpected()? {
            Tilde => self.fof_unary_formula(),
            _ => self.fof_unitary_formula(),
        }
    }

    fn fof_binary_assoc(&mut self, first: Box<FofFormula>) -> Result<Box<FofFormula>> {
        let op = self.peek().expect("called with no operator");
        let assoc_op = match op {
            Ampersand => And,
            Pipe => Or,
            _ => panic!("bad op"),
        };
        let mut children = vec![first];

        while self.peek() == Some(op.clone()) {
            self.shift()?;
            let next = self.fof_unit_formula()?;
            children.push(next);
        }

        Ok(Box::new(Assoc(assoc_op, children)))
    }

    fn fof_binary_nonassoc(&mut self, left: Box<FofFormula>) -> Result<Box<FofFormula>> {
        let op = self.peek().expect("called with no operator");
        self.shift()?;
        let right = self.fof_unit_formula()?;

        Ok(Box::new(match op {
            LeftArrow => NonAssoc(Implies, left, right),
            RightArrow => NonAssoc(Implies, right, left),
            BothArrow => NonAssoc(Equivalent, left, right),
            TildeBothArrow => Unary(Not, Box::new(NonAssoc(Equivalent, left, right))),
            _ => panic!("bad op"),
        }))
    }

    fn fof_logic_formula(&mut self) -> Result<Box<FofFormula>> {
        let first = match self.peek() {
            Some(Tilde) => self.fof_unary_formula(),
            _ => self.fof_unitary_formula(),
        }?;

        match self.peek() {
            Some(t) => match t {
                Ampersand | Pipe => self.fof_binary_assoc(first),
                LeftArrow | RightArrow | BothArrow | TildeBothArrow => {
                    self.fof_binary_nonassoc(first)
                }
                _ => Ok(first),
            },
            _ => Ok(first),
        }
    }

    fn fof_formula(&mut self) -> Result<Box<FofFormula>> {
        self.fof_logic_formula()
    }

    fn fof(&mut self) -> Result<Statement> {
        self.shift()?;
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

    fn include(&mut self) -> Result<Statement> {
        self.shift()?;
        self.expect(&LParen)?;

        let name = self.name()?;
        self.expect(&RParen)?;
        self.expect(&Period)?;

        Ok(Statement::Include(name))
    }

    fn statement(&mut self) -> Result<Option<Statement>> {
        self.record_start();
        match self.peek() {
            Some(LowerWord(word)) => (match (*word).as_ref() {
                "fof" => self.fof(),
                "include" => self.include(),
                _ => self.error(UnsupportedDialect(self.start, (*word).clone())),
            }).map(Some),
            Some(_) => self.unexpected(),
            _ => Ok(None),
        }
    }
}

impl<T> Iterator for Parser<T>
where
    T: Iterator<Item = Result<(Position, Token)>>,
{
    type Item = Result<(Position, Statement)>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.statement() {
            Ok(Some(token)) => Some(Ok((self.start, token))),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}
