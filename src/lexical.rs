use std::cell::RefCell;
use std::iter::Peekable;
use std::rc::Rc;
use std::sync::Arc;

use super::byte::*;
use super::errors::*;
use super::errors::LexicalError::*;
use super::intern::StringCache;
use super::position::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Token {
    LParen,
    RParen,
    LBrack,
    RBrack,
    Comma,
    Period,
    Colon,
    Ampersand,
    Pipe,
    Tilde,
    LeftArrow,
    RightArrow,
    BothArrow,
    TildeBothArrow,
    Equals,
    NotEquals,
    Exclamation,
    Question,
    Defined(Arc<String>),
    UpperWord(Arc<String>),
    LowerWord(Arc<String>),
    SingleQuoted(Arc<String>),
    Integer(Arc<String>)
}

enum Tokenized {
    Next(Token),
    Again,
    End,
}

use self::Tokenized::*;

pub struct Tokenizer<T>
where T: Iterator<Item=Result<(Position, u8)>> {
    stream: Peekable<T>,
    start: Position,
    cache: Rc<RefCell<StringCache>>
}

impl<T> Tokenizer<T>
where T: Iterator<Item=Result<(Position, u8)>> {
    pub fn new(stream: T, cache: Rc<RefCell<StringCache>>) -> Self {
        let stream = stream.peekable();
        let start = Position::default();
        Tokenizer {
            stream,
            start,
            cache
        }
    }

    fn intern(&self, string: String) -> Arc<String> {
        self.cache.borrow_mut().intern(string)
    }

    fn peek(&mut self) -> Option<u8> {
        match self.stream.peek() {
            Some(Ok((_, byte))) => Some(*byte),
            _ => None
        }
    }

    fn shift(&mut self) -> Result<()> {
        match self.stream.next() {
            Some(Err(e)) => Err(e),
            _ => Ok(())
        }
    }

    fn record_start(&mut self) {
        match self.stream.peek() {
            Some(Ok((position, _))) => {
                self.start = *position;
            }
            _ => {}
        }
    }

    fn confirm(&mut self, token: Token) -> Result<Tokenized> {
        self.shift()?;
        Ok(Next(token))
    }

    fn error<Any>(&self, error: LexicalError) -> Result<Any> {
        Err(Error::Lexical(error))
    }

    fn single_comment(&mut self) -> Result<Tokenized> {
        loop {
            self.shift()?;
            match self.peek() {
                Some(b'\n') | None => { return Ok(Again); },
                Some(_) => {},
            }
        }
    }

    fn multi_comment(&mut self) -> Result<Tokenized> {
        loop {
            self.shift()?;
            match self.peek() {
                Some(b'*') => {
                    self.shift()?;
                    if self.peek() == Some(b'/') {
                        self.shift()?;
                        return Ok(Again)
                    }
                },
                Some(_) => {
                    self.shift()?;
                },
                None => {
                    self.shift()?;
                    return self.error(UnclosedComment(self.start))
                }
            }
        }
    }

    fn alphanumeric(&mut self) -> Result<Arc<String>> {
        let mut bytes = vec![];

        loop {
            match self.peek() {
                Some(byte) => if alphanumeric(byte) {
                    self.shift()?;
                    bytes.push(byte);
                } else {
                    break;
                },
                _ => {
                    break;
                }
            }
        }

        Ok(self.intern(String::from_utf8(bytes).unwrap()))
    }

    fn lower_word(&mut self) -> Result<Tokenized> {
        self.alphanumeric().map(|x| Next(Token::LowerWord(x)))
    }

    fn upper_word(&mut self) -> Result<Tokenized> {
        self.alphanumeric().map(|x| Next(Token::UpperWord(x)))
    }

    fn defined(&mut self) -> Result<Tokenized> {
        self.shift()?;
        self.alphanumeric().map(|x| Next(Token::Defined(x)))
    }

    fn single_quoted(&mut self) -> Result<Tokenized> {
        self.shift()?;

        let mut bytes = vec![];
        loop {
            match self.peek() {
                Some(b'\'') => {
                    self.shift()?;
                    let text = self.intern(String::from_utf8(bytes).unwrap());
                    return Ok(Next(Token::SingleQuoted(text)))
                },
                Some(b'\\') => {
                    self.shift()?;
                    match self.peek() {
                        Some(escaped) => {
                            if escaped == b'\'' || escaped == b'\\' {
                                bytes.push(escaped);
                                self.shift()?;
                            }
                            else {
                                return self.error(BadEscape(
                                    self.start,
                                    escaped
                                ));
                            }
                        },
                        None => {
                            self.shift()?;
                            return self.error(UnclosedQuote(self.start));
                        }
                    }
                },
                Some(c) => if printable(c) {
                    self.shift()?;
                    bytes.push(c);
                } else {
                    return self.error(NonPrintable(self.start, c));
                },
                None => {
                    self.shift()?;
                    return self.error(UnclosedQuote(self.start));
                }
            }
        }
    }

    fn integer(&mut self) -> Result<Tokenized> {
        let mut bytes = vec![self.peek().unwrap()];
        self.shift()?;
        loop {
            match self.peek() {
                Some(digit) => if numeric(digit) {
                    bytes.push(digit);
                    self.shift()?;
                } else {
                    break;
                }
                None => {
                    self.shift()?;
                    break;
                }
            }
        }

        let text = self.intern(String::from_utf8(bytes).unwrap());
        Ok(Next(Token::Integer(text)))
    }

    fn tokenize(&mut self) -> Result<Tokenized> {
        self.record_start();
        if let Some(byte) = self.peek() {
            match byte {
                b' ' | b'\t' | b'\r' | b'\n' => {
                    self.shift()?;
                    Ok(Again)
                },
                b'%' => self.single_comment(),
                b'/' => {
                    self.shift()?;
                    match self.peek() {
                        Some(b'*') => self.multi_comment(),
                        _ => self.error(UnknownOperator(self.start))
                    }
                },
                b'(' => self.confirm(Token::LParen),
                b')' => self.confirm(Token::RParen),
                b'[' => self.confirm(Token::LBrack),
                b']' => self.confirm(Token::RBrack),
                b',' => self.confirm(Token::Comma),
                b'.' => self.confirm(Token::Period),
                b':' => self.confirm(Token::Colon),
                b'&' => self.confirm(Token::Ampersand),
                b'|' => self.confirm(Token::Pipe),
                b'~' => self.confirm(Token::Tilde),
                b'?' => self.confirm(Token::Question),
                b'!' => {
                    self.shift()?;
                    match self.peek() {
                        Some(b'=') => self.confirm(Token::NotEquals),
                        _ => Ok(Next(Token::Exclamation))
                    }
                },
                b'=' => {
                    self.shift()?;
                    match self.peek() {
                        Some(b'>') => self.confirm(Token::RightArrow),
                        _ => Ok(Next(Token::Equals))
                    }
                },
                b'<' => {
                    self.shift()?;
                    match self.peek() {
                        Some(b'=') => {
                            self.shift()?;
                            match self.peek() {
                                Some(b'>') => self.confirm(Token::BothArrow),
                                _ => Ok(Next(Token::LeftArrow))
                            }
                        },
                        Some(b'~') => {
                            self.shift()?;
                            match self.peek() {
                                Some(b'>') => self.confirm(Token::TildeBothArrow),
                                Some(_) => self.error(UnknownOperator(self.start)),
                                None => {
                                    self.shift()?;
                                    self.error(UnknownOperator(self.start))
                                }
                            }
                        },
                        _ => self.error(UnknownOperator(self.start))
                    }
                },
                b'$' => self.defined(),
                b'A' ... b'Z' => self.upper_word(),
                b'a' ... b'z' => self.lower_word(),
                b'\'' => self.single_quoted(),
                b'0' ... b'9' => self.integer(),
                other => self.error(UnknownByte(self.start, other))
            }
        }
        else {
            self.shift()?;
            Ok(End)
        }
    }

    fn token(&mut self) -> Result<Option<(Position, Token)>> {
        loop {
            self.record_start();
            let attempt = self.tokenize()?;
            match attempt {
                Next(token) => {
                    return Ok(Some((self.start, token)));
                },
                Again => {
                    continue;
                },
                End => {
                    return Ok(None);
                }
            }
        }
    }
}

impl<T> Iterator for Tokenizer<T>
where T: Iterator<Item=Result<(Position, u8)>> {
    type Item = Result<(Position, Token)>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.token() {
            Ok(Some(t)) => Some(Ok(t)),
            Ok(None) => None,
            Err(e) => Some(Err(e))
        }
    }
}
