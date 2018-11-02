use std::io;
use std::mem;
use std::vec::Vec;

use crate::error::Error;
use crate::error::LexicalError::*;
use crate::position::Position;
use crate::token::Token;
use crate::token::Token::*;

#[derive(Debug)]
enum State {
    Ready,
    Equals,
    Tilde,
    Less,
    LessEquals,
    LessTilde,
    Exclamation,
    Lower,
    Upper,
    Integer,
    Defined,
    SingleQuoted,
    SingleQuotedEscape,
    DoubleQuoted,
    DoubleQuotedEscape,
    SingleComment,
    MultiCommentStart,
    MultiComment,
    MultiCommentEnd,
}

/// A stream of tokens, wrapping an underlying byte stream
///
/// Never returns the EOF token.
/// It is an error to continue iterating over a `Lexer` which has previously produced an error.
#[derive(Debug)]
pub struct Lexer<T: Iterator<Item = Result<u8, io::Error>>> {
    stream: T,
    error: bool,
    state: State,
    read: u8,
    buffer: Vec<u8>,
    position: Position,
}

impl<T: Iterator<Item = Result<u8, io::Error>>> Lexer<T> {
    /// Create a new stream by wrapping another
    pub fn new(stream: T) -> Self {
        Lexer {
            stream,
            error: false,
            position: Position::default(),
            read: b'\0',
            buffer: vec![],
            state: State::Ready,
        }
    }

    fn error<E: Into<Error>, X>(&mut self, error: E) -> Result<X, (Position, Error)> {
        self.error = true;
        Err((self.position, error.into()))
    }

    fn byte(&mut self) -> Result<u8, (Position, Error)> {
        let read = self.read;
        if read != b'\0' {
            self.read = b'\0';
            return Ok(read);
        }

        match self.stream.next() {
            Some(Ok(b'\0')) => self.error(UnknownByte(b'\0')),
            Some(Ok(c)) => {
                self.position.update(c);
                Ok(c)
            }
            Some(Err(e)) => self.error(e),
            None => Ok(b'\0'),
        }
    }

    fn put_back(&mut self, byte: u8) {
        self.read = byte;
    }

    fn buffered(&mut self) -> String {
        let buffer = mem::replace(&mut self.buffer, vec![]);
        unsafe { String::from_utf8_unchecked(buffer) }
    }

    fn step(&mut self, byte: u8) -> Result<Option<Token>, (Position, Error)> {
        match self.state {
            State::Ready => match byte {
                b'\0' => Ok(Some(EOF)),
                b' ' | b'\t' | b'\r' | b'\n' => Ok(None),
                b'%' => {
                    self.state = State::SingleComment;
                    Ok(None)
                }
                b'/' => {
                    self.state = State::MultiCommentStart;
                    Ok(None)
                }
                b'(' => Ok(Some(LParen)),
                b')' => Ok(Some(RParen)),
                b'[' => Ok(Some(LBrack)),
                b']' => Ok(Some(RBrack)),
                b',' => Ok(Some(Comma)),
                b'.' => Ok(Some(Period)),
                b':' => Ok(Some(Colon)),
                b'&' => Ok(Some(Ampersand)),
                b'|' => Ok(Some(Pipe)),
                b'~' => {
                    self.state = State::Tilde;
                    Ok(None)
                }
                b'=' => {
                    self.state = State::Equals;
                    Ok(None)
                }
                b'<' => {
                    self.state = State::Less;
                    Ok(None)
                }
                b'?' => Ok(Some(Question)),
                b'!' => {
                    self.state = State::Exclamation;
                    Ok(None)
                }
                b'a'...b'z' => {
                    self.state = State::Lower;
                    self.buffer.push(byte);
                    Ok(None)
                }
                b'A'...b'Z' => {
                    self.state = State::Upper;
                    self.buffer.push(byte);
                    Ok(None)
                }
                b'0'...b'9' => {
                    self.state = State::Integer;
                    self.buffer.push(byte);
                    Ok(None)
                }
                b'$' => {
                    self.state = State::Defined;
                    Ok(None)
                }
                b'\'' => {
                    self.state = State::SingleQuoted;
                    Ok(None)
                }
                b'"' => {
                    self.state = State::DoubleQuoted;
                    Ok(None)
                }
                b => self.error(UnknownByte(b)),
            },
            State::Tilde => match byte {
                b'|' => {
                    self.state = State::Ready;
                    Ok(Some(TildePipe))
                }
                b'&' => {
                    self.state = State::Ready;
                    Ok(Some(TildeAmpersand))
                }
                _ => {
                    self.state = State::Ready;
                    self.put_back(byte);
                    Ok(Some(Tilde))
                }
            },
            State::Equals => match byte {
                b'>' => {
                    self.state = State::Ready;
                    Ok(Some(RightArrow))
                }
                _ => {
                    self.state = State::Ready;
                    self.put_back(byte);
                    Ok(Some(Equals))
                }
            },
            State::Less => match byte {
                b'=' => {
                    self.state = State::LessEquals;
                    Ok(None)
                }
                b'~' => {
                    self.state = State::LessTilde;
                    Ok(None)
                }
                _ => self.error(UnknownOperator("<".into())),
            },
            State::LessEquals => match byte {
                b'>' => {
                    self.state = State::Ready;
                    Ok(Some(BothArrow))
                }
                _ => {
                    self.state = State::Ready;
                    self.put_back(byte);
                    Ok(Some(LeftArrow))
                }
            },
            State::LessTilde => match byte {
                b'>' => {
                    self.state = State::Ready;
                    Ok(Some(TildeBothArrow))
                }
                _ => self.error(UnknownOperator("<~".into())),
            },
            State::Exclamation => match byte {
                b'=' => {
                    self.state = State::Ready;
                    Ok(Some(NotEquals))
                }
                _ => {
                    self.state = State::Ready;
                    self.put_back(byte);
                    Ok(Some(Exclamation))
                }
            },
            State::Lower => match byte {
                b'A'...b'Z' | b'a'...b'z' | b'0'...b'9' | b'_' => {
                    self.buffer.push(byte);
                    Ok(None)
                }
                _ => {
                    self.state = State::Ready;
                    let token = Lower(self.buffered());
                    self.put_back(byte);
                    Ok(Some(token))
                }
            },
            State::Upper => match byte {
                b'A'...b'Z' | b'a'...b'z' | b'0'...b'9' | b'_' => {
                    self.buffer.push(byte);
                    Ok(None)
                }
                _ => {
                    self.state = State::Ready;
                    let token = Upper(self.buffered());
                    self.put_back(byte);
                    Ok(Some(token))
                }
            },
            State::Integer => match byte {
                b'0'...b'9' => {
                    self.buffer.push(byte);
                    Ok(None)
                }
                _ => {
                    self.state = State::Ready;
                    let token = Integer(self.buffered());
                    self.put_back(byte);
                    Ok(Some(token))
                }
            },
            State::Defined => match byte {
                b'A'...b'Z' | b'a'...b'z' | b'0'...b'9' | b'_' => {
                    self.buffer.push(byte);
                    Ok(None)
                }
                _ => {
                    self.state = State::Ready;
                    let token = Defined(self.buffered());
                    self.put_back(byte);
                    Ok(Some(token))
                }
            },
            State::SingleQuoted => match byte {
                b'\0' => self.error(UnclosedQuote),
                32...126 => match byte {
                    b'\'' => {
                        self.state = State::Ready;
                        let token = SingleQuoted(self.buffered());
                        Ok(Some(token))
                    }
                    b'\\' => {
                        self.state = State::SingleQuotedEscape;
                        Ok(None)
                    }
                    _ => {
                        self.buffer.push(byte);
                        Ok(None)
                    }
                },
                _ => self.error(NonPrintable(byte)),
            },
            State::SingleQuotedEscape => match byte {
                b'\'' | b'\\' => {
                    self.buffer.push(byte);
                    self.state = State::SingleQuoted;
                    Ok(None)
                }
                b'\0' => self.error(UnclosedQuote),
                _ => self.error(BadEscape(byte)),
            },
            State::DoubleQuoted => match byte {
                b'\0' => self.error(UnclosedQuote),
                32...126 => match byte {
                    b'"' => {
                        self.state = State::Ready;
                        let token = DoubleQuoted(self.buffered());
                        Ok(Some(token))
                    }
                    b'\\' => {
                        self.state = State::DoubleQuotedEscape;
                        Ok(None)
                    }
                    _ => {
                        self.buffer.push(byte);
                        Ok(None)
                    }
                },
                _ => self.error(NonPrintable(byte)),
            },
            State::DoubleQuotedEscape => match byte {
                b'"' | b'\\' => {
                    self.buffer.push(byte);
                    self.state = State::DoubleQuoted;
                    Ok(None)
                }
                b'\0' => self.error(UnclosedQuote),
                _ => self.error(BadEscape(byte)),
            },
            State::SingleComment => match byte {
                b'\n' | b'\0' => {
                    self.state = State::Ready;
                    Ok(None)
                }
                _ => Ok(None),
            },
            State::MultiCommentStart => match byte {
                b'*' => {
                    self.state = State::MultiComment;
                    Ok(None)
                }
                _ => self.error(UnknownOperator("/".into())),
            },
            State::MultiComment => match byte {
                b'*' => {
                    self.state = State::MultiCommentEnd;
                    Ok(None)
                }
                b'\0' => self.error(UnclosedComment),
                _ => Ok(None),
            },
            State::MultiCommentEnd => match byte {
                b'/' => {
                    self.state = State::Ready;
                    Ok(None)
                }
                _ => {
                    self.state = State::MultiComment;
                    Ok(None)
                }
            },
        }
    }

    fn token(&mut self) -> Result<Token, (Position, Error)> {
        loop {
            let byte = self.byte()?;
            let result = self.step(byte)?;
            if let Some(token) = result {
                return Ok(token);
            }
        }
    }
}

impl<T: Iterator<Item = Result<u8, io::Error>>> Iterator for Lexer<T> {
    type Item = Result<(Position, Token), (Position, Error)>;

    fn next(&mut self) -> Option<Self::Item> {
        assert!(!self.error, "lexer used while in an error state");
        match self.token() {
            Ok(EOF) => None,
            Ok(t) => Some(Ok((self.position, t))),
            Err(e) => Some(Err(e)),
        }
    }
}
