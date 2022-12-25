extern crate regex;

use crate::value::{value, Float, Integer, List, ValueRef};
use regex::Regex;
use std::collections::VecDeque;
use std::io::{BufRead, BufReader, Read};
use std::iter::Peekable;

#[derive(Debug)]
pub enum Error {
    Unexpected(String),
    Unclosed(ValueRef),
    Syntax(String),
    Io(std::io::Error),
}

pub type ParseResult = Result<ValueRef, Error>;

struct Tokenizer<R> {
    regex: Regex,
    reader: BufReader<R>,
    buf: VecDeque<String>,
}

impl<R: Read> Tokenizer<R> {
    fn new(source: R) -> Self {
        let regex = Regex::new(r"(\()|(\))|(')|([^\s()']+)").unwrap();
        let reader = BufReader::new(source);
        let buf = VecDeque::new();

        Self { regex, reader, buf }
    }

    fn fetch(&mut self) -> Result<(), Error> {
        while self.buf.is_empty() {
            let mut line = String::new();
            let len = self.reader.read_line(&mut line).map_err(Error::Io)?;
            self.buf
                .extend(self.regex.find_iter(&line).map(|s| s.as_str().to_owned()));
            if len == 0 {
                break;
            }
        }

        Ok(())
    }
}

impl<R: Read> Iterator for Tokenizer<R> {
    type Item = Result<String, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.fetch() {
            Ok(()) => self.buf.pop_front().map_or_else(|| None, |s| Some(Ok(s))),
            Err(e) => Some(Err(e)),
        }
    }
}

pub struct Parser<R: Read> {
    tokens: Peekable<Tokenizer<R>>,
}

impl<R: Read> Parser<R> {
    pub fn new(source: R) -> Result<Self, Error> {
        let tokens = Tokenizer::new(source).peekable();
        Ok(Self { tokens })
    }

    fn parse(&mut self, token: String) -> ParseResult {
        Ok(match token.as_str() {
            "nil" => value!(),
            "(" => {
                let mut list = List::new();
                loop {
                    let peek = self.tokens.peek();
                    if let Some(res) = peek {
                        if let Ok(s) = res {
                            match s.as_str() {
                                ")" => {
                                    let _ = self.tokens.next(); // drop
                                    break value!(list);
                                }
                                // if there is a token left next won't be None
                                _ => list.push(self.next().unwrap()?),
                            }
                        } else {
                            return Err(self.tokens.next().unwrap().unwrap_err());
                        }
                    } else {
                        return Err(Error::Unclosed(value!(list)));
                    }
                }
            }
            ")" => return Err(Error::Unexpected(token)),
            "'" => value!(
                value!("quote"),
                self.next().ok_or_else(|| Error::Syntax(token))??
            ),
            s => {
                if let Ok(i) = s.parse::<Integer>() {
                    value!(i)
                } else if let Ok(f) = s.parse::<Float>() {
                    value!(f)
                } else {
                    value!(token)
                }
            }
        })
    }
}

impl<R: Read> Iterator for Parser<R> {
    type Item = ParseResult;

    fn next(&mut self) -> Option<Self::Item> {
        let front = self.tokens.next()?;
        Some(match front {
            Ok(s) => self.parse(s),
            Err(e) => Err(e),
        })
    }
}
