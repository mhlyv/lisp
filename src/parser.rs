extern crate regex;

use crate::value::{value, Float, Integer, List, ValueRef};
use regex::Regex;
use std::collections::VecDeque;
use std::io::{BufRead, BufReader, Read};

#[derive(Debug)]
pub enum Error {
    Unexpected(String),
    Unclosed(ValueRef),
    Syntax(String),
    Io(std::io::Error),
}

pub type ParseResult = Result<ValueRef, Error>;

pub struct Parser {
    tokens: VecDeque<String>,
}

impl Parser {
    pub fn new<R: Read>(source: R) -> Result<Self, Error> {
        let regex = Regex::new(r"(\()|(\))|(;[^\n]*$)|(')|([^\s();']+)").unwrap();
        let source = BufReader::new(source);
        let mut tokens = VecDeque::new();

        for line in source.lines() {
            let line = line.map_err(Error::Io)?;
            tokens.extend(regex.find_iter(&line).map(|s| s.as_str().to_owned()))
        }

        Ok(Self { tokens })
    }

    fn parse(&mut self, token: String) -> ParseResult {
        Ok(match token.as_str() {
            "nil" => value!(),
            "(" => {
                let mut list = List::new();
                loop {
                    match self.tokens.front().map(|s| s.as_str()) {
                        Some(")") => {
                            let _ = self.tokens.pop_front(); // drop
                            break value!(list);
                        }
                        // if there is a token left next won't be None
                        Some(_) => list.push(self.next().unwrap()?),
                        None => return Err(Error::Unclosed(value!(list))),
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

impl Iterator for Parser {
    type Item = ParseResult;

    fn next(&mut self) -> Option<Self::Item> {
        let front = self.tokens.pop_front()?;
        Some(self.parse(front))
    }
}
