#![macro_use]

use super::{CompileError, SourcePos};
use super::lexer::{self, Token, SourceToken};
use super::scope::CowScope;

pub trait Parser<'a> {
    fn parse(tokens: TokenStream<'a>, scope: CowScope<'a>) -> Result<Self, CompileError>;
}

#[derive(Copy)]
pub struct TokenStream<'a> {
    tokens: &'a [SourceToken],
    pos: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [SourceToken]) -> TokenStream<'a> {
        TokenStream {
            tokens: tokens,
            pos: 0
        }
    }

    pub fn next(&mut self) -> Option<SourceToken> {
        let res = self.peek(0);
        self.pos += 1;
        res
    }

    pub fn is_empty(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn slice(&self, from: usize, to: usize) -> TokenStream<'a> {
        TokenStream {
            tokens: &self.tokens[from..to],
            pos: 0,
        }
    }

    fn offset(&self, offset: isize) -> usize {
        ((self.pos as isize) + offset) as usize //really?
    }

    pub fn seek(&mut self, offset: isize) {
        self.pos = self.offset(offset);
    }

    pub fn peek(&self, offset: isize) -> Option<SourceToken> {
        if self.pos >= self.tokens.len() {
            None
        } else {
            Some(self.tokens[self.offset(offset)])
        }
    }

    pub fn end_source_pos(&self) -> SourcePos {
        self.tokens[self.tokens.len()-1].pos
    }
}

macro_rules! expect_value(
    ( $token:expr, $ty:path, $err:expr ) => (
        match $token {
            Some(t) => {
                match t.token {
                    $ty(x) => Ok(x),
                    ref x => Err(CompileError::new(format!($err, x)).with_pos(t.pos))
                }
            },
            None => Err(CompileError::new(format!($err, "EOF")))
        }
    );
    ( $token:expr, $ty:path ) => (
        match $token {
            Some(t) => {
                match t.token {
                    $ty(x) => Ok(x),
                    _ => Err(())
                }
            },
            None => Err(())
        }
    );
);

macro_rules! expect(
    ( $token:expr, $ty:pat, $err:expr ) => (
        match $token {
            Some(t) => {
                match t.token {
                    $ty => Ok(t.token),
                    ref x => Err(CompileError::new(format!($err, x)).with_pos(t.pos))
                }
            },
            None => Err(CompileError::new(format!($err, "EOF")))
        }
    );
    ( $token:expr, $ty:pat ) => (
        match $token {
            Some(t) => {
                match t.token {
                    $ty => Ok(t.token),
                    _ => Err(())
                }
            },
            None => Err(())
        }
    );
);

// Advances an iterator to the matching parenthesis
// Expects tokens to be pointing at a token equal to `open`
pub fn match_paren<'a>(tokens: TokenStream, open: lexer::Token, close: lexer::Token) -> Result<TokenStream, CompileError> {
    let mut tokens = tokens;
    let mut depth = 1i32;
    loop {
        match tokens.next().map(|x| x.token) {
            Some(x) if x == open => {
                depth += 1;
            }
            Some(x) if x == close => {
                depth -= 1;
            }
            None => {
                return Err(CompileError::new(format!("expected `{}`, got EOF", close))
                           .with_pos(tokens.end_source_pos()));
            }
            _ => { }
        }
        if depth <= 1 {
            break;
        }
    }
    Ok(tokens)
}
