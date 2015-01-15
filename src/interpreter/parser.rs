#![macro_use]

use super::CompileError;
use super::lexer::{self, Token, TokenSlice, SourceToken};
use super::scope::CowScope;
use std::iter;
use std::slice;

pub trait Parser<'a> {
	fn parse(tokens: &'a TokenSlice, scope: CowScope<'a>) -> Result<Self, CompileError>;
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
					$ty => Ok(&t.token),
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
					$ty => Ok(&t.token),
					_ => Err(())
				}
			},
			None => Err(())
		}
	);
);

// Advances an iterator to the matching parenthesis
// Assumes the first opening paren was already consumed
pub fn match_paren<'a> (iter: &mut iter::Enumerate<slice::Iter<'a, SourceToken>>,
						open: lexer::Token, close: lexer::Token) -> Result<(), CompileError> {
	let mut depth = 1i32;
	while depth > 0 {
		let next = iter.next();
		match next.map(|(_, x)| &x.token) {
			Some(x) if *x == open => {
				depth += 1;
			}
			Some(x) if *x == close => {
				depth -= 1;
			}
			None => {
				return Err(CompileError::new(format!("expected `{}`, got EOF", open)));
			}
			_ => { }
		}
	}
	Ok(())
}
