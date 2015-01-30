use std::fmt;
use std::borrow::Cow;
use std::string::CowString;

pub mod parser;
pub mod function;
pub mod functioncall;
pub mod functiondef;
pub mod expr;
pub mod lexer;
pub mod scope;
pub mod identifier;

#[cfg(test)]
mod test;

pub struct Program<'a> {
	id_map: identifier::IdMap<'a>,
	scope: scope::Scope<'a>,
}

impl<'a> Program<'a> {
	pub fn compile(source: &'a str) -> Result<Program<'a>, CompileError> {
		let id_map = identifier::IdMap::new();
		let mut scope = scope::Scope::new();
		let tokens = try!(lexer::lex(source, &id_map));
		unimplemented!();
	}
}

#[derive(Clone, Copy)]
pub struct SourcePos {
	pub line: usize,
	pub col: usize,
}

impl fmt::Display for SourcePos {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}:{}", self.line, self.col)
	}
}

pub struct CompileError {
	pub msg: CowString<'static>,
	pub pos: Option<SourcePos>,
}

impl CompileError {
	pub fn new(msg: String) -> CompileError {
		CompileError {
			msg: Cow::Owned(msg),
			pos: None,
		}
	}

	pub fn new_static(msg: &'static str) -> CompileError {
		CompileError {
			msg: Cow::Borrowed(msg),
			pos: None,
		}
	}

	pub fn with_pos(self, pos: SourcePos) -> CompileError {
		CompileError {
			msg: self.msg,
			pos: Some(pos),
		}
	}
}

impl fmt::Display for CompileError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.pos {
			Some(pos) => write!(f, "{} :: {}", pos, self.msg),
			None => write!(f, "{}", self.msg),
		}
	}
}

fn is_truthy(v: f32) -> bool {
	v > 0_f32
}

fn from_bool(v: bool) -> f32 {
	if v { TRUE } else { FALSE }
}

static TRUE: f32 = 1_f32;
static FALSE: f32 = -1_f32;
