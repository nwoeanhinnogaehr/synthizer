use std::fmt;

pub mod lexer;
pub mod expr;
pub mod scope;
pub mod parser;

#[deriving(Clone)]
pub struct SourcePos {
	pub line: uint,
	pub col: uint,
}

impl fmt::Show for SourcePos {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}:{}", self.line, self.col)
	}
}

pub struct CompileError {
	pub msg: String,
	pub pos: Option<SourcePos>,
}

impl fmt::Show for CompileError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.pos {
			Some(pos) => write!(f, "{} :: {}", pos, self.msg),
			None => write!(f, "{}", self.msg),
		}
	}
}
