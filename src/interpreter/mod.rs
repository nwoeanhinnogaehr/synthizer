use std::fmt;

pub mod lexer;
pub mod expr;
pub mod scope;
pub mod function;
pub mod sum;

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

fn is_truthy(v: f32) -> bool {
	v > 0_f32
}

fn from_bool(v: bool) -> f32 {
	if v { TRUE } else { FALSE }
}

static TRUE: f32 = 1_f32;
static FALSE: f32 = -1_f32;
