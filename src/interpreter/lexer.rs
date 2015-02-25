use regex::Regex;
use super::{CompileError, SourcePos};
use super::identifier::{Identifier, IdMap};
use std::fmt;

/// The various types that a token can be
#[derive(Debug, Copy, PartialEq)]
pub enum Token {
	Ident(Identifier),
	Const(f32),
	Operator(Operator),
	Symbol(Symbol),
}

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Operator {
	Add,
	Sub,
	Mul,
	Div,
	Exp,
	Mod,
	Neg,
	Less,
	Greater,
	Equ,
	NotEqu,
	ApproxEqu,
	Not,
	And,
	Or,
	Xor,
	GreaterEqual,
	LessEqual,
}

impl Operator {
	fn parse(s: &str) -> Option<Operator> {
	use self::Operator::*;
		Some(match s {
			"+" => Add,
			"-" => Sub,
			"*" => Mul,
			"/" => Div,
			"^" => Exp,
			"%" => Mod,
			"==" => Equ,
			"!=" => NotEqu,
			"~=" => ApproxEqu,
			"<" => Less,
			">" => Greater,
			"<=" => LessEqual,
			">=" => GreaterEqual,
			"!" => Not,
			"&&" => And,
			"||" => Or,
			"^^" => Xor,
			_ => return None,
		})
	}
}

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Symbol {
	Period,
	Comma,
	Equals,
	Colon,
	Semicolon,
	QuestionMark,
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	LeftBracket,
	RightBracket,
}

impl Symbol {
	fn parse(s: &str) -> Option<Symbol> {
	use self::Symbol::*;
		Some(match s {
			"." => Period,
			"," => Comma,
			"=" => Equals,
			":" => Colon,
			";" => Semicolon,
			"?" => QuestionMark,
			"(" => LeftParen,
			")" => RightParen,
			"{" => LeftBrace,
			"}" => RightBrace,
			"[" => LeftBracket,
			"]" => RightBracket,
			_ => return None,
		})
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Token::*;
		match *self {
			Ident(x) => write!(f, "Id({})", x),
			Operator(x) => write!(f, "{:?}", x),
			Const(x) => write!(f, "Const({})", x),
			Symbol(x) => write!(f, "{:?}", x),
		}
	}
}

impl Token {
	fn with_pos(self, pos: SourcePos) -> SourceToken {
		SourceToken {
			token: self,
			pos: pos,
		}
	}
}

/// Stores the type and position of a token
#[derive(Copy)]
pub struct SourceToken {
	pub token: Token,
	pub pos: SourcePos,
}

impl fmt::Display for SourceToken {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}\t`{}`", self.pos, self.token)
	}
}

static IDENT_REGEX: Regex = regex!(r"[a-zA-Z_~']+[a-zA-Z_~0-9']*");
static WHITESPACE_REGEX: Regex = regex!(r"[ \t]+");
static CONST_REGEX: Regex = regex!(r"([0-9]+\.?[0-9]*|[0-9]*\.?[0-9]+)([eE]-?[0-9]+)?");
static OPERATOR_REGEX: Regex = regex!(r"\^\^|>=|<=|~=|[\+\*/\^><!%-]|&&|\|\||==|!=");
static SYMBOL_REGEX: Regex = regex!(r"[\.,=:;\?\(\)\{\}\]\[]");
static COMMENT_REGEX: Regex = regex!(r"//.*");
static NEWLINE_REGEX: Regex = regex!(r"[\n\r]");

pub fn lex<'a>(string: &'a str, idmap: &'a IdMap<'a>) -> Result<Vec<SourceToken>, CompileError> {
	let mut walk = string;
	let mut tokens = Vec::new();
	let mut pos = SourcePos { line: 1, col: 1 };

	while walk.len() > 0 {
		// Strip comments
		if let Some((0, x)) = COMMENT_REGEX.find(walk) {
			walk = &walk[x..];
			// TODO does this advance pos correctly?
			continue;
		}

		// Strip whitespace
		if let Some((0, x)) = WHITESPACE_REGEX.find(walk) {
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// Strip newlines
		if let Some((0, x)) = NEWLINE_REGEX.find(walk) {
			walk = &walk[x..];
			pos.line += 1;
            pos.col = 0;
			continue;
		}

		// Add operators
		if let Some((0, x)) = OPERATOR_REGEX.find(walk) {
			let op = Operator::parse(&walk[0..x]).unwrap(); // If this fails either the regex or the parser is wrong.
			tokens.push(Token::Operator(op).with_pos(pos));
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// Add identifiers
		if let Some((0, x)) = IDENT_REGEX.find(walk) {
			tokens.push(Token::Ident(idmap.id(&walk[0..x])).with_pos(pos));
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// Add symbols
		if let Some((0, x)) = SYMBOL_REGEX.find(walk) {
			let sym = Symbol::parse(&walk[0..x]).unwrap(); // If this fails either the regex or the parser is wrong.
			tokens.push(Token::Symbol(sym).with_pos(pos));
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// Add numerical literals as f32 (TODO allow different precision)
		if let Some((0, x)) = CONST_REGEX.find(walk) {
			let v = walk[0..x].parse().unwrap(); // If this fails either the regex or the parser is wrong.
			tokens.push(Token::Const(v).with_pos(pos));
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// If none of the checks above found a token, then it's not supported.
		return Err(CompileError::new(format!("unrecognized token `{}`", walk.char_at(0))).with_pos(pos));
	}
	Ok(tokens)
}
