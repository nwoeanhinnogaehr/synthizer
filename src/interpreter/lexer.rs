use regex::Regex;
use super::{CompileError, SourcePos};
use std::fmt;

/// The various types that a token can be
#[derive(Clone, PartialEq)]
pub enum Token<'a> {
	Ident(&'a str),
	Const(f32),
	Operator(&'a str),
	Symbol(char),
	Newline,
}

impl<'a> fmt::String for Token<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Token::*;
		match *self {
			Ident(x) | Operator(x) => write!(f, "{}", x),
			Const(x) => write!(f, "{}", x),
			Symbol(x) => write!(f, "{}", x),
			Newline => write!(f, "\\n")
		}
	}
}

impl<'a> Token<'a> {
	fn with_pos(self, pos: SourcePos) -> SourceToken<'a> {
		SourceToken {
			token: self,
			pos: pos,
		}
	}
}

/// Stores the type and position of a token
#[derive(Clone)]
pub struct SourceToken<'a> {
	pub token: Token<'a>,
	pub pos: SourcePos,
}

impl<'a> fmt::String for SourceToken<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "`{}`;{}", self.token, self.pos)
	}
}

static IDENT_REGEX: Regex = regex!(r"[a-zA-Z_~']+[a-zA-Z_~0-9']*");
static WHITESPACE_REGEX: Regex = regex!(r"[ \t]+");
static NEWLINE_REGEX: Regex = regex!(r"[\n]+");
static CONST_REGEX: Regex = regex!(r"([0-9]+\.?[0-9]*|[0-9]*\.?[0-9]+)([eE]-?[0-9]+)?");
static OPERATOR_REGEX: Regex = regex!(r"\^\^|>=|<=|~=|[\+\*/\^><!%-]|&&|\|\||==|!=");
static SYMBOL_REGEX: Regex = regex!(r"[\.,=:\(\)\{\}\]\[]");
static COMMENT_REGEX: Regex = regex!(r"//.*");

pub type TokenList<'a> = Vec<SourceToken<'a>>;
pub type TokenSlice<'a> = [SourceToken<'a>];

pub fn lex<'a>(string: &'a str) -> Result<TokenList<'a>, CompileError> {
	let mut walk = string;
	let mut tokens = Vec::new();
	let mut pos = SourcePos { line: 1us, col: 1us };

	while walk.len() > 0 {
		// Strip comments
		let comment_match = COMMENT_REGEX.find(walk);
		if let Some((0, x)) = comment_match {
			walk = &walk[x..];
			continue;
		}

		// Strip whitespace
		let whitespace_match = WHITESPACE_REGEX.find(walk);
		if let Some((0, x)) = whitespace_match {
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// Add operators as strings
		let operator_match = OPERATOR_REGEX.find(walk);
		if let Some((0, x)) = operator_match {
			tokens.push(Token::Operator(&walk[0..x]).with_pos(pos));
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// Add identifiers as strings
		let ident_match = IDENT_REGEX.find(walk);
		if let Some((0, x)) = ident_match {
			tokens.push(Token::Ident(&walk[0..x]).with_pos(pos));
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// Add symbols as chars
		let symbol_match = SYMBOL_REGEX.find(walk);
		if let Some((0, x)) = symbol_match {
			assert!(x == 1);
			tokens.push(Token::Symbol(walk.char_at(0)).with_pos(pos));
			walk = &walk[x..];
			pos.col += x;
			continue;
		}

		// Add numerical literals as f32
		let const_match = CONST_REGEX.find(walk);
		if let Some((0, x)) = const_match {
			if let Some(v) = walk[0..x].parse() {
				tokens.push(Token::Const(v).with_pos(pos));
				walk = &walk[x..];
				pos.col += x;
				continue;
			} else {
				panic!("internal error: error parsing numerical constant, the lexer is probably broken");
			}
		}

		// Add newline tokens
		let newline_match = NEWLINE_REGEX.find(walk);
		if let Some((0, x)) = newline_match {
			tokens.push(Token::Newline.with_pos(pos));
			walk = &walk[x..];
			pos.line += x;
			pos.col = 1;
			continue;
		}

		// If none of the checks above found a token, then it's not supported.
		return Err(CompileError::new(format!("unrecognized token `{}`", walk.char_at(0))).with_pos(pos));
	}
	Ok(tokens)
}
