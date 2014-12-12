use regex::Regex;
use super::{CompileError, SourcePos};

/// The various types that a token can be
#[deriving(Show, Clone, PartialEq)]
pub enum Token<'a> {
	Ident(&'a str),
	Const(f32),
	Operator(&'a str),
	Newline,
	Paren(char),
	Colon,
	Equals,
	Period,
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
#[deriving(Show, Clone)]
pub struct SourceToken<'a> {
	pub token: Token<'a>,
	pub pos: SourcePos,
}

static IDENT_REGEX: Regex = regex!(r"[a-zA-Z_~']+[a-zA-Z_~0-9']*");
static WHITESPACE_REGEX: Regex = regex!(r"[ \t]+");
static NEWLINE_REGEX: Regex = regex!(r"[\n]+");
static CONST_REGEX: Regex = regex!(r"([0-9]+\.?[0-9]*|[0-9]*\.?[0-9]+)([eE]-?[0-9]+)?");
static OPERATOR_REGEX: Regex = regex!(r"\^\^|>=|<=|~=|[\+\*/\^><!%-]|&&|\|\||==|!=");
static PAREN_REGEX: Regex = regex!(r"[\(\)\{\}\]\[]");
static COLON_REGEX: Regex = regex!(r":");
static EQUALS_REGEX: Regex = regex!(r"=");
static PERIOD_REGEX: Regex = regex!(r"\.");
static COMMENT_REGEX: Regex = regex!(r"//.*");

pub type TokenList<'a> = Vec<SourceToken<'a>>;
pub type TokenSlice<'a> = [SourceToken<'a>];

pub fn lex<'a>(string: &'a str) -> Result<TokenList<'a>, CompileError> {
	let mut walk = string;
	let mut tokens = Vec::new();
	let mut pos = SourcePos { line: 1u, col: 1u };

	while walk.len() > 0 {
		// Strip comments
		let comment_match = COMMENT_REGEX.find(walk);
		if let Some((0, x)) = comment_match {
			walk = walk[x..];
			continue;
		}

		// Strip whitespace
		let whitespace_match = WHITESPACE_REGEX.find(walk);
		if let Some((0, x)) = whitespace_match {
			walk = walk[x..];
			pos.col += x;
			continue;
		}

		// Add operators as strings
		let operator_match = OPERATOR_REGEX.find(walk);
		if let Some((0, x)) = operator_match {
			tokens.push(Token::Operator(walk[0..x]).with_pos(pos));
			walk = walk[x..];
			pos.col += x;
			continue;
		}

		// Add identifiers as strings
		let ident_match = IDENT_REGEX.find(walk);
		if let Some((0, x)) = ident_match {
			tokens.push(Token::Ident(walk[0..x]).with_pos(pos));
			walk = walk[x..];
			pos.col += x;
			continue;
		}

		// Add parens as chars
		let paren_match = PAREN_REGEX.find(walk);
		if let Some((0, x)) = paren_match {
			assert!(x == 1);
			tokens.push(Token::Paren(walk.char_at(0)).with_pos(pos));
			walk = walk[x..];
			pos.col += x;
			continue;
		}

		// Add numerical literals as f32
		let const_match = CONST_REGEX.find(walk);
		if let Some((0, x)) = const_match {
			if let Some(v) = from_str::<f32>(walk[0..x]) {
				tokens.push(Token::Const(v).with_pos(pos));
				walk = walk[x..];
				pos.col += x;
				continue;
			} else {
				panic!("internal error: error parsing numerical constant, the lexer is probably broken");
			}
		}

		// Add colon token
		let colon_match = COLON_REGEX.find(walk);
		if let Some((0, x)) = colon_match {
			tokens.push(SourceToken { token: Token::Colon, pos: pos });
			walk = walk[x..];
			pos.col += x;
			continue;
		}

		// Add equals token
		let equals_match = EQUALS_REGEX.find(walk);
		if let Some((0, x)) = equals_match {
			tokens.push(SourceToken { token: Token::Equals, pos: pos });
			walk = walk[x..];
			pos.col += x;
			continue;
		}

		// Add period token
		let period_match = PERIOD_REGEX.find(walk);
		if let Some((0, x)) = period_match {
			tokens.push(SourceToken { token: Token::Period, pos: pos });
			walk = walk[x..];
			pos.col += x;
			continue;
		}

		// Add newline tokens
		let newline_match = NEWLINE_REGEX.find(walk);
		if let Some((0, x)) = newline_match {
			tokens.push(Token::Newline.with_pos(pos));
			walk = walk[x..];
			pos.line += x;
			pos.col = 1;
			continue;
		}

		// If none of the checks above found a token, then it's not supported.
		return Err(CompileError::new(format!("unrecognized token `{}`", walk.char_at(0))).with_pos(pos));
	}
	Ok(tokens)
}
