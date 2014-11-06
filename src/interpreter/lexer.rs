use regex::Regex;
use super::{CompileError, SourcePos};

#[deriving(Show, Clone)]
pub enum TokenType<'a> {
	// tokenize gives:
	Ident(&'a str),
	Const(&'a str),
	Operator(&'a str),
	Newline,
	Paren(&'a str),
	Colon,
	Equals,
	Period,
}

#[deriving(Show, Clone)]
pub struct Token<'a> {
	pub t: TokenType<'a>,
	pub pos: SourcePos,
}

static IDENT_REGEX: Regex = regex!(r"[a-zA-Z_~']+[a-zA-Z_~0-9']*");
static WHITESPACE_REGEX: Regex = regex!(r"[ \t]+");
static NEWLINE_REGEX: Regex = regex!(r"[\n]+");
static CONST_REGEX: Regex = regex!(r"[0-9]+\.?[0-9]*|[0-9]*\.?[0-9]+");
static OPERATOR_REGEX: Regex = regex!(r"\^\^|>=|<=|~=|[\+\*/\^><!%-]|&&|\|\||==|!=");
static PAREN_REGEX: Regex = regex!(r"[\(\)\{\}\]\[]");
static COLON_REGEX: Regex = regex!(r":");
static EQUALS_REGEX: Regex = regex!(r"=");
static PERIOD_REGEX: Regex = regex!(r"\.");
static COMMENT_REGEX: Regex = regex!(r"//.*");

pub fn tokenize<'a>(s: &'a str) -> Result<Vec<Token<'a>>, CompileError> {
	let mut walk = s;
	let mut tokens = Vec::new();
	let mut pos = SourcePos { line: 1u, col: 1u };

	while walk.len() > 0 {
		let mut found = false;

		let ident_match = IDENT_REGEX.find(walk);
		let whitespace_match = WHITESPACE_REGEX.find(walk);
		let newline_match = NEWLINE_REGEX.find(walk);
		let const_match = CONST_REGEX.find(walk);
		let operator_match = OPERATOR_REGEX.find(walk);
		let paren_match = PAREN_REGEX.find(walk);
		let colon_match = COLON_REGEX.find(walk);
		let equals_match = EQUALS_REGEX.find(walk);
		let period_match = PERIOD_REGEX.find(walk);
		let comment_match = COMMENT_REGEX.find(walk);
		if let Some((0, x)) = comment_match { // Strip comments, don't add any tokens
			walk = walk[x..];
			found = true;
		} else if let Some((0, x)) = operator_match {
			tokens.push(Token { t: Operator(walk[0..x]), pos: pos });
			walk = walk[x..];
			found = true;
			pos.col += x;
		} else if let Some((0, x)) = ident_match {
			tokens.push(Token { t: Ident(walk[0..x]), pos: pos });
			walk = walk[x..];
			found = true;
			pos.col += x;
		} else if let Some((0, x)) = whitespace_match {
			walk = walk[x..];
			found = true;
			pos.col += x;
		} else if let Some((0, x)) = newline_match {
			tokens.push(Token { t: Newline, pos: pos });
			walk = walk[x..];
			found = true;
			pos.line += x;
			pos.col = 1;
		} else if let Some((0, x)) = const_match {
			tokens.push(Token { t: Const(walk[0..x]), pos: pos });
			walk = walk[x..];
			found = true;
			pos.col += x;
		} else if let Some((0, x)) = paren_match {
			tokens.push(Token { t: Paren(walk[0..x]), pos: pos });
			walk = walk[x..];
			found = true;
			pos.col += x;
		} else if let Some((0, x)) = colon_match {
			tokens.push(Token { t: Colon, pos: pos });
			walk = walk[x..];
			found = true;
			pos.col += x;
		} else if let Some((0, x)) = equals_match {
			tokens.push(Token { t: Equals, pos: pos });
			walk = walk[x..];
			found = true;
			pos.col += x;
		} else if let Some((0, x)) = period_match {
			tokens.push(Token { t: Period, pos: pos });
			walk = walk[x..];
			found = true;
			pos.col += x;
		}

		if !found {
			return Err(CompileError { msg: format!("unrecognized token `{}`", walk[0..1]), pos: pos});
		}
	}
	Ok(tokens)
}

