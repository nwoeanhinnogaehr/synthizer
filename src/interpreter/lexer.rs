use regex::Regex;

#[deriving(Show)]
pub enum Token<'a> {
	// tokenize gives:
	Ident(&'a str),
	Const(&'a str),
	Operator(&'a str),
	Newline,
	Whitespace(&'a str),
	Paren(&'a str),
	Colon,
	Equals,
	Period,
}

static IDENT_REGEX: Regex = regex!(r"[a-zA-Z_~']+[a-zA-Z_~0-9']*");
static WHITESPACE_REGEX: Regex = regex!(r"[ \t]+");
static NEWLINE_REGEX: Regex = regex!(r"[\n]+");
static CONST_REGEX: Regex = regex!(r"[0-9]+\.?[0-9]*");
static OPERATOR_REGEX: Regex = regex!(r"\^\^|>=|<=|~=|[\+\*/\^><!%-]|&&|\|\||==|!=");
static PAREN_REGEX: Regex = regex!(r"[\(\)\{\}]|\[|\]");
static COLON_REGEX: Regex = regex!(r":");
static EQUALS_REGEX: Regex = regex!(r"=");
static PERIOD_REGEX: Regex = regex!(r"\.");
static COMMENT_REGEX: Regex = regex!(r"//.*");

pub fn tokenize<'a>(s: &'a str) -> Vec<Token<'a>> {
	let mut walk = s;
	let mut tokens = Vec::new();
	let mut lineindex = 1u;
	let mut linenum = 1u;

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
			tokens.push(Operator(walk[0..x]));
			walk = walk[x..];
			found = true;
			lineindex += x;
		} else if let Some((0, x)) = ident_match {
			tokens.push(Ident(walk[0..x]));
			walk = walk[x..];
			found = true;
			lineindex += x;
		} else if let Some((0, x)) = whitespace_match {
			tokens.push(Whitespace(walk[0..x]));
			walk = walk[x..];
			found = true;
			lineindex += x;
		} else if let Some((0, x)) = newline_match {
			tokens.push(Newline);
			walk = walk[x..];
			found = true;
			linenum += 1;
			lineindex = 0;
		} else if let Some((0, x)) = const_match {
			tokens.push(Const(walk[0..x]));
			walk = walk[x..];
			found = true;
			lineindex += x;
		} else if let Some((0, x)) = paren_match {
			tokens.push(Paren(walk[0..x]));
			walk = walk[x..];
			found = true;
			lineindex += x;
		} else if let Some((0, x)) = colon_match {
			tokens.push(Colon);
			walk = walk[x..];
			found = true;
			lineindex += x;
		} else if let Some((0, x)) = equals_match {
			tokens.push(Equals);
			walk = walk[x..];
			found = true;
			lineindex += x;
		} else if let Some((0, x)) = period_match {
			tokens.push(Period);
			walk = walk[x..];
			found = true;
			lineindex += x;
		}


		if !found {
			fail!("unrecognized token at line {}:{}", linenum, lineindex);
		}
	}
	tokens
}

