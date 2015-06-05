use super::common::Context;
use super::tokens::*;

use regex::Regex;
use std::str::FromStr;

static IDENT_REGEX: Regex = regex!(r"[a-zA-Z_~']+[a-zA-Z_~0-9']*");
static WHITESPACE_REGEX: Regex = regex!(r"[ \t]+");
static CONST_REGEX: Regex = regex!(r"([0-9]+\.?[0-9]*|[0-9]*\.?[0-9]+)([eE]-?[0-9]+)?");
static OPERATOR_REGEX: Regex = regex!(r"\^\^|>=|<=|[\+\*/\^><!%-]|&&|\|\||==|!=");
static SYMBOL_REGEX: Regex = regex!(r"if|else|[\.,=:;\?\(\)\{\}\]\[\\]");
static BOOLEAN_REGEX: Regex = regex!(r"true|false");
static COMMENT_REGEX: Regex = regex!(r"//.*");
static NEWLINE_REGEX: Regex = regex!(r"[\n\r]");

pub fn lex<'a>(ctxt: &'a Context<'a>) {
    let mut walk = &ctxt.source[..];
    let mut tokens = ctxt.tokens.borrow_mut();
    let mut pos = SourcePos::new();

    while walk.len() > 0 {
        // Strip whitespace
        if let Some((0, x)) = WHITESPACE_REGEX.find(walk) {
            walk = &walk[x..];
            pos.add_chars(x);
            continue;
        }

        // Strip comments
        if let Some((0, x)) = COMMENT_REGEX.find(walk) {
            walk = &walk[x..];
            pos.add_chars(x);
            continue;
        }

        // Add operators
        if let Some((0, x)) = OPERATOR_REGEX.find(walk) {
            // If this fails either the regex or the parser is wrong.
            let op = Operator::parse(&walk[0..x]).unwrap();
            tokens.push(Node(Token::Operator(op), pos));
            walk = &walk[x..];
            pos.add_chars(x);
            continue;
        }

        // Add symbols
        if let Some((0, x)) = SYMBOL_REGEX.find(walk) {
            // If this fails either the regex or the parser is wrong.
            let sym = Symbol::parse(&walk[0..x]).unwrap();
            tokens.push(Node(Token::Symbol(sym), pos));
            walk = &walk[x..];
            pos.add_chars(x);
            continue;
        }

        // Add boolean literals
        if let Some((0, x)) = BOOLEAN_REGEX.find(walk) {
            // If this fails either the regex or the parser is wrong.
            let val = bool::from_str(&walk[0..x]).unwrap();
            tokens.push(Node(Token::Boolean(val), pos));
            walk = &walk[x..];
            pos.add_chars(x);
            continue;
        }

        // Add identifiers
        if let Some((0, x)) = IDENT_REGEX.find(walk) {
            let id = ctxt.names.borrow_mut().new_id(&walk[0..x]);
            tokens.push(Node(Token::Ident(id), pos));
            walk = &walk[x..];
            pos.add_chars(x);
            continue;
        }

        // Strip newlines
        if let Some((0, x)) = NEWLINE_REGEX.find(walk) {
            walk = &walk[x..];
            pos.add_line();
            continue;
        }

        if let Some((0, x)) = CONST_REGEX.find(walk) {
            let v = walk[0..x].parse().unwrap(); // If this fails either the regex or the parser is wrong.
            tokens.push(Node(Token::Const(v), pos));
            walk = &walk[x..];
            pos.add_chars(x);
            continue;
        }

        // If none of the checks above found a token, then it's not supported.
        ctxt.emit_error("unrecognized token", pos);
        walk = &walk[1..];
        pos.add_chars(1);
    }
}
