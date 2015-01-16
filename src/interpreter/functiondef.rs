use super::scope::CowScope;
use super::CompileError;
use super::lexer::{Token, TokenSlice, Symbol, Operator};
use super::function::Function;
use super::parser::Parser;
use super::identifier::{IdMap, Identifier};
use std::collections::VecMap;

/// Represents a function definition written in synthizer
#[derive(Show)]
pub struct FunctionDef<'a> {
	block: Block<'a>,
	args: VecMap<Option<f32>>, // Default arguments
	pub ident: Identifier,
}
impl<'a> Parser<'a> for FunctionDef<'a> {
	/// Parse a function definition from a token stream. Scope is used to find function definitions
	fn parse(tokens: &'a TokenSlice, scope: CowScope<'a>) -> Result<FunctionDef<'a>, CompileError> {
		let mut iter = tokens.iter().enumerate();

		let mut args = VecMap::new();

		let fn_ident = try!(expect_value!(iter.next().map(|(_, x)| x), Token::Ident, "expected function name, got `{}`"));
		try!(expect!(iter.next().map(|(_, x)| x), Token::Symbol(Symbol::LeftParen), "expected `(`, got `{}`"));

		loop {
			let next = iter.next().map(|(_, x)| x);
			let pos = next.map(|x| x.pos);
			let arg_ident = try!(expect_value!(next, Token::Ident, "expected argument name, got `{}`"));
			if args.contains_key(&arg_ident) {
				return Err(CompileError::new(format!("argument {} defined twice", arg_ident)).with_pos(pos.unwrap()));
			}

			let next = iter.next().map(|(_, x)| x);
			let (pos, mut token) = (next.map(|x| x.pos), next.map(|x| &x.token));
			if let Some(&Token::Symbol(Symbol::Equals))  = token {
				let nexttoken = iter.next().map(|(_, x)| x);
				let value = if let Ok(_) = expect!(nexttoken, Token::Operator(Operator::Sub)) {
					-try!(expect_value!(iter.next().map(|(_, x)| x), Token::Const, "expected numerical constant, got `{}`"))
				} else {
					try!(expect_value!(nexttoken, Token::Const, "expected numerical constant, got `{}`"))
				};
				args.insert(arg_ident, Some(value));
				token = iter.next().map(|(_, x)| &x.token);
			}
			match token {
				Some(&Token::Symbol(Symbol::Comma)) => {
					if !args.contains_key(&arg_ident) {
						args.insert(arg_ident, None);
					}
				}
				Some(&Token::Symbol(Symbol::RightParen)) => {
					if !args.contains_key(&arg_ident) {
						args.insert(arg_ident, None);
					}
					break;
				}
				Some(token) => return Err(CompileError::new(format!("expected `=`, `,` or `)`, got `{}`", token)).with_pos(pos.unwrap())),
				None => return Err(CompileError::new_static("expected `=`, `,` or `)`, got EOF")),
			}
		}
		try!(expect!(iter.next().map(|(_, x)| x), Token::Symbol(Symbol::Colon), "expected `:` following function argument declaration, got `{}`"));

		let pos = iter.next().map(|(x, _)| x);
		let block = match pos {
			Some(pos) => try!(Parser::parse(&tokens[pos..], scope)),
			None => return Err(CompileError::new_static("expected block, got EOF")),
		};

		Ok(FunctionDef {
			block: block,
			args: args,
			ident: fn_ident,
		})
	}
}
impl<'a> Function for FunctionDef<'a> {
	fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
		unimplemented!();
	}
}

#[derive(Show)]
struct Block<'a> {
	statements: Vec<(Operator, Statement<'a>)>,
}

impl<'a> Parser<'a> for Block<'a> {
	fn parse(tokens: &'a TokenSlice, scope: CowScope<'a>) -> Result<Block<'a>, CompileError> {
		let mut iter = tokens.iter();
		let mut statements = Vec::new();
		match expect!(iter.next(), Token::Symbol(Symbol::LeftBrace)) {
			// If it starts with `{` it's a block
			Ok(_) => {
				loop {
					// if `[`
					//	condition -> operator -> statement
					// if operator
					//  operator -> statement
					// else
					//  statement
				}
			}

			// otherwise it's a statement
			Err(_) => {
				statements.push((Operator::Add, try!(Parser::parse(tokens, scope))));
			}
		}
		Ok(Block {
			statements: statements,
		})
	}
}
impl<'a> Function for Block<'a> {
	fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
		unimplemented!();
	}
}

#[derive(Show)]
struct Statement<'a>;
impl<'a> Parser<'a> for Statement<'a> {
	fn parse(tokens: &'a TokenSlice, scope: CowScope<'a>) -> Result<Statement<'a>, CompileError> {
		Ok(Statement)
	}
}
impl<'a> Function for Statement<'a> {
	fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
		unimplemented!();
	}
}
