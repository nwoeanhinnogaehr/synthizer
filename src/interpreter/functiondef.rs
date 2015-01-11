use super::scope::CowScope;
use super::CompileError;
use super::lexer::{Token, TokenSlice};
use super::function::Function;
use super::parser::Parser;
use std::collections::HashMap;

/// Represents a function definition written in synthizer
pub struct FunctionDef<'a> {
	block: Block<'a>,
	args: HashMap<&'a str, Option<f32>>, // Default arguments
	name: &'a str,
}
impl<'a> Parser<'a> for FunctionDef<'a> {
	/// Parse a function definition from a token stream. Scope is used to find function definitions
	fn parse(tokens: &'a TokenSlice<'a>, scope: CowScope<'a>) -> Result<FunctionDef<'a>, CompileError> {
		let mut iter = tokens.iter().enumerate();

		let mut args = HashMap::new();

		let fn_name = try!(expect_value!(iter.next().map(|(_, x)| x), Token::Ident, "expected function name, got `{}`"));
		try!(expect!(iter.next().map(|(_, x)| x), Token::Symbol('('), "expected `(`, got `{}`"));

		loop {
			let arg_name = try!(expect_value!(iter.next().map(|(_, x)| x), Token::Ident, "expected argument name, got `{}`"));

			let next = iter.next().map(|(_, x)| x);
			let (pos, mut token) = (next.map(|x| x.pos), next.map(|x| &x.token));
			if let Some(&Token::Symbol('='))  = token {
					let nexttoken = iter.next().map(|(_, x)| x);
					let value = if let Ok(_) = expect!(nexttoken, Token::Operator("-")) {
						-try!(expect_value!(iter.next().map(|(_, x)| x), Token::Const, "expected numerical constant, got `{}`"))
					} else {
						try!(expect_value!(nexttoken, Token::Const, "expected numerical constant, got `{}`"))
					};
					args.insert(arg_name, Some(value));
					token = iter.next().map(|(_, x)| &x.token);
			}
			match token {
				Some(&Token::Symbol(',')) => {
					args.insert(arg_name, None);
				}
				Some(&Token::Symbol(')')) => {
					args.insert(arg_name, None);
					break;
				}
				Some(token) => return Err(CompileError::new(format!("expected `=`, `,` or `)`, got `{}`", token)).with_pos(pos.unwrap())),
				None => return Err(CompileError::new_static("expected `=`, `,` or `)`, got EOF")),
			}
		}
		try!(expect!(iter.next().map(|(_, x)| x), Token::Symbol(':'), "expected `:` following function argument declaration, got `{}`"));

		let pos = iter.next().map(|(x, _)| x);
		let block = match pos {
			Some(pos) => try!(Parser::parse(&tokens[pos..], scope)),
			None => return Err(CompileError::new_static("expected block, got EOF")),
		};

		Ok(FunctionDef {
			block: block,
			args: args,
			name: fn_name,
		})
	}
}
impl<'a> FunctionDef<'a> {
	pub fn name(&self) -> &'a str {
		self.name
	}
}
impl<'a> Function for FunctionDef<'a> {
	fn call(&self, scope: CowScope) -> Result<f32, CompileError> {
		unimplemented!();
	}
}

struct Block<'a>;
impl<'a> Parser<'a> for Block<'a> {
	fn parse(tokens: &'a TokenSlice<'a>, scope: CowScope<'a>) -> Result<Block<'a>, CompileError> {
		unimplemented!();
	}
}
impl<'a> Function for Block<'a> {
	fn call(&self, scope: CowScope) -> Result<f32, CompileError> {
		unimplemented!();
	}
}

struct Statement<'a>;
impl<'a> Parser<'a> for Statement<'a> {
	fn parse(tokens: &'a TokenSlice<'a>, scope: CowScope<'a>) -> Result<Statement<'a>, CompileError> {
		unimplemented!();
	}
}
impl<'a> Function for Statement<'a> {
	fn call(&self, scope: CowScope) -> Result<f32, CompileError> {
		unimplemented!();
	}
}
