use super::scope::{CowScope, FnId};
use super::CompileError;
use super::parser::Parser;
use super::expr::Expression;
use super::function::Function;
use super::parser;
use super::lexer::{Token, TokenSlice};
use std::fmt;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::borrow::Cow;

/// Represents a function call written in synthizer
#[derive(Clone)]
pub struct FunctionCall {
	func: FnId, // the function the call refers to as a scope id
	args: HashMap<String, Expression>,
	name: String,
}
impl<'a> Parser<'a> for FunctionCall {
	/// Parse a function call from a token stream. Scope is used to find function definitions
	fn parse(tokens: &'a TokenSlice<'a>, scope: CowScope<'a>) -> Result<FunctionCall, CompileError> {
		let mut iter = tokens.iter().enumerate();

		let mut args = HashMap::new();

		// Function name
		let fn_name = try!(expect_value!(iter.next().map(|(_, x)| x),
			Token::Ident, "expected function name, got `{}`"));

		let fn_id = try!(scope.func_id(fn_name).ok_or(
				CompileError::new(format!("function `{}` called but not defined in scope", fn_name))));

		// Opening paren
		try!(expect!(iter.next().map(|(_, x)| x), Token::Symbol('('), "expected `(`, got `{}`"));

		// Parse arguments
		'outer: loop {
			let token = iter.next().map(|(_, x)| x);
			if let Ok(_) = expect!(token, Token::Symbol(')')) {
				// Got closing paren, end of list
				break;
			} else {
				// Argument name
				let arg_name = try!(expect_value!(token, Token::Ident, "expected `)` or argument name, got `{}`"));

				// Mark the position of the start of the expression so we can later extract a slice
				let next = iter.next();
				let (expr_start, token) = (next.map(|(x, _)| x), next.map(|(_, x)| x));

				// Equals
				try!(expect!(token, Token::Symbol('='), "expected `=`, got `{}`"));

				loop {
					let next = iter.next();
					let (pos, token) = (next.map(|(x, _)| x), next.map(|(_, x)| x));

					// Adds all the tokens we have scanned so far in the inner loop to the args
					// list
					let mut write_arg = |&mut :| -> Result<(), CompileError> {
						let slice = &tokens[expr_start.unwrap() + 1..pos.unwrap()];
						let expr = try!(Parser::parse(slice, scope.clone()));
						match args.entry(arg_name.to_string()) {
							Entry::Occupied(_) => {
								return Err(CompileError::new(format!("argument {} has already been defined", arg_name)));
							}
							Entry::Vacant(e) => {
								e.insert(expr);
							}
						}
						Ok(())
					};

					match token.map(|x| &x.token) {
						// Advance to next argument
						Some(&Token::Symbol(',')) => {
							try!(write_arg());
							break;
						}

						// End of function call
						Some(&Token::Symbol(')')) => {
							try!(write_arg());
							break 'outer;
						}

						// We need to handle nested parens so we can include parens in the argument
						// expressions
						Some(&Token::Symbol('(')) => {
							try!(parser::match_paren(&mut iter, Token::Symbol('('), Token::Symbol(')')));
						}

						None => {
							return Err(CompileError::new_static("Expected expression, got EOF"));
						}

						_ => { }
					}
				}
			}
		}

		Ok(FunctionCall {
			func: fn_id,
			name: fn_name.to_string(),
			args: args,
		})
	}
}

impl FunctionCall {
	pub fn name(&self) -> &str {
		self.name.as_slice()
	}
}

impl fmt::Show for FunctionCall {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

impl Function for FunctionCall {
	fn call(&self, scope: CowScope) -> Result<f32, CompileError> {
		let mut inner = scope.clone().into_owned();
		match scope.get_func(self.func) {
			Some(f) => {
				for (n, a) in self.args.iter() {
					inner.define_var(n.as_slice(), try!(a.call(scope.clone())));
				}
				f.call(Cow::Owned(inner))
			}
			None => panic!("internal error: function id is invalid!"),
		}
	}
}
