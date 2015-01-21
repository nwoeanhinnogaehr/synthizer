use super::scope::{CowScope};
use super::CompileError;
use super::parser::{Parser, TokenStream};
use super::expr::Expression;
use super::function::Function;
use super::lexer::{Token, Symbol};
use super::identifier::{Identifier, IdMap};
use std::collections::VecMap;
use std::borrow::Cow;

/// Represents a function call written in synthizer
#[derive(Show, Clone)]
pub struct FunctionCall {
	pub func: Identifier, // the function the call refers to as a scope id
	args: VecMap<Expression>,
}
impl<'a> Parser<'a> for FunctionCall {
	/// Parse a function call from a token stream. Scope is used to find function definitions
	fn parse(tokens: TokenStream<'a>, scope: CowScope<'a>) -> Result<FunctionCall, CompileError> {
		let mut tokens = tokens;
		let mut args = VecMap::new();

		// Opening bracket
		try!(expect!(tokens.next(), Token::Symbol(Symbol::LeftBracket), "expected `[`, got `{}`"));

		// Function name
		let fn_ident = try!(expect_value!(tokens.next(),
			Token::Ident, "expected function name, got `{}`"));

		// Parse arguments
		'outer: loop {
			let token = tokens.next();
			if let Ok(_) = expect!(token, Token::Symbol(Symbol::RightBracket)) {
				// Got closing paren, end of list
				break;
			} else {
				// Argument name
				let arg_ident = try!(expect_value!(token, Token::Ident, "expected `]` or argument name, got `{}`"));

				// Equals
				try!(expect!(tokens.next(), Token::Symbol(Symbol::Equals), "expected `=`, got `{}`"));

				// Mark the position of the start of the expression so we can later extract a slice
				let expr_start = tokens.pos();

				loop {
					let token = tokens.next();

					// Adds all the tokens we have scanned so far in the inner loop to the args
					// list
					let mut write_arg = |&mut:| -> Result<(), CompileError> {
						let slice = tokens.slice(expr_start, tokens.pos() - 1);
						let expr = try!(Parser::parse(slice, scope.clone()));
						match args.get(&arg_ident) {
							Some(_) => {
								return Err(CompileError::new(format!("argument {} has already been defined", arg_ident)));
							}
							None => {
								args.insert(arg_ident, expr);
							}
						}
						Ok(())
					};

					match token.map(|x| x.token) {
						// Advance to next argument
						Some(Token::Symbol(Symbol::Comma)) => {
							try!(write_arg());
							break;
						}

						// End of function call
						Some(Token::Symbol(Symbol::RightBracket)) => {
							try!(write_arg());
							break 'outer;
						}

						// We need to handle nested parens so we can include parens in the argument
						// expressions
						Some(Token::Symbol(Symbol::LeftBracket)) => {
							//TODO borrowing issues with the closure above, need to rewrite it out.
							//tokens = try!(parser::match_paren(tokens, Token::Symbol(Symbol::LeftBracket), Token::Symbol(Symbol::RightBracket)));
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
			func: fn_ident,
			args: args,
		})
	}
}

impl Function for FunctionCall {
	fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
		let mut inner = scope.clone().into_owned();
		match scope.get_func(self.func) {
			Some(f) => {
				for (n, a) in self.args.iter() {
					inner.set_var(n, try!(a.call(scope.clone(), idmap)));
				}
				f.call(Cow::Owned(inner), idmap)
			}
			None => panic!("internal error: function id is invalid!"),
		}
	}
}
