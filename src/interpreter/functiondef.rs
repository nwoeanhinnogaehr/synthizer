use super::scope::CowScope;
use super::CompileError;
use super::lexer::{Token, TokenSlice, Symbol, Operator};
use super::function::Function;
use super::parser::Parser;
use super::identifier::{IdMap, Identifier};
use super::expr::Expression;
use super::functioncall::FunctionCall;
use std::collections::VecMap;

/// Represents a function definition written in synthizer
#[derive(Show)]
pub struct FunctionDef {
	statement: Statement,
	args: VecMap<Option<f32>>, // Default arguments
	pub ident: Identifier,
}
impl<'a> Parser<'a> for FunctionDef {
	/// Parse a function definition from a token stream. Scope is used to find function definitions
	fn parse(tokens: &'a TokenSlice, scope: CowScope<'a>) -> Result<FunctionDef, CompileError> {
		let mut iter = tokens.iter().enumerate();

		let mut args = VecMap::new();

		try!(expect!(iter.next().map(|(_, x)| x), Token::Symbol(Symbol::LeftBracket), "expected `[`, got `{}`"));

		let fn_ident = try!(expect_value!(iter.next().map(|(_, x)| x), Token::Ident, "expected function name, got `{}`"));

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
				Some(&Token::Symbol(Symbol::RightBracket)) => {
					if !args.contains_key(&arg_ident) {
						args.insert(arg_ident, None);
					}
					break;
				}
				Some(token) => return Err(CompileError::new(format!("expected `=`, `,` or `]`, got `{}`", token)).with_pos(pos.unwrap())),
				None => return Err(CompileError::new_static("expected `=`, `,` or `]`, got EOF")),
			}
		}
		//try!(expect!(iter.next().map(|(_, x)| x), Token::Symbol(Symbol::Colon), "expected `:` following function argument declaration, got `{}`"));

		let pos = iter.next().map(|(x, _)| x);
		let statement = match pos {
			Some(pos) => try!(Parser::parse(&tokens[pos..], scope)),
			None => return Err(CompileError::new_static("expected block, got EOF")),
		};

		Ok(FunctionDef {
			statement: statement,
			args: args,
			ident: fn_ident,
		})
	}
}
impl Function for FunctionDef {
	fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
		unimplemented!();
	}
}

#[derive(Show)]
enum Statement {
	Block(Vec<(Option<Expression>, Operator, Statement)>),
	Expr(Expression),
	Func(FunctionCall)
}

impl<'a> Parser<'a> for Statement {
	fn parse(tokens: &'a TokenSlice, scope: CowScope<'a>) -> Result<Statement, CompileError> {
		let mut iter = tokens.iter();
		let mut statements = Vec::new();
		match expect!(iter.next(), Token::Symbol(Symbol::LeftBrace)) {
			// If it starts with `{` it's a block
			Ok(_) => {
				loop {
					let token = iter.next();
					// if `[`
					//	condition -> operator -> statement
					// if operator
					//  operator -> statement
					// if `}`
					//  break
					// else
					//  statement
					match token.map(|x| x.token) {
						Some(Token::Symbol(Symbol::LeftBracket)) => {

						}
						Some(Token::Operator(op)) => {

						}
						Some(Token::Symbol(Symbol::RightBrace)) => {
							break;
						}
						Some(token) => {

						}
						None => {
							return Err(CompileError::new_static("expected block, expression, or function, got EOF"));
						}
					}
				}
			}

			// otherwise it's a expression or function
			Err(_) => {
				match Parser::parse(tokens, scope.clone()) { // Try to parse it as a function
					Ok(call) =>
						return Ok(Statement::Func(call)),
					Err(e) => // If that fails, try as an expression.
					{println!("{}", e);
						return Ok(Statement::Expr(try!(Parser::parse(tokens, scope))))},
				}
			}
		}
		Ok(Statement::Block(statements))
	}
}
impl Function for Statement {
	fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
		match self {
			&Statement::Func(ref x) =>
				x.call(scope, idmap),
			&Statement::Expr(ref x) =>
				x.call(scope, idmap),
			_ => unimplemented!(),
		}
	}
}
