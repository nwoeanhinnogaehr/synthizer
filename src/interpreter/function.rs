use super::expr::Expression;
use super::{CompileError, is_truthy};
use super::scope::{CowScope, FnId};
use super::sum::Sum;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use super::lexer::{Token, TokenSlice};
use std::fmt;
use std::num::FloatMath;
use std::num::Float;
use std::f32::consts;
use super::parseutil;
use std::borrow::Cow;

#[cfg(test)]
use super::lexer;
#[cfg(test)]
use super::{TRUE, FALSE};

/// Something that can be called with arguments given in the scope
pub trait Function {
	fn call<'s>(&self, scope: CowScope<'s>) -> Result<f32, CompileError>;
}

macro_rules! bind_function(
	( $name:ident, $func:ident ( $($arg:ident = $val:expr),* ) ) => (
		#[deriving(Copy)]
		pub struct $name;
		impl $name {
			pub fn new() -> $name {
				$name
			}
		}
		impl Function for $name {
			#[allow(unused_variables)] // Compiler complains that scope is not used on functions with no args.
			fn call(&self, scope: CowScope) -> Result<f32, CompileError> {
				Ok($func($(
					// Insert each argument from the scope
					match scope.var_id(stringify!($arg)) {
						Some(id) => scope.get_var(id).unwrap(),

						None => match $val {
							Some(val) => val,
							None => return Err(CompileError::new(format!("function `{}` requires argument `{}` but it is not defined in scope", stringify!($func), stringify!($arg)))),
						}
					}
				),*))
			}
		}
	);
)

fn sin(freq: f32, amp: f32, phase: f32, time: f32) -> f32 {
	(freq*time*consts::PI_2 + phase).sin()*amp
}

fn sqrt(x: f32) -> f32 {
	x.sqrt()
}

fn abs(x: f32) -> f32 {
	x.abs()
}

bind_function!(SinFunction, sin(freq=None, amp=Some(1_f32), phase=Some(0_f32), time=None))
bind_function!(SqrtFunction, sqrt(x=None))
bind_function!(AbsFunction, abs(x=None))

/// Always returns a specific constant
#[deriving(Copy)]
pub struct ConstFunction {
	val: f32,
}
impl ConstFunction {
	pub fn new(val: f32) -> ConstFunction {
		ConstFunction {
			val: val,
		}
	}
}
impl Function for ConstFunction {
	fn call(&self, _: CowScope) -> Result<f32, CompileError> {
		Ok(self.val)
	}
}

/// Wraps an expression from the expr module.
#[deriving(Clone)]
pub struct ExprFunction<'a> {
	expr: Expression<'a>,
}
impl<'a> ExprFunction<'a> {
	pub fn new(expr: Expression<'a>) -> ExprFunction<'a> {
		ExprFunction {
			expr: expr,
		}
	}
}
impl<'a> Function for ExprFunction<'a> {
	fn call<'s>(&self, scope: CowScope<'s>) -> Result<f32, CompileError> {
		self.expr.eval(scope)
	}
}

/// Returns the result of calling the function if the condition is truthy, else 0
pub struct CondFunction<'a> {
	cond: &'a (Function + 'a),
	func: &'a (Function + 'a),
}
impl<'a> CondFunction<'a> {
	pub fn new(cond: &'a Function, func: &'a Function) -> CondFunction<'a> {
		CondFunction {
			cond: cond,
			func: func,
		}
	}
}
impl<'a> Function for CondFunction<'a> {
	fn call<'s>(&self, scope: CowScope<'s>) -> Result<f32, CompileError> {
		if is_truthy(try!(self.cond.call(scope.clone()))) {
			self.func.call(scope)
		} else {
			Ok(0_f32)
		}
	}
}

/// A SumFunction represents the sum of a bunch of other functions.
pub struct SumFunction<'a> {
	sum: &'a Sum<'a>
}
impl<'a> SumFunction<'a> {
	pub fn new(sum: &'a Sum<'a>) -> SumFunction<'a> {
		SumFunction {
			sum: sum,
		}
	}
}
impl<'a> Function for SumFunction<'a> {
	fn call<'s>(&self, scope: CowScope<'s>) -> Result<f32, CompileError> {
		self.sum.eval(scope)
	}
}

/// Represents a function call written in synthizer
#[deriving(Clone)]
pub struct SyntFunctionCall<'a> {
	func: FnId, // the function the call refers to as a scope id
	args: HashMap<&'a str, ExprFunction<'a>>, // the value of each argument passed is a function
	name: &'a str,
}
impl<'a> SyntFunctionCall<'a> {
	/// Parse a function call from a token stream. Scope is used to find function definitions
	pub fn new(tokens: &'a TokenSlice<'a>, scope: CowScope<'a>) -> Result<SyntFunctionCall<'a>, CompileError> {
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
					let write_arg = || -> Result<(), CompileError> {
						let slice = tokens[expr_start.unwrap() + 1..pos.unwrap()];
						let expr = try!(Expression::new(slice, scope.clone()));
						let expr_func = ExprFunction::new(expr);
						match args.entry(arg_name) {
							Entry::Occupied(_) => {
								return Err(CompileError::new(format!("argument {} has already been defined", arg_name)));
							}
							Entry::Vacant(e) => {
								e.set(expr_func);
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
							try!(parseutil::match_paren(&mut iter, Token::Symbol('('), Token::Symbol(')')));
						}

						None => {
							return Err(CompileError::new_static("Expected expression, got EOF"));
						}

						_ => { }
					}
				}
			}
		}

		Ok(SyntFunctionCall {
			func: fn_id,
			name: fn_name,
			args: args,
		})
	}

	pub fn name(&self) -> &'a str {
		self.name
	}
}
impl<'a> fmt::Show for SyntFunctionCall<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}
impl<'a> Function for SyntFunctionCall<'a> {
	fn call<'s>(&self, scope: CowScope<'s>) -> Result<f32, CompileError> {
		let mut inner = scope.clone().into_owned();
		match scope.get_func(self.func) {
			Some(f) => {
				for (n, a) in self.args.iter() {
					inner.define_var(n.to_string(), try!(a.call(scope.clone())));
				}
				f.call(Cow::Owned(inner))
			}
			None => panic!("internal error: function id is invalid!"),
		}
	}
}

/// Represents a function definition written in synthizer
pub struct SyntFunctionDef<'a> {
	func: &'a (Function + 'a), // Internally a SumFunction containing CondFunctions containing ExprFunctions
	args: HashMap<&'a str, Option<f32>>, // Default arguments
	name: &'a str,
}
impl<'a> SyntFunctionDef<'a> {
	/// Parse a function definition from a token stream. Scope is used to find function definitions
	pub fn new<'s>(tok: Vec<Token<'a>>, scope: CowScope<'a>) -> Result<SyntFunctionDef<'a>, CompileError> {
		unimplemented!();
	}

	pub fn name(&self) -> &'a str {
		self.name
	}
}
impl<'a> Function for SyntFunctionDef<'a> {
	fn call(&self, scope: CowScope) -> Result<f32, CompileError> {
		unimplemented!();
	}
}

#[test]
fn const_function_test() {
	let f = ConstFunction::new(42_f32);
	assert_eq!(f.call(&Scope::new()).unwrap(), 42_f32);
}

#[test]
fn expr_function_test() {
	let mut s = Scope::new();
	s.define_var("a", 9_f32);
	let t = lexer::tokenize("5*3+a").unwrap();
	let e = Expression::new(t.as_slice(), &s).unwrap();
	let f = ExprFunction::new(e);
	assert_eq!(f.call(&s).unwrap(), 24_f32);
}

#[test]
fn sin_test() {
	let mut s = Scope::new();
	s.define_var("freq", 440_f32);
	s.define_var("time", 0_f32);
	s.define_var("amp", 0.5_f32);
	let f = SinFunction::new();
	assert_eq!(f.call(&s).unwrap(), 0_f32);
}

#[test]
fn cond_test() {
	let cond_truthy = ConstFunction::new(TRUE);
	let cond_falsey = ConstFunction::new(FALSE);
	let expr = ConstFunction::new(42_f32);
	let f_truthy = CondFunction::new(&cond_truthy, &expr);
	let f_falsey = CondFunction::new(&cond_falsey, &expr);
	let s = Scope::new();
	assert_eq!(f_truthy.call(&s).unwrap(), 42_f32);
	assert_eq!(f_falsey.call(&s).unwrap(), 0_f32);
}

#[test]
fn sum_test() {
	let sum = Sum::new(vec![
		box ConstFunction::new(1_f32) as Box<Function>,
		box ConstFunction::new(2_f32) as Box<Function>,
		box ConstFunction::new(3_f32) as Box<Function>,
		box ConstFunction::new(4_f32) as Box<Function>]);
	let f = SumFunction::new(&sum);
	let s = Scope::new();
	assert_eq!(f.call(&s).unwrap(), 10_f32);
}
