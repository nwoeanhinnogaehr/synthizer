use super::expr::Expression;
use super::{CompileError, is_truthy};
use super::scope::Scope;
use super::sum::Sum;
use std::collections::HashMap;
use super::lexer::{Token, TokenType};
use std::fmt;
use std::num::FloatMath;
use std::f32::consts;

#[cfg(test)]
use super::lexer;
#[cfg(test)]
use super::{TRUE, FALSE};

/// Something that can be called with arguments given in the scope
pub trait Function {
	fn call<'s>(&self, scope: &'s Scope<'s>) -> Result<f32, CompileError>;
}

macro_rules! bind_function(
	( $name:ident, $func:ident ( $($arg:ident = $val:expr),* ) ) => (
		pub struct $name;
		impl $name {
			pub fn new() -> $name {
				$name
			}
		}
		impl Function for $name {
			#[allow(unused_variables)] // Compiler complains that scope is not used on functions with no args.
			fn call(&self, scope: &Scope) -> Result<f32, CompileError> {
				Ok($func($(
					// Insert each argument from the scope
					match scope.var_id(stringify!($arg)) {
						Some(id) => scope.get_var(id).unwrap(),

						None => match $val {
							Some(val) => val,
							None => return Err(CompileError {
								msg: format!("function `{}` requires argument `{}` but it is not defined in scope", stringify!($func), stringify!($arg)),
								pos: None }),
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

bind_function!(SinFunction, sin(freq=None, amp=Some(1_f32), phase=Some(0_f32), time=None))

/// Always returns a specific constant
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
	fn call(&self, _: &Scope) -> Result<f32, CompileError> {
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
	fn call<'s>(&self, scope: &'s Scope<'s>) -> Result<f32, CompileError> {
		self.expr.eval(scope)
	}
}

/// Returns the result of calling the function if the condition is truthy, else 0
pub struct CondFunction<'a> {
	cond: &'a Function + 'a,
	func: &'a Function + 'a,
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
	fn call<'s>(&self, scope: &'s Scope<'s>) -> Result<f32, CompileError> {
		if is_truthy(try!(self.cond.call(scope))) {
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
	fn call<'s>(&self, scope: &'s Scope<'s>) -> Result<f32, CompileError> {
		self.sum.eval(scope)
	}
}

/// Represents a function call written in synthizer
#[deriving(Clone)]
pub struct SyntFunctionCall<'a> {
	func: uint, // the function the call refers to as a scope id
	args: HashMap<&'a str, ExprFunction<'a>>, // the value of each argument passed is a function 
	name: &'a str,
}
impl<'a> SyntFunctionCall<'a> {
	/// Parse a function call from a token stream. Scope is used to find function definitions
	pub fn new(tok: &'a[Token<'a>], scope: &'a Scope<'a>) -> Result<SyntFunctionCall<'a>, CompileError> {
		enum State {
			Name, Start, ArgName, ArgExpr
		}
		let mut state = State::Name;
		let mut iter = tok.iter();
		let mut index = 0u;

		// These will all be initialized in the loop (assuming no bugs!) but rust won't let me
		// leave them uninitialized.
		let mut name = "";
		let mut func = 0u;
		let mut args = HashMap::new();
		let mut argname = "";
		let mut argexpr_start = 0u;
		let mut argexpr_end = 0u;
		let mut paren_depth = 0i;
		
		loop {
			let token = match iter.next() {
				Some(x) => x,
				None => return Err(CompileError { msg: "unexpected end of file in function call".to_string(), pos: None }),
			};
			match state {
				State::Name => {
					name = match token.t {
						TokenType::Ident(name) => name,
						_ => return Err(CompileError { msg: format!("expected identifier, got `{}`", token.t), pos: Some(token.pos) }),
					};
					func = match scope.func_id(name) {
						Some(id) => id,
						None => return Err(CompileError { msg: format!("function `{}` is called but was not defined in scope", name), pos: Some(token.pos) }),
					};
					state = State::Start;
				}

				State::Start => {
					match token.t {
						TokenType::Paren('(') => { },
						_ => return Err(CompileError { msg: format!("expected `(`, got `{}`", token.t), pos: Some(token.pos) }),
					}
					state = State::ArgName;
				}

				State::ArgName => {
					match token.t {
						TokenType::Ident(name) => {
							argname = name;
							state = State::ArgName;
						},
						TokenType::Equals => {
							argexpr_start = index + 1;
							argexpr_end = index + 1;
							state = State::ArgExpr;
						},
						TokenType::Paren(')') => {
							break;
						}
						_ => return Err(CompileError { msg: format!("expected identifier, `=` or `)`, got `{}`", token.t), pos: Some(token.pos) }),
					};
				}

				State::ArgExpr => {
					match token.t {
						TokenType::Equals => {
							if paren_depth == 0 {
								let nextargname = match tok[argexpr_end - 1].t {
									TokenType::Ident(name) => name,
									x => return Err(CompileError { msg: format!("expected identifiter, got `{}`", x), pos: Some(token.pos) }),
								};
								let expr = try!(Expression::new(tok[argexpr_start..argexpr_end - 1], scope));
								let exprfn = ExprFunction::new(expr);
								args.insert(argname, exprfn);

								argname = nextargname;
								argexpr_start = index + 1;
								argexpr_end = index + 1;
							} else {
								argexpr_end += 1;
							}
						},
						TokenType::Paren(')') => {
							if paren_depth == 0 {
								let expr = try!(Expression::new(tok[argexpr_start..argexpr_end], scope));
								let exprfn = ExprFunction::new(expr);
								args.insert(argname, exprfn);
								break;
							} else {
								argexpr_end += 1;
							}
							paren_depth -= 1;
						},
						TokenType::Paren('(') => {
							paren_depth += 1;
							argexpr_end += 1;
						},
						_ => argexpr_end += 1,
					}
				}
			}
			index += 1;
		}
		Ok(SyntFunctionCall {
			func: func,
			args: args,
			name: name,
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
	fn call<'s>(&self, scope: &'s Scope<'s>) -> Result<f32, CompileError> {
		let mut inner = Scope::new();
		// XXX these two blocks are cheap as fuck and will be replaced eventually when we can just
		// do scope.clone()
		{
			let mut i = 0;
			loop {
				let v = scope.get_var(i);
				match v {
					Some(v) => inner.define_var(scope.var_name(i).unwrap(), v),
					None => break,
				}
				i += 1;
			}
		}
		{
			let mut i = 0;
			loop {
				let v = scope.get_func(i);
				match v {
					Some(v) => inner.define_func(scope.func_name(i).unwrap(), v),
					None => break,
				}
				i += 1;
			}
		}
		match scope.get_func(self.func) {
			Some(f) => {
				for (n, a) in self.args.iter() {
					inner.define_var(*n, try!(a.call(scope)));
				}
				f.call(&inner)
			}
			None => panic!("internal error: function id is invalid!"),
		}
	}
}

/// Represents a function definition written in synthizer
pub struct SyntFunctionDef<'a> {
	func: &'a Function + 'a, // Internally a SumFunction containing CondFunctions containing ExprFunctions
	args: HashMap<&'a str, Option<f32>>, // Default arguments
	name: &'a str,
}
impl<'a> SyntFunctionDef<'a> {
	/// Parse a function definition from a token stream. Scope is used to find function definitions
	pub fn new<'s>(tok: Vec<Token<'a>>, scope: &'s Scope<'a>) -> Result<SyntFunctionDef<'a>, CompileError> {
		unimplemented!();
	}

	pub fn name(&self) -> &'a str {
		self.name
	}
}
impl<'a> Function for SyntFunctionDef<'a> {
	fn call(&self, scope: &Scope) -> Result<f32, CompileError> {
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
