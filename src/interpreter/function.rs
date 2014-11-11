use super::expr::Expression;
use super::{CompileError, is_truthy};
use super::scope::Scope;
use super::sum::Sum;
use std::collections::HashMap;
use super::lexer::Token;

#[cfg(test)]
use super::lexer;
#[cfg(test)]
use super::{TRUE, FALSE};

// Something that can be called with arguments given in the scope
pub trait Function {
	fn call(&self, scope: &Scope) -> Result<f32, CompileError>;
}

macro_rules! bind_function(
	( $name:ident, $func:ident ( $($arg:ident = $val:expr),* ) ) => (
		struct $name;
		impl $name {
			fn new() -> $name {
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
	(freq*time*Float::two_pi() + phase).sin()*amp
}

bind_function!(SinFunction, sin(freq=None, amp=Some(1_f32), phase=Some(0_f32), time=None))

// Always returns a specific constant
struct ConstFunction {
	val: f32,
}
impl ConstFunction {
	fn new(val: f32) -> ConstFunction {
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

// Wraps an expression from the expr module.
struct ExprFunction<'a> {
	expr: &'a Expression<'a>,
}
impl<'a> ExprFunction<'a> {
	fn new(expr: &'a Expression<'a>) -> ExprFunction<'a> {
		ExprFunction {
			expr: expr,
		}
	}
}
impl<'a> Function for ExprFunction<'a> {
	fn call(&self, scope: &Scope) -> Result<f32, CompileError> {
		self.expr.eval(scope)
	}
}

// Returns the result of calling the function if the condition is truthy, else 0
struct CondFunction<'a> {
	cond: &'a Function + 'a,
	func: &'a Function + 'a,
}
impl<'a> CondFunction<'a> {
	fn new(cond: &'a Function, func: &'a Function) -> CondFunction<'a> {
		CondFunction {
			cond: cond,
			func: func,
		}
	}
}
impl<'a> Function for CondFunction<'a> {
	fn call(&self, scope: &Scope) -> Result<f32, CompileError> {
		if is_truthy(try!(self.cond.call(scope))) {
			self.func.call(scope)
		} else {
			Ok(0_f32)
		}
	}
}

// A SumFunction represents the sum of a bunch of other functions.
struct SumFunction<'a> {
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
	fn call(&self, scope: &Scope) -> Result<f32, CompileError> {
		self.sum.eval(scope)
	}
}

// Represents a function call written in synthizer
struct SyntFunctionCall<'a> {
	func: &'a Function + 'a, // the function the call refers to
	args: HashMap<&'a str, &'a Function + 'a>, // the value of each argument passed is a function (internally an ExprFunction)
	name: &'a str,
}
impl<'a> SyntFunctionCall<'a> {
	// Parse a function call from a token stream. Scope is used to find function definitions
	fn new<'s>(tok: &'a [Token<'a>], scope: &'s Scope<'a>) -> Result<SyntFunctionCall<'a>, CompileError> {
		unimplemented!();
	}

	fn name(&self) -> &'a str {
		self.name
	}
}
impl<'a> Function for SyntFunctionCall<'a> {
	fn call(&self, scope: &Scope) -> Result<f32, CompileError> {
		unimplemented!();
	}
}

// Represents a function definition written in synthizer
struct SyntFunctionDef<'a> {
	func: &'a Function + 'a, // Internally a SumFunction containing CondFunctions containing ExprFunctions
	args: HashMap<&'a str, Option<f32>>, // Default arguments
	name: &'a str,
}
impl<'a> SyntFunctionDef<'a> {
	// Parse a function definition from a token stream. Scope is used to find function definitions
	fn new<'s>(tok: &'a [Token<'a>], scope: &'s Scope<'a>) -> Result<SyntFunctionDef<'a>, CompileError> {
		unimplemented!();
	}

	fn name(&self) -> &'a str {
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
	let e = Expression::new(lexer::tokenize("5*3+a").unwrap().as_slice(), &s).unwrap();
	let f = ExprFunction::new(&e);
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
