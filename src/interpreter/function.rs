use super::expr::Expression;
use super::{CompileError, is_truthy};
use super::scope::Scope;

#[cfg(test)]
use super::lexer;
#[cfg(test)]
use super::{TRUE, FALSE};

// Something that can be called with arguments given in the scope
pub trait Function {
	fn call(&self, scope: &Scope) -> Result<f32, CompileError>;
}

macro_rules! bind_function(
    ( $name:ident, $func:ident ( $($arg:ident),* ) ) => (
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

						None => return Err(CompileError {
							msg: format!("function `{}` requires argument `{}` but it is not defined in scope", stringify!($func), stringify!($arg)),
							pos: None }),
					}
				),*))
			}
		}
    );
)

fn sin(a: f32) -> f32 {
	a.sin()
}

bind_function!(SinFunction, sin(a))

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
	cond: Box<Function + 'a>,
	func: Box<Function + 'a>,
}
impl<'a> CondFunction<'a> {
	fn new(cond: Box<Function + 'a>, func: Box<Function + 'a>) -> CondFunction<'a> {
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

#[test]
fn const_function_test() {
	let f = ConstFunction::new(42_f32);
	assert!(f.call(&Scope::new()).unwrap() == 42_f32);
}

#[test]
fn expr_function_test() {
	let mut s = Scope::new();
	s.define_var("a", 9_f32);
	let e = Expression::new(lexer::tokenize("5*3+a").unwrap().as_slice(), &s).unwrap();
	let f = ExprFunction::new(&e);
	assert!(f.call(&s).unwrap() == 24_f32);
}

#[test]
fn sin_test() {
	let a = 2_f32;
	let mut s = Scope::new();
	s.define_var("a", a);
	let f = SinFunction::new();
	assert!(f.call(&s).unwrap() == a.sin());
}

#[test]
fn cond_test() {
	let cond_truthy = ConstFunction::new(TRUE);
	let cond_falsey = ConstFunction::new(FALSE);
	let expr = ConstFunction::new(42_f32);
	let f_truthy = CondFunction::new(box cond_truthy, box expr);
	let f_falsey = CondFunction::new(box cond_falsey, box expr);
	let s = Scope::new();
	assert!(f_truthy.call(&s).unwrap() == 42_f32);
	assert!(f_falsey.call(&s).unwrap() == 0_f32);
}
