use super::expr::Expression;
use super::{CompileError};
use super::scope::Scope;

#[cfg(test)]
use super::lexer;

// Something that can be called with arguments given in the scope
trait Function {
	fn call(&self, scope: &Scope) -> Result<f32, CompileError>;
}

macro_rules! bind_function(
    ( $name:ident, $func:ident ( $($arg:ident),+ ) ) => (
		struct $name;
		impl $name {
			fn new() -> $name {
				$name
			}
		}
		impl Function for $name {
			fn call(&self, scope: &Scope) -> Result<f32, CompileError> {
				Ok($func($(
					match scope.var_id(stringify!($arg)) {
						Some(id) => scope.get_var(id).unwrap(),

						None => return Err(CompileError {
							msg: format!("function requires argument `{}` but not passed or defined in scope", stringify!($arg)),
							pos: None }),
					}
				),+))
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

struct Sum<'a> {
	fns: Vec<Box<Function + 'a>>,
}

impl<'a> Sum<'a> {
	fn eval(&self, scope: &'a Scope) -> Result<f32, CompileError> {
		let mut sum = 0_f32;
		for f in self.fns.iter() {
			sum += try!(f.call(scope));
		}
		Ok(sum)
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
	s.define("a", 9_f32);
	let e = Expression::new(lexer::tokenize("5*3+a").unwrap().as_slice(), &s).unwrap();
	let f = ExprFunction::new(&e);
	assert!(f.call(&s).unwrap() == 24_f32);
}

#[test]
fn sin_test() {
	let a = 2_f32;
	let mut s = Scope::new();
	s.define("a", a);
	let f = SinFunction::new();
	assert!(f.call(&s).unwrap() == a.sin());
}
