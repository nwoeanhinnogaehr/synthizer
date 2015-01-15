use super::{CompileError, is_truthy};
use super::scope::CowScope;
use super::identifier::IdMap;
use std::num::Float;
use std::f32::consts;
use std::cell::RefCell;

#[cfg(test)]
use super::lexer;
#[cfg(test)]
use super::{TRUE, FALSE};

/// Something that can be called with arguments given in the scope
pub trait Function {
	fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError>;
}

macro_rules! bind_function(
	( $name:ident, $func:ident ( $($arg:ident = $val:expr),* ) ) => {
		#[derive(Copy)]
		pub struct $name;
		impl $name {
			pub fn new<'a>(idmap: &'a RefCell<IdMap<'a>>) -> $name {
			//pub fn new() -> $name {
				$(
					idmap.borrow_mut().define(stringify!($arg));
				)*
				$name
			}
		}
		impl Function for $name {
			#[allow(unused_variables)] // Compiler complains that scope is not used on functions with no args.
			fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
				Ok($func($(
					// Insert each argument from the scope
					match scope.get_var(idmap.id(stringify!($arg)).unwrap()) {
						Some(val) => val,

						None => match $val {
							Some(val) => val,
							None => return Err(CompileError::new(format!("function `{}` requires argument `{}` but it is not defined in scope", stringify!($func), stringify!($arg)))),
						}
					}
				),*))
			}
		}
	};
);

fn sin(freq: f32, amp: f32, phase: f32, time: f32) -> f32 {
	(freq*time*consts::PI_2 + phase).sin()*amp
}

fn sqrt(x: f32) -> f32 {
	x.sqrt()
}

fn abs(x: f32) -> f32 {
	x.abs()
}

bind_function!(SinFunction, sin(freq=None, amp=Some(1_f32), phase=Some(0_f32), time=None));
bind_function!(SqrtFunction, sqrt(x=None));
bind_function!(AbsFunction, abs(x=None));

/// Always returns a specific constant
#[derive(Copy)]
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
	fn call(&self, _: CowScope, _: &IdMap) -> Result<f32, CompileError> {
		Ok(self.val)
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
	fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
		if is_truthy(try!(self.cond.call(scope.clone(), idmap))) {
			self.func.call(scope, idmap)
		} else {
			Ok(0_f32)
		}
	}
}

#[test]
fn const_function_test() {
	let f = ConstFunction::new(42_f32);
	assert_eq!(f.call(&Scope::new()).unwrap(), 42_f32);
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
