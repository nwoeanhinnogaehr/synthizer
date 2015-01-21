use super::function::Function;
use super::identifier::{Identifier};
use std::collections::VecMap;
use std::borrow::Cow;

#[derive(Clone)]
enum Value<'a> {
	Number(f32),
	Function(&'a (Function + 'static)),
}

pub type CowScope<'a> = Cow<'a, Scope<'a>, Scope<'a>>;

/// Holds a number of variables, functions and their values.
#[derive(Clone)]
pub struct Scope<'a> {
	values: VecMap<Value<'a>>,
}

// TODO warn on type errors
impl<'a> Scope<'a> {
	/// Constructs an empty scope object.
	pub fn new() -> Scope<'a> {
		Scope {
			values: VecMap::new(),
		}
	}

	pub fn set_var(&mut self, id: Identifier, value: f32) {
		self.values.insert(id, Value::Number(value));
	}

	pub fn get_var(&self, id: Identifier) -> Option<f32> {
		match self.values.get(&id) {
			Some(&Value::Number(x)) => Some(x),
			_ => None,
		}
	}

	pub fn set_func(&mut self, id: Identifier, func: &'a (Function + 'static)) {
		self.values.insert(id, Value::Function(func));
	}

	pub fn get_func(&self, id: Identifier) -> Option<&'a (Function + 'static)> {
		match self.values.get(&id) {
			Some(&Value::Function(x)) => Some(x),
			_ => None,
		}
	}
}
