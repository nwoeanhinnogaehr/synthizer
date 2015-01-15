use super::function::Function;
use super::identifier::Identifier;
use std::collections::VecMap;
use std::borrow::Cow;

pub type CowScope<'a> = Cow<'a, Scope<'a>, Scope<'a>>;

/// Holds a number of variables, functions and their values.
#[derive(Clone)]
pub struct Scope<'a> {
	vars: VecMap<f32>,
	funcs: VecMap<&'a (Function + 'static)>,
}

impl<'a> Scope<'a> {
	/// Constructs an empty scope object.
	pub fn new() -> Scope<'a> {
		Scope {
			vars: VecMap::new(),
			funcs: VecMap::new(),
		}
	}

	pub fn set_var(&mut self, id: Identifier, value: f32) {
		self.vars.insert(id, value);
	}

	pub fn get_var(&self, id: Identifier) -> Option<f32> {
		self.vars.get(&id).map(|x| *x)
	}

	pub fn set_func(&mut self, id: Identifier, func: &'a (Function + 'static)) {
		self.funcs.insert(id, func);
	}

	pub fn get_func(&self, id: Identifier) -> Option<&'a (Function + 'static)> {
		self.funcs.get(&id).map(|x| *x)
	}
}
