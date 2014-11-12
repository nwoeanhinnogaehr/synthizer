use std::collections::hash_map::{HashMap, Occupied, Vacant};
use super::function::Function;

/// Holds a number of variables, functions and their values.
#[deriving(Clone)]
pub struct Scope<'a> {
	var_values: Vec<f32>, // values of vars by id
	vars: HashMap<&'a str, uint>, // map of var name -> var id

	func_values: Vec<&'a Box<Function + 'a>>,
	funcs: HashMap<&'a str, uint>,
}

impl<'a> Scope<'a> {
	/// Constructs an empty scope object.
	pub fn new() -> Scope<'a> {
		Scope {
			var_values: Vec::new(),
			vars: HashMap::new(),
			func_values: Vec::new(),
			funcs: HashMap::new(),
		}
	}

	/// Returns a uint identifier which can be passed to set_var to quickly update the value of a
	/// variable.
	pub fn var_id(&self, var: &'a str) -> Option<uint> {
		match self.vars.get(&var) {
			Some(v) => Some(*v),
			None => None,
		}
	}

	/// Sets a variable to an id retrieved from var_id. Return value indicates whether it was set
	/// sucessfully or not.
	pub fn set_var(&mut self, var_id: uint, value: f32) -> bool {
		if var_id > self.var_values.len() {
			return false;
		}
		self.var_values[var_id] = value;
		true
	}

	/// Returns the value of a variable by id.
	pub fn get_var(&self, var_id: uint) -> Option<f32> {
		if var_id >= self.var_values.len() {
			None
		} else {
			Some(self.var_values[var_id])
		}
	}

	/// Sets a variable to a specified value, creating the variable if it does not exist.
	pub fn define_var(&mut self, var: &'a str, value: f32) {
		let nv = self.vars.len();
		match self.vars.entry(var) {
			Occupied(entry) => {
				self.var_values[*entry.get()] = value;
			},
			Vacant(entry) => {
				entry.set(nv);
				self.var_values.push(value);
			},
		}
	}

	/// Returns the number of variables set in the scope.
	pub fn num_vars(&self) -> uint {
		self.vars.len()
	}

	/// Defines a function in the scope.
	pub fn define_func(&mut self, name: &'a str, func: &'a Box<Function>) {
		let nv = self.funcs.len();
		match self.funcs.entry(name) {
			Occupied(entry) => {
				self.func_values[*entry.get()] = func;
			},
			Vacant(entry) => {
				entry.set(nv);
				self.func_values.push(func);
			},
		}
	}

	/// Gets a function previously defined in the scope by id.
	pub fn get_func(&self, func_id: uint) -> Option<&'a Function> {
		if func_id >= self.func_values.len() {
			None
		} else {
			Some(&**self.func_values[func_id])
		}
	}

	// Returns the id of a function previously defined in the scope by name.
	pub fn func_id(&self, name: &'a str) -> Option<uint> {
		match self.funcs.get(&name) {
			Some(id) => Some(*id),
			None => None,
		}
	}
}
