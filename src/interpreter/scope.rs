use std::collections::hash_map::{HashMap, Occupied, Vacant};
use super::function::Function;
use std::rc::Rc;

/// Holds a temporary fast reference to a variable defined in a scope. Only valid for the scope it
/// was leased from and it's children.
pub type VarId = uint;

/// Holds a temporary fast reference to a function defined in a scope. Only valid for the scope it
/// was leased from and it's children.
pub type FnId = uint;

/// Holds a number of variables, functions and their values.
#[deriving(Clone)]
pub struct Scope<'a> {
	var_values: Vec<f32>, // values of vars by id
	vars: HashMap<&'a str, VarId>, // map of var name -> var id

	func_values: Vec<Rc<&'a (Function + 'a)>>,
	funcs: HashMap<&'a str, FnId>,
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
	pub fn var_id(&self, var: &'a str) -> Option<VarId> {
		match self.vars.get(&var) {
			Some(v) => Some(*v),
			None => None,
		}
	}

	/// Returns the name of a variable from its id.
	pub fn var_name(&self, var_id: VarId) -> Option<&'a str> {
		for (k, v) in self.vars.iter() {
			if *v == var_id {
				return Some(*k);
			}
		}
		None
	}

	/// Sets a variable to an id retrieved from var_id. Return value indicates whether it was set
	/// sucessfully or not.
	pub fn set_var(&mut self, var_id: VarId, value: f32) -> bool {
		if var_id > self.var_values.len() {
			return false;
		}
		self.var_values[var_id] = value;
		true
	}

	/// Returns the value of a variable by id.
	pub fn get_var(&self, var_id: VarId) -> Option<f32> {
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
	pub fn define_func(&mut self, name: &'a str, func: Rc<&'a (Function + 'a)>) {
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
	pub fn get_func(&self, func_id: FnId) -> Option<Rc<&'a (Function + 'a)>> {
		if func_id >= self.func_values.len() {
			None
		} else {
			Some(self.func_values[func_id].clone())
		}
	}

	/// Returns the id of a function previously defined in the scope by name.
	pub fn func_id(&self, name: &'a str) -> Option<FnId> {
		match self.funcs.get(&name) {
			Some(id) => Some(*id),
			None => None,
		}
	}

	/// Returns the name of a function from its id.
	pub fn func_name(&self, func_id: FnId) -> Option<&'a str> {
		for (k, v) in self.funcs.iter() {
			if *v == func_id {
				return Some(*k);
			}
		}
		None
	}
}
