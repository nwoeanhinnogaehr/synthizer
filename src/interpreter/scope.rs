use std::collections::hash_map::{HashMap, Occupied, Vacant};

// Holds a number of variables and their values.
#[deriving(Show, Clone)]
pub struct Scope<'s> {
	var_values: Vec<f32>, // values of vars by id
	vars: HashMap<&'s str, uint>, // map of var name -> var id
}

impl<'a> Scope<'a> {
	// Constructs an empty scope object.
	pub fn new() -> Scope<'a> {
		Scope {
			var_values: Vec::new(),
			vars: HashMap::new(),
		}
	}

	// Returns a uint identifier which can be passed to set_var to quickly update the value of a
	// variable.
	pub fn var_id(&self, var: &'a str) -> Option<uint> {
		match self.vars.find(&var) {
			Some(v) => Some(*v),
			None => None,
		}
	}

	// Sets a variable to an id retrieved from var_id. Return value indicates whether it was set
	// sucessfully or not.
	pub fn set_var(&mut self, var_id: uint, value: f32) -> bool {
		if var_id > self.var_values.len() {
			return false;
		}
		self.var_values[var_id] = value;
		true
	}

	// Returns the value of a variable by id.
	pub fn get_var(&self, var_id: uint) -> Option<f32> {
		if var_id >= self.var_values.len() {
			None
		} else {
			Some(self.var_values[var_id])
		}
	}

	// Sets a variable to a specified value, creating the variable if it does not exist.
	pub fn define(&mut self, var: &'a str, value: f32) {
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

	// Returns the number of variables set in the scope.
	pub fn num_vars(&self) -> uint {
		self.vars.len()
	}
}
