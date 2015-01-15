use std::collections::hash_map::{HashMap, Entry};
use std::cell::RefCell;

pub type Identifier = usize;

pub struct IdMap<'a> {
	count: Identifier,
	id_map: HashMap<&'a str, Identifier>,
	name_map: HashMap<Identifier, &'a str>,
}

impl<'a> IdMap<'a> {
	pub fn new() -> RefCell<IdMap<'a>> {
		RefCell::new(IdMap {
			count: 0,
			id_map: HashMap::new(),
			name_map: HashMap::new(),
		})
	}

	/// Returns a new identifier or an existing one if the name is already in the map
	pub fn define(&mut self, name: &'a str) -> Identifier {
		match self.id_map.entry(name) {
			Entry::Occupied(ref e) => {
				*e.get()
			},
			Entry::Vacant(e) => {
				self.count += 1;
				self.name_map.insert(self.count, name);
				*e.insert(self.count)
			},
		}
	}
	pub fn id(&self, name: &'a str) -> Option<Identifier> {
		self.id_map.get(&name).map(|x| *x)
	}
	pub fn name(&self, id: Identifier) -> Option<&'a str> {
		self.name_map.get(&id).map(|x| *x)
	}
}
