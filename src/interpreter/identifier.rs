use std::collections::hash_map::{HashMap, Entry};
use std::collections::VecMap;
use std::cell::{RefCell, Cell};

pub type Identifier = usize;

#[derive(Debug)]
pub struct IdMap<'a> {
	count: Cell<Identifier>,
	id_map: RefCell<HashMap<&'a str, Identifier>>,
	pub name_map: RefCell<VecMap<&'a str>>,
}

impl<'a> IdMap<'a> {
	pub fn new() -> IdMap<'a> {
		IdMap {
			count: Cell::new(0),
			id_map: RefCell::new(HashMap::new()),
			name_map: RefCell::new(VecMap::new()),
		}
	}

	/// Returns a new identifier or an existing one if the name is already in the map
	pub fn id(&self, name: &'a str) -> Identifier {
		match self.id_map.borrow_mut().entry(name) {
			Entry::Occupied(ref e) => {
				*e.get()
			},
			Entry::Vacant(e) => {
				self.count.set(self.count.get() + 1);
				self.name_map.borrow_mut().insert(self.count.get(), name);
				*e.insert(self.count.get())
			},
		}
	}
	pub fn name(&self, id: Identifier) -> Option<&'a str> {
		self.name_map.borrow().get(&id).map(|x| *x)
	}
}
