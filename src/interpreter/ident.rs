use std::collections::HashMap;

/// Represents an identifier name in program source code.
pub type Identifier = usize;

#[derive(Debug)]
pub struct NameTable<'a> {
    identifier_names: HashMap<Identifier, &'a str>,
    identifier_ids: HashMap<&'a str, Identifier>,
    max_id: usize,
}

impl<'a> NameTable<'a> {
    pub fn new() -> NameTable<'a> {
        NameTable {
            identifier_names: HashMap::new(),
            identifier_ids: HashMap::new(),
            max_id: 0,
        }
    }

    pub fn new_id(&mut self, name: &'a str) -> Identifier {
        if let Some(id) = self.get_id(name) {
            return id;
        }
        let id = self.max_id;
        self.max_id += 1;
        self.identifier_names.insert(id, name);
        self.identifier_ids.entry(name).or_insert(id);
        id
    }
    /// Creates a new identifier which cannot be looked up by name, since it doesn't have one.
    pub fn new_anon(&mut self) -> Identifier {
        let id = self.max_id;
        self.max_id += 1;
        self.identifier_names.insert(id, "*anon*");
        id
    }
    pub fn get_id(&'a self, name: &'a str) -> Option<Identifier> {
        self.identifier_ids.get(name).map(|x| *x)
    }
    pub fn get_name(&'a self, id: Identifier) -> Option<&'a str> {
        self.identifier_names.get(&id).map(|x| *x)
    }
}
