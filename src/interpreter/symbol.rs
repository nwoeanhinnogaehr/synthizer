use super::lexer::SourcePos;
use super::ast::{Node, Function};
use std::collections::hash_map::{HashMap, Entry};
use std::collections::VecMap;
use std::cell::{RefCell};
use std::fmt;

pub type Identifier = usize;

#[derive(Debug)]
pub struct IdMap<'a> {
    count: Identifier,
    id_map: HashMap<&'a str, Identifier>,
    pub name_map: VecMap<&'a str>,
}

impl<'a> IdMap<'a> {
    pub fn new() -> IdMap<'a> {
        IdMap {
            count: 0,
            id_map: HashMap::new(),
            name_map: VecMap::new(),
        }
    }

    /// Returns a new identifier or an existing one if the name is already in the map
    pub fn id(&mut self, name: &'a str) -> Identifier {
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
    pub fn name(&self, id: Identifier) -> Option<&'a str> {
        self.name_map.get(&id).map(|x| *x)
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub ty: Option<Type>,
}

impl Symbol {
    pub fn new() -> Symbol {
        Symbol {
            ty: None,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable<'a> {
    id_map: RefCell<IdMap<'a>>,
    pub tree_pos: RefCell<Vec<usize>>,
    symbols: RefCell<HashMap<usize, VecMap<Symbol>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> SymbolTable<'a> {
        let mut sym = HashMap::new();
        sym.insert(0, VecMap::new());
        SymbolTable {
            id_map: RefCell::new(IdMap::new()),
            tree_pos: RefCell::new(vec![0]),
            symbols: RefCell::new(sym),
        }
    }
    pub fn get_id(&self, name: &'a str) -> Identifier {
        self.id_map.borrow_mut().id(name)
    }
    pub fn get_name(&self, id: Identifier) -> &'a str {
        self.id_map.borrow().name(id).unwrap()
    }
    pub fn enter_scope(&self, pos: SourcePos) {
        self.tree_pos.borrow_mut().push(pos.index);
        match self.symbols.borrow_mut().entry(pos.index) {
            Entry::Vacant(v) => { v.insert(VecMap::new()); }
            _ => { }
        }
    }
    pub fn leave_scope(&self) {
        self.tree_pos.borrow_mut().pop();
    }
    pub fn get_symbol(&self, id: Identifier) -> Option<(Symbol, usize)> {
        for (pos, idx) in self.tree_pos.borrow().iter().enumerate().rev() {
            let b = self.symbols.borrow();
            let table = b.get(idx).unwrap();
            if let Some(x) = table.get(&id).map(|x| x.clone()) {
                return Some((x, pos));
            }
        }
        None
    }
    pub fn set_symbol(&self, id: Identifier, sym: Symbol) {
        let pb = self.tree_pos.borrow();
        let pos = pb.last().unwrap();
        self.symbols.borrow_mut().get_mut(pos).unwrap().insert(id, sym);
    }
    pub fn check_cycle(&self, pos: SourcePos) -> bool {
        for &p in self.tree_pos.borrow().iter().rev() {
            if pos.index == p {
                return true
            }
        }
        false
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Number,
    Boolean,
    Function(Node<Function>),
    Infer,
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        let id = |x|
            match x {
                &Type::Number => 0,
                &Type::Boolean => 1,
                &Type::Function(..) => 2,
                &Type::Infer => 3,
            };
        id(self) == id(other)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Number => write!(f, "Number"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Function(_) => write!(f, "Function"),
            Type::Infer => write!(f, "Indeterminate"),
        }
    }
}
