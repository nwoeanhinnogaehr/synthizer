use super::ident::Identifier;

use std::collections::{HashMap, VecMap};

/// Represents a single scope, translates to source code locations.
pub type ScopeId = usize;

/// Holds an exact location within a ScopedTable.
#[derive(Clone, Debug)]
pub struct ScopePos {
    pub scope: Vec<ScopeId>,
    pub scope_lengths: Vec<usize>,
}

#[derive(Clone, Debug)]
pub struct Symbol<T> where T: Clone {
    pub scope: ScopePos,
    pub val: T,
}

pub struct ScopedTable<T> where T: Clone {
    symbols: HashMap<ScopeId, VecMap<Symbol<T>>>,
    scope: Vec<ScopeId>,
    scope_lengths: Vec<usize>,
}

impl<T> ScopedTable<T> where T: Clone {
    pub fn new() -> ScopedTable<T> {
        let mut symbols = HashMap::new();
        symbols.insert(0, VecMap::new());
        ScopedTable {
            symbols: symbols,
            scope: vec![0],
            scope_lengths: vec![0],
        }
    }

    /// Push a list of scopes which will all be popped off at once when pop() is called.
    pub fn push_scope(&mut self, scopes: &[ScopeId]) {
        self.scope.push_all(scopes);
        self.scope_lengths.push(scopes.len());
    }

    /// Push a single scope, which will be popped off when pop() is called.
    pub fn push(&mut self, scope: ScopeId) {
        self.scope.push(scope);
        self.scope_lengths.push(1);
        self.symbols.entry(scope).or_insert(VecMap::new());
    }

    /// Push a single scope onto the end of the current group.
    /// It will be popped off along with the rest of the group when pop() is called.
    pub fn push_to_scope(&mut self, scope: ScopeId) {
        self.scope.push(scope);
        *self.scope_lengths.last_mut().unwrap() += 1;
        self.symbols.entry(scope).or_insert(VecMap::new());
    }

    /// Pops the topmost group of scopes from the stack.
    pub fn pop(&mut self) {
        assert!(self.scope_lengths.len() > 1, "tried to leave the outermost scope!");
        for _ in 0..self.scope_lengths.pop().unwrap() {
            self.scope.pop().unwrap();
        }
    }

    /// Returns the current stack of scopes.
    pub fn get_scope_pos(&self) -> ScopePos {
        ScopePos {
            scope: self.scope.clone(),
            scope_lengths: self.scope_lengths.clone(),
        }
    }

    /// Sets a source identifer with the given scope identifier to a value.
    pub fn set_val(&mut self, id: Identifier, scope: ScopeId, val: T) {
        self.push_to_scope(scope);
        let scope_pos = self.get_scope_pos();
        let id_map = self.symbols.get_mut(&scope).unwrap();
        id_map.insert(id, Symbol {
                scope: scope_pos,
                val: val,
            });
    }

    /// Searches backwards through the scope stack until a symbol with the given identifier is
    /// found, and returns the symbol.
    pub fn get_symbol(&self, id: Identifier) -> Option<&Symbol<T>> {
        for block_pos in self.scope.iter().rev() {
            let id_map = self.symbols.get(&block_pos).unwrap();
            if let Some(symbol) = id_map.get(&id) {
                return Some(symbol)
            }
        }
        None
    }

    /// Searches backwards through the scope stack until a symbol with the given identifier is
    /// found, and returns the depth down the stack it was found at.
    pub fn get_symbol_depth(&self, id: Identifier) -> Option<usize> {
        for (idx, block_pos) in self.scope.iter().rev().enumerate() {
            let id_map = self.symbols.get(&block_pos).unwrap();
            if let Some(_) = id_map.get(&id) {
                return Some(idx)
            }
        }
        None
    }
}
