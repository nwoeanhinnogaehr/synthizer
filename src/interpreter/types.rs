use super::ident::Identifier;

use std::collections::{HashMap, VecMap};
use std::fmt;

#[derive(Clone, Debug)]
pub struct Symbol {
    pub scope: Vec<BlockPos>,
    pub id: Identifier,
    pub ty: Option<Type>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Type {
    Number,
    Boolean,
    Function(Identifier),

    /// With recursive functions, it may not be possible to tell exactly what the type is without
    /// further information. This is different from a None type, which means that this is a
    /// logically inconsistent or indeterminable type. The user should never see this.
    Indeterminate,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Number => write!(f, "Number"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Function(_) => write!(f, "Function"),
            Type::Indeterminate => write!(f, "Indeterminate"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub args: VecMap<Type>, // From Identifier to Type
    pub returns: Type,
}

impl FunctionType {
    pub fn new(args: VecMap<Type>, returns: Type) -> FunctionType {
        FunctionType {
            args: args,
            returns: returns,
        }
    }
}

/// The source index of the opening brace that defines the start of a scope.
pub type BlockPos = usize;

#[derive(Debug)]
pub struct TypeTable {
    symbols: HashMap<BlockPos, VecMap<Symbol>>, // From Identifier to Symbol
    scope: Vec<BlockPos>,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        let mut symbols = HashMap::new();
        symbols.insert(0, VecMap::new());
        TypeTable {
            symbols: symbols,
            scope: vec![0],
        }
    }

    pub fn enter_block(&mut self, scope: BlockPos) {
        self.scope.push(scope);
        self.symbols.entry(scope).or_insert(VecMap::new());
    }
    pub fn leave_block(&mut self) {
        assert!(self.scope.len() > 1, "tried to leave the outermost scope!");
        self.scope.pop();
    }

    pub fn has_scope_cycle(&self, pos: BlockPos) -> bool {
        for &scope in self.scope.iter() {
            if scope == pos {
                return true;
            }
        }
        false
    }

    // Always sets the type in the innermost scope.
    pub fn set_type(&mut self, id: Identifier, ty: Option<Type>) {
        let block_pos = *self.scope.last().unwrap();
        let id_map = self.symbols.get_mut(&block_pos).unwrap();
        let scope = self.scope.clone();
        let symbol = id_map.entry(id).or_insert(Symbol {
                scope: scope,
                id: id,
                ty: None,
            });
        symbol.ty = ty;
    }
    // Also returns the depth down the scope stack that it had to search to find the symbol.
    pub fn get_symbol(&self, id: Identifier) -> Option<(&Symbol, usize)> {
        self.get_symbol_within(&self.scope, id)
    }
    // Allows you to specify the scope stack to search within.
    pub fn get_symbol_within(&self,
                             scope: &[BlockPos],
                             id: Identifier) -> Option<(&Symbol, usize)> {
        for (idx, block_pos) in scope.iter().rev().enumerate() {
            let id_map = self.symbols.get(&block_pos).unwrap();
            if let Some(symbol) = id_map.get(&id) {
                return Some((symbol, idx))
            }
        }
        None
    }
}
