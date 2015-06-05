use super::ast;
use super::types::FunctionType;
use super::ident::Identifier;
use super::tokens::Node;

use std::collections::VecMap;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub enum Function {
    User(UserFunction),
    Builtin(BuiltinFunction),
}

#[derive(Debug, Clone)]
pub struct UserFunction {
    pub ty: Option<FunctionType>,
    pub node: Node<ast::Function>,
}

impl Deref for UserFunction {
    type Target = Node<ast::Function>;

    fn deref<'a>(&'a self) -> &'a Node<ast::Function> {
        &self.node
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinFunction {
    pub ty: FunctionType,
    //fn ptr...
}

#[derive(Debug)]
pub struct FunctionTable {
    map: VecMap<Function>, // from Identifier.
}

// Holds the actual implementation details of functions.
impl FunctionTable {
    pub fn new() -> FunctionTable {
        FunctionTable {
            map: VecMap::new(),
        }
    }

    pub fn insert(&mut self, ident: Identifier, func: Function) {
        // Insert will not overwrite the previous value unless we remove it first
        self.map.remove(&ident);
        self.map.insert(ident, func);
    }

    pub fn get<'a>(&'a self, ident: Identifier) -> Option<&'a Function> {
        self.map.get(&ident)
    }

    pub fn get_mut<'a>(&'a mut self, ident: Identifier) -> Option<&'a mut Function> {
        self.map.get_mut(&ident)
    }
}
