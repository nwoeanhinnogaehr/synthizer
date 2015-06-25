use super::ast;
use super::types::FunctionType;
use super::ident::Identifier;
use super::tokens::{Node, SourcePos};

use std::collections::{VecMap, BitSet};
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
    pub args: ast::ArgumentList,
    //fn ptr...
}

impl BuiltinFunction {
    pub fn new(ty: FunctionType) -> BuiltinFunction {
        let mut args = Vec::new();
        for (id, _) in &ty.args {
            args.push(ast::Argument(Some(Node(id, SourcePos::anon())), None));
        }
        BuiltinFunction {
            ty: ty,
            args: args,
        }
    }
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

#[derive(Debug)]
pub struct CallStack {
    stack: Vec<Identifier>,
    recursive: BitSet,
}

impl CallStack {
    pub fn new() -> CallStack {
        CallStack {
            stack: Vec::new(),
            recursive: BitSet::new(),
        }
    }
    pub fn push(&mut self, id: Identifier) {
        for &func in &self.stack {
            if func == id {
                self.recursive.insert(id);
                break;
            }
        }
        self.stack.push(id);
    }
    pub fn pop(&mut self) {
        self.stack.pop();
    }
    pub fn is_recursive(&self, id: Identifier) -> bool {
        self.recursive.contains(&id)
    }
}
