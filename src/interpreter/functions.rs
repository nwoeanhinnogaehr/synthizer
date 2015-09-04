use super::ast;
use super::types::{FunctionType};
use super::ident::Identifier;
use super::tokens::{Node, SourcePos};

use vec_map::VecMap;
use std::ops::Deref;
use bit_set::BitSet;

#[derive(Debug, Clone)]
pub enum Function {
    User(UserFunction),
    Pointer(PointerFunction),
    External(ExternalFunction),
}

impl Function {
    pub fn has_concrete_type(&self) -> bool {
        match *self {
            Function::User(ref f) => f.ty.is_some(),
            Function::Pointer(_) |
            Function::External(_) => true,
        }
    }
    pub fn args(&self) -> &ast::ArgumentList {
        match *self {
            Function::User(ref def) => { def.args() },
            Function::Pointer(ref def) => { &def.args }
            Function::External(ref def) => { &def.args }
        }
    }
    pub fn ty(&self) -> Option<&FunctionType> {
        match *self {
            Function::User(ref def) => { def.ty.as_ref() },
            Function::Pointer(ref def) => { Some(&def.ty) }
            Function::External(ref def) => { Some(&def.ty) }
        }
    }
    pub fn set_ty(&mut self, ty: FunctionType) {
        match *self {
            Function::User(ref mut def) => { def.ty = Some(ty) }
            Function::Pointer(ref mut def) => { def.ty = ty }
            Function::External(ref mut def) => { def.ty = ty }
        }
    }
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
pub struct ExternalFunction {
    pub ty: FunctionType,
    pub args: ast::ArgumentList,
    pub symbol: &'static str,
}

impl ExternalFunction {
    pub fn new(symbol: &'static str, ty: FunctionType) -> ExternalFunction {
        let mut args = Vec::new();
        for (id, _) in &ty.args {
            args.push(ast::Argument::Ident(Node(id, SourcePos::anon())));
        }
        ExternalFunction {
            ty: ty,
            args: args,
            symbol: symbol,
        }
    }
}

#[derive(Debug, Clone)]
#[allow(raw_pointer_derive)]
pub struct PointerFunction {
    pub ty: FunctionType,
    pub args: ast::ArgumentList,
    pub ptr: *mut (),
}

impl PointerFunction {
    pub fn new(ty: FunctionType, ptr: *mut ()) -> PointerFunction {
        let mut args = Vec::new();
        for (id, _) in &ty.args {
            args.push(ast::Argument::Ident(Node(id, SourcePos::anon())));
        }
        PointerFunction {
            ty: ty,
            args: args,
            ptr: ptr,
        }
    }
}

#[derive(Debug)]
pub struct FunctionTable {
    pub map: VecMap<Function>, // from Identifier.
}

// Holds the actual implementation details of functions.
impl FunctionTable {
    pub fn new() -> FunctionTable {
        FunctionTable {
            map: VecMap::new(),
        }
    }

    pub fn insert(&mut self, ident: Identifier, func: Function) {
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
