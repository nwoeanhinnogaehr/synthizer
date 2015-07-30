use super::issue::{IssueTracker, Level};
use super::tokens::{Token, SourcePos, Node};
use super::ast::Root;
use super::types::TypeTable;
use super::ident::{Identifier, NameTable};
use super::functions::{self, FunctionTable, CallStack};
use super::types::{Type, FunctionType};

use std::cell::RefCell;
use std::borrow::Cow;
use std::collections::VecMap;

use llvm;
use cbox::CBox;

pub struct Context<'a> {
    pub filename: String,
    pub source: String,
    pub issues: RefCell<IssueTracker<'a>>,
    pub types: RefCell<TypeTable>,
    pub names: RefCell<NameTable<'a>>,
    pub functions: RefCell<FunctionTable>,
    pub tokens: RefCell<Vec<Node<Token>>>,
    pub ast: RefCell<Root>,
    pub callstack: RefCell<CallStack>,
    pub llvm: CBox<llvm::Context>,
}

impl<'a> Context<'a> {
    pub fn new(filename: String, source: String) -> Context<'a> {
        let ctx = Context {
            filename: filename,
            source: source,
            issues: RefCell::new(IssueTracker::new()),
            types: RefCell::new(TypeTable::new()),
            names: RefCell::new(NameTable::new()),
            functions: RefCell::new(FunctionTable::new()),
            tokens: RefCell::new(Vec::new()),
            ast: RefCell::new(Vec::new()),
            callstack: RefCell::new(CallStack::new()),
            llvm: llvm::Context::new(),
        };
        ctx.add_intrinsics();
        ctx
    }

    pub fn emit_error<T>(&'a self, msg: T, pos: SourcePos) where T: Into<Cow<'static, str>> {
        self.issues.borrow_mut().new_issue(self, pos, Level::Error, msg);
    }
    pub fn emit_warning<T>(&'a self, msg: T, pos: SourcePos) where T: Into<Cow<'static, str>> {
        self.issues.borrow_mut().new_issue(self, pos, Level::Warning, msg);
    }

    pub fn lookup_name(&'a self, id: Identifier) -> String {
        self.names.borrow().get_name(id).unwrap().into()
    }

    pub fn add_external_function(&self, name: &'static str, symbol: &'static str, ty: &FunctionType) {
        let id = self.names.borrow_mut().new_id(name);
        let func = functions::Function::External(functions::ExternalFunction::new(symbol, ty.clone()));
        let ty = Type::Function(id);
        self.functions.borrow_mut().insert(id, func);
        self.types.borrow_mut().set_val(id, 0, ty);
    }

    pub fn add_intrinsics(&self) {
        let x_id = self.names.borrow_mut().new_id("x");
        let mut arg_map = VecMap::new();
        arg_map.insert(x_id, Type::Number);
        let num_num_ty = &FunctionType {
            returns: Type::Number,
            args: arg_map
        };
        self.add_external_function("sin", "llvm.sin.f32", num_num_ty);
        self.add_external_function("cos", "llvm.cos.f32", num_num_ty);
        self.add_external_function("log", "llvm.log.f32", num_num_ty);
        self.add_external_function("log10", "llvm.log10.f32", num_num_ty);
        self.add_external_function("log2", "llvm.log2.f32", num_num_ty);
        self.add_external_function("exp", "llvm.exp.f32", num_num_ty);
        self.add_external_function("exp2", "llvm.exp2.f32", num_num_ty);
        self.add_external_function("sqrt", "llvm.sqrt.f32", num_num_ty);
        self.add_external_function("abs", "llvm.fabs.f32", num_num_ty);
        self.add_external_function("floor", "llvm.floor.f32", num_num_ty);
        self.add_external_function("ceil", "llvm.ceil.f32", num_num_ty);
        self.add_external_function("trunc", "llvm.trunc.f32", num_num_ty);
        self.add_external_function("round", "llvm.round.f32", num_num_ty);
    }
}

pub fn read_file(filename: &str) -> Result<String, String> {
    use std::fs::File;
    use std::io::Read;

    let mut file = match File::open(filename) {
        Err(why) => {
            return Err(format!("couldn't open {}: {}", filename, why));
        }
        Ok(file) => file,
    };
    let mut code = String::new();
    match file.read_to_string(&mut code) {
        Err(why) => {
            return Err(format!("couldn't read {}: {}", filename, why));
        }
        Ok(_) => { }
    }
    return Ok(code);
}
