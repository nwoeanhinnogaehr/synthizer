use super::issue::{IssueTracker, Level};
use super::tokens::{Token, SourcePos, Node};
use super::ast::Root;
use super::types::TypeTable;
use super::ident::{Identifier, NameTable};
use super::functions::{FunctionTable, CallStack};

use std::cell::RefCell;
use std::borrow::Cow;

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
        Context {
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
        }
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
