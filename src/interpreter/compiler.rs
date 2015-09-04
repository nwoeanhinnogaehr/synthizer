use super::common::Context;
use super::codegen::{CodeGenerator, GLOBAL_INIT_FN_NAME};
use super::lexer::lex;
use super::parser::parse;
use super::typecheck::typecheck;
use super::types::{Type, FunctionType};
use super::functions::{ExternalFunction, PointerFunction, Function};
use super::issue::IssueTracker;

use llvm;
use llvm::ExecutionEngine;
use std::mem;

pub struct Compiler<'a> {
    ctxt: &'a Context<'a>,
    codegen: Option<CodeGenerator<'a>>,
    engine: Option<llvm::JitEngine<'a>>,
    stage: Stage,
}

#[derive(Debug, PartialEq)]
pub enum Stage {
    Lex,
    Parse,
    Typecheck,
    Codegen,
    Complete
}

impl<'a> Compiler<'a> {
    pub fn new(ctxt: &'a Context<'a>) -> Compiler<'a> {
        Compiler {
            ctxt: ctxt,
            codegen: None,
            engine: None,
            stage: Stage::Lex,
        }
    }

    pub fn compile(&mut self) -> Result<IssueTracker<'a>, IssueTracker<'a>> {
        // front end
        self.define_intrinsics();
        if !self.lex() {
            return Err(self.ctxt.issues.borrow().clone());
        }
        if !self.parse() {
            return Err(self.ctxt.issues.borrow().clone());
        }
        if !self.typecheck() {
            return Err(self.ctxt.issues.borrow().clone());
        }
        if !self.codegen() {
            return Err(self.ctxt.issues.borrow().clone());
        }
        Ok(self.ctxt.issues.borrow().clone())
    }

    pub fn lex(&mut self) -> bool {
        assert_eq!(self.stage, Stage::Lex);
        self.stage = Stage::Parse;
        lex(self.ctxt);
        return if self.ctxt.issues.borrow().has_errors() {
            false
        } else {
            self.stage = Stage::Parse;
            true
        }
    }
    pub fn parse(&mut self) -> bool {
        assert_eq!(self.stage, Stage::Parse);
        parse(self.ctxt);
        return if self.ctxt.issues.borrow().has_errors() {
            false
        } else {
            self.stage = Stage::Typecheck;
            true
        }
    }
    pub fn typecheck(&mut self) -> bool {
        assert_eq!(self.stage, Stage::Typecheck);
        typecheck(self.ctxt);
        return if self.ctxt.issues.borrow().has_errors() {
            false
        } else {
            self.stage = Stage::Codegen;
            true
        }
    }
    pub fn codegen(&mut self) -> bool {
        assert_eq!(self.stage, Stage::Codegen);
        let cg = CodeGenerator::new(self.ctxt);
        let cg_ptr: &'a CodeGenerator<'a> = unsafe { mem::transmute(&cg) };
        cg_ptr.codegen();
        self.engine = Some(llvm::JitEngine::new(&cg_ptr.module, llvm::JitOptions { opt_level: 3 }).unwrap());
        self.codegen = Some(cg);
        return if self.ctxt.issues.borrow().has_errors() {
            false
        } else {
            self.stage = Stage::Complete;
            true
        }
    }

    pub unsafe fn define_pointer_function(&self, name: &'static str, ty: FunctionType, ptr: *mut ()) {
        assert_eq!(self.stage, Stage::Lex);
        let id = self.ctxt.names.borrow_mut().new_id(name);
        let func = Function::Pointer(PointerFunction::new(ty, mem::transmute(ptr)));
        let ty = Type::Function(id);
        self.ctxt.functions.borrow_mut().insert(id, func);
        self.ctxt.types.borrow_mut().set_val(id, 0, ty);
    }

    pub fn define_external_function(&self, name: &'static str, symbol: &'static str, ty: FunctionType) {
        assert_eq!(self.stage, Stage::Lex);
        let id = self.ctxt.names.borrow_mut().new_id(name);
        let func = Function::External(ExternalFunction::new(symbol, ty));
        let ty = Type::Function(id);
        self.ctxt.functions.borrow_mut().insert(id, func);
        self.ctxt.types.borrow_mut().set_val(id, 0, ty);
    }

    pub fn define_intrinsics(&self) {
        let num_num_ty = &make_fn_ty!(self.ctxt, fn(x: Number) -> Number);
        let num_2num_ty = &make_fn_ty!(self.ctxt, fn(x: Number, y: Number) -> Number);

        self.define_external_function("sin", "llvm.sin.f64", num_num_ty.clone());
        self.define_external_function("cos", "llvm.cos.f64", num_num_ty.clone());
        self.define_external_function("log", "llvm.log.f64", num_num_ty.clone());
        self.define_external_function("log10", "llvm.log10.f64", num_num_ty.clone());
        self.define_external_function("log2", "llvm.log2.f64", num_num_ty.clone());
        self.define_external_function("exp", "llvm.exp.f64", num_num_ty.clone());
        self.define_external_function("exp2", "llvm.exp2.f64", num_num_ty.clone());
        self.define_external_function("sqrt", "llvm.sqrt.f64", num_num_ty.clone());
        self.define_external_function("abs", "llvm.fabs.f64", num_num_ty.clone());
        self.define_external_function("floor", "llvm.floor.f64", num_num_ty.clone());
        self.define_external_function("ceil", "llvm.ceil.f64", num_num_ty.clone());
        self.define_external_function("trunc", "llvm.trunc.f64", num_num_ty.clone());
        self.define_external_function("round", "llvm.round.f64", num_num_ty.clone());

        self.define_external_function("pow", "llvm.pow.f64", num_2num_ty.clone());
        self.define_external_function("min", "llvm.minnum.f64", num_2num_ty.clone());
        self.define_external_function("max", "llvm.maxnum.f64", num_2num_ty.clone());
    }

    /// Defines a function as externally accessible through get_fn after compilation
    pub fn define_entrypoint(&self, name: &'a str, ty: FunctionType) {
        // must be done before typecheck
        assert!(self.stage != Stage::Complete && self.stage != Stage::Codegen);
        let id = self.ctxt.names.borrow_mut().new_id(name);
        self.ctxt.entrypoints.borrow_mut().insert(id, ty);
    }

    pub unsafe fn get_fn<A, R>(&self, name: &str) -> Option<extern fn(A) -> R> {
        assert_eq!(self.stage, Stage::Complete);
        match self.codegen.as_ref().unwrap().module.get_function(name) {
            Some(f) => {
                Some(self.engine.as_ref().unwrap().get_function(f))
            },
            None => None,
        }
    }

    pub fn get_init_fn(&self) -> extern fn(()) {
        unsafe {
            self.get_fn(GLOBAL_INIT_FN_NAME).unwrap()
        }
    }
}
