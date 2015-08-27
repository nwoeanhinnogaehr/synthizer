use super::common::Context;
use super::codegen::{CodeGenerator, GLOBAL_INIT_FN_NAME};
use super::lexer::lex;
use super::parser::parse;
use super::typecheck::typecheck;
use super::tokens::Number;
use super::types::{Type, FunctionType};
use super::functions::{ExternalFunction, Function};
use super::issue::IssueTracker;

use vec_map::VecMap;
use llvm;
use llvm::ExecutionEngine;
use std::mem;
use std::marker::PhantomData;

pub struct EntryPoints<'a> {
    pub init: extern fn(()),
    pub main: Option<extern fn(Number) -> Number>,
    _ghost: PhantomData<&'a ()>,
}

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
        self.define_entrypoints();
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
        return !self.ctxt.issues.borrow().has_errors();
    }
    pub fn parse(&mut self) -> bool {
        assert_eq!(self.stage, Stage::Parse);
        self.stage = Stage::Typecheck;
        parse(self.ctxt);
        return !self.ctxt.issues.borrow().has_errors();
    }
    pub fn typecheck(&mut self) -> bool {
        assert_eq!(self.stage, Stage::Typecheck);
        self.stage = Stage::Codegen;
        typecheck(self.ctxt);
        return !self.ctxt.issues.borrow().has_errors();
    }
    pub fn codegen(&mut self) -> bool {
        assert_eq!(self.stage, Stage::Codegen);
        self.stage = Stage::Complete;
        let cg = CodeGenerator::new(self.ctxt);
        let cg_ptr: &'a CodeGenerator<'a> = unsafe { mem::transmute(&cg) };
        cg_ptr.codegen();
        self.engine = Some(llvm::JitEngine::new(&cg_ptr.module, llvm::JitOptions { opt_level: 3 }).unwrap());
        self.codegen = Some(cg);
        return !self.ctxt.issues.borrow().has_errors();
    }

    pub fn define_external_function(&self, name: &'static str, symbol: &'static str, ty: &FunctionType) {
        let id = self.ctxt.names.borrow_mut().new_id(name);
        let func = Function::External(ExternalFunction::new(symbol, ty.clone()));
        let ty = Type::Function(id);
        self.ctxt.functions.borrow_mut().insert(id, func);
        self.ctxt.types.borrow_mut().set_val(id, 0, ty);
    }

    pub fn define_intrinsics(&self) {
        let x_id = self.ctxt.names.borrow_mut().new_id("x");
        let y_id = self.ctxt.names.borrow_mut().new_id("y");
        let mut arg_map = VecMap::new();
        arg_map.insert(x_id, Type::Number);
        let num_num_ty = &FunctionType {
            returns: Type::Number,
            args: arg_map.clone()
        };
        arg_map.insert(y_id, Type::Number);
        let num_2num_ty = &FunctionType {
            returns: Type::Number,
            args: arg_map.clone()
        };

        self.define_external_function("sin", "llvm.sin.f64", num_num_ty);
        self.define_external_function("cos", "llvm.cos.f64", num_num_ty);
        self.define_external_function("log", "llvm.log.f64", num_num_ty);
        self.define_external_function("log10", "llvm.log10.f64", num_num_ty);
        self.define_external_function("log2", "llvm.log2.f64", num_num_ty);
        self.define_external_function("exp", "llvm.exp.f64", num_num_ty);
        self.define_external_function("exp2", "llvm.exp2.f64", num_num_ty);
        self.define_external_function("sqrt", "llvm.sqrt.f64", num_num_ty);
        self.define_external_function("abs", "llvm.fabs.f64", num_num_ty);
        self.define_external_function("floor", "llvm.floor.f64", num_num_ty);
        self.define_external_function("ceil", "llvm.ceil.f64", num_num_ty);
        self.define_external_function("trunc", "llvm.trunc.f64", num_num_ty);
        self.define_external_function("round", "llvm.round.f64", num_num_ty);

        self.define_external_function("pow", "llvm.pow.f64", num_2num_ty);
        self.define_external_function("min", "llvm.minnum.f64", num_2num_ty);
        self.define_external_function("max", "llvm.maxnum.f64", num_2num_ty);
    }

    pub fn define_entrypoints(&self) {
        let main_id = self.ctxt.names.borrow_mut().new_id("main");
        let time_id = self.ctxt.names.borrow_mut().new_id("time");
        let mut arg_map = VecMap::new();
        arg_map.insert(time_id, Type::Number);
        let main_ty = FunctionType {
            returns: Type::Number,
            args: arg_map.clone()
        };
        self.ctxt.entrypoints.borrow_mut().insert(main_id, main_ty);
    }

    unsafe fn get_fn<A, R>(&self, name: &'static str) -> Option<extern fn(A) -> R> {
        match self.codegen.as_ref().unwrap().module.get_function(name) {
            Some(f) => {
                Some(self.engine.as_ref().unwrap().get_function(f))
            },
            None => None,
        }
    }

    pub fn get_entrypoints(&self) -> EntryPoints<'a> {
        unsafe {
            EntryPoints {
                _ghost: PhantomData,
                init: self.get_fn(GLOBAL_INIT_FN_NAME).unwrap(),
                main: self.get_fn("main"),
            }
        }
    }
}
