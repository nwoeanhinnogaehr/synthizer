use super::common::Context;
use super::codegen::{CodeGenerator, GLOBAL_INIT_FN_NAME};
use super::lexer::lex;
use super::parser::parse;
use super::typecheck::typecheck;
use super::tokens::Number;
use super::types::{Type, FunctionType};
use super::functions::{ExternalFunction, Function};

use std::collections::VecMap;
use std::cell::RefCell;
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
    codegen: RefCell<Option<CodeGenerator<'a>>>,
    engine: RefCell<Option<llvm::JitEngine<'a>>>,
}

impl<'a> Compiler<'a> {
    pub fn new(ctxt: &'a Context<'a>) -> Compiler<'a> {
        Compiler {
            ctxt: ctxt,
            codegen: RefCell::new(None),
            engine: RefCell::new(None),
        }
    }

    pub fn compile(&self) -> bool {
        self.add_intrinsics();
        self.define_entrypoints();

        // front end
        lex(self.ctxt);
        parse(self.ctxt);
        typecheck(self.ctxt);
        println!("{}", *self.ctxt.issues.borrow());
        if self.ctxt.issues.borrow().has_errors() {
            return false;
        }

        // XXX backend
        let cg = CodeGenerator::new(self.ctxt);
        let cg_ptr: &'a CodeGenerator<'a> = unsafe { mem::transmute(&cg) };
        cg_ptr.codegen();
        let mut engine = self.engine.borrow_mut();
        *engine = Some(llvm::JitEngine::new(&cg_ptr.module, llvm::JitOptions { opt_level: 3 }).unwrap());
        let mut codegen = self.codegen.borrow_mut();
        *codegen = Some(cg);
        true
    }

    pub fn add_external_function(&self, name: &'static str, symbol: &'static str, ty: &FunctionType) {
        let id = self.ctxt.names.borrow_mut().new_id(name);
        let func = Function::External(ExternalFunction::new(symbol, ty.clone()));
        let ty = Type::Function(id);
        self.ctxt.functions.borrow_mut().insert(id, func);
        self.ctxt.types.borrow_mut().set_val(id, 0, ty);
    }

    fn add_intrinsics(&self) {
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

        self.add_external_function("pow", "llvm.pow.f32", num_2num_ty);
        self.add_external_function("min", "llvm.minnum.f32", num_2num_ty);
        self.add_external_function("max", "llvm.maxnum.f32", num_2num_ty);
    }

    fn define_entrypoints(&self) {

    }

    unsafe fn get_fn<A, R>(&self, name: &'static str) -> Option<extern fn(A) -> R> {
        match self.codegen.borrow().as_ref().unwrap().module.get_function(name) {
            Some(f) => {
                Some(self.engine.borrow().as_ref().unwrap().get_function(f))
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
