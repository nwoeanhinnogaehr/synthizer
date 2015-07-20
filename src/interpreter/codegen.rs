use super::common::Context;
use super::ast::*;
use super::tokens::{Number, Boolean, NodeImpl, Node, Operator};
use super::types::{Type, TypeTable};
use super::scope::ScopedTable;
use super::ident::Identifier;
use super::functions::{self, FunctionTable};

use llvm;
use llvm::{Compile, ExecutionEngine, CastFrom};
use cbox::*;
use std::cell::{RefCell, Ref};

const GLOBAL_INIT_FN_NAME: &'static str = "__global_init";

pub fn codegen<'a>(ctxt: &'a Context<'a>) {
    CodeGenerator::new(ctxt).codegen();
}

struct CodeGenerator<'a> {
    ctxt: &'a Context<'a>,
    types: Ref<'a, TypeTable>,
    functions: Ref<'a, FunctionTable>,
    llvm: &'a llvm::Context,
    module: CSemiBox<'a, llvm::Module>,
    builder: CSemiBox<'a, llvm::Builder>,
    values: RefCell<ScopedTable<*const llvm::Value>>,
}

impl<'a> CodeGenerator<'a> {
    fn new(ctxt: &'a Context<'a>) -> CodeGenerator<'a> {
        CodeGenerator {
            ctxt: ctxt,
            types: ctxt.types.borrow(),
            functions: ctxt.functions.borrow(),
            llvm: &ctxt.llvm,
            module: llvm::Module::new(&ctxt.filename, &ctxt.llvm),
            builder: llvm::Builder::new(&ctxt.llvm),
            values: RefCell::new(ScopedTable::new()),
        }
    }

    fn codegen(&self) {
        self.codegen_root(&self.ctxt.ast.borrow());

        println!("{:?}", self.module);
        self.module.verify().unwrap();

        {
            let init = self.module.get_function(GLOBAL_INIT_FN_NAME).unwrap();
            let ee = llvm::JitEngine::new(&self.module, llvm::JitOptions { opt_level: 0 }).unwrap();
            //let ee = llvm::Interpreter::new(&self.module, ()).unwrap();
            ee.with_function(init, |init: extern fn(())| {
                init(());
            });
        }
    }

    fn codegen_root(&self, root: &Root) {
        let unit_ty = llvm::Type::get::<()>(self.llvm);
        let init_fn = self.module.add_function(GLOBAL_INIT_FN_NAME,
                                               llvm::Type::new_function(unit_ty, &[]));
        let mut block = init_fn.append("entry");
        self.builder.position_at_end(block);

        for item in root {
            match *item {
                Item::Assignment(ref x) => {
                    self.builder.position_at_end(block);
                    self.codegen_global_assignment(x, init_fn);
                    block = self.builder.get_position();
                },
                Item::FunctionDef(ref x) => {
                    self.codegen_global_function(x);
                }
            }
        }

        self.builder.build_ret_void();
    }

    fn codegen_global_function(&self, func: &FunctionDef) {
        let ident = func.ident();
        if !self.functions.get(ident).unwrap().has_concrete_type() {
            return; // in other words, the function was never used, so the type of it's signature is unknown
        }
        let name = self.ctxt.lookup_name(ident);
        let ty = self.type_to_llvm(self.types.get_symbol(ident).unwrap().val);
        let num_args = func.args().len();
        println!("TYPE {:?}", ty);
        let llvm_func = self.module.add_function(&name, ty);
        self.values.borrow_mut().set_val(ident, func.ident_pos().index,
                                         llvm_func as *const llvm::Function as *const llvm::Value);
        let entry = llvm_func.append("entry");
        self.builder.position_at_end(entry);
        self.values.borrow_mut().push(func.ident_pos().index);
        for i in 0..num_args {
            let ref arg = func.args()[i];
            &llvm_func[i].set_name(&self.ctxt.lookup_name(arg.ident().unwrap()));
            self.values.borrow_mut().set_val(arg.ident().unwrap(), arg.pos().index,
                                             &llvm_func[i] as *const llvm::Arg as *const llvm::Value);
        }
        let res = self.codegen_block(&func.block, llvm_func);
        self.builder.build_ret(res);
        self.values.borrow_mut().pop();
    }

    fn codegen_global_assignment(&self, assign: &Assignment, func: &llvm::Function) {
        let synt_ty = self.types.get_symbol(assign.ident()).unwrap().val;
        let ty = self.type_to_llvm(synt_ty);
        let name = &self.ctxt.lookup_name(assign.ident());
        let global = self.module.add_global(name, ty);
        global.set_initializer(llvm::Value::new_undef(ty));
        let val = self.codegen_expr(assign.expr(), func);
        self.builder.build_store(val, global);

        // hopefully there's some way to make this not use raw pointers.
        self.values.borrow_mut().set_val(assign.ident(), assign.ident_pos().index,
                                         global as *const llvm::GlobalValue as *const llvm::Value);
    }

    fn codegen_assignment(&self, assign: &Assignment, func: &llvm::Function) {
        let name = &self.ctxt.lookup_name(assign.ident());
        let val = self.codegen_expr(assign.expr(), func);
        val.set_name(name);

        // hopefully there's some way to make this not use raw pointers.
        self.values.borrow_mut().set_val(assign.ident(), assign.ident_pos().index,
                                         val as *const llvm::Value);
    }

    fn codegen_expr(&self, expr: &Expression, func: &llvm::Function) -> &llvm::Value {
        match *expr {
            Expression::Constant(Node(v, _)) => v.compile(self.llvm),
            Expression::Boolean(Node(v, _)) => v.compile(self.llvm),
            Expression::Infix(ref v) => self.codegen_infix(v, func),
            Expression::Prefix(ref v) => self.codegen_prefix(v, func),
            Expression::Variable(ref v) => self.codegen_var(**v, func),
            Expression::Conditional(ref v) => self.codegen_conditional(v, func),
            Expression::Block(ref v) => self.codegen_block(v, func),
            _ => unimplemented!(),
        }
    }

    fn codegen_block(&self, block: &Node<Block>, func: &llvm::Function) -> &llvm::Value {
        self.values.borrow_mut().push(block.pos().index);
        let mut value = None;
        for stmnt in block.item() {
            match *stmnt {
                Statement::Assignment(ref v) => {
                    self.codegen_assignment(v, func);
                },
                Statement::Expression(ref v) => {
                    let expr = self.codegen_expr(v, func);
                    if expr.get_type().is_float() {
                        value = match value {
                            None => Some(expr),
                            Some(v) => Some(self.builder.build_add(v, expr)),
                        };
                    } else {
                        value = Some(expr);
                    }
                },
            }
        }
        self.values.borrow_mut().pop();
        value.unwrap()
    }

    fn codegen_var(&self, ident: Identifier, _: &llvm::Function) -> &llvm::Value {
        let values = self.values.borrow();
        let sym = values.get_symbol(ident).unwrap();
        let value = unsafe { &*sym.val };
        if llvm::GlobalValue::cast(value).is_some() {
            self.builder.build_load(value)
        } else {
            value
        }
    }

    fn codegen_conditional(&self, cond: &Conditional, func: &llvm::Function) -> &llvm::Value {
        let cond_val = self.codegen_expr(cond.cond(), func);
        let then_block = func.append("then");
        let else_block = func.append("else");
        self.builder.build_cond_br(cond_val, then_block, Some(else_block));

        let merge_block = func.append("merge");

        self.builder.position_at_end(then_block);
        let then_val = self.codegen_expr(cond.then(), func);
        self.builder.build_br(merge_block);
        let then_block = self.builder.get_position();

        self.builder.position_at_end(else_block);
        let else_val = self.codegen_expr(cond.els(), func);
        self.builder.build_br(merge_block);
        let else_block = self.builder.get_position();

        self.builder.position_at_end(merge_block);
        assert!(then_val.get_type() == else_val.get_type());
        let phi = self.builder.build_phi(then_val.get_type(), "iftmp");
        phi.add_incoming(then_val, then_block);
        phi.add_incoming(else_val, else_block);
        phi
    }

    fn codegen_infix(&self, infix: &Infix, func: &llvm::Function) -> &llvm::Value {
        let lhs = self.codegen_expr(infix.left(), func);
        let rhs = self.codegen_expr(infix.right(), func);
        match infix.op() {
            Operator::Add => self.builder.build_add(lhs, rhs),
            Operator::Sub => self.builder.build_sub(lhs, rhs),
            Operator::Mul => self.builder.build_mul(lhs, rhs),
            Operator::Div => self.builder.build_div(lhs, rhs),
            Operator::Less => self.builder.build_cmp(lhs, rhs, llvm::Predicate::LessThan),
            Operator::Greater => self.builder.build_cmp(lhs, rhs, llvm::Predicate::GreaterThan),
            Operator::LessEqual => self.builder.build_cmp(lhs, rhs, llvm::Predicate::LessThanOrEqual),
            Operator::GreaterEqual => self.builder.build_cmp(lhs, rhs, llvm::Predicate::GreaterThanOrEqual),
            Operator::Equal => self.builder.build_cmp(lhs, rhs, llvm::Predicate::Equal),
            Operator::NotEqual => self.builder.build_cmp(lhs, rhs, llvm::Predicate::NotEqual),
            Operator::Or => self.builder.build_or(lhs, rhs),
            Operator::And => self.builder.build_and(lhs, rhs),
            Operator::Xor => self.builder.build_xor(lhs, rhs),
            _ => unreachable!(),
        }
    }

    fn codegen_prefix(&self, prefix: &Prefix, func: &llvm::Function) -> &llvm::Value {
        let expr = self.codegen_expr(prefix.expr(), func);
        match prefix.op() {
            Operator::Sub => self.builder.build_sub(0f32.compile(self.llvm), expr),
            Operator::Not => self.builder.build_not(expr),
            _ => unreachable!(),
        }
    }

    fn type_to_llvm(&self, ty: Type) -> &llvm::Type {
        match ty {
            Type::Number => llvm::Type::get::<Number>(self.llvm),
            Type::Boolean => llvm::Type::get::<Boolean>(self.llvm),
            Type::Function(id) => {
                let func = self.functions.get(id).unwrap();
                let ty = match *func {
                    functions::Function::User(ref f) => f.ty.as_ref().unwrap(),
                    functions::Function::Builtin(ref f) => &f.ty,
                };
                let args: Vec<&llvm::Type> = ty.args.iter().map(|(_, &ty)| self.type_to_llvm(ty)).collect();
                let ret = self.type_to_llvm(ty.returns);
                llvm::Type::new_function(ret, &args)
            }
            _ => unreachable!(),
        }
    }
}

