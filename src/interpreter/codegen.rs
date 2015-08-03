use super::common::Context;
use super::ast::*;
use super::tokens::{Number, Boolean, NodeImpl, Node, Operator, SourcePos};
use super::types::{Type, TypeTable};
use super::scope::ScopedTable;
use super::ident::Identifier;
use super::functions::{self, FunctionTable, ExternalFunction};

use llvm;
use llvm::{Compile, ExecutionEngine, CastFrom};
use cbox::*;
use std::cell::{RefCell, Ref};
use std::collections::VecMap;
use std::ops::Deref;
use std::mem;
use std::rc::Rc;

#[derive(Clone, Debug)]
struct FnSignature {
    ret: Option<Rc<RefCell<FnSignature>>>,
    args: VecMap<(Option<usize>, Option<Rc<RefCell<FnSignature>>>)>
}

#[derive(Clone)]
pub struct ValueWrapper<'a> {
    value: &'a llvm::Value,
    sig: Option<Rc<RefCell<FnSignature>>>,
}

impl<'a> ValueWrapper<'a> {
    fn new(value: &'a llvm::Value, sig: Option<Rc<RefCell<FnSignature>>>) -> ValueWrapper<'a> {
        ValueWrapper {
            value: value,
            sig: sig
        }
    }

    fn map(&self, value: &'a llvm::Value) -> ValueWrapper<'a> {
        ValueWrapper {
            value: value,
            sig: self.sig.clone()
        }
    }
}

impl<'a> Into<ValueWrapper<'a>> for &'a llvm::Value {
    fn into(self) -> ValueWrapper<'a> {
        ValueWrapper::new(self, None)
    }
}

impl<'a> Deref for ValueWrapper<'a> {
    type Target = &'a llvm::Value;
    fn deref<'b>(&'b self) -> &'b &'a llvm::Value {
        &self.value
    }
}

pub const GLOBAL_INIT_FN_NAME: &'static str = "__global_init";

pub fn codegen<'a>(ctxt: &'a Context<'a>) {
    //XXX
    let cg = CodeGenerator::new(ctxt);
    let foo: &'a CodeGenerator<'a> = unsafe { mem::transmute(&cg) };
    foo.codegen();
}

pub struct CodeGenerator<'a> {
    ctxt: &'a Context<'a>,
    types: Ref<'a, TypeTable>,
    functions: Ref<'a, FunctionTable>,
    llvm: &'a llvm::Context,
    pub module: CSemiBox<'a, llvm::Module>,
    builder: CSemiBox<'a, llvm::Builder>,
    values: RefCell<ScopedTable<ValueWrapper<'a>>>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(ctxt: &'a Context<'a>) -> CodeGenerator<'a> {
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

    pub fn codegen(&'a self) {
        for (ident, func) in &self.functions.map {
            match *func {
                functions::Function::External(ref def) => {
                    self.codegen_external_function(ident, def);
                },
                _ => { },
            }
        }
        self.codegen_root(&self.ctxt.ast.borrow());

        println!("{:?}", self.module);
        self.module.verify().unwrap();
    }

    fn codegen_root(&'a self, root: &Root) {
        let unit_ty = llvm::Type::get::<()>(self.llvm);
        let init_fn = self.module.add_function(GLOBAL_INIT_FN_NAME,
                                               llvm::Type::new_function(unit_ty, &[]));
        let block = init_fn.append("entry");
        self.builder.position_at_end(block);

        for item in root {
            match *item {
                Item::Assignment(ref x) => {
                    self.codegen_global_assignment(x, init_fn);
                },
                Item::FunctionDef(ref x) => {
                    self.codegen_global_function(x, init_fn);
                }
            }
        }

        self.builder.build_ret_void();
    }

    fn codegen_external_function(&'a self, ident: Identifier, func: &ExternalFunction) -> ValueWrapper<'a> {
        let ty = Type::Function(ident);
        let func = self.module.add_function(func.symbol, self.type_to_llvm(ty, false));
        let val = ValueWrapper::new(func, self.type_to_signature(ty));
        self.store_val(Node(ident, SourcePos::anon()), val.clone());
        val
    }

    fn codegen_global_function(&'a self, func: &FunctionDef, owning_fn: &llvm::Function) -> ValueWrapper<'a> {
        let owning_block = self.builder.get_position();
        let ident = func.ident();
        let name = self.ctxt.lookup_name(ident);

        let (llvm_func, func_struct, sig, func_args) = self.codegen_function_decl(func, owning_fn, &name);

        let struct_ty = func_struct.get_type();
        let g_struct = self.module.add_global(&name, struct_ty);
        g_struct.set_initializer(llvm::Value::new_undef(struct_ty));
        self.builder.build_store(func_struct, g_struct);

        self.codegen_function_body(func, func_args, llvm_func, sig, owning_block, g_struct)
    }

    fn codegen_closure(&'a self, func: &FunctionDef, owning_fn: &llvm::Function) -> ValueWrapper<'a> {
        let owning_block = self.builder.get_position();
        let (llvm_func, func_struct, sig, func_args) = self.codegen_function_decl(func, owning_fn, "closure");
        self.codegen_function_body(func, func_args, llvm_func, sig, owning_block, func_struct)
    }

    fn codegen_function_body(&'a self, func: &FunctionDef, func_args: Vec<Argument>,
                             llvm_func: &'a llvm::Function, sig: Rc<RefCell<FnSignature>>,
                             owning_block: &llvm::BasicBlock, g_struct: &'a llvm::Value) -> ValueWrapper<'a> {
        self.store_val(func.ident, ValueWrapper::new(g_struct, Some(sig.clone())));

        // setup args
        self.values.borrow_mut().push(func.ident_pos().index);
        for i in 0..func.args.len() {
            let ref arg = func_args[i];
            let id = arg.ident().unwrap();
            llvm_func[i].set_name(&self.ctxt.lookup_name(id));
            self.store_val(Node(id, arg.pos()),
                           ValueWrapper::new(&llvm_func[i], sig.borrow().args[id].1.clone()));
        }
        // codegen block
        let entry = llvm_func.append("entry");
        self.builder.position_at_end(entry);
        let res = self.codegen_block(&func.block, llvm_func);
        self.builder.build_ret(res.value);
        self.values.borrow_mut().pop();
        self.builder.position_at_end(owning_block);

        // update the signature to add in the signature of the return value
        sig.borrow_mut().ret = res.sig.clone();
        self.store_val(func.ident, ValueWrapper::new(g_struct, Some(sig.clone())));

        ValueWrapper::new(g_struct, Some(sig))
    }

    fn codegen_function_decl(&'a self, func: &FunctionDef, owning_fn: &llvm::Function, name: &str) ->
            (&llvm::Function, &llvm::Value, Rc<RefCell<FnSignature>>, Vec<Argument>) {
        let ident = func.ident();
        let synt_ty = self.types.get_symbol(ident).unwrap().val;
        let ty = self.type_to_llvm(synt_ty, false);
        let sig = self.type_to_signature(synt_ty).unwrap();
        let num_args = func.args().len();
        let mut func_args = func.args().clone();
        func_args.sort_by(|a, b| a.ident().cmp(&b.ident()));
        let llvm_func = self.module.add_function(name, ty);
        llvm_func.add_attributes(&[llvm::Attribute::NoUnwind]);

        let mut fn_struct_values: Vec<&llvm::Value> = Vec::new();
        fn_struct_values.push(llvm_func);

        // catalog default args and set their signatures
        let mut default_idx = 0;
        for i in 0..num_args {
            let ref arg = func_args[i];

            let default = if let Argument::Assign(_, ref expr) = *arg {
                let default_val = self.codegen_expr(expr, owning_fn);
                fn_struct_values.push(default_val.value);
                default_idx += 1;
                Some(default_idx)
            } else {
                None
            };

            sig.borrow_mut().args[arg.ident().unwrap()].0 = default;
        }
        let func_struct = llvm::Value::new_struct(self.llvm, &fn_struct_values, true);
        (llvm_func, func_struct, sig, func_args)
    }

    fn codegen_global_assignment(&'a self, assign: &Assignment, func: &llvm::Function) {
        let val = self.codegen_expr(assign.expr(), func);
        let name = &self.ctxt.lookup_name(assign.ident());
        let global = self.module.add_global(name, val.get_type());
        global.set_initializer(llvm::Value::new_undef(val.get_type()));
        self.builder.build_store(val.value, global);
        self.store_val(assign.ident, val);
    }

    fn store_val(&self, ident: Node<Identifier>, value: ValueWrapper<'a>) {
        self.values.borrow_mut().set_val(*ident, ident.pos().index, value);
    }

    fn codegen_assignment(&'a self, assign: &Assignment, func: &llvm::Function) {
        let name = &self.ctxt.lookup_name(assign.ident());
        let val = self.codegen_expr(assign.expr(), func);
        val.set_name(name);

        self.store_val(assign.ident, val);
    }

    fn codegen_expr(&'a self, expr: &Expression, func: &llvm::Function) -> ValueWrapper<'a> {
        match *expr {
            Expression::Constant(Node(v, _)) => v.compile(self.llvm).into(),
            Expression::Boolean(Node(v, _)) => v.compile(self.llvm).into(),
            Expression::Infix(ref v) => self.codegen_infix(v, func),
            Expression::Prefix(ref v) => self.codegen_prefix(v, func),
            Expression::Variable(ref v) => self.codegen_var(*v, func),
            Expression::Conditional(ref v) => self.codegen_conditional(v, func),
            Expression::Block(ref v) => self.codegen_block(v, func),
            Expression::Closure(ref v) => self.codegen_closure(v, func),
            Expression::FunctionCall(ref v) => self.codegen_function_call(v, func),
        }
    }

    fn codegen_struct_load(&self, val: &llvm::Value, index: usize) -> &llvm::Value {
        if val.get_type().is_pointer() {
            let ptr = self.builder.build_gep(val,
                                             &[0.compile(self.llvm),
                                               (index as i32).compile(self.llvm)]);
            self.builder.build_load(ptr)
        } else {
            self.builder.build_extract_value(val, index)
        }
    }

    fn codegen_function_call(&'a self, call: &FunctionCall, func: &llvm::Function) -> ValueWrapper<'a> {
        let callee_expr = self.codegen_expr(call.callee(), func);
        let callee = match llvm::Function::cast(callee_expr.value) {
            Some(callee) => callee,
            None => llvm::Function::cast(self.codegen_struct_load(callee_expr.value, 0)).unwrap(),
        };
        let sig = callee_expr.sig.as_ref().unwrap().borrow();
        let mut call_args = VecMap::new();
        match call.ty {
            CallType::Named => {
                for (id, &(default, _)) in sig.args.iter() {
                    let mut found = false;
                    for arg in call.args() {
                        match *arg {
                            Argument::Assign(ident, ref expr) => {
                                if id == *ident {
                                    call_args.insert(id, self.codegen_expr(expr, func).value);
                                    found = true;
                                }
                            },
                            Argument::OpAssign(ident, Node(op, _), ref expr) => {
                                if id == *ident {
                                    let lhs = self.codegen_var(ident, func);
                                    let rhs = self.codegen_expr(expr, func);
                                    let arg_value = self.codegen_binary_op(op, lhs, rhs);
                                    call_args.insert(id, arg_value.value);
                                    found = true;
                                }
                            }
                            Argument::Ident(ident) => {
                                if id == *ident {
                                    call_args.insert(id, self.codegen_var(ident, func).value);
                                    found = true;
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    if !found {
                        call_args.insert(id, self.codegen_struct_load(callee_expr.value, default.unwrap()));
                    }
                }
            },
            CallType::Ordered => {
                let mut sig_args = sig.args.iter();
                for arg in call.args() {
                    match *arg {
                        Argument::Expr(ref expr) => {
                            call_args.insert(sig_args.next().unwrap().0, self.codegen_expr(expr, func).value);
                        }
                        _ => unreachable!(),
                    }
                }
                for (id, &(default, _)) in sig_args {
                    call_args.insert(id, self.codegen_struct_load(callee_expr.value, default.unwrap()));
                }
            }
        }

        let arg_vec: Vec<_> = call_args.values().map(|x| *x).collect();
        ValueWrapper::new(self.builder.build_call(callee, &arg_vec), sig.ret.clone())
    }

    fn codegen_block(&'a self, block: &Node<Block>, func: &llvm::Function) -> ValueWrapper<'a> {
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
                            Some(v) => Some(v.map(self.builder.build_add(*v, *expr))),
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

    fn codegen_var(&'a self, ident: Node<Identifier>, _: &llvm::Function) -> ValueWrapper<'a> {
        let (value, sig) = {
            let values = self.values.borrow();
            let ref sym = values.get_symbol(*ident).unwrap().val;
            (sym.value, sym.sig.clone())

        };
        ValueWrapper::new(
            if llvm::GlobalValue::cast(value).is_some() &&
               llvm::Function::cast(value).is_none() { // it will only be a function if it's an intrinsic,
                                                       // which we can't load.
                self.builder.build_load(value)
            } else {
                value
            }, sig)
    }

    fn codegen_conditional(&'a self, cond: &Conditional, func: &llvm::Function) -> ValueWrapper<'a> {
        let cond_val = self.codegen_expr(cond.cond(), func);
        let then_block = func.append("then");
        let else_block = func.append("else");
        self.builder.build_cond_br(*cond_val, then_block, Some(else_block));

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
        assert_eq!(then_val.get_type(), else_val.get_type());
        let phi = self.builder.build_phi(then_val.get_type(), "iftmp");
        phi.add_incoming(*then_val, then_block);
        phi.add_incoming(*else_val, else_block);
        then_val.map(phi)
    }

    fn codegen_infix(&'a self, infix: &Infix, func: &llvm::Function) -> ValueWrapper<'a> {
        let lhs = self.codegen_expr(infix.left(), func);
        let rhs = self.codegen_expr(infix.right(), func);
        self.codegen_binary_op(infix.op(), lhs, rhs)
    }

    fn codegen_binary_op(&self, op: Operator, lhs: ValueWrapper, rhs: ValueWrapper) -> ValueWrapper {
        let lhs = *lhs;
        let rhs = *rhs;
        match op {
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
            Operator::Mod => self.builder.build_rem(lhs, rhs),
            Operator::Exp => {
                let pow_fn = self.module.get_function("llvm.pow.f32").unwrap();
                self.builder.build_call(pow_fn, &[lhs, rhs])
            }
            _ => unreachable!(),
        }.into()
    }

    fn codegen_prefix(&'a self, prefix: &Prefix, func: &llvm::Function) -> ValueWrapper<'a> {
        let expr = self.codegen_expr(prefix.expr(), func);
        expr.map(match prefix.op() {
            Operator::Sub => self.builder.build_sub(0f32.compile(self.llvm), *expr),
            Operator::Not => self.builder.build_not(*expr),
            _ => unreachable!(),
        })
    }

    fn maybe_make_fn_ptr<'c>(&'c self, ty: &'c llvm::Type) -> &llvm::Type {
        if ty.is_function() {
            llvm::Type::new_pointer(ty)
        } else {
            ty
        }
    }

    fn type_to_signature(&self, ty: Type) -> Option<Rc<RefCell<FnSignature>>> {
        match ty {
            Type::Number => None,
            Type::Boolean => None,
            Type::Function(id) => {
                let func = self.functions.get(id).unwrap();
                let ty = func.ty().unwrap();
                let mut default_count = 0;
                let args = ty.args.iter().zip(func.args().iter()).map(|((id, &ty), arg)| {
                    if arg.expr().is_some() {
                        default_count += 1;
                        (id, (Some(default_count), self.type_to_signature(ty)))
                    } else {
                        (id, (None, self.type_to_signature(ty)))
                    }
                }).collect();
                let ret = self.type_to_signature(ty.returns);
                Some(Rc::new(RefCell::new(FnSignature {
                    ret: ret,
                    args: args,
                })))
            }
            _ => unreachable!(),
        }
    }

    fn type_to_llvm(&self, ty: Type, make_fn_struct: bool) -> &llvm::Type {
        match ty {
            Type::Number => llvm::Type::get::<Number>(self.llvm),
            Type::Boolean => llvm::Type::get::<Boolean>(self.llvm),
            Type::Function(id) => {
                let func = self.functions.get(id).unwrap();
                let ty = func.ty().unwrap();
                let arg_map: VecMap<&llvm::Type> = ty.args.iter().map(|(id, &ty)|
                    (id, self.maybe_make_fn_ptr(self.type_to_llvm(ty, true)))).collect();
                let args: Vec<&llvm::Type> = arg_map.values().map(|x| *x).collect();
                let ret = self.maybe_make_fn_ptr(self.type_to_llvm(ty.returns, true));
                let func_ty = llvm::Type::new_function(ret, &args);
                if make_fn_struct {
                    let mut struct_types: Vec<&llvm::Type> = vec![llvm::Type::new_pointer(func_ty)];
                    for (ty, arg) in args.iter().zip(func.args().iter()) {
                        match *arg {
                            Argument::Assign(..) => struct_types.push(ty),
                            _ => { },
                        }
                    }
                    let struct_ty = llvm::StructType::new(self.llvm, &struct_types, true);
                    struct_ty
                } else {
                    func_ty
                }
            }
            _ => unreachable!(),
        }
    }
}

