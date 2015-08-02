use super::ast::*;
use super::types::*;
use super::tokens::{Operator, Node, NodeImpl};
use super::common::Context;
use super::ident::Identifier;
use super::functions;

use std::cell::RefMut;
use std::collections::VecMap;

pub fn typecheck<'a>(ctxt: &'a Context<'a>) {
    let mut t = TypeChecker::new(ctxt);
    t.check();
}

pub struct TypeChecker<'a> {
    types: RefMut<'a, TypeTable>,
    ctxt: &'a Context<'a>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(ctxt: &'a Context<'a>) -> TypeChecker<'a> {
        TypeChecker {
            ctxt: ctxt,
            types: ctxt.types.borrow_mut(),
        }
    }

    pub fn check(&mut self) {
        self.check_root(&mut *self.ctxt.ast.borrow_mut());
    }

    fn check_root(&mut self, root: &mut Root) {
        for item in root.iter() {
            match *item {
                Item::FunctionDef(ref f) => {
                    self.typeof_function_def(&f);
                }
                Item::Assignment(ref a) => {
                    self.typeof_assignment(&a);
                }
            };
        }
        root.retain(|item|
            match *item {
                Item::FunctionDef(ref f) => {
                    if !self.ctxt.functions.borrow().get(f.ident()).unwrap().has_concrete_type() {
                        self.ctxt.emit_warning("function is never used", f.pos());
                        false
                    } else {
                        true
                    }
                },
                _ => true
            }
        );
    }

    pub fn typeof_assignment(&mut self, assign: &Node<Assignment>) -> Option<Type> {
        let ty = self.typeof_expr(&assign.expr());
        match ty {
            None =>
                self.ctxt.emit_error("could not determine type of assignment", assign.pos()),
            Some(ty) => {
                if let Some(old_sym) = self.types.get_symbol(assign.ident()) {
                    if old_sym.val != ty && Some(0) == self.types.get_symbol_depth(assign.ident()) {
                        self.ctxt.emit_warning(format!("variable was previously assigned type `{}`",
                                                     old_sym.val),
                                             assign.pos());
                    }
                }
                if let Type::Indeterminate = ty {
                    self.ctxt.emit_error("expression references a function with ambiguous type",
                                         assign.expr_pos());
                }
                self.types.set_val(assign.ident(), assign.pos().index, ty);
            }
        }
        ty
    }

    pub fn typeof_function_def(&mut self, def: &Node<FunctionDef>) -> Option<Type> {
        if let Some(0) = self.types.get_symbol_depth(def.ident()) {
            self.ctxt.emit_warning("function declaration shadows previous declaration of same name", def.pos());
        }
        let ty = Type::Function(def.ident());
        self.types.set_val(def.ident(), 0, ty);
        Some(ty)
    }

    pub fn typeof_expr(&mut self, expr: &Expression) -> Option<Type> {
        match *expr {
            Expression::Constant(_) => Some(Type::Number),
            Expression::Boolean(_) => Some(Type::Boolean),
            Expression::Variable(ref id) => self.typeof_var(id),
            Expression::Infix(ref v) => self.typeof_infix(v),
            Expression::Prefix(ref v) => self.typeof_prefix(v),
            Expression::Conditional(ref c) => self.typeof_conditional(c),
            Expression::Block(ref b) => self.typeof_block(b),
            Expression::FunctionCall(ref c) => self.typeof_function_call(c),
            Expression::Closure(ref c) => self.typeof_function_def(c),
        }
    }

    pub fn typeof_function_call(&mut self, call: &Node<FunctionCall>) -> Option<Type> {
        let func_id = match self.typeof_expr(call.callee()) {
            Some(Type::Function(f)) => f,

            Some(ref ty) => {
                self.ctxt.emit_error(format!("expected function, got type `{}`", ty), call.callee_pos());
                return None;
            },

            None => {
                self.ctxt.emit_error("could not determine type of function", call.callee_pos());
                return None;
            },
        };
        let func_def = self.types.get_symbol(func_id).unwrap().clone();
        let func = self.ctxt.functions.borrow().get(func_id).unwrap().clone();

        let mut def_args = Vec::new();

        let mut undef_args = func.args().clone();
        // check that the args in the call match the args in the def
        for arg in call.args() {
            if let Some(id) = arg.ident() {
                match undef_args.iter().position(|x| x.ident().unwrap() == id) {
                    Some(pos) => {
                        def_args.push((arg.clone(), false));
                        undef_args.remove(pos);
                    }
                    None => {
                        self.ctxt.emit_error(format!("unexpected argument `{}`",
                                                     self.ctxt.lookup_name(id)), arg.pos());
                        return None;
                    }
                }
            } else {
                if undef_args.is_empty() {
                        self.ctxt.emit_error("unexpected argument", arg.pos());
                        return None;
                }
                let expr = match *arg {
                    Argument::Expr(ref expr) => { expr.clone() },
                    _ => unreachable!(),
                };
                let id = undef_args.remove(0).ident().unwrap();
                def_args.push((Argument::Assign(Node(id, expr.pos()), expr), false));
            }
        }
        // set default args that weren't set previously
        undef_args.retain(|arg|
            match *arg {
                Argument::Assign(_, _) => {
                    def_args.push((arg.clone(), true));
                    false
                }
                _ => true
            }
        );

        // if any arguments were not defined above, die
        for unassigned in undef_args.iter() {
            self.ctxt.emit_error(format!("argument `{}` is required",
                                         self.ctxt.lookup_name(unassigned.ident().unwrap())),
                                 call.args_pos());
        }
        if undef_args.len() > 0 {
            return None
        }

        let mut arg_types = VecMap::new();

        let recursive = self.ctxt.callstack.borrow().is_recursive(func_id);
        if recursive {
            return Some(Type::Indeterminate);
        }
        self.ctxt.callstack.borrow_mut().push(func_id);

        // determine the type of the arguments
        for &(ref arg, is_default) in &def_args {
            if is_default {
                self.types.push_scope(&func_def.scope.scope);
            }
            match *arg {
                Argument::OpAssign(id, op, ref expr) => {
                    let ty = self.typeof_expr(&Expression::Infix(Box::new(Node(Infix {
                        op: op,
                        left: Expression::Variable(id),
                        right: expr.clone(),
                    }, arg.pos()))));
                    match ty {
                        None => return None,
                        Some(ty) => {
                            arg_types.insert(*id, ty);
                        }
                    }
                }
                Argument::Assign(id, ref expr) => {
                    let ty = self.typeof_expr(expr);
                    match ty {
                        None => return None,
                        Some(ty) => {
                            arg_types.insert(*id, ty);
                        }
                    }
                }
                Argument::Ident(id) => {
                    let ty = self.typeof_var(&id);
                    match ty {
                        None => return None,
                        Some(ty) => {
                            arg_types.insert(*id, ty);
                        }
                    }
                }
                _ => unreachable!(),
            }
            if is_default {
                self.types.pop();
            }
        }

        let return_ty = match func {
            functions::Function::User(ref def) => {
                self.types.push_scope(&func_def.scope.scope);
                for ((id, ty), &(ref arg, _)) in arg_types.iter().zip(def_args.iter()) {
                    self.types.set_val(id, arg.pos().index, *ty);
                    if let Type::Function(func_id) = *ty {
                        self.types.set_val(func_id, arg.pos().index, Type::Function(func_id));
                    }
                }
                let ty = self.typeof_block(&def.block);
                self.types.pop();
                match ty {
                    Some(ty) => ty,
                    None => {
                        self.ctxt.emit_error("could not determine return type of function", def.pos());
                        return None;
                    }
                }
            }

            functions::Function::Builtin(ref def) => { def.ty.returns }
            functions::Function::External(ref def) => { def.ty.returns }
        };

        self.ctxt.callstack.borrow_mut().pop();

        let calcd_type = FunctionType::new(arg_types, return_ty);
        if let Some(def_type) = func.ty() {
            let mut types_match = true;
            for ((old, new), &(ref arg, _)) in def_type.args.iter().zip(calcd_type.args.iter()).zip(def_args.iter()) {
                if let ((_, &Type::Function(_)), (_, &Type::Function(_))) = (new, old) {
                    // If they are both functions, do nothing.
                    // Their compatibility will already have been validated
                } else if old != new {
                    self.ctxt.emit_error(format!("expected type `{}` for argument `{}`, got `{}`",
                                                 old.1,
                                                 self.ctxt.lookup_name(arg.ident().unwrap()),
                                                 new.1),
                                         arg.pos());
                    types_match = false;
                }
            }
            if !types_match {
                return None;
            }
        }
        let mut func = func;
        match func {
            functions::Function::User(ref mut def) => {
                def.ty = Some(calcd_type);
            },
            _ => { }
        };
        self.ctxt.functions.borrow_mut().insert(func_id, func);
        Some(return_ty)
    }

    pub fn typeof_block(&mut self, block: &Node<Block>) -> Option<Type> {
        self.types.push(block.pos().index);
        let count = block.iter().filter(|&x| {
            match x {
                &Statement::Expression(..) => true,
                _ => false,
            }
        }).count();
        let mut ty = None;
        for stmnt in block.item() {
            match stmnt {
                &Statement::Assignment(ref a) => {
                    if self.typeof_assignment(a).is_none() {
                        self.ctxt.emit_error("could not determine type of block assignment", a.pos());
                        return None
                    }
                }
                &Statement::Expression(ref e) => {
                    ty = self.typeof_expr(e);
                    match ty {
                        Some(Type::Number) => { },
                        Some(Type::Indeterminate) => {
                            if count > 1 {
                                ty = Some(Type::Number);
                            } else {
                                self.ctxt.emit_error("type of expression is ambiguous", e.pos());
                                ty = None;
                            }
                        }
                        Some(_) => {
                            if count > 1 {
                                self.ctxt.emit_error(format!("expected type `Number`, got `{}`", ty.unwrap()), e.pos());
                                ty = None;
                            }
                        }
                        None => {
                            self.ctxt.emit_error("could not determine type of statement", e.pos());
                        }

                    }
                }
            }
        }
        self.types.pop();
        ty
    }

    pub fn typeof_conditional(&mut self, cond: &Node<Conditional>) -> Option<Type> {
        match self.typeof_expr(&cond.cond()) {
            Some(x) => {
                if x != Type::Boolean {
                    self.ctxt.emit_error(format!("expected type `Boolean`, got `{}`", x), cond.cond_pos());
                    return None;
                }
            },
            None => {
                self.ctxt.emit_error("type of condition could not be determined", cond.cond_pos());
                return None;
            }
        }
        let then_ty = match self.typeof_expr(cond.then()) {
            Some(x) => x,
            None => {
                self.ctxt.emit_error("type of conditional then expression could not be determined", cond.then_pos());
                return None;
            }
        };
        let else_ty = match self.typeof_expr(cond.els()) {
            Some(x) => x,
            None => {
                self.ctxt.emit_error("type of conditional else expression could not be determined", cond.els_pos());
                return None;
            }
        };
        if else_ty == Type::Indeterminate && then_ty != Type::Indeterminate {
            return Some(then_ty);
        }
        if then_ty == Type::Indeterminate && else_ty != Type::Indeterminate {
            return Some(else_ty);
        }
        if let (Type::Function(_), Type::Function(_)) = (then_ty, else_ty) {
            return Some(then_ty);
            //unimplemented!();
        }
        if else_ty != then_ty {
            self.ctxt.emit_error(format!(
                "then branch of conditional is of type `{}` but else branch is of type `{}`",
                then_ty, else_ty), cond.els_pos());
            return None;
        }

        Some(then_ty)
    }

    pub fn typeof_var(&mut self, ident: &Node<Identifier>) -> Option<Type> {
        match self.types.get_symbol(*ident.item()) {
            Some(s) => {
                Some(s.val)
            }
            None => {
                self.ctxt.emit_error(format!("no variable named `{}` is in scope",
                                             self.ctxt.lookup_name(*ident.item())),
                                     ident.pos());
                None
            }
        }
    }

    pub fn typeof_infix(&mut self, infix: &Node<Infix>) -> Option<Type> {
        let lhs_ty = match self.typeof_expr(infix.left()) {
            Some(x) => x,
            None => {
                self.ctxt.emit_error("type of infix lhs could not be determined",
                                     infix.left_pos());
                return None;
            }
        };
        let rhs_ty = match self.typeof_expr(infix.right()) {
            Some(x) => x,
            None => {
                self.ctxt.emit_error("type of infix rhs could not be determined",
                                     infix.right_pos());
                return None;
            }
        };
        match infix.op() {
            Operator::Add |
            Operator::Sub |
            Operator::Mul |
            Operator::Div |
            Operator::Exp |
            Operator::Mod => {
                if lhs_ty == rhs_ty && (lhs_ty == Type::Number || lhs_ty == Type::Indeterminate) {
                    Some(lhs_ty)
                } else if lhs_ty == Type::Number && rhs_ty == Type::Indeterminate ||
                          rhs_ty == Type::Number && lhs_ty == Type::Indeterminate {
                    Some(Type::Number)
                } else {
                    self.ctxt.emit_error(format!(
                        "cannot apply numerical operator to types `{}` and `{}`",
                        lhs_ty, rhs_ty), infix.op_pos());
                    None
                }
            }
            Operator::Less |
            Operator::ApproxEqual |
            Operator::GreaterEqual |
            Operator::LessEqual |
            Operator::Greater => {
                if lhs_ty == rhs_ty && lhs_ty == Type::Number {
                    Some(Type::Boolean)
                } else if lhs_ty == rhs_ty && lhs_ty == Type::Indeterminate {
                    Some(Type::Indeterminate)
                } else if lhs_ty == Type::Number && rhs_ty == Type::Indeterminate ||
                          rhs_ty == Type::Number && lhs_ty == Type::Indeterminate {
                    Some(Type::Boolean)
                } else {
                    self.ctxt.emit_error(format!(
                        "cannot apply comparison operator to types `{}` and `{}`",
                        lhs_ty, rhs_ty), infix.op_pos());
                    None
                }
            }
            Operator::Equal |
            Operator::NotEqual => {
                if lhs_ty == rhs_ty &&
                   (lhs_ty == Type::Number || lhs_ty == Type::Boolean) {
                    Some(Type::Boolean)
                } else if lhs_ty == rhs_ty && lhs_ty == Type::Indeterminate {
                    Some(Type::Indeterminate)
                } else if (lhs_ty == Type::Number || lhs_ty == Type::Boolean)
                              && rhs_ty == Type::Indeterminate ||
                          (rhs_ty == Type::Number || rhs_ty == Type::Boolean)
                              && lhs_ty == Type::Indeterminate {
                    Some(Type::Boolean)
                } else {
                    self.ctxt.emit_error(format!(
                        "cannot apply equality operator to types `{}` and `{}`",
                        lhs_ty, rhs_ty), infix.op_pos());
                    None
                }
            }
            Operator::And |
            Operator::Or |
            Operator::Xor => {
                if lhs_ty == rhs_ty && (lhs_ty == Type::Boolean || lhs_ty == Type::Indeterminate) {
                    Some(lhs_ty)
                } else if lhs_ty == Type::Boolean && rhs_ty == Type::Indeterminate ||
                          rhs_ty == Type::Boolean && lhs_ty == Type::Indeterminate {
                    Some(Type::Boolean)
                } else {
                    self.ctxt.emit_error(format!(
                        "cannot apply logical operator to types `{}` and `{}`",
                        lhs_ty, rhs_ty), infix.op_pos());
                    None
                }
            }
            _ => {
                unreachable!();
            }
        }
    }

    pub fn typeof_prefix(&mut self, prefix: &Node<Prefix>) -> Option<Type> {
        let expr_ty = match self.typeof_expr(prefix.expr()) {
            Some(x) => x,
            None => {
                self.ctxt.emit_error("type of prefix expression could not be determined", prefix.expr_pos());
                return None;
            }
        };
        match prefix.op() {
            Operator::Sub => {
                if expr_ty == Type::Number || expr_ty == Type::Indeterminate {
                    Some(Type::Number)
                } else {
                    self.ctxt.emit_error(format!("expected `Number`, got `{}`", expr_ty),
                                         prefix.expr_pos());
                    None
                }
            }
            Operator::Not => {
                if expr_ty == Type::Boolean || expr_ty == Type::Indeterminate {
                    Some(Type::Boolean)
                } else {
                    self.ctxt.emit_error(format!("expected `Boolean`, got `{}`", expr_ty),
                                         prefix.expr_pos());
                    None
                }
            }
            _ => {
                unreachable!();
            }
        }
    }
}
