use super::ast::*;
use super::types::*;
use super::tokens::{Operator, Node, NodeImpl, SourcePos};
use super::common::Context;
use super::ident::{Identifier, NameTable};
use super::functions;

use std::cell::RefMut;
use std::collections::VecMap;

pub fn typecheck<'a>(ctxt: &'a Context<'a>) {
    let mut t = TypeChecker::new(ctxt);
    t.check();
}

pub struct TypeChecker<'a> {
    types: RefMut<'a, TypeTable>,
    names: RefMut<'a, NameTable<'a>>,
    ctxt: &'a Context<'a>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(ctxt: &'a Context<'a>) -> TypeChecker<'a> {
        TypeChecker {
            ctxt: ctxt,
            types: ctxt.types.borrow_mut(),
            names: ctxt.names.borrow_mut(),
        }
    }

    pub fn check(&mut self) {
        self.check_root(&*self.ctxt.ast.borrow());
    }

    fn check_root(&mut self, root: &Root) {
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
        //TODO emit warnings when functions or variables are not used.
    }

    fn typeof_assignment(&mut self, assign: &Node<Assignment>) -> Option<Type> {
        let ty = self.typeof_expr(&assign.expr());
        if let Some((old_ty, 0)) = self.types.get_symbol(assign.ident()) {
            if old_ty.ty != ty {
                self.ctxt.emit_warning(format!("variable was previously assigned type `{}`",
                                             old_ty.ty.unwrap()),
                                     assign.pos());
            }
        }
        if ty.is_none() {
            self.ctxt.emit_error("could not determine type of assignment", assign.pos());
        } else {
            if let Some(Type::Indeterminate) = ty {
                self.ctxt.emit_error("expression references a function with ambiguous type", assign.expr_pos());
            }
            self.types.set_type(assign.ident, ty);
        }
        println!("assign identifier `{}` type {:?}", self.names.get_name(assign.ident()).unwrap(), ty);
        ty
    }

    fn typeof_function_def(&mut self, def: &Node<FunctionDef>) -> Option<Type> {
        if let Some((_, 0)) = self.types.get_symbol(def.ident()) {
            self.ctxt.emit_warning("function declaration shadows previous declaration of same name", def.pos());
        }
        let ty = Some(Type::Function(def.ident()));
        self.types.set_type(def.ident, ty);
        ty
    }

    fn typeof_expr(&mut self, expr: &Expression) -> Option<Type> {
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

    fn typeof_function_call(&mut self, call: &Node<FunctionCall>) -> Option<Type> {
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
        let def_scope = match self.types.get_symbol(func_id) {
            Some((s, _)) => s.scope.clone(),
            None => panic!("fuck"),
        };
        let func = match self.ctxt.functions.borrow().get(func_id) {
            Some(f) => f.clone(),
            None => unreachable!(),
        };

        let is_aliased = false; //TODO set this for better error messages

        let mut def_args = Vec::new();

        let mut undef_args = match func {
            functions::Function::User(ref def) => {
                let recursive = self.types.has_scope_cycle(def.block_pos().index);
                if recursive {
                    return Some(Type::Indeterminate);
                }
                def.args().clone()
            }

            functions::Function::Builtin(ref def) => {
                def.args.clone()
            }
        };

        // first set explicitly defined args, like a=5 or *a
        for arg in call.args() {
            match *arg {
                Argument(Some(Node(id, _)), Some(_)) => {
                    // check if an argument with the same id is in the function
                    // definition.
                    match undef_args.iter().position(|x| match *x {
                        Argument(Some(Node(def_id, _)), _) if id == def_id => true,
                        _ => false,
                    }) {
                        Some(pos) => {
                            undef_args.remove(pos);
                            def_args.push(arg.clone());
                        }
                        None =>
                            self.ctxt.emit_error(format!("unexpected named argument `{}`",
                                                         self.names.get_name(id).unwrap()), arg.pos()),
                    }
                }
                Argument(Some(Node(id, _)), None) => {
                    if call.ty() == CallType::Partial {
                        match undef_args.iter().position(|x| match *x {
                            Argument(Some(Node(def_id, _)), _) if id == def_id => true,
                            _ => false,
                        }) {
                            Some(pos) => {
                                undef_args.remove(pos);
                                def_args.push(arg.clone());
                            }
                            None =>
                                self.ctxt.emit_error("unexpected argument unbind expression", arg.pos()),
                        }
                    } else {
                        self.ctxt.emit_error("expected identifier", arg.pos());
                    }
                }
                _ => { },
            }
        }

        let num_positional_args = undef_args.len();
        // next set expression only args, like 2+2
        for arg in call.args() {
            match *arg {
                Argument(None, Some(ref expr)) => {
                    if undef_args.len() > 0 {
                        def_args.push(Argument(Some(Node(*undef_args[0].0.unwrap().item(), arg.pos())),
                                               Some(expr.clone())));
                        undef_args.remove(0);
                    } else {
                        self.ctxt.emit_error(format!("unexpected argument, {}function `{}` takes {} positional argument{}",
                                                     if is_aliased { "aliased " } else { "" },
                                                     self.names.get_name(func_id).unwrap(),
                                                     num_positional_args,
                                                     if num_positional_args == 1 { "" } else { "s" }),
                                             arg.pos());
                    }
                }
                _ => { },
            }
        }
        // set default args that weren't set previously
        for arg in undef_args.iter().rev() {
            match *arg {
                Argument(Some(_), Some(_)) => {
                    def_args.insert(0, arg.clone());
                }
                _ => { }
            }
        }
        undef_args.retain(|x| match *x {
            Argument(Some(_), Some(_)) => false,
            _ => true,
        });

        // check that all args are defined somewhere for implicit calls
        if call.ty() == CallType::Implicit {
            undef_args.retain(|x| match *x {
                Argument(Some(id), None) => self.types.get_symbol(*id.item()).is_none(),
                _ => true,
            });
        }

        if call.ty() == CallType::Partial {
            let mut args = Vec::new();
            args.push_all(&undef_args);
            args.push_all(&def_args);
            let new_ident = self.names.new_anon();
            let new_type = Type::Function(new_ident);
            self.types.set_type(Node(new_ident, SourcePos::anon()), Some(new_type));
            match func {
                functions::Function::User(ref def) => {
                    let mut new_def = def.item().clone();
                    new_def.args = Node(args, call.args_pos());
                    self.ctxt.functions.borrow_mut().insert(new_ident, functions::Function::User(
                        functions::UserFunction {
                            ty: None,
                            node: Node(new_def, call.pos())
                        }));
                },
                functions::Function::Builtin(ref def) => {
                    self.ctxt.functions.borrow_mut().insert(new_ident, functions::Function::Builtin(
                        functions::BuiltinFunction {
                            ty: def.ty.clone(),
                            args: args,
                        }));
                },
            }
            Some(new_type)
        } else { // Not partial application
            for unassigned in undef_args.iter() {
                self.ctxt.emit_error(format!("argument `{}` is required",
                                             self.names.get_name(unassigned.ident().unwrap()).unwrap()),
                                     call.args_pos());
            }
            if undef_args.len() > 0 {
                return None
            }

            let mut arg_types = VecMap::new();

            // determine the type of the arguments
            for arg in &def_args {
                match *arg {
                    Argument(Some(Node(id, _)), Some(ref expr)) => {
                        let ty = self.typeof_expr(expr);
                        match ty {
                            None => return None,
                            Some(ty) => {
                                arg_types.insert(id, ty);
                            }
                        }
                    }
                    _ => { },
                }
            }

            let return_ty = match func {
                functions::Function::User(ref def) => {
                    self.types.enter_scope(&def_scope.scope);
                    for (id, ty) in &arg_types {
                        self.types.set_type(Node(id, SourcePos::anon()), Some(*ty));
                    }
                    let ty = self.typeof_block(&def.block);
                    self.types.leave_block();
                    match ty {
                        Some(ty) => ty,
                        None => {
                            self.ctxt.emit_error("could not determine return type of function", def.pos());
                            return None;
                        }
                    }
                }

                functions::Function::Builtin(ref def) => {
                    def.ty.returns
                }
            };

            let calcd_type = FunctionType::new(arg_types, return_ty);
            if let Some(def_type) = match func {
                functions::Function::User(ref def) => def.ty.as_ref(),
                functions::Function::Builtin(ref def) => Some(&def.ty),
            } {
                let mut types_match = true;
                for ((old, new), arg) in def_type.args.iter().zip(calcd_type.args.iter()).zip(def_args.iter()) {
                    if let ((_, &Type::Function(_)), (_, &Type::Function(_))) = (new, old) {
                        // If they are both functions, do nothing.
                        // Their compatibility will already have been validated
                    } else if old != new {
                        if is_aliased {
                            self.ctxt.emit_error(format!("expected type `{}` for argument `{}` to aliased function `{}`, got type `{}`",
                                                         old.1,
                                                         self.names.get_name(arg.ident().unwrap()).unwrap(),
                                                         self.names.get_name(func_id).unwrap(),
                                                         new.1),
                                                 arg.pos());
                        } else {
                            self.ctxt.emit_error(format!("expected type `{}` for argument `{}`, got `{}`",
                                                         old.1,
                                                         self.names.get_name(arg.ident().unwrap()).unwrap(),
                                                         new.1),
                                                 arg.pos());
                        }
                        types_match = false;
                    }
                }
                if !types_match {
                    return None;
                }
            }
            let mut func = func;
            if let functions::Function::User(ref mut def) = func {
                def.ty = Some(calcd_type);
            }
            self.ctxt.functions.borrow_mut().insert(func_id, func);
            Some(return_ty)
        }
    }

    fn typeof_block(&mut self, block: &Node<Block>) -> Option<Type> {
        self.types.enter_block(block.pos().index);
        for stmnt in block.item() {
            match stmnt {
                &Statement::Assignment(ref a) => {
                    if self.typeof_assignment(a).is_none() {
                        self.ctxt.emit_error("could not determine type of block assignment", a.pos());
                        return None
                    }
                }
                _ => { }
            }
        }
        let count = block.iter().filter(|&x| {
            match x {
                &Statement::Expression(..) => true,
                _ => false,
            }
        }).count();
        let mut ty = None;
        for stmnt in block.item() {
            match stmnt {
                &Statement::Expression(ref e) => {
                    ty = self.typeof_expr(e);
                    match ty {
                        Some(Type::Number) => { },
                        Some(Type::Indeterminate) => {
                            if count > 1 {
                                ty = Some(Type::Number);
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
                _ => { }
            }
        }
        self.types.leave_block();
        ty
    }

    fn typeof_conditional(&mut self, cond: &Node<Conditional>) -> Option<Type> {
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
        if let &Some(ref els) = cond.els() {
            let else_ty = match self.typeof_expr(&els) {
                Some(x) => x,
                None => {
                    self.ctxt.emit_error("type of conditional else expression could not be determined", els.pos());
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
                unimplemented!();
            }
            if else_ty != then_ty {
                self.ctxt.emit_error(format!(
                    "then branch of conditional is of type `{}` but else branch is of type `{}`",
                    then_ty, else_ty), els.pos());
                return None;
            }
        }
        Some(then_ty)
    }

    fn typeof_var(&mut self, ident: &Node<Identifier>) -> Option<Type> {
        match self.types.get_symbol(*ident.item()) {
            Some((s, _)) => {
                s.ty
            }
            None => {
                self.ctxt.emit_error(format!("no variable named `{}` is in scope",
                                             self.names.get_name(*ident.item()).unwrap()),
                                     ident.pos());
                None
            }
        }
    }

    fn typeof_infix(&mut self, infix: &Node<Infix>) -> Option<Type> {
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

    fn typeof_prefix(&mut self, prefix: &Node<Prefix>) -> Option<Type> {
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
