use super::ast::*;
use super::issue::{IssueTracker, Level};
use super::symbol::{Type, SymbolTable, Identifier, Symbol};
use super::lexer::Operator;

pub struct TypeChecker<'a, 'b> {
    issues: &'a IssueTracker<'a>,
    ast: &'b Root,
    symtab: &'a SymbolTable<'a>,
}

impl<'a, 'b> TypeChecker<'a, 'b> {
    pub fn new(ast: &'b Root, symtab: &'a SymbolTable<'a>, issues: &'a IssueTracker<'a>) -> TypeChecker<'a, 'b> {
        TypeChecker {
            ast: ast,
            symtab: symtab,
            issues: issues,
        }
    }

    pub fn check(&self) -> bool {
        self.check_root(self.ast)
    }

    fn check_root(&self, root: &Root) -> bool {
        let mut ok = true;
        for item in root.iter() {
            ok &= match item {
                &Item::FunctionDef(ref f) =>
                    self.check_function_def(&f),
                &Item::Assignment(ref a) =>
                    self.check_assignment(&a),
            };
        }
        ok
    }

    fn check_assignment(&self, assign: &Node<Assignment>) -> bool {
        if let Some((_, 0)) = self.symtab.get_symbol(assign.ident()) {
            self.issues.new_issue(assign.pos(), Level::Warning,
                                  "constant declaration shadows previous declaration of same name");
        }
        let mut sym = Symbol::new();
        sym.ty = self.typeof_expr(&assign.expr());
        let ok = sym.ty.is_some();
        if !ok {
            self.issues.new_issue(assign.pos(), Level::Error, "type of assignment could not be determined");
            return false;
        }
        self.symtab.set_symbol(assign.ident(), sym);
        ok
    }

    fn check_function_def(&self, def: &Node<FunctionDef>) -> bool {
        if let Some((_, 0)) = self.symtab.get_symbol(def.ident()) {
            self.issues.new_issue(def.pos(), Level::Warning,
                                  "function declaration shadows previous declaration of same name");
        }
        let mut sym = Symbol::new();
        sym.ty = Some(Type::Function(def.func.clone()));
        self.symtab.set_symbol(def.ident(), sym);
        true
    }

    fn typeof_expr(&self, expr: &Expression) -> Option<Type> {
        match expr {
            &Expression::Constant(_) => Some(Type::Number),
            &Expression::Boolean(_) => Some(Type::Boolean),
            &Expression::Variable(ref id) => self.typeof_var(id),
            &Expression::Infix(ref v) => self.typeof_infix(v),
            &Expression::Prefix(ref v) => self.typeof_prefix(v),
            &Expression::Conditional(ref c) => self.typeof_conditional(c),
            &Expression::Block(ref b) => self.typeof_block(b),
            &Expression::FunctionCall(ref c) => self.typeof_function_call(c),
        }
    }

    fn typeof_function_call(&self, call: &Node<FunctionCall>) -> Option<Type> {
        let def = match self.symtab.get_symbol(call.ident()) {
            Some((Symbol { ty: Some(Type::Function(f)) }, _)) => f.clone(),
            Some((Symbol { ty: Some(ty) }, _)) => {
                self.issues.new_issue(call.ident_pos(), Level::Error,
                                      format!("expected type `Function`, got `{}`", ty));
                return None;
            },
            None => {
                self.issues.new_issue(call.ident_pos(), Level::Error, "undefined identifier");
                return None;
            },
            _ => {
                self.issues.new_issue(call.ident_pos(), Level::Error, "identifier has no type");
                return None;
            },
        };

        let mut undef_args = def.args().clone();
        let mut def_args = Vec::new();

        // first set explicitly defined args, like a=5
        for arg in call.args() {
            match *arg.item() {
                Argument(Some(Node(id, _)), Some(_)) => {
                    undef_args.retain(|x| match *x.item() {
                        Argument(Some(Node(def_id, _)), _) if def_id == id => false,
                        _ => true,
                    });
                    def_args.push(arg.clone());
                }
                _ => { },
            }
        }

        // next set expression only args, like 2+2
        for arg in call.args() {
            match *arg.item() {
                Argument(None, Some(ref expr)) => {
                    if undef_args.len() > 0 {
                        def_args.push(Node(Argument(undef_args[0].item().0, Some(expr.clone())), arg.pos()));
                        undef_args.remove(0);
                    } else {
                        self.issues.new_issue(arg.pos(), Level::Warning, "unused argument");
                    }
                }
                _ => { },
            }
        }

        // set default args that weren't set previously
        for arg in undef_args.iter().rev() {
            match *arg.item() {
                Argument(Some(_), Some(_)) => {
                    def_args.insert(0, arg.clone());
                }
                _ => { }
            }
        }

        undef_args.retain(|x| match *x.item() {
            Argument(Some(_), Some(_)) => false,
            _ => true,
        });

        // check that all args are defined somewhere for implicit calls
        if call.ty() == CallType::Implicit {
            undef_args.retain(|x| match *x.item() {
                Argument(Some(id), None) => self.symtab.get_symbol(*id.item()).is_none(),
                _ => true,
            });
        }

        // determine the type of the arguments
        for arg in &def_args {
            match *arg.item() {
                Argument(Some(Node(id, _)), Some(ref expr)) => {
                    let mut sym = Symbol::new();
                    sym.ty = self.typeof_expr(expr);
                    if let None = sym.ty {
                        return None;
                    }
                    self.symtab.enter_scope(def.pos());
                    if let Some((prev, 1)) = self.symtab.get_symbol(id) {
                        if sym.ty.is_some() && prev.ty.is_some() && sym.ty != prev.ty {
                            self.issues.new_issue(expr.pos(), Level::Error,
                                format!("expected type `{}`, got `{}`", prev.ty.unwrap(), sym.ty.unwrap()));
                            return None;
                        }
                    }
                    self.symtab.set_symbol(id, sym);
                    self.symtab.leave_scope();
                }
                _ => { },
            }
        }

        for unassigned in undef_args.iter() {
            self.issues.new_issue(call.args_pos(), Level::Error, format!(
                    "argument `{}` is required",
                    self.symtab.get_name(unassigned.ident().unwrap())));
        }

        if undef_args.len() > 0 {
            return None
        }

        let recursive = self.symtab.check_cycle(def.pos());
        if recursive {
            return Some(Type::Infer);
        }

        self.symtab.enter_scope(def.pos());
        let ty = self.typeof_block(&def.block);
        self.symtab.leave_scope();
        ty
    }

    fn typeof_block(&self, block: &Node<Block>) -> Option<Type> {
        self.symtab.enter_scope(block.pos());
        for stmnt in block.item() {
            match stmnt {
                &Statement::Assignment(ref a) => {
                    if !self.check_assignment(a) {
                        self.issues.new_issue(a.pos(), Level::Error,
                            "could not determine type of block assignment");
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
                        Some(Type::Infer) => ty = Some(Type::Number),
                        Some(_) => {
                            if count > 1 {
                                self.issues.new_issue(e.pos(), Level::Error,
                                    format!("expected type `Number`, got `{}`", ty.unwrap()));
                                ty = None;
                            }
                        }
                        None => {
                            self.issues.new_issue(e.pos(), Level::Error,
                                "could not determine type of statement");
                        }

                    }
                }
                _ => { }
            }
        }
        self.symtab.leave_scope();
        ty
    }

    fn typeof_conditional(&self, cond: &Node<Conditional>) -> Option<Type> {
        match self.typeof_expr(&cond.cond()) {
            Some(x) => {
                if x != Type::Boolean {
                    self.issues.new_issue(cond.cond_pos(), Level::Error,
                                          format!("expected type `Boolean`, got `{}`", x));
                    return None;
                }
            },
            None => {
                self.issues.new_issue(cond.cond_pos(), Level::Error,
                                      "type of condition could not be determined");
                return None;
            }
        }
        let then_ty = match self.typeof_expr(cond.then()) {
            Some(x) => x,
            None => {
                self.issues.new_issue(cond.then_pos(), Level::Error,
                                      "type of conditional then expression could not be determined");
                return None;
            }
        };
        if let &Some(ref els) = cond.els() {
            let else_ty = match self.typeof_expr(&els) {
                Some(x) => x,
                None => {
                    self.issues.new_issue(els.pos(), Level::Error,
                                          "type of conditional else expression could not be determined");
                    return None;
                }
            };
            if else_ty == Type::Infer && then_ty != Type::Infer {
                return Some(then_ty);
            }
            if then_ty == Type::Infer && else_ty != Type::Infer {
                return Some(else_ty);
            }
            if else_ty != then_ty {
                    self.issues.new_issue(els.pos(), Level::Error, format!(
                                          "then branch of conditional is of type `{}` but else branch is of type `{}`",
                                          then_ty, else_ty));
                    return None;
            }
        }
        Some(then_ty)
    }

    fn typeof_var(&self, ident: &Node<Identifier>) -> Option<Type> {
        match self.symtab.get_symbol(*ident.item()) {
            Some((s, _)) => s.ty,
            None => {
                self.issues.new_issue(ident.pos(), Level::Error,
                                      format!("variable `{}` is undefined",
                                          self.symtab.get_name(*ident.item())));
                None
            }
        }
    }

    fn typeof_infix(&self, infix: &Node<Infix>) -> Option<Type> {
        let lhs_ty = match self.typeof_expr(infix.left()) {
            Some(x) => x,
            None => {
                self.issues.new_issue(infix.left_pos(), Level::Error,
                                      "type of infix lhs could not be determined");
                return None;
            }
        };
        let rhs_ty = match self.typeof_expr(infix.right()) {
            Some(x) => x,
            None => {
                self.issues.new_issue(infix.right_pos(), Level::Error,
                                      "type of infix rhs could not be determined");
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
                if lhs_ty == rhs_ty && (lhs_ty == Type::Number || lhs_ty == Type::Infer) {
                    Some(lhs_ty)
                } else if lhs_ty == Type::Number && rhs_ty == Type::Infer ||
                          rhs_ty == Type::Number && lhs_ty == Type::Infer {
                    Some(Type::Number)
                } else {
                    self.issues.new_issue(infix.op_pos(), Level::Error, format!(
                                          "cannot apply numerical operator to types `{}` and `{}`",
                                          lhs_ty, rhs_ty));
                    None
                }
            }
            Operator::Less |
            Operator::ApproxEqu |
            Operator::GreaterEqual |
            Operator::LessEqual |
            Operator::Greater => {
                if lhs_ty == rhs_ty && lhs_ty == Type::Number {
                    Some(Type::Boolean)
                } else if lhs_ty == rhs_ty && lhs_ty == Type::Infer {
                    Some(Type::Infer)
                } else if lhs_ty == Type::Number && rhs_ty == Type::Infer ||
                          rhs_ty == Type::Number && lhs_ty == Type::Infer {
                    Some(Type::Boolean)
                } else {
                    self.issues.new_issue(infix.op_pos(), Level::Error, format!(
                                          "cannot apply comparison operator to types `{}` and `{}`",
                                          lhs_ty, rhs_ty));
                    None
                }
            }
            Operator::Equ |
            Operator::NotEqu => {
                if lhs_ty == rhs_ty &&
                   (lhs_ty == Type::Number || lhs_ty == Type::Boolean) {
                    Some(Type::Boolean)
                } else if lhs_ty == rhs_ty && lhs_ty == Type::Infer {
                    Some(Type::Infer)
                } else if (lhs_ty == Type::Number || lhs_ty == Type::Boolean) && rhs_ty == Type::Infer ||
                          (rhs_ty == Type::Number || rhs_ty == Type::Boolean) && lhs_ty == Type::Infer {
                    Some(Type::Boolean)
                } else {
                    self.issues.new_issue(infix.op_pos(), Level::Error, format!(
                                          "cannot apply equality operator to types `{}` and `{}`",
                                          lhs_ty, rhs_ty));
                    None
                }
            }
            Operator::And |
            Operator::Or |
            Operator::Xor => {
                if lhs_ty == rhs_ty && (lhs_ty == Type::Boolean || lhs_ty == Type::Infer) {
                    Some(lhs_ty)
                } else if lhs_ty == Type::Boolean && rhs_ty == Type::Infer ||
                          rhs_ty == Type::Boolean && lhs_ty == Type::Infer {
                    Some(Type::Boolean)
                } else {
                    self.issues.new_issue(infix.op_pos(), Level::Error, format!(
                                          "cannot apply logical operator to types `{}` and `{}`",
                                          lhs_ty, rhs_ty));
                    None
                }
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn typeof_prefix(&self, prefix: &Node<Prefix>) -> Option<Type> {
        let expr_ty = match self.typeof_expr(prefix.expr()) {
            Some(x) => x,
            None => {
                self.issues.new_issue(prefix.expr_pos(), Level::Error,
                                      "type of prefix expression could not be determined");
                return None;
            }
        };
        match prefix.op() {
            Operator::Sub => {
                if expr_ty == Type::Number {
                    Some(Type::Number)
                } else {
                    self.issues.new_issue(prefix.expr_pos(), Level::Error, format!(
                                          "expected `Number`, got `{:?}`", expr_ty));
                    None
                }
            }
            Operator::Not => {
                if expr_ty == Type::Boolean {
                    Some(Type::Boolean)
                } else {
                    self.issues.new_issue(prefix.expr_pos(), Level::Error, format!(
                                          "expected `Boolean`, got `{:?}`", expr_ty));
                    None
                }
            }
            _ => {
                unreachable!();
            }
        }
    }
}
