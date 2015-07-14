use super::common::Context;
use super::ast::*;
use super::tokens::{NodeImpl, Node, Operator};
use super::types::{TypeTable};

use llvm;
use llvm::Compile;
use cbox::*;
use std::cell::RefMut;

pub fn codegen<'a>(ctxt: &'a Context<'a>) {
    CodeGenerator::new(ctxt).codegen();
}

struct CodeGenerator<'a> {
    ctxt: &'a Context<'a>,
    module: CSemiBox<'a, llvm::Module>,
    llvm: &'a CBox<llvm::Context>,
    types: RefMut<'a, TypeTable>,
    builder: CSemiBox<'a, llvm::Builder>,
}

impl<'a> CodeGenerator<'a> {
    fn new(ctxt: &'a Context<'a>) -> CodeGenerator<'a> {
        CodeGenerator {
            ctxt: ctxt,
            module: llvm::Module::new(&ctxt.filename, &ctxt.llvm),
            builder: llvm::Builder::new(&ctxt.llvm),
            llvm: &ctxt.llvm,
            types: ctxt.types.borrow_mut(),
        }
    }

    fn codegen(&mut self) {
        self.codegen_root(&self.ctxt.ast.borrow());
    }

    fn codegen_root(&mut self, root: &Root) {
        for item in root {
            match *item {
                Item::Assignment(ref x) => {
                    self.codegen_assignment(x);
                },
                Item::FunctionDef(ref x) => { },
            }
        }
        self.module.verify().unwrap();
        println!("{:?}", self.module);
    }

    fn codegen_assignment(&self, assign: &Assignment) {
        let val = self.expr_to_value(assign.expr());
        self.module.add_global_constant(&self.ctxt.lookup_name(assign.ident()), val);
    }

    fn expr_to_value(&self, expr: &Expression) -> &llvm::Value {
        match *expr {
            Expression::Constant(Node(v, _)) => v.compile(self.llvm),
            Expression::Boolean(Node(v, _)) => v.compile(self.llvm),
            Expression::Infix(ref v) => self.infix_to_value(v.item()),
            Expression::Prefix(ref v) => self.prefix_to_value(v.item()),
            _ => unimplemented!(),
        }
    }

    fn infix_to_value(&self, infix: &Infix) -> &llvm::Value {
        let lhs = self.expr_to_value(infix.left());
        let rhs = self.expr_to_value(infix.right());
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
            Operator::Xor => unimplemented!(),
            _ => unreachable!(),
        }
    }

    fn prefix_to_value(&self, prefix: &Prefix) -> &llvm::Value {
        let expr = self.expr_to_value(prefix.expr());
        match prefix.op() {
            Operator::Sub => self.builder.build_neg(expr),
            Operator::Not => self.builder.build_not(expr),
            _ => unreachable!(),
        }
    }
}

