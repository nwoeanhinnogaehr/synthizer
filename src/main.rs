#![feature(plugin, collections)]
#![plugin(regex_macros, docopt_macros)]

extern crate regex;
extern crate rustc_serialize;
extern crate docopt;

pub mod interpreter;

docopt!(Args, "
Usage:
  synthizer <file>
  synthizer --help

Options:
  -h, --help       Show this message.
");

use interpreter::common::{Context, read_file};
use interpreter::lexer::lex;
use interpreter::parser::parse;
use interpreter::typecheck::typecheck;
use std::collections::VecMap;

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    let filename = args.arg_file;
    let source = read_file(&filename).unwrap();
    let ctxt = Context::new(filename, source);

    //TODO move this
    let id = ctxt.names.borrow_mut().new_id("sin");
    let arg_id = ctxt.names.borrow_mut().new_id("rad");
    let mut args = VecMap::new();
    args.insert(arg_id, ::interpreter::types::Type::Number);
    let fn_ty = ::interpreter::types::FunctionType { args: args, returns: ::interpreter::types::Type::Number };
    let func  = ::interpreter::functions::Function::Builtin(::interpreter::functions::BuiltinFunction { ty: fn_ty });
    let ty = ::interpreter::types::Type::Function(id);
    ctxt.functions.borrow_mut().insert(id, func);
    ctxt.types.borrow_mut().set_type(id, Some(ty));

    lex(&ctxt);
    parse(&ctxt);
    typecheck(&ctxt);
    println!("{}", *ctxt.issues.borrow());
}
