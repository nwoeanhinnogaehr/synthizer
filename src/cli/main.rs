#![feature(plugin, optin_builtin_traits, vecmap)]
#![plugin(regex_macros, docopt_macros)]

extern crate docopt;
extern crate rustc_serialize;
extern crate interpreter;

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
use interpreter::tokens::{Node, SourcePos};
use std::collections::VecMap;

#[allow(dead_code)]
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
    let func  = ::interpreter::functions::Function::Builtin(::interpreter::functions::BuiltinFunction::new(fn_ty));
    let ty = ::interpreter::types::Type::Function(id);
    ctxt.functions.borrow_mut().insert(id, func);
    ctxt.types.borrow_mut().set_val(id, SourcePos::anon().index, ty);

    lex(&ctxt);
    parse(&ctxt);
    typecheck(&ctxt);
    println!("{}", *ctxt.issues.borrow());
}
