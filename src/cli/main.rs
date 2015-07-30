#![feature(plugin, optin_builtin_traits)]
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
use interpreter::codegen::codegen;

#[allow(dead_code)]
fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    let filename = args.arg_file;
    let source = read_file(&filename).unwrap();
    let ctxt = Context::new(filename, source);
    ctxt.add_intrinsics();

    lex(&ctxt);
    parse(&ctxt);
    typecheck(&ctxt);
    codegen(&ctxt);
    println!("{}", *ctxt.issues.borrow());
}
