#![feature(plugin, optin_builtin_traits, vecmap, vec_push_all)]
#![plugin(regex_macros, docopt_macros)]

extern crate regex;
extern crate llvm;
extern crate cbox;
extern crate bit_set;

pub mod common;
pub mod ident;
pub mod types;
pub mod tokens;
pub mod ast;
pub mod issue;
pub mod lexer;
pub mod parser;
pub mod functions;
pub mod typecheck;
pub mod codegen;
pub mod scope;

#[macro_use]
pub mod tests;
