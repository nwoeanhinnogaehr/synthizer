#![feature(plugin, optin_builtin_traits, vecmap, vec_push_all, bitset)]
#![plugin(regex_macros, docopt_macros)]

extern crate regex;

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

#[macro_use]
pub mod tests;
