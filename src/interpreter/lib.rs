#![feature(plugin, optin_builtin_traits)]
#![plugin(regex_macros, docopt_macros)]

extern crate regex;
extern crate llvm;
extern crate cbox;
extern crate bit_set;
extern crate sound_stream;
extern crate hound;
extern crate vec_map;
extern crate llvm_sys;

//pub mod common;
//pub mod ident;
//#[macro_use] pub mod types;
//pub mod tokens;
pub mod ast;
//pub mod issue;
//pub mod lexer;
//pub mod parser;
//pub mod functions;
//pub mod typecheck;
//pub mod codegen;
//pub mod scope;
//pub mod compiler;
//pub mod audio;
pub mod synthizer;

#[macro_use]
pub mod tests;

#[cfg(test)]
pub mod test_grammar;
