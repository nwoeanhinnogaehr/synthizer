#![feature(plugin, core, fs, io, collections, std_misc)]
#![plugin(regex_macros, docopt_macros)]

extern crate regex;
extern crate "rustc-serialize" as rustc_serialize;
extern crate docopt;

use std::fs::File;
use std::io::Read;

pub mod interpreter;

docopt!(Args, "
Usage:
  synthizer [--tokens] [--idmap] <file>
  synthizer --help

Options:
  -h, --help       Show this message.
  -t, --tokens     Show tokens
  -i, --idmap      Show identifier map
");

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    let filename = args.arg_file;
    //println!("input file: {}", filename);

    let mut file = match File::open(&filename) {
        Err(why) => panic!("couldn't open {}: {}", filename, why.description()),
        Ok(file) => file,
    };

    // This is all temporary testing cruft. Don't read it.
    let mut code = String::new();
    match file.read_to_string(&mut code) {
        Err(why) => panic!("couldn't read {}: {}", filename, why.description()),
        Ok(_) => {
            let idmap = interpreter::identifier::IdMap::new();
            match interpreter::lexer::lex(code.as_slice(), &idmap) {
                Ok(tok) => {
                    if args.flag_tokens {
                        println!("\nTokens (n={})", tok.len());
                        for t in tok.iter() {
                            println!("\t{}", t);
                        }
                        println!("");
                    }
                    let (mut scope, sin, sqrt, abs);
                    scope = interpreter::scope::Scope::new();
                    sin = interpreter::function::SinFunction::new();
                    sqrt = interpreter::function::SqrtFunction::new();
                    abs = interpreter::function::AbsFunction::new();
                    scope.set_func(idmap.id("~"), &sin);
                    scope.set_func(idmap.id("sqrt"), &sqrt);
                    scope.set_func(idmap.id("abs"), &abs);
                    if args.flag_idmap {
                        println!("Identifier map:");
                        for (v, n) in idmap.name_map.borrow().iter() {
                            println!("\t{} -> {}", v, n);
                        }
                        println!("");
                    }
                    let mut tok_str = interpreter::parser::TokenStream::new(tok.as_slice());
                    while !tok_str.is_empty() {
                        let res: interpreter::parser::ParseResult<interpreter::functiondef::FunctionDef>
                            = interpreter::parser::Parser::parse(tok_str);
                        match res {
                            Ok(x) => {
                                println!("{:?}", x);
                                tok_str.set_pos(x.token_offset);
                            },
                            Err(e) => {
                                println!("{}", e);
                                break;
                            },
                        }
                    }
                },
                Err(e) => {
                    println!("Error lexing: {}", e);
                }
            }
        }
    };
}
