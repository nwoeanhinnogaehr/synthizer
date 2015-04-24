#![feature(plugin, collections, into_cow, debug_builders)]
#![plugin(regex_macros, docopt_macros)]

extern crate regex;
extern crate rustc_serialize;
extern crate docopt;

use std::fs::File;
use std::io::Read;

pub mod interpreter;

use interpreter::issue::IssueTracker;
use interpreter::parser::Parser;

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
        Err(why) => panic!("couldn't open {}: {}", filename, why),
        Ok(file) => file,
    };

    // This is all temporary testing cruft. Don't read it.
    let mut code = String::new();
    match file.read_to_string(&mut code) {
        Err(why) => panic!("couldn't read {}: {}", filename, why),
        Ok(_) => {
            let (issues, symtab, ast);
            issues = IssueTracker::new(&filename, &code);
            symtab = interpreter::symbol::SymbolTable::new();
            if let Some(tok) = interpreter::lexer::lex(&issues, &code, &symtab) {
                println!("lex: true");
                let mut parser = Parser::new(tok, &issues);
                match parser.parse() {
                    Some(x) => {
                        println!("parse: true");
                        ast = x; // borrow checker madness... move into a longer span
                        let mut typechecker = interpreter::typecheck::TypeChecker::new(&ast, &symtab, &issues);
                        let ok = typechecker.check();
                        println!("typecheck: {}", ok);
                    }
                    None => {
                        println!("parse: false");
                    },
                }
            } else {
                println!("lex: false");
            }
            println!("{}", issues);
            if !issues.is_ok() {
                return;
            }
        }
    };
}
