#![feature(plugin, core, collections, std_misc)]
#![plugin(regex_macros, docopt_macros)]

extern crate regex;
extern crate "rustc-serialize" as rustc_serialize;
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
            let (issues, idmap);
            issues = IssueTracker::new(&filename, &code);
            idmap = interpreter::identifier::IdMap::new();
            match interpreter::lexer::lex(&issues, code.as_slice(), &idmap) {
                Some(tok) => {
                    if args.flag_tokens {
                        println!("\nTokens (n={})", tok.len());
                        for t in tok.iter() {
                            println!("\t{:?}", t);
                        }
                        println!("");
                    }
                    if args.flag_idmap {
                        println!("Identifier map:");
                        for (v, n) in idmap.name_map.borrow().iter() {
                            println!("\t{} -> {}", v, n);
                        }
                        println!("");
                    }

                    let mut parser = Parser::new(tok, &issues);
                    let ast = parser.parse();
                    println!("{:?}", ast);
                },
                None => { }
            }

            println!("{}", issues);
            if !issues.is_ok() {
                return;
            }
        }
    };
}
