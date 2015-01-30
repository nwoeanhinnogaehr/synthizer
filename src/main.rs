#![feature(slicing_syntax, plugin, core, io, std_misc, path, collections)]

extern crate regex;
#[plugin] extern crate regex_macros;
extern crate "rustc-serialize" as rustc_serialize;
#[plugin] extern crate docopt_macros;
extern crate docopt;

use std::old_io::File;
use std::borrow::Cow;

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
	let path = Path::new(args.arg_file);
	let display = path.display();
	println!("input file: {}", display);

	let mut file = match File::open(&path) {
		Err(why) => panic!("couldn't open {}: {}", display, why.desc),
		Ok(file) => file,
	};

	match file.read_to_string() {
		Err(why) => panic!("couldn't read {}: {}", display, why.desc),
		Ok(string) => {
			let idmap = interpreter::identifier::IdMap::new();
			match interpreter::lexer::lex(string.as_slice(), &idmap) {
				Ok(tok) => {
					if args.flag_tokens {
						println!("\nTokens:");
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
					let res: Result<interpreter::functiondef::FunctionDef, interpreter::CompileError>
						= interpreter::parser::Parser::parse(interpreter::parser::TokenStream::new(tok.as_slice()), Cow::Borrowed(&scope));
					match res {
						Ok(x) => {
							println!("{:?}", x);
						},
						Err(e) => {
							println!("{}", e);
						},
					}
				},
				Err(e) => {
					println!("Error lexing: {}", e);
				}
			}
		}
	};
}
