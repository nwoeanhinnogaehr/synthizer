#![feature(slicing_syntax)]
#![feature(plugin)]
#![allow(unstable)]

extern crate regex;
#[plugin] extern crate regex_macros;
extern crate "rustc-serialize" as rustc_serialize;
extern crate docopt;
#[plugin] extern crate docopt_macros;

use std::io::File;
use std::borrow::Cow;

pub mod interpreter;

docopt!(Args, "
Usage:
  synthizer <file>
  synthizer --help

Options:
  -h, --help       Show this message.
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
					let mut scope = interpreter::scope::Scope::new();
					let sin = &interpreter::function::SinFunction::new();
					let sqrt = &interpreter::function::SqrtFunction::new();
					let abs = &interpreter::function::AbsFunction::new();
					scope.set_func(idmap.id("~"), sin);
					scope.set_func(idmap.id("sqrt"), sqrt);
					scope.set_func(idmap.id("abs"), abs);
					println!("identifier map: {:?}", idmap);
					let res: Result<interpreter::functiondef::FunctionDef, interpreter::CompileError> = interpreter::parser::Parser::parse(tok.as_slice(), Cow::Borrowed(&scope));
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
