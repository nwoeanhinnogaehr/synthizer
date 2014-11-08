#![feature(if_let)]
#![feature(slicing_syntax)]
#![feature(phase)]
#![feature(macro_rules)]
#![allow(dead_code)]
#[phase(plugin)]
extern crate regex_macros;
extern crate regex;
extern crate time;

use std::io::File;

mod interpreter;

fn main() {
	let path = Path::new("examples/expr.synt");
	let display = path.display();
	println!("input file: {}", display);

	let mut file = match File::open(&path) {
		Err(why) => panic!("couldn't open {}: {}", display, why.desc),
		Ok(file) => file,
	};

	match file.read_to_string() {
		Err(why) => panic!("couldn't read {}: {}", display, why.desc),
		Ok(string) => {
			match interpreter::lexer::tokenize(string.as_slice()) {
				Ok(tok) => {
					println!("tokens: {}\n\n", tok);
					let scope = interpreter::scope::Scope::new();
					match interpreter::expr::Expression::new(tok.as_slice(), &scope) {
						Ok(expr) => {
							println!("expr: {}", expr);
							println!("evals to {}", expr.eval(&scope));
						},
						Err(e) => {
							println!("Error parsing expr: {}", e);
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
