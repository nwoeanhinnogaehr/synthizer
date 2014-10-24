#![feature(if_let)]
#![feature(slicing_syntax)]
#![feature(phase)]
#[phase(plugin)]
extern crate regex_macros;
extern crate regex;

use std::io::File;

mod interpreter;

fn main() {
	let path = Path::new("examples/expr.synt");
	let display = path.display();
	println!("input file: {}", display);

	let mut file = match File::open(&path) {
		Err(why) => fail!("couldn't open {}: {}", display, why.desc),
		Ok(file) => file,
	};

	match file.read_to_string() {
		Err(why) => fail!("couldn't read {}: {}", display, why.desc),
		Ok(string) => {
			match interpreter::lexer::tokenize(string.as_slice()) {
				Ok(tok) => {
					println!("lexer says: {}", tok);
					match interpreter::expr::Expression::new(tok.as_slice()) {
						Ok(mut expr) => {
							expr.try_set_var("pi", 3.141592653589);
							println!("parser says: {}", expr);
							println!("expr evals to {}", expr.eval());
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
