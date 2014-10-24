// Used internally for evalulating expressions
// Note: for now, negative is falsy and positive (including zero) is truthy

use interpreter::lexer::Token;
use interpreter::lexer;
use interpreter::parser;
use std::collections::HashMap;
use std::collections::hashmap::{Occupied, Vacant};
use std::num;

#[deriving(Show)]
enum ExprToken {
	Op(Operator),
	Value(f32),
	Var(uint),
	LParen,
	RParen,
}

#[deriving(Show)]
enum Operator {
	Add,
	Sub,
	Mul,
	Div,
	Exp,
	Mod,
	Neg,
	Less,
	Greater,
	Equ,
	NotEqu,
	ApproxEqu,
	Not,
	And,
	Or,
	Xor,
	GreaterEqual,
	LessEqual,
}

fn precedence(op: Operator) -> int {
	match op {
		And | Or | Xor => 0,
		Equ | NotEqu | ApproxEqu => 1,
		Less | Greater | GreaterEqual | LessEqual => 2,
		Add | Sub => 3,
		Mul | Div | Mod => 4,
		Neg | Not | Exp => 6,
	}
}

fn nargs(op: Operator) -> uint {
	match op {
		Neg | Not => 1,
		_ => 2,
	}
}

#[deriving(PartialEq)]
enum Associativity {
	Left,
	Right,
}

fn associativity(op: Operator) -> Associativity {
	match op {
		Exp => Right,
		_ => Left,
	}
}

#[deriving(Show)]
pub struct Expression<'a> {
	rpn: Vec<ExprToken>, // reverse polish notation
	var_values: Vec<f32>, // values of vars by id
	vars: HashMap<&'a str, uint>, // map of var name -> var id
	value: Option<f32>, // holds the last value of the expression, if it's been evaluated and vars have not been updated
}

impl<'a> Expression<'a> {
	// converts a token slice from the lexer into an expression that can be evaluated
	pub fn new(tok: &[Token<'a>]) -> Result<Expression<'a>, String> {
		let mut vars: HashMap<&'a str, uint> = HashMap::new();
		let mut err: Option<String> = None;
		let out: Vec<ExprToken> = tok.iter().filter_map(|&t| {
			if err.is_some() {
				return None;
			}
			match t {
				// Try to parse constants as f32. subject to maybe change but probably not.
				lexer::Const(v) => {
					if let Some(x) = from_str::<f32>(v) {
						Some(Value(x))
					} else {
						err = Some("internal error: error parsing constant, indicating the lexer is probably broken".to_string());
						None
					}
				},
				lexer::Operator(v) => {
					Some(Op(match v {
						"+" => Add,
						"-" => Sub,
						"*" => Mul,
						"/" => Div,
						"^" => Exp,
						"%" => Mod,
						"==" => Equ,
						"!=" => NotEqu,
						"~=" => ApproxEqu,
						"<" => Less,
						">" => Greater,
						"<=" => LessEqual,
						">=" => GreaterEqual,
						"!" => Not,
						"&&" => And,
						"||" => Or,
						"^^" => Xor,
						x => {
							err = Some(format!("unexpected operator: `{}`", x));
							return None; // report first error only
						}
					}))
				},
				lexer::Paren(v) => {
					match v {
						"(" => Some(LParen),
						")" => Some(RParen),
						x => {
							err = Some(format!("unexpected paren type: `{}`", x));
							return None;
						}
					}
				},
				// An identifier in the context of an expression is always a variable (TODO FALSE!!)
				lexer::Ident(v) => {
					let nv = vars.len();
					match vars.entry(v) {
						Occupied(entry) => {
							Some(Var(*entry.get()))
						},
						Vacant(entry) => {
							entry.set(nv);
							Some(Var(nv))
						},
					}
				},
				// Discard whitesapce
				lexer::Newline => {
					None
				},
				x => {
					err = Some(format!("unexpected token in expression `{}`", x));
					return None;
				}
			}
		}).collect();

		if let Some(e) = err {
			return Err(e)
		}

		// Handle special case with unary minus
		// If an subtraction operator is preceded by another operator, left paren, or the start of the
		// expression, make it a negation operator.
		let out: Vec<ExprToken> = out.iter().enumerate().map(|(i, &v)| {
			match v {
				Op(Sub) => {
					if i == 0 || match out[i-1] { Op(_) | LParen => true, _ => false } {
						Op(Neg)
					} else {
						v
					}
				},
				_ => v,
			}
		}).collect();

		let out = try!(shunting_yard(out.as_slice()));

		Ok(Expression {
			rpn: out,
			var_values: Vec::from_elem(vars.len(), 0f32),
			vars: vars,
			value: None,
		})
	}

	// http://en.wikipedia.org/wiki/Reverse_Polish_notation
	pub fn eval(&mut self) -> Result<f32, String> {
		// If the expression is constant, return it's value
		if let Some(v) = self.value {
			return Ok(v);
		}

		let mut stack: Vec<f32> = Vec::new();
		for &t in self.rpn.iter() {
			match t {
				Value(v) => {
					stack.push(v);
				},
				Op(op) => {
					let n = nargs(op);
					if stack.len() < n {
						return Err(format!("invalid expression: not enough args for operator {}", op));
					}
					let args: Vec<f32> = range(0, n).map(|_| stack.pop().unwrap()).collect();
					stack.push(match op {
						Add => {
							args[1] + args[0]
						},
						Sub => {
							args[1] - args[0]
						},
						Mul => {
							args[1] * args[0]
						},
						Div => {
							args[1] / args[0]
						},
						Exp => {
							args[1].powf(args[0])
						},
						Mod => {
							let c = num::abs(args[1]/args[0]).fract()*num::abs(args[0]);
							if args[1] < 0f32 { -c } else { c }
						},
						Neg => {
							-args[0]
						},
						Not => {
							if args[0] > 0f32
								{ -1f32 } else { 1f32 }
						},
						Less => {
							if args[1] < args[0]
								{ 1f32 } else { -1f32 }
						},
						Greater => {
							if args[1] > args[0]
								{ 1f32 } else { -1f32 }
						},
						LessEqual => {
							if args[1] <= args[0]
								{ 1f32 } else { -1f32 }
						},
						GreaterEqual => {
							if args[1] >= args[0]
								{ 1f32 } else { -1f32 }
						},
						Equ => {
							if args[1] == args[0]
								{ 1f32 } else { -1f32 }
						},
						NotEqu => {
							if args[1] != args[0]
								{ 1f32 } else { -1f32 }
						},
						ApproxEqu => {
							if num::abs(args[1] - args[0]) < 0.0001
								{ 1f32 } else { -1f32 }
						},
						And => {
							if args[1] > 0f32 && args[0] > 0f32
								{ 1f32 } else { -1f32 }
						},
						Or => {
							if args[1] > 0f32 || args[0] > 0f32
								{ 1f32 } else { -1f32 }
						},
						Xor => {
							if (args[1] > 0f32) ^ (args[0] > 0f32)
								{ 1f32 } else { -1f32 }
						},
					});
				},
				Var(id) => {
					stack.push(self.var_values[id]);
				},
				x => return Err(format!("unexpected token in expression: `{}`", x)),
			};
		}
		match stack.len() {
			1 => {
				let val = stack.pop().unwrap();
				self.value = Some(val);
				Ok(val)
			},
			0 => {
				Err("zero values in expression".to_string())
			},
			_ => {
				Err("too many values in expression".to_string())
			},
		}
	}

	// Returns a uint identifier which can be passed to set_var to quickly update the value of a
	// variable.
	pub fn var_id(&self, var: &'a str) -> Option<uint> {
		match self.vars.find(&var) {
			Some(v) => Some(*v),
			None => None,
		}
	}

	// Sets a variable to an id retrieved from var_id. Return value indicates whether it was set
	// sucessfully or not.
	pub fn set_var(&mut self, var_id: uint, value: f32) -> bool {
		if var_id > self.var_values.len() {
			return false;
		}
		self.var_values[var_id] = value;
		self.value = None; // Setting a var clears evaled value
		true
	}

	// Attempts to set a variable by name. If the variable does not exist, nothing is done.
	pub fn try_set_var(&mut self, var: &'a str, value: f32) {
		if let Some(id) = self.var_id(var) {
			self.set_var(id, value);
		}
	}
}

// http://en.wikipedia.org/wiki/Shunting-yard_algorithm
// todo iterators can be used better here
fn shunting_yard(tok: &[ExprToken]) -> Result<Vec<ExprToken>, String> {
	let mut out = Vec::new();
	let mut stack: Vec<ExprToken> = Vec::new();
	for &t in tok.iter() {
		match t {
			Value(_) | Var(_) => {
				out.push(t);
			}
			Op(op1) => {
				while stack.len() > 0 {
					let top = stack[stack.len()-1];
					match top {
						Op(op2) => {
							if (associativity(op1) == Left && precedence(op1) <= precedence(op2)) ||
								(precedence(op1) < precedence(op2)) {
								stack.pop();
								out.push(top);
							} else {
								break;
							}
						},
						_ => break,
					}
				}
				stack.push(t);
			},
			LParen => {
				stack.push(t);
			},
			RParen => {
				let mut foundleft = false;
				while stack.len() > 0 {
					let op = stack.pop().unwrap();
					match op {
						Op(_) => {
							out.push(op);
						},
						LParen => {
							foundleft = true;
							break;
						},
						x => return Err(format!("internal error: unexpected value on stack: `{}`", x)),
					}
				}
				if !foundleft {
					return Err("mismatched parens: skewed right".to_string());
				}
			},
		};
	}
	while stack.len() > 0 {
		let top = stack[stack.len()-1];
		match top {
			Op(_) => {
				stack.pop();
				out.push(top);
			},
			LParen | RParen => return Err("mismatched parens: skewed left".to_string()),
			x => return Err(format!("internal error: non operator on stack: `{}`", x)),
		}
	}
	Ok(out)
}
