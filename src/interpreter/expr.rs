// Used internally for evalulating expressions

use super::lexer::Token;
use super::lexer;
use super::{CompileError, SourcePos};
use super::scope::Scope;
use std::collections::HashMap;
use std::collections::hash_map::{Occupied, Vacant};
use std::num;

#[deriving(Show, Clone)]
enum ExprToken {
	Op(Operator),
	Value(f32),
	Var(uint),
	LParen,
	RParen,
}

#[deriving(Show, Clone)]
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

#[deriving(PartialEq)]
enum Associativity {
	Left,
	Right,
}

impl Operator {
	fn precedence(self) -> int {
		match self {
			And | Or | Xor => 0,
			Equ | NotEqu | ApproxEqu => 1,
			Less | Greater | GreaterEqual | LessEqual => 2,
			Add | Sub => 3,
			Mul | Div | Mod => 4,
			Neg | Not | Exp => 6,
		}
	}

	fn num_args(self) -> uint {
		match self {
			Neg | Not => 1,
			_ => 2,
		}
	}

	fn associativity(self) -> Associativity {
		match self {
			Exp => Right,
			_ => Left,
		}
	}
}

// An expression is a mathematical statement containing a number of operators, constants and
// variables. It evaluates to a single number.
#[deriving(Show, Clone)]
pub struct Expression<'a> {
	rpn: Vec<ExprToken>, // reverse polish notation
	pos: SourcePos,
}

impl<'a> Expression<'a> {
	// converts a token slice from the lexer into an expression that can be evaluated
	pub fn new<'s>(tok: &'a [Token<'a>], scope: &'s Scope<'a>) -> Result<Expression<'a>, CompileError> {
		let out = try!(to_expr_tokens(tok, scope));
		let out = match shunting_yard(out.as_slice()) {
			Ok(out) => out,
			Err(e) => return Err(CompileError { msg: e.to_string(), pos: tok[0].pos })
		};

		Ok(Expression {
			rpn: out,
			pos: tok[0].pos, // Point to the location in the source that the expression started at.
		})
	}

	// Replaces variables with their values in the given scope
	pub fn fold_scope(&mut self, scope: &'a Scope<'a>) {
		for tok in self.rpn.iter_mut() {
			match tok {
				&Var(id) => {
					match scope.get_var(id) {
						Some(id) => *tok = Value(id),
						None => { }
					}
				},
				_ => { }
			}
		}
	}

	// Evaluates the value of the expression
	pub fn eval(&self, scope: &'a Scope<'a>) -> Result<f32, CompileError> {
		match eval_rpn(self.rpn.as_slice(), scope) {
			Ok(val) => Ok(val),
			Err(e) => Err(CompileError { msg: e, pos: self.pos }),
		}
	}
}

// Converts tokens from the lexer into ExprTokens, which are simplified to drop any strings and
// contain only information understandable by the expression parser. Any variables encountered are
// defined in the scope.
fn to_expr_tokens(tok: &[Token], scope: &Scope) -> Result<Vec<ExprToken>, CompileError> {
	if tok.is_empty() {
		return Err(CompileError { msg: "empty expression in file".to_string(), pos: SourcePos { line: 0, col: 0 }});
	}
	let mut out = Vec::new();

	for &t in tok.iter() {
		match t.t {
			// Try to parse constants as f32. subject to maybe change but probably not.
			lexer::Const(v) => {
				if let Some(x) = from_str::<f32>(v) {
					out.push(Value(x))
				} else {
					panic!("internal error: error parsing constant, the lexer is probably broken");
				}
			},
			lexer::Operator(v) => {
				out.push(Op(match v {
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
						return Err(CompileError { msg: format!("unexpected operator in expression: `{}`", x), pos: t.pos });
					}
				}))
			},
			lexer::Paren(v) => {
				out.push(match v {
					"(" => LParen,
					")" => RParen,
					x => {
						return Err(CompileError { msg: format!("unexpected paren type in expression: `{}`", x), pos: t.pos });
					}
				})
			},
			// An identifier in the context of an expression is always a variable
			lexer::Ident(v) => {
				match scope.var_id(v) {
					Some(id) => out.push(Var(id)),
					None => {
						return Err(CompileError { msg: format!("variable `{}` appears in expression but is not defined in scope", v), pos: t.pos })
					}
				}
			},
			// Discard whitesapce
			lexer::Newline => { },

			x => {
				return Err(CompileError { msg: format!("unexpected token in expression `{}`", x), pos: t.pos });
			}
		}
	}

	// Handle special case with unary minus
	// If an subtraction operator is preceded by another operator, left paren, or the start of the
	// expression, make it a negation operator.
	// would be nice if you could use map_in_place here, but can't because of enumerate.
	let out: Vec<ExprToken> = out.iter().enumerate().map(|(i, &v)| {
		match v {
			Op(Sub) => {
				if i == 0 || match out[i-1] { Op(_) | LParen => true, _ => false } {
					Op(Neg)
				} else {
					v
				}
			},
			_ => v
		}
	}).collect();

	Ok(out)
}

// http://en.wikipedia.org/wiki/Shunting-yard_algorithm
fn shunting_yard(tok: &[ExprToken]) -> Result<Vec<ExprToken>, &'static str> {
	let mut out = Vec::new();
	let mut stack: Vec<ExprToken> = Vec::new();

	for &t in tok.iter() {
		match t {
			Value(_) | Var(_) => {
				out.push(t);
			}

			Op(op1) => {
				while stack.len() > 0 {
					let top = *stack.last().unwrap(); // unwrap() can't fail, see condition on while loop
					match top {
						Op(op2) => {
							if op1.associativity() == Left && op1.precedence() <= op2.precedence()
								|| op1.precedence() < op2.precedence() {
								stack.pop();
								out.push(top);
							} else {
								break;
							}
						},
						_ => break
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
						x => panic!("internal error: unexpected value on stack: `{}`", x)
					}
				}
				if !foundleft {
					return Err("mismatched parens: skewed right");
				}
			}
		};
	}

	while stack.len() > 0 {
		let top = stack[stack.len()-1];
		match top {
			Op(_) => {
				stack.pop();
				out.push(top);
			},
			LParen | RParen => return Err("mismatched parens: skewed left"),
			x => panic!("internal error: non operator on stack: `{}`", x)
		}
	}

	Ok(out)
}

// http://en.wikipedia.org/wiki/Reverse_Polish_notation
fn eval_rpn(rpn: &[ExprToken], scope: &Scope) -> Result<f32, String> {
	let mut stack = Vec::new();

	for &t in rpn.iter() {
		match t {
			Value(v) => {
				stack.push(v);
			},

			Var(id) => {
				match scope.get_var(id) {
					Some(val) => stack.push(val),
					None => return Err(format!("Attempted to access a nonexistent variable (id={})", id))
				}
			},

			Op(op) => {
				// Pop args off stack
				let n = op.num_args();
				if stack.len() < n {
					return Err(format!("invalid expression: not enough args for operator {}", op));
				}
				// unwrap() can't fail because of the size check above.
				let args: Vec<f32> = range(0, n).map(|_| stack.pop().unwrap()).collect();

				// Do calculation and push result
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

			x => return Err(format!("unexpected token in expression: `{}`", x))
		}
	}
	match stack.len() {
		1 => {
			let val = stack.pop().unwrap();
			Ok(val)
		},
		0 => {
			Err("zero values in expression".to_string())
		},
		_ => {
			Err("too many values in expression".to_string())
		}
	}
}
