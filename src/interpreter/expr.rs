// Used internally for evalulating expressions

use super::lexer::{Token, TokenType};
use super::{CompileError, SourcePos, from_bool, is_truthy};
use super::scope::Scope;
use super::function::{Function, SyntFunctionCall};
use std::num::Float;

#[deriving(Show, Clone)]
enum ExprToken<'a> {
	Op(Operator),
	Value(f32),
	Var(uint),
	LParen,
	RParen,
	Fn(SyntFunctionCall<'a>),
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
		use self::Operator::*;
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
		use self::Operator::*;
		match self {
			Neg | Not => 1,
			_ => 2,
		}
	}

	fn associativity(self) -> Associativity {
		use self::Operator::*;
		match self {
			Exp => Associativity::Right,
			_ => Associativity::Left,
		}
	}
}

/// An expression is a mathematical statement containing a number of operators, constants and
/// variables. It evaluates to a single number.
#[deriving(Show, Clone)]
pub struct Expression<'a> {
	rpn: Vec<ExprToken<'a>>, // reverse polish notation
	pos: SourcePos,
}

impl<'a> Expression<'a> {
	/// Converts a token slice from the lexer into an expression that can be evaluated
	pub fn new(tok: &'a [Token<'a>], scope: &'a Scope<'a>) -> Result<Expression<'a>, CompileError> {
		let out = try!(to_expr_tokens(tok, scope));
		let out = match shunting_yard(out.as_slice()) {
			Ok(out) => out,
			Err(e) => return Err(CompileError { msg: e.to_string(), pos: Some(tok[0].pos) })
		};

		Ok(Expression {
			rpn: out,
			pos: tok[0].pos, // Point to the location in the source that the expression started at.
		})
	}

	/// Replaces variables with their values in the given scope
	pub fn fold_scope(&mut self, scope: &Scope) {
		for tok in self.rpn.iter_mut() {
			match tok {
				&ExprToken::Var(id) => {
					match scope.get_var(id) {
						Some(id) => *tok = ExprToken::Value(id),
						None => { }
					}
				},
				_ => { }
			}
		}
	}

	/// Evaluates the value of the expression
	pub fn eval<'s>(&self, scope: &'s Scope<'s>) -> Result<f32, CompileError> {
		match eval_rpn(self.rpn.as_slice(), scope) {
			Ok(val) => Ok(val),
			Err(e) => Err(CompileError { msg: e, pos: Some(self.pos) }),
		}
	}
}

// Converts tokens from the lexer into ExprTokens, which are simplified to drop any strings and
// contain only information understandable by the expression parser. Any variables encountered are
// defined in the scope.
fn to_expr_tokens<'a>(tok: &'a [Token<'a>], scope: &'a Scope<'a>) -> Result<Vec<ExprToken<'a>>, CompileError> {
	if tok.is_empty() {
		return Err(CompileError { msg: "empty expression in file".to_string(), pos: None });
	}
	let mut out = Vec::new();

	let mut iter = tok.iter().peekable();
	let mut index = 0u;
	loop {
		let t = match iter.next() {
			Some(t) => t,
			None => break,
		};
		match t.t {
			// Try to parse constants as f32. subject to maybe change but probably not.
			TokenType::Const(v) => {
				out.push(ExprToken::Value(v))
			},
			TokenType::Operator(v) => {
				use self::Operator::*;
				out.push(ExprToken::Op(match v {
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
						return Err(CompileError { msg: format!("unexpected operator in expression: `{}`", x), pos: Some(t.pos) });
					}
				}))
			},
			TokenType::Paren(v) => {
				out.push(match v {
					'(' => ExprToken::LParen,
					')' => ExprToken::RParen,
					x => {
						return Err(CompileError { msg: format!("unexpected paren type in expression: `{}`", x), pos: Some(t.pos) });
					}
				})
			},
			// An identifier in the context of an expression is always a variable except for when
			// it's a function.
			TokenType::Ident(v) => {
				let is_fn = if let Some(t) = iter.peek() { t.t == TokenType::Paren('(') } else { false };

				if is_fn {
					let call_start = index;
					let mut call_end = index;
					let mut depth = 0u;
					loop {
						match iter.next() {
							Some(token) => {
								match token.t {
									TokenType::Paren('(') => depth += 1,
									TokenType::Paren(')') => depth -= 1,
									_ => { },
								}
								call_end += 1;
								index += 1;
							},
							None => return Err(CompileError { msg: format!("unexpected end of file in function call"), pos: Some(t.pos) })
						}
						if depth == 0 {
							break;
						}
					}
					match scope.func_id(v) {
						Some(_) => {
							let func = try!(SyntFunctionCall::new(tok[call_start..call_end + 1], scope));
							out.push(ExprToken::Fn(func));
						},
						None => {
							return Err(CompileError { msg: format!("function `{}` appears in expression but is not defined in scope", v), pos: Some(t.pos) })
						}
					}
				} else {
					match scope.var_id(v) {
						Some(id) => out.push(ExprToken::Var(id)),
						None => {
							return Err(CompileError { msg: format!("variable `{}` appears in expression but is not defined in scope", v), pos: Some(t.pos) })
						}
					}
				}
			},
			// Discard whitesapce
			TokenType::Newline => { },

			x => {
				return Err(CompileError { msg: format!("unexpected token in expression `{}`", x), pos: Some(t.pos) });
			}
		}
		index += 1;
	}

	// Handle special case with unary minus
	// If an subtraction operator is preceded by another operator, left paren, or the start of the
	// expression, make it a negation operator.
	// would be nice if you could use map_in_place here, but can't because of enumerate.
	for i in range(0, out.len()) {
		match out[i] {
			ExprToken::Op(Operator::Sub) => {
				if i == 0 || match out[i - 1] { ExprToken::Op(_) | ExprToken::LParen => true, _ => false } {
					out[i] = ExprToken::Op(Operator::Neg);
				}
			}
			_ => { }
		}
	}

	Ok(out)
}

// http://en.wikipedia.org/wiki/Shunting-yard_algorithm
fn shunting_yard<'a>(tok: &[ExprToken<'a>]) -> Result<Vec<ExprToken<'a>>, &'static str> {
	let mut out = Vec::new();
	let mut stack: Vec<ExprToken> = Vec::new();

	for t in tok.iter() {
		match t {
			&ExprToken::Value(_) | &ExprToken::Var(_) | &ExprToken::Fn(_) => {
				out.push(t.clone());
			},

			&ExprToken::Op(op1) => {
				while stack.len() > 0 {
					let top = stack.last().unwrap().clone(); // unwrap() can't fail, see condition on while loop
					match top {
						ExprToken::Op(op2) => {
							if op1.associativity() == Associativity::Left && op1.precedence() <= op2.precedence()
								|| op1.precedence() < op2.precedence() {
								stack.pop();
								out.push(top.clone());
							} else {
								break;
							}
						},
						_ => break
					}
				}
				stack.push(t.clone());
			},

			&ExprToken::LParen => {
				stack.push(t.clone());
			},

			&ExprToken::RParen => {
				let mut foundleft = false;
				while stack.len() > 0 {
					let op = stack.pop().unwrap();
					match op {
						ExprToken::Op(_) => {
							out.push(op.clone());
						},
						ExprToken::LParen => {
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
		let top = stack[stack.len()-1].clone();
		match top {
			ExprToken::Op(_) => {
				stack.pop();
				out.push(top.clone());
			},
			ExprToken::LParen | ExprToken::RParen => return Err("mismatched parens: skewed left"),
			x => panic!("internal error: non operator on stack: `{}`", x)
		}
	}

	Ok(out)
}

// http://en.wikipedia.org/wiki/Reverse_Polish_notation
fn eval_rpn<'s>(rpn: &[ExprToken], scope: &'s Scope<'s>) -> Result<f32, String> {
	let mut stack = Vec::new();

	for t in rpn.iter() {
		match t {
			&ExprToken::Value(v) => {
				stack.push(v);
			},

			&ExprToken::Fn(ref v) => {
				stack.push(match v.call(scope) {
					Ok(v) => v,
					Err(e) => return Err(format!("{}", e)),
				});
			}

			&ExprToken::Var(id) => {
				match scope.get_var(id) {
					Some(val) => stack.push(val),
					None => return Err(format!("Attempted to access a nonexistent variable (id={})", id))
				}
			},

			&ExprToken::Op(op) => {
				use self::Operator::*;

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
						let c = (args[1]/args[0]).abs().fract()*args[0].abs();
						if args[1] > 0_f32 { c } else { -c }
					},
					Neg => {
						-args[0]
					},
					Not => {
						from_bool(is_truthy(-args[0]))
					},
					Less => {
						from_bool(args[1] < args[0])
					},
					Greater => {
						from_bool(args[1] > args[0])
					},
					LessEqual => {
						from_bool(args[1] <= args[0])
					},
					GreaterEqual => {
						from_bool(args[1] >= args[0])
					},
					Equ => {
						from_bool(args[1] == args[0])
					},
					NotEqu => {
						from_bool(args[1] != args[0])
					},
					ApproxEqu => {
						from_bool((args[1] - args[0]).abs() < 0.0001)
					},
					And => {
						from_bool(is_truthy(args[1]) && is_truthy(args[0]))
					},
					Or => {
						from_bool(is_truthy(args[1]) || is_truthy(args[0]))
					},
					Xor => {
						from_bool(is_truthy(args[1]) ^ is_truthy(args[0]))
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
