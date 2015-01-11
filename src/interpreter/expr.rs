use super::parser::Parser;
use super::lexer::{Token, TokenSlice};
use super::{CompileError, SourcePos, from_bool, is_truthy};
use super::scope::CowScope;
use super::function::Function;
use super::functioncall::FunctionCall;
use std::num::Float;

#[derive(Clone)]
pub struct Expression {
	rpn: Vec<ExprToken>,
	pos: SourcePos,
}

impl<'a> Parser<'a> for Expression {
	fn parse(tokens: &'a TokenSlice<'a>, scope: CowScope<'a>) -> Result<Expression, CompileError> {
		let out = try!(to_expr_tokens(tokens, scope));
		let out = try!(shunting_yard(out));

		Ok(Expression {
			rpn: out,
			pos: tokens[0].pos, // Point to the location in the source that the expression started at.
		})
	}
}

impl Expression {
	/// Replaces variables with their values in the given scope
	pub fn fold_scope(&mut self, scope: CowScope) {
		for tok in self.rpn.iter_mut() {
			match *tok {
				ExprToken::Var(id) => {
					match scope.get_var(id) {
						Some(id) => *tok = ExprToken::Value(id),
						None => { }
					}
				},
				_ => { }
			}
		}
	}
}

impl Function for Expression {
	/// Evaluates the value of the expression
	fn call(&self, scope: CowScope) -> Result<f32, CompileError> {
		eval_rpn(&self.rpn, scope)
	}
}

#[derive(Show, Clone)]
enum ExprToken {
	Op(Operator),
	Value(f32),
	Var(usize),
	LParen,
	RParen,
	Fn(FunctionCall),
}
#[derive(Show, Clone, Copy)]
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

impl Operator {
	fn parse(s: &str) -> Option<Operator> {
	use self::Operator::*;
		Some(match s {
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
			_ => return None,
		})
	}
}

#[derive(PartialEq)]
enum Associativity {
	Left,
	Right,
}

impl Operator {
	fn precedence(&self) -> i32 {
		use self::Operator::*;
		match *self {
			And | Or | Xor => 0,
			Equ | NotEqu | ApproxEqu => 1,
			Less | Greater | GreaterEqual | LessEqual => 2,
			Add | Sub => 3,
			Mul | Div | Mod => 4,
			Neg | Not | Exp => 6,
		}
	}

	fn num_args(&self) -> usize {
		use self::Operator::*;
		match *self {
			Neg | Not => 1,
			_ => 2,
		}
	}

	fn associativity(&self) -> Associativity {
		use self::Operator::*;
		match *self {
			Exp => Associativity::Right,
			_ => Associativity::Left,
		}
	}
}

// Converts tokens from the lexer into ExprTokens, which are simplified to drop any strings and
// contain only information understandable by the expression parser.
fn to_expr_tokens<'a>(tokens: &'a TokenSlice<'a>, scope: CowScope<'a>) -> Result<Vec<ExprToken>, CompileError> {
	if tokens.is_empty() {
		return Err(CompileError::new_static("empty expression in file"));
	}

	let mut out = Vec::new();
	let mut iter = tokens.iter().peekable();
	let mut index = 0us;

	loop {
		let sourcetoken = match iter.next() {
			Some(v) => v,
			None => break,
		};
		match sourcetoken.token {
			Token::Const(v) => {
				out.push(ExprToken::Value(v))
			},

			Token::Operator(v) => {
				out.push(ExprToken::Op(match Operator::parse(v) {
					Some(op) => op,
					None => return Err(CompileError::new_static("invalid operator in expression")
									   .with_pos(sourcetoken.pos)),
				}))
			},

			Token::Symbol(v) => {
				out.push(match v {
					'(' => ExprToken::LParen,
					')' => ExprToken::RParen,
					x => return Err(CompileError::new(format!("unexpected symbol in expression: `{}`", x))
								   .with_pos(sourcetoken.pos)),
				})
			},

			// An identifier in the context of an expression can represent a variable or a function
			Token::Ident(v) => {
				let is_fn = if let Some(t) = iter.peek() { t.token == Token::Symbol('(') } else { false };

				if is_fn {
					// Attempt to find the end of the function call
					let call_start = index;
					let mut call_end = index;
					let mut depth = 0u32;
					loop {
						match iter.next() {
							Some(t) => {
								match t.token {
									Token::Symbol('(') => depth += 1,
									Token::Symbol(')') => depth -= 1,
									_ => { },
								}
								call_end += 1;
								index += 1;
							},
							None => return Err(CompileError::new(format!("unexpected EOF in function call"))
											   .with_pos(sourcetoken.pos))
						}
						if depth == 0 {
							break;
						}
					}

					// Resolve it to a function
					match scope.func_id(v) {
						Some(_) => {
							let func = try!(Parser::parse(&tokens[call_start..call_end + 1], scope.clone()));
							out.push(ExprToken::Fn(func));
						},
						None => {
							return Err(CompileError::new(format!("function `{}` appears in expression but is not defined in scope", v)).with_pos(sourcetoken.pos))
						}
					}
				} else { // Identifier references a variable
					match scope.var_id(v) {
						Some(id) => out.push(ExprToken::Var(id)),
						None => {
							return Err(CompileError::new(format!("variable `{}` appears in expression but is not defined in scope", v)).with_pos(sourcetoken.pos))
						}
					}
				}
			},

			// Discard newlines
			Token::Newline => { },
		}
		index += 1;
	}

	// Handle special case with unary minus
	// If an subtraction operator is preceded by another operator, left paren, or the start of the
	// expression, make it a negation operator.
	for i in range(0, out.len()) {
		if let ExprToken::Op(Operator::Sub) = out[i] {
			if i == 0 || match out[i - 1] { ExprToken::Op(_) | ExprToken::LParen => true, _ => false } {
				out[i] = ExprToken::Op(Operator::Neg);
			}
		}
	}

	Ok(out)
}

// http://en.wikipedia.org/wiki/Shunting-yard_algorithm
fn shunting_yard<'a>(tokens: Vec<ExprToken>) -> Result<Vec<ExprToken>, CompileError> {
	let mut out = Vec::new();
	let mut stack: Vec<ExprToken> = Vec::new();

	for token in tokens.iter() {
		match token {
			&ExprToken::Value(_) | &ExprToken::Var(_) | &ExprToken::Fn(_) => {
				out.push(token.clone());
			},

			&ExprToken::Op(ref op1) => {
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
				stack.push(token.clone());
			},

			&ExprToken::LParen => {
				stack.push(token.clone());
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
						x => panic!("internal error: unexpected value on stack: `{:?}`", x)
					}
				}
				if !foundleft {
					return Err(CompileError::new_static("mismatched parens: skewed right"));
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
			ExprToken::LParen | ExprToken::RParen => return Err(CompileError::new_static("mismatched parens: skewed left")),
			x => panic!("internal error: non operator on stack: `{:?}`", x)
		}
	}

	Ok(out)
}

// http://en.wikipedia.org/wiki/Reverse_Polish_notation
fn eval_rpn<'s>(rpn: &Vec<ExprToken>, scope: CowScope<'s>) -> Result<f32, CompileError> {
	let mut stack = Vec::new();

	for t in rpn.iter() {
		match t {
			&ExprToken::Value(v) => {
				stack.push(v);
			},

			&ExprToken::Fn(ref v) => {
				stack.push(match v.call(scope.clone()) {
					Ok(v) => v,
					Err(e) => return Err(CompileError::new(format!("{}", e)))
				});
			}

			&ExprToken::Var(id) => {
				match scope.get_var(id) {
					Some(val) => stack.push(val),
					None => return Err(CompileError::new(format!("Attempted to access a nonexistent variable (id={})", id)))
				}
			},

			&ExprToken::Op(op) => {
				use self::Operator::*;

				// Pop args off stack
				let n = op.num_args();
				if stack.len() < n {
					return Err(CompileError::new(format!("invalid expression: not enough args for operator {:?}", op)));
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

			x => return Err(CompileError::new(format!("unexpected token in expression: `{:?}`", x)))
		}
	}
	match stack.len() {
		1 => {
			let val = stack.pop().unwrap();
			Ok(val)
		},
		0 => {
			Err(CompileError::new_static("zero values in expression"))
		},
		_ => {
			Err(CompileError::new_static("too many values in expression"))
		}
	}
}
