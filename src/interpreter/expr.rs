// Used internally for evalulating expressions
// Note: for now, negative is falsy and positive (including zero) is truthy

use interpreter::lexer::Token;
use interpreter::lexer;
use std::collections::HashMap;
use std::collections::hashmap::{Occupied, Vacant};
use std::num;

#[deriving(Show)]
enum ExprToken<'a> {
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
        Equ | NotEqu | ApproxEqu => 1,
        Less | Greater | GreaterEqual | LessEqual => 2,
        And | Or | Xor => 0,
		Add => 3,
		Sub => 3,
		Mul => 4,
		Div => 4,
        Mod => 4,
        Neg => 6,
        Not => 6,
		Exp => 6,
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
	rpn: Vec<ExprToken<'a>>, // reverse polish notation
	var_values: Vec<f32>, // values of vars by id
	vars: HashMap<&'a str, uint>, // map of var name -> var id
	value: Option<f32>, // holds the last value of the expression, if it's been evaluated and vars have not been updated
}

impl<'a> Expression<'a> {
    // converts a token slice from the lexer into an expression that can be evaluated
    pub fn new<'a>(tok: Vec<Token<'a>>) -> Expression {
        let mut vars: HashMap<&'a str, uint> = HashMap::new();
        let out: Vec<ExprToken<'a>> = tok.iter().filter_map(|&t| {
            match t {
                // Try to parse constants as f32. subject to maybe change but probably not.
                lexer::Const(v) => {
                    if let Some(x) = from_str::<f32>(v) {
                        Some(Value(x))
                    } else {
                        fail!("invalid const, regex must be broken!");
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
                        _ => fail!("unexpected operator"),
                    }))
                },
                lexer::Paren(v) => {
                    if v == "(" {
                        Some(LParen)
                    } else if v == ")" {
                        Some(RParen)
                    } else {
                        None
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
                lexer::Whitespace(_) | lexer::Newline => {
                    None
                },
                _ => {
                    fail!("not expression!");
                }
            }
        }).collect();

        // Handle special case with unary minus
        // If an subtraction operator is preceded by another operator, left paren, or the start of the
        // expression, make it a negation operator.
        let out = out.iter().enumerate().map(|(i, &v)| {
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

        Expression {
            rpn: shunting_yard(out),
            var_values: Vec::from_elem(vars.len(), 0f32),
            vars: vars,
            value: None,
        }
    }

	// http://en.wikipedia.org/wiki/Reverse_Polish_notation
	pub fn eval(&mut self) -> f32 { 
		// If the expression is constant, return it's value
		if let Some(v) = self.value {
			return v;
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
						fail!("invalid expression: not enough args for operator {}", op);
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
                            let c = Float::fract(num::abs(args[1]/args[0]))*num::abs(args[1]);
                            if args[1] < 0f32 { -c } else { c }
                        },
                        Neg => {
                            -args[0]
                        },
                        Not => {
                            if args[0] > 0f32 { -1f32 } else { 1f32 }
                        },
                        Less => {
                            if args[1] < args[0] { 1f32 } else { -1f32 }
                        },
                        Greater => {
                            if args[1] > args[0] { 1f32 } else { -1f32 }
                        },
                        LessEqual => {
                            if args[1] <= args[0] { 1f32 } else { -1f32 }
                        },
                        GreaterEqual => {
                            if args[1] >= args[0] { 1f32 } else { -1f32 }
                        },
                        Equ => {
                            if args[1] == args[0] { 1f32 } else { -1f32 }
                        },
                        NotEqu => {
                            if args[1] != args[0] { 1f32 } else { -1f32 }
                        },
                        ApproxEqu => {
                            if args[1] - args[0] < 0.00001 { 1f32 } else { -1f32 }
                        },
                        And => {
                            if args[1] > 0f32 && args[0] > 0f32 { 1f32 } else { -1f32 }
                        },
                        Or => {
                            if args[1] > 0f32 || args[0] > 0f32 { 1f32 } else { -1f32 }
                        },
                        Xor => {
                            if (args[1] > 0f32) ^ (args[0] > 0f32) { 1f32 } else { -1f32 }
                        },
					});
				},
				Var(id) => {
					stack.push(self.var_values[id]);
				},
				_ => fail!("unexpected token in expression"),
			};
		}
		if stack.len() == 1 {
			let val = stack.pop().unwrap();
			self.value = Some(val);
			val
		} else {
			fail!("to many values in expression");
		}
	}

	pub fn var_id(&self, var: &'a str) -> Option<uint> {
		match self.vars.find(&var) {
			Some(v) => Some(*v),
			None => None,
		}
	}

	pub fn set_var(&mut self, var_id: uint, value: f32) {
		if var_id > self.var_values.len() {
			fail!("invalid var id passed to set_var");
		}
		self.var_values[var_id] = value;
		self.value = None // Setting a var clears evaled value
	}

	pub fn try_set_var(&mut self, var: &'a str, value: f32) {
		if let Some(id) = self.var_id(var) {
			self.set_var(id, value);
		}
	}
}

// http://en.wikipedia.org/wiki/Shunting-yard_algorithm
fn shunting_yard(tok: Vec<ExprToken>) -> Vec<ExprToken> {
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
						_ => fail!("not sure how this can happen, but maybe"),
					}
				}
				if !foundleft {
					fail!("mismatched parens");
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
			LParen | RParen => fail!("mismatched parens"),
			_ => fail!("non operator on stack!"),
		}
	}
	out
}
