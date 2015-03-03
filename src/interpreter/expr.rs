use super::parser::{Parser, TokenStream, ParseData, ParseResult};
use super::lexer::{Token, Symbol, Operator};
use super::{CompileError, from_bool, is_truthy};
use super::scope::CowScope;
use super::function::Function;
use super::identifier::{Identifier, IdMap};
use super::functioncall::FunctionCall;
use std::num::Float;

#[derive(Debug, Clone)]
pub struct Expression {
    rpn: Vec<ExprToken>,
}

impl<'a> Parser<'a> for Expression {
    fn parse(tokens: TokenStream<'a>) -> ParseResult<Expression> {
        let mut tokens = tokens;
        let out = try!(to_expr_tokens(&mut tokens));
        let out = try!(shunting_yard(out));

        Ok(ParseData {
            ast: Expression {
                rpn: out,
            },
            token_offset: tokens.pos(),
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
    fn call(&self, scope: CowScope, _: &IdMap) -> Result<f32, CompileError> {
        eval_rpn(&self.rpn, scope)
    }
}

#[derive(Debug, Clone)]
enum ExprToken {
    Op(Operator),
    Value(f32),
    Var(Identifier),
    LParen,
    RParen,
    Func(FunctionCall),
}


#[derive(PartialEq)]
enum Associativity {
    Left,
    Right,
}

type ExprOperator = Operator;

impl ExprOperator {
    fn precedence(&self) -> i32 {
        use super::lexer::Operator::*;
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
        use super::lexer::Operator::*;
        match *self {
            Neg | Not => 1,
            _ => 2,
        }
    }

    fn associativity(&self) -> Associativity {
        use super::lexer::Operator::*;
        match *self {
            Exp => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

// Converts tokens from the lexer into ExprTokens, which are simplified to drop any strings and
// contain only information understandable by the expression parser.
fn to_expr_tokens<'a>(tokens: &mut TokenStream<'a>) -> Result<Vec<ExprToken>, CompileError> {
    if tokens.is_empty() {
        return Err(CompileError::new_static("empty expression in file"));
    }

    let mut out = Vec::new();

    loop {
        let sourcetoken = match tokens.next() {
            Some(v) => v,
            None => break,
        };
        match sourcetoken.token {
            Token::Const(v) => {
                out.push(ExprToken::Value(v))
            },

            Token::Operator(v) => {
                out.push(ExprToken::Op(v))
            },

            Token::Symbol(Symbol::LeftBracket) => {
                tokens.seek(-1);
                let res = try!(Parser::parse(*tokens));
                tokens.set_pos(res.token_offset);
                out.push(ExprToken::Func(res.ast));
            },

            Token::Symbol(v) => {
                out.push(match v {
                    Symbol::LeftParen => ExprToken::LParen,
                    Symbol::RightParen => ExprToken::RParen,
                    x => return Err(CompileError::new(format!("unexpected symbol in expression: `{:?}`", x))
                                   .with_pos(sourcetoken.pos)),
                })
            },

            // An identifier in the context of an expression is a variable
            Token::Ident(v) => {
                out.push(ExprToken::Var(v))
            },
        }
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
            &ExprToken::Value(_) | &ExprToken::Var(_) | &ExprToken::Func(_) => {
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
                                out.push(top);
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
                            out.push(op);
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
                out.push(top);
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

            &ExprToken::Var(id) => {
                match scope.get_var(id) {
                    Some(val) => stack.push(val),
                    None => return Err(CompileError::new(format!("Attempted to access a nonexistent variable (id={})", id)))
                }
            },

            &ExprToken::Op(op) => {
                use super::lexer::Operator::*;

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
