use super::scope::CowScope;
use super::CompileError;
use super::lexer::{Token, Symbol, Operator};
use super::function::Function;
use super::parser::{self, Parser, TokenStream, ParseResult, ParseData};
use super::identifier::{IdMap, Identifier};
use super::expr::Expression;
use std::collections::VecMap;

/// Represents a function definition written in synthizer
#[derive(Debug)]
pub struct FunctionDef {
    statement: Option<Statement>,
    args: VecMap<Option<Expression>>, // Default arguments
    ident: Option<Identifier>,
}

impl FunctionDef {
    pub fn new() -> FunctionDef {
        FunctionDef {
            statement: None,
            args: VecMap::new(),
            ident: None,
        }
    }

    pub fn ident(&self) -> Identifier {
        self.ident.expect("incomplete function definition! no identifier was attached")
    }

    pub fn set_ident(&mut self, ident: Identifier) {
        self.ident = Some(ident);
    }

    pub fn set_statement(&mut self, s: Statement) {
        self.statement = Some(s);
    }

    pub fn statement(&self) -> &Statement {
        self.statement.as_ref().expect("incomplete function definition! no statement was attached")
    }

    pub fn statement_mut(&mut self) -> &mut Statement {
        self.statement.as_mut().expect("incomplete function definition! no statement was attached")
    }

    pub fn args(&self) -> &VecMap<Option<Expression>> {
        &self.args
    }

    pub fn args_mut(&mut self) -> &mut VecMap<Option<Expression>> {
        &mut self.args
    }
}

impl<'a> Parser<'a> for FunctionDef {
    /// Parse a function definition from a token stream. Scope is used to find function definitions
    fn parse(tokens: TokenStream<'a>) -> ParseResult<FunctionDef> {
        let mut tokens = tokens;
        let mut def = FunctionDef::new();

        try!(expect!(tokens.next(), Token::Symbol(Symbol::LeftBracket), "expected `[`, got `{}`"));

        def.set_ident(try!(expect_value!(tokens.next(), Token::Ident, "expected function name, got `{}`")));

        // Find arguments
        'outer: loop {
            let mut args = def.args_mut();
            let next = tokens.next();

            // If we hit a right bracket, the argument list is finished
            if expect!(next, Token::Symbol(Symbol::RightBracket)).is_ok() {
                break;
            }

            // Get argument name
            let arg_ident = try!(expect_value!(next, Token::Ident, "expected argument name, got `{}`"));
            if args.contains_key(&arg_ident) {
                return Err(CompileError::new(format!("argument {} defined twice", arg_ident))
                           .with_pos(next.unwrap().pos));
            }

            let next = tokens.next();
            match next.map(|x| x.token) {
                Some(Token::Symbol(Symbol::Comma)) => {
                    args.insert(arg_ident, None);
                },

                Some(Token::Symbol(Symbol::RightBracket)) => {
                    args.insert(arg_ident, None);
                    break 'outer;
                },

                Some(Token::Symbol(Symbol::Equals)) => {
                    let expr_start = tokens.pos();
                    loop {
                        let next = tokens.next();
                        match next.map(|x| x.token) {
                            Some(Token::Symbol(Symbol::LeftBracket)) => {
                                tokens.seek(-1);
                                tokens = try!(parser::match_paren(tokens,
                                                                  Token::Symbol(Symbol::LeftBracket),
                                                                  Token::Symbol(Symbol::RightBracket)));
                            }

                            Some(Token::Symbol(Symbol::Comma)) => {
                                args.insert(arg_ident,
                                        Some(try!(Parser::parse(
                                            tokens.slice(expr_start, tokens.pos()-1))).ast));
                                break;
                            },

                            Some(Token::Symbol(Symbol::RightBracket)) => {
                                args.insert(arg_ident,
                                        Some(try!(Parser::parse(
                                            tokens.slice(expr_start, tokens.pos()-1))).ast));
                                break 'outer;
                            },

                            None => return Err(CompileError::new_static(
                                    "expected expression, got EOF").with_pos(tokens.end_source_pos())),

                            Some(_) => { }
                        }
                    }
                }

                None => return Err(CompileError::new_static(
                        "expected `,`, `]`, or `=` following argument identifier, got EOF")
                    .with_pos(tokens.end_source_pos())),

                Some(x) => return Err(CompileError::new(format!(
                        "expected `,`, `]`, or `=` following argument identifier, got `{:?}`", x))
                    .with_pos(next.unwrap().pos)),
            }

        }

        def.set_statement(
            if !tokens.is_empty() {
                let res = try!(Parser::parse(tokens));
                tokens.set_pos(res.token_offset);
                res.ast
            } else {
                return Err(CompileError::new_static("expected block, got EOF")
                           .with_pos(tokens.end_source_pos()));
            });

        Ok(ParseData {
            ast: def,
            token_offset: tokens.pos(),
        })
    }
}
impl Function for FunctionDef {
    fn call(&self, _: CowScope, _: &IdMap) -> Result<f32, CompileError> {
        unimplemented!();
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub condition: Option<Expression>,
    pub operator: Operator,
    pub statement: Statement,
}

#[derive(Debug)]
pub enum Statement {
    Block(Vec<BlockStatement>),
    Expr(Expression),
}

impl<'a> Parser<'a> for Statement {
    fn parse(tokens: TokenStream<'a>) -> ParseResult<Statement> {
        let mut tokens = tokens;
        let statement = match expect!(tokens.peek(0), Token::Symbol(Symbol::LeftBrace)) {
            // If it starts with `{` it's a block
            Ok(_) => {
                let mut statements = Vec::new();
                tokens.next(); // consume brace
                loop {
                    let operator = match tokens.next().map(|x| x.token) {
                        Some(Token::Operator(op)) => op,
                        Some(Token::Symbol(Symbol::Semicolon)) => continue,
                        Some(Token::Symbol(Symbol::RightBrace)) => break,
                        Some(x) =>
                            return Err(CompileError::new(format!("expected operator, `;` or `}}`, got {}", x))
                                       .with_pos(tokens.peek(0).map(|x| x.pos).unwrap())),
                        None =>
                            return Err(CompileError::new_static("expected operator, ';' or `}`, got EOF")
                                       .with_pos(tokens.end_source_pos()))
                    };
                    let mut condition = None;
                    let start_pos = tokens.pos();
                    let mut pos;
                    if let Some(Token::Symbol(Symbol::LeftBrace)) = tokens.peek(0).map(|x| x.token) {
                        tokens = try!(parser::match_paren(tokens,
                                                          Token::Symbol(Symbol::LeftBrace),
                                                          Token::Symbol(Symbol::RightBrace)));
                    }
                    loop {
                        pos = tokens.pos();
                        match tokens.next().map(|x| x.token) {
                            Some(Token::Symbol(Symbol::Semicolon)) => break,
                            Some(Token::Symbol(Symbol::QuestionMark)) => {
                                // parse condition
                                let start_pos = pos;
                                let mut pos;
                                loop {
                                    pos = tokens.pos();
                                    match tokens.next().map(|x| x.token) {
                                        Some(Token::Symbol(Symbol::Semicolon)) => break,
                                        Some(_) => { },
                                        None => return Err(CompileError::new_static(
                                                "expected `;` to end condition, got EOF")
                                            .with_pos(tokens.end_source_pos())),
                                    }
                                }
                                condition = Some(try!(Parser::parse(
                                            tokens.slice(start_pos+1, pos))).ast);
                                break;
                            }
                            Some(_) => { }
                            None =>
                                return Err(CompileError::new_static(
                                        "expected `;` or `?` to end expression, got EOF")
                                    .with_pos(tokens.end_source_pos())),
                        }
                    }
                    let res = try!(Parser::parse(tokens.slice(start_pos, pos)));
                    statements.push(BlockStatement {
                        condition: condition,
                        operator: operator,
                        statement: res.ast,
                    });
                }
                Statement::Block(statements)
            }

            // otherwise it's an expression
            Err(_) => {
                let res = try!(Parser::parse(tokens));
                tokens.set_pos(res.token_offset);
                Statement::Expr(res.ast)
            }
        };
        Ok(ParseData {
            ast: statement,
            token_offset: tokens.pos(),
        })
    }
}
impl Function for Statement {
    fn call(&self, scope: CowScope, idmap: &IdMap) -> Result<f32, CompileError> {
        match self {
            &Statement::Expr(ref x) =>
                x.call(scope, idmap),
            _ => unimplemented!(),
        }
    }
}
