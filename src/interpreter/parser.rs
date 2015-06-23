use super::issue::Level;
use super::tokens::{SourcePos, Token, Symbol, Bracket, Associativity, Node, NodeImpl};
use super::ident::Identifier;
use super::common::Context;
use super::ast::*;
use super::functions::{self, FunctionTable};

use std::borrow::Cow;
use std::cell::{Ref, RefMut};

macro_rules! try_opt(
    ( $val:expr ) => {
        match $val {
            Some(x) => x,
            None => return None,
        }
    }
);

macro_rules! expect_value(
    ( $tokens:ident, $ty:path ) => {
        $tokens.next().and_then(|t| {
            match t.item() {
                &$ty(v) => Some(Node(v, t.pos())),
                _ => None
            }
        })
    };
);

macro_rules! expect(
    ( $tokens:ident, $ty:pat ) => {
        $tokens.next().and_then(|x| {
            match x.item() {
                &$ty => Some(x),
                _ => None,
            }
        })
    };
);

pub fn parse<'a>(ctxt: &'a Context<'a>) {
    let mut parser = Parser::new(ctxt);
    parser.parse();
}

#[derive(Debug)]
struct Parser<'a> {
    ctxt: &'a Context<'a>,
    tokens: Ref<'a, Vec<Node<Token>>>,
    functions: RefMut<'a, FunctionTable>,
    sub_stack: Vec<(usize, usize, usize)>,
}

impl<'a> Parser<'a> {
    fn new(ctxt: &'a Context<'a>) -> Parser<'a> {
        let len = ctxt.tokens.borrow().len();
        Parser {
            ctxt: ctxt,
            tokens: ctxt.tokens.borrow(),
            functions: ctxt.functions.borrow_mut(),
            sub_stack: vec![(0, 0, len)],
        }
    }

    fn next(&mut self) -> Option<Node<Token>> {
        self.seek(1);
        self.peek(-1)
    }

    fn next_token(&mut self) -> Option<Token> {
        self.seek(1);
        self.peek(-1).item()
    }

    fn is_empty(&self) -> bool {
        self.index() >= self.end_index()
    }

    fn index(&self) -> usize {
        self.sub_stack.last().unwrap().0
    }

    fn start_index(&self) -> usize {
        self.sub_stack.last().unwrap().1
    }

    fn end_index(&self) -> usize {
        self.sub_stack.last().unwrap().2
    }

    fn len(&self) -> usize {
        self.end_index() - self.start_index()
    }

    fn enter_subsection(&mut self, from: usize, to: usize) {
        //println!("enter {} {}", from, to);
        assert!(from >= self.start_index() && to <= self.end_index() && from <= to);
        let index = self.index();
        self.sub_stack.push((index, from, to));
    }

    // returns index subsection was left at
    fn leave_subsection(&mut self) -> usize {
        //println!("leave");
        let section = self.sub_stack.pop().unwrap();
        section.0
    }

    // leaves subsection and replaces index with index of subsection
    fn integrate_subsection(&mut self) {
        let index = self.leave_subsection();
        self.set_index(index);
    }

    fn offset(&self, offset: isize) -> usize {
        ((self.index() as isize) + offset) as usize
    }

    fn set_index(&mut self, index: usize) {
        self.sub_stack.last_mut().unwrap().0 = index;
    }

    fn seek(&mut self, offset: isize) {
        let index = self.offset(offset);
        self.set_index(index);
    }

    fn in_bounds(&self, index: usize) -> bool {
        if index >= self.end_index() || index < self.start_index() {
            false
        } else {
            true
        }
    }

    fn peek(&self, offset: isize) -> Option<Node<Token>> {
        let index = self.offset(offset);
        if self.in_bounds(index) {
            self.peek_index(index)
        } else {
            None
        }
    }

    fn peek_index(&self, index: usize) -> Option<Node<Token>> {
        if self.in_bounds(index) {
            Some(self.tokens[index])
        } else {
            None
        }
    }

    fn peek_token(&self, offset: isize) -> Option<Token> {
        self.peek(offset).item()
    }

    fn peek_source_pos(&self, offset: isize) -> Option<SourcePos> {
        self.peek(offset).pos()
    }

    fn peek_source_pos_or_end(&self, offset: isize) -> SourcePos {
        self.peek_source_pos(offset).unwrap_or(self.end_source_pos())
    }

    fn end_source_pos(&self) -> SourcePos {
        if self.tokens.len() > 0 {
            let index = self.end_index() - 1;
            if index < self.tokens.len() {
                let mut pos = self.tokens[index].pos();
                pos.add_chars(1);
                pos
            } else {
                panic!("internal error");
            }
        } else {
            SourcePos::new()
        }
    }

    // Advances to the matching parenthesis
    // Expects the first opening paren to already have been consumed.
    fn match_bracket(&mut self) -> bool {
        let open = match self.peek_token(-1) {
            Some(x) => x,
            _ => return false,
        };
        let close = match open {
            Token::Symbol(Symbol::LeftBracket(x)) =>
                Token::Symbol(Symbol::RightBracket(x)),
            _ => return false,
        };

        let mut depth = 1i32;
        while depth > 0 {
            match self.next_token() {
                Some(x) if x == open => {
                    depth += 1;
                }
                Some(x) if x == close => {
                    depth -= 1;
                }
                None => {
                    self.emit_error_here(format!("expected `{}`", close));
                    return false;
                }
                _ => { }
            }
        }
        return true;
    }

    fn emit_error_here<S>(&self, msg: S) where S: Into<Cow<'static, str>> {
        self.ctxt.issues.borrow_mut().new_issue(self.ctxt, self.peek_source_pos_or_end(-1),
                                                Level::Error, msg);
    }

    pub fn parse(&mut self) {
        let mut items = self.ctxt.ast.borrow_mut();
        while !self.is_empty() {
            match self.parse_item() {
                Some(i) => items.push(i),
                None => return,
            }
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        let expr = try_opt!(self.pratt_expression(0));
        self.seek(1);
        Some(expr)
    }

    // Implements a Pratt parser for parsing expressions
    fn pratt_expression(&mut self, rbp: i32) -> Option<Expression> {
        //println!("expr {}", rbp);
        let mut token = self.next();
        let mut left = try_opt!(self.pratt_nud(token));
        let mut next = self.next();
        while rbp < try_opt!(self.pratt_lbp(next)) {
            token = next;
            left = try_opt!(self.pratt_led(left, token));
            next = self.next();
        }
        self.seek(-1); // when we break out of the loop, next hasn't been consumed yet, so we need to step back
        Some(left)
    }

    fn pratt_nud(&mut self, token: Option<Node<Token>>) -> Option<Expression> {
        //println!("nud {:?}", token);
        match token.item() {
            Some(Token::Const(v)) => Some(Expression::Constant(Node(v, token.pos().unwrap()))),

            Some(Token::Boolean(v)) => Some(Expression::Boolean(Node(v, token.pos().unwrap()))),

            Some(Token::Ident(id)) => Some(Expression::Variable(Node(id, token.pos().unwrap()))),

            // unary operator
            Some(Token::Operator(op)) if op.can_take_x_args(1) => {
                Some(Expression::Prefix(Box::new(Node(Prefix {
                    op: Node(op, token.pos().unwrap()),
                    expr: try_opt!(self.pratt_expression(100))
                }, token.pos().unwrap()))))
            }

            // start of group
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Round))) => {
                let expr = try_opt!(self.pratt_expression(1));
                if expect!(self, Token::Symbol(Symbol::RightBracket(Bracket::Round))).is_none() {
                    self.emit_error_here("expected `)`");
                    return None;
                }
                Some(expr)
            }

            // start of block
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Curly))) => {
                self.seek(-1);
                let block = try_opt!(self.parse_block());
                Some(Expression::Block(block))
            }

            // closure
            Some(Token::Symbol(Symbol::Backslash)) => {
                self.seek(-1);
                let def = try_opt!(self.parse_closure());
                Some(Expression::Closure(Box::new(def)))
            }

            _ => {
                self.emit_error_here("expected constant, variable, opening bracket or unary operator");
                return None;
            }
        }
    }

    fn pratt_led(&mut self, left: Expression, right: Option<Node<Token>>) -> Option<Expression> {
        //println!("led {:?} {:?}", left, right);
        match right.item() {
            // binary operator
            Some(Token::Operator(op)) => {
                if !op.can_take_x_args(2) {
                    self.emit_error_here("expected binary operator");
                    return None;
                }
                let precedence = op.precedence() -
                    if op.associativity() == Associativity::Right { 1 } else { 0 };
                let pos = left.pos();
                Some(Expression::Infix(Box::new(Node(Infix {
                    op: Node(op, right.pos().unwrap()),
                    left: left,
                    right: try_opt!(self.pratt_expression(precedence)),
                }, pos))))
            }

            // extra closing paren
            Some(Token::Symbol(Symbol::RightBracket(Bracket::Round))) => {
                self.emit_error_here("unexpected `)`");
                return None;
            }

            // function call
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Round))) |
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Curly))) |
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Square))) => {
                self.seek(-1);
                let call = try_opt!(self.parse_function_call(left));
                Some(Expression::FunctionCall(Box::new(call)))
            }

            // if
            Some(Token::Symbol(Symbol::If)) => {
                let pos = left.pos();
                let then = left;
                let cond = try_opt!(self.pratt_expression(1));
                if let Some(Token::Symbol(Symbol::Else)) = self.next_token() {
                    let els = try_opt!(self.pratt_expression(1));
                    Some(Expression::Conditional(Box::new(Node(Conditional {
                        cond: cond,
                        then: then,
                        els: Some(els),
                    }, pos))))
                } else {
                    self.seek(-1);
                    Some(Expression::Conditional(Box::new(Node(Conditional {
                        cond: cond,
                        then: then,
                        els: None,
                    }, pos))))
                }
            }

            _ => {
                panic!("expected left denotation");
            }
        }
    }

    fn pratt_lbp(&mut self, token: Option<Node<Token>>) -> Option<i32> {
        //println!("lbp {:?}", token);
        match token.item() {
            Some(Token::Operator(op)) => Some(op.precedence()),

            Some(Token::Symbol(Symbol::If)) => Some(2),
            Some(Token::Symbol(Symbol::Else)) => Some(0),

            // end of group
            Some(Token::Symbol(Symbol::RightBracket(Bracket::Round))) => Some(1),

            // function call
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Round))) |
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Curly))) |
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Square))) => Some(1000),

            None => Some(0),
            _ => {
                self.emit_error_here("expected binary operator or function call");
                return None;
            }
        }
    }

    fn parse_assignment(&mut self) -> Option<Node<Assignment>> {
        let pos = self.peek_source_pos_or_end(0);
        let ident = try_opt!(self.parse_ident());
        try_opt!(self.parse_symbol(Symbol::Equals));
        let expr = try_opt!(self.parse_expression());

        Some(Node(Assignment {
            ident: ident,
            expr: expr
        }, pos))
    }

    fn parse_symbol(&mut self, symbol: Symbol) -> Option<()> {
        match self.next_token() {
            Some(Token::Symbol(x)) if x == symbol => Some(()),
            _ => {
                self.emit_error_here(format!("expected {}", symbol));
                None
            },
        }
    }

    fn parse_ident(&mut self) -> Option<Node<Identifier>> {
        match expect_value!(self, Token::Ident) {
            None => {
                self.emit_error_here("expected identifier");
                None
            }
            x => x,
        }
    }

    fn parse_function_def(&mut self) -> Option<Node<FunctionDef>> {
        let pos = self.peek_source_pos_or_end(0);
        let ident = try_opt!(self.parse_ident());
        let func = try_opt!(self.parse_function());

        self.functions.insert(*ident, functions::Function::User(
                functions::UserFunction {
                    ty: None,
                    node: Node(func.item().clone(), pos)
                }));

        Some(Node(FunctionDef {
            ident: ident,
            func: func,
        }, pos))
    }

    fn parse_closure(&mut self) -> Option<Node<FunctionDef>> {
        let pos = self.peek_source_pos_or_end(0);
        try_opt!(self.parse_symbol(Symbol::Backslash));
        let func = try_opt!(self.parse_function());
        let ident = self.ctxt.names.borrow_mut().new_anon();
        self.functions.insert(ident, functions::Function::User(
                functions::UserFunction {
                    ty: None,
                    node: Node(func.item().clone(), pos)
                }));

        Some(Node(FunctionDef {
            ident: Node(ident, pos),
            func: func,
        }, pos))
    }

    fn find_smart(&mut self, search: Token) -> Option<usize> {
        let start_idx = self.index();
        loop {
            let token = self.next_token();
            match token {
                Some(t) if t == search => {
                    break;
                }
                Some(Token::Symbol(Symbol::LeftBracket(_))) => {
                    if !self.match_bracket() {
                        self.set_index(start_idx);
                        return None;
                    }
                },
                Some(Token::Symbol(Symbol::Backslash)) => {
                    let closure_block = try_opt!(self.find_smart(
                            Token::Symbol(Symbol::LeftBracket(Bracket::Curly))));
                    self.set_index(closure_block+1);
                    if !self.match_bracket() {
                        self.set_index(start_idx);
                        return None;
                    }
                }
                None => {
                    self.set_index(start_idx);
                    return None;
                },
                _ => { }
            }
        }
        self.seek(-1);
        let index = self.index();
        assert!(self.peek_token(0).unwrap() == search);
        self.set_index(start_idx);
        Some(index)
    }

    fn parse_function(&mut self) -> Option<Node<Function>> {
        let pos = self.peek_source_pos_or_end(0);

        let idx = self.index();
        let block_idx = match self.find_smart(Token::Symbol(Symbol::LeftBracket(Bracket::Curly))) {
            Some(x) => x,
            None => {
                self.ctxt.emit_error("expected `{` to start function block", self.end_source_pos());
                return None;
            }
        };
        // parse arg list (everything until start of block)
        self.enter_subsection(idx, block_idx);
        let args = try_opt!(self.parse_arg_list(true));
        self.integrate_subsection();

        let block = try_opt!(self.parse_block());

        Some(Node(Function {
            args: args,
            block: block,
        }, pos))
    }

    fn parse_block(&mut self) -> Option<Node<Block>> {
        let pos = self.peek_source_pos_or_end(0);
        try_opt!(self.parse_symbol(Symbol::LeftBracket(Bracket::Curly)));
        let brace = self.find_smart(Token::Symbol(Symbol::RightBracket(Bracket::Curly)));
        if brace.is_none() {
            self.ctxt.emit_error("expected `}`", self.end_source_pos());
            return None;
        }
        let mut stmts = Vec::new();
        let idx = self.index();
        self.enter_subsection(idx, brace.unwrap());
        loop {
            let semi = self.find_smart(Token::Symbol(Symbol::Semicolon));
            let semi_idx = semi.unwrap_or(self.end_index());
            if semi_idx == self.index() {
                break;
            }
            let idx = self.index();
            self.enter_subsection(idx, semi_idx);
            let stmt = try_opt!(self.parse_statement());
            stmts.push(stmt);
            self.integrate_subsection();
            if semi.is_none() {
                self.seek(-1);
                break;
            }
        }
        self.integrate_subsection();
        self.seek(1);
        Some(Node(stmts, pos))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match (self.next_token(), self.next_token()) {
            (Some(Token::Ident(_)), Some(Token::Symbol(Symbol::Equals))) => {
                self.seek(-2);
                Some(Statement::Assignment(try_opt!(self.parse_assignment())))
            },
            _ => {
                self.seek(-2);
                Some(Statement::Expression(try_opt!(self.parse_expression())))
            }
        }
    }

    // an item is a top level construct: either an assignment or a function definition
    fn parse_item(&mut self) -> Option<Item> {
        try_opt!(self.parse_ident());

        match self.next_token() {
            Some(Token::Symbol(Symbol::Equals)) => {
                self.seek(-2);
                let semi = self.find_smart(Token::Symbol(Symbol::Semicolon));
                if semi.is_none() {
                    self.ctxt.emit_error("expected `;`", self.end_source_pos());
                    return None;
                }
                let idx = self.index();
                self.enter_subsection(idx, semi.unwrap());
                let assign = try_opt!(self.parse_assignment());
                self.integrate_subsection();
                Some(Item::Assignment(assign))
            },
            _ => {
                self.seek(-1);
                let brace = self.find_smart(Token::Symbol(Symbol::LeftBracket(Bracket::Curly)));
                if brace.is_some() {
                    self.seek(-1);
                    Some(Item::FunctionDef(try_opt!(self.parse_function_def())))
                } else {
                    self.emit_error_here("expected assignment or function definition");
                    None
                }
            }
        }
    }

    fn parse_arg_list(&mut self, def_type: bool) -> Option<Node<ArgumentList>> {
        let pos = self.peek_source_pos_or_end(0);
        let mut args = Vec::new();
        if self.len() == 0 {
            return Some(Node(args, pos))
        }
        loop {
            let idx = self.index();
            let comma = self.find_smart(Token::Symbol(Symbol::Comma));
            let token_idx = comma.unwrap_or(self.end_index());
            self.enter_subsection(idx, token_idx);
            let arg = try_opt!(self.parse_arg(def_type));
            args.push(arg);
            self.integrate_subsection();
            if comma.is_none() {
                break;
            }
        }
        self.seek(-1); // no comma on the last one
        Some(Node(args, pos))
    }

    // if def_type, expression only args are not allowed, if not then identifier only args are
    // parsed as rhs expressions rather than the lhs
    fn parse_arg(&mut self, def_type: bool) -> Option<Argument> {
        let (one, two) = (self.next(), self.next());
        match (one.item(), two.item()) {
            (Some(Token::Ident(id)), Some(Token::Symbol(Symbol::Equals))) => {
                let expr = try_opt!(self.parse_expression());
                Some(Argument(Some(Node(id, one.pos().unwrap())), Some(expr)))
            }
            (Some(Token::Ident(id)), None) => {
                if !def_type { //it's actually an expression containing a single identifier
                    self.seek(-2);
                    let expr = try_opt!(self.parse_expression());
                    Some(Argument(None, Some(expr)))
                } else {
                    Some(Argument(Some(Node(id, one.pos().unwrap())), None))
                }
            }
            _ => {
                self.seek(-1);
                if def_type {
                    self.emit_error_here("expected identifier");
                    return None
                } else {
                    self.seek(-1);
                    let expr = try_opt!(self.parse_expression());
                    Some(Argument(None, Some(expr)))
                }
            }
        }
    }

    fn parse_function_call(&mut self, callee: Expression) -> Option<Node<FunctionCall>> {
        let pos = self.peek_source_pos_or_end(0);
        match self.next_token() {
            Some(Token::Symbol(Symbol::LeftBracket(brace))) => {
                let ty = match brace {
                    Bracket::Round => CallType::Explicit,
                    Bracket::Square => CallType::Implicit,
                    Bracket::Curly => CallType::Partial,
                };

                let end = self.find_smart(Token::Symbol(Symbol::RightBracket(brace)));
                if end.is_none() {
                    self.ctxt.emit_error(format!("expected `{}`", Symbol::RightBracket(brace)),
                                         self.end_source_pos());
                    return None;
                }
                let idx = self.index();
                self.enter_subsection(idx, end.unwrap());
                let args = try_opt!(self.parse_arg_list(false));
                self.integrate_subsection();
                self.seek(1); //consume closing brace
                Some(Node(FunctionCall {
                    callee: callee,
                    args: args,
                    ty: ty,
                }, pos))
            },
            _ => {
                self.emit_error_here("expected `(`, `[`, or `{`");
                return None;
            }
        }
    }
}
