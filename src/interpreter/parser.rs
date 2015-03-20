#![macro_use]

use super::issue::{IssueTracker, Level};
use super::lexer::{Number, SourcePos, Token, PW, PWImpl, Symbol, Bracket, Operator, Associativity};
use super::identifier::Identifier;
use std::borrow::{IntoCow, Cow};

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
            match t.token() {
                $ty(v) => Some((v, t.pos())),
                _ => None
            }
        })
    };
);

macro_rules! expect(
    ( $tokens:ident, $ty:pat ) => {
        $tokens.next().and_then(|x| {
            match x.token() {
                $ty => Some(x),
                _ => None,
            }
        })
    };
);

macro_rules! find_next_skip_brackets(
    ( $parser:ident, $( $token:expr ),+ ) => {
        {
            let start_idx = $parser.index();
            let out;
            loop {
                let token = $parser.next_token();
                match token {
                    $(Some(x) if x == $token => {
                        out = token;
                        break;
                    }),+
                    Some(Token::Symbol(Symbol::LeftBracket(_))) => {
                        if !$parser.match_bracket() {
                            out = None;
                            break;
                        }
                    },
                    None => {
                        out = None;
                        break;
                    },
                    _ => { }
                }
            }
            let index = $parser.index();
            $parser.set_index(start_idx);
            (out, index)
        }
    }
);

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    tokens: Cow<'a, [PW<Token>]>,
    issues: &'a IssueTracker<'a>,
    sub_stack: Vec<(usize, usize, usize)>,
}

impl<'a> Parser<'a> {
    pub fn new<U>(tokens: U, issues: &'a IssueTracker<'a>) -> Parser<'a>
            where U: IntoCow<'a, [PW<Token>]> {
        let ctokens = tokens.into_cow();
        let len = ctokens.len();
        Parser {
            tokens: ctokens,
            issues: issues,
            sub_stack: vec![(0, 0, len)],
        }
    }

    pub fn next(&mut self) -> Option<PW<Token>> {
        self.seek(1);
        self.peek(-1)
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.seek(1);
        self.peek(-1).token()
    }

    pub fn source_pos(&self) -> Option<SourcePos> {
        self.peek(-1).pos()
    }

    pub fn is_empty(&self) -> bool {
        self.index() >= self.end_index()
    }

    pub fn index(&self) -> usize {
        self.sub_stack.last().unwrap().0
    }

    pub fn start_index(&self) -> usize {
        self.sub_stack.last().unwrap().1
    }

    pub fn end_index(&self) -> usize {
        self.sub_stack.last().unwrap().2
    }

    pub fn len(&self) -> usize {
        self.end_index() - self.start_index()
    }

    pub fn enter_subsection(&mut self, from: usize, to: usize) {
        //println!("enter {} {}", from, to);
        assert!(from >= self.start_index() && to <= self.end_index() && from <= to);
        let index = self.index();
        self.sub_stack.push((index, from, to));
    }

    // returns index subsection was left at
    pub fn leave_subsection(&mut self) -> usize {
        //println!("leave");
        let section = self.sub_stack.pop().unwrap();
        section.0
    }

    // leaves subsection and replaces index with index of subsection
    pub fn integrate_subsection(&mut self) {
        let index = self.leave_subsection();
        self.set_index(index);
    }

    fn offset(&self, offset: isize) -> usize {
        ((self.index() as isize) + offset) as usize
    }

    pub fn set_index(&mut self, index: usize) {
        self.sub_stack.last_mut().unwrap().0 = index;
    }

    pub fn seek(&mut self, offset: isize) {
        let index = self.offset(offset);
        self.set_index(index);
    }

    pub fn in_bounds(&self, index: usize) -> bool {
        if index >= self.end_index() || index < self.start_index() {
            false
        } else {
            true
        }
    }

    pub fn peek(&self, offset: isize) -> Option<PW<Token>> {
        let index = self.offset(offset);
        if self.in_bounds(index) {
            self.peek_index(index)
        } else {
            None
        }
    }

    pub fn peek_index(&self, index: usize) -> Option<PW<Token>> {
        if self.in_bounds(index) {
            Some(self.tokens[index])
        } else {
            None
        }
    }

    pub fn peek_token(&self, offset: isize) -> Option<Token> {
        self.peek(offset).token()
    }

    pub fn peek_source_pos(&self, offset: isize) -> Option<SourcePos> {
        self.peek(offset).pos()
    }

    pub fn peek_source_pos_or_end(&self, offset: isize) -> SourcePos {
        self.peek_source_pos(offset).unwrap_or(self.end_source_pos())
    }

    pub fn end_source_pos(&self) -> SourcePos {
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
    pub fn match_bracket(&mut self) -> bool {
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
                    self.new_issue(Level::Error, format!("expected `{}`", close));
                    return false;
                }
                _ => { }
            }
        }
        return true;
    }

    pub fn new_issue<S>(&self, ty: Level, msg: S) where S: IntoCow<'static, str> {
        self.issues.new_issue(self.peek_source_pos_or_end(-1), ty, msg);
    }
    pub fn new_issue_at<S>(&self, ty: Level, pos: SourcePos, msg: S) where S: IntoCow<'static, str> {
        self.issues.new_issue(pos, ty, msg);
    }

    pub fn parse(&mut self) -> Option<Root> {
        let mut items = Vec::new();
        while !self.is_empty() {
            items.push(try_opt!(self.parse_item()));
        }
        Some(items)
    }

    pub fn parse_expression(&mut self) -> Option<Expression> {
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
        match token.token() {
            Some(Token::Const(v)) => Some(Expression::Constant((v, token.pos().unwrap()))),
            Some(Token::Ident(id)) => Some(Expression::Variable((id, token.pos().unwrap()))),
            Some(Token::Operator(Operator::Sub)) => {
                Some(Expression::Prefix(Box::new((Prefix {
                    op: (Operator::Sub, token.pos().unwrap()),
                    expr: try_opt!(self.pratt_expression(100))
                }, token.pos().unwrap()))))
            }
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Round))) => {
                let expr = try_opt!(self.pratt_expression(1));
                if expect!(self, Token::Symbol(Symbol::RightBracket(Bracket::Round))).is_none() {
                    self.new_issue(Level::Error, "expected `)`");
                    return None;
                }
                Some(expr)
            }
            Some(Token::Symbol(Symbol::LeftBracket(Bracket::Curly))) => {
                self.seek(-1);
                let block = try_opt!(self.parse_block());
                Some(Expression::Block(block))
            }
            Some(Token::Symbol(Symbol::Backslash)) => {
                self.seek(-1);
                let closure = try_opt!(self.parse_closure());
                Some(Expression::Closure(closure))
            }

            _ => {
                self.new_issue(Level::Error, "expected constant, variable, opening bracket or unary operator");
                return None;
            }
        }
    }

    fn pratt_led(&mut self, left: Expression, right: Option<Node<Token>>) -> Option<Expression> {
        //println!("led {:?} {:?}", left, right);
        match right.token() {
            Some(Token::Operator(op)) => {
                let precedence = op.precedence() -
                    if op.associativity() == Associativity::Right { 1 } else { 0 };
                Some(Expression::Infix(Box::new((Infix {
                    op: (op, right.pos().unwrap()),
                    left: left,
                    right: try_opt!(self.pratt_expression(precedence)),
                }, right.pos().unwrap()))))
            }
            Some(Token::Symbol(Symbol::RightBracket(Bracket::Round))) => {
                self.new_issue(Level::Error, "unexpected `)`");
                return None;
            }
            Some(Token::Symbol(Symbol::LeftBracket(_))) => {
                self.seek(-2);
                let call = try_opt!(self.parse_function_call());
                Some(Expression::FunctionCall(call))
            }
            Some(Token::Symbol(Symbol::If)) => {
                let then = left;
                let cond = try_opt!(self.pratt_expression(1));
                if let Some(Token::Symbol(Symbol::Else)) = self.next_token() {
                    let els = try_opt!(self.pratt_expression(1));
                    Some(Expression::Conditional(Box::new((Conditional {
                        cond: cond,
                        then: then,
                        els: Some(els),
                    }, right.pos().unwrap()))))
                } else {
                    self.seek(-1);
                    Some(Expression::Conditional(Box::new((Conditional {
                        cond: cond,
                        then: then,
                        els: None,
                    }, right.pos().unwrap()))))
                }
            }
            _ => {
                self.new_issue(Level::Error, "(internal error: unreachable) expected left denotation");
                return None;
            }
        }
    }

    fn pratt_lbp(&mut self, token: Option<Node<Token>>) -> Option<i32> {
        //println!("lbp {:?}", token);
        match token.token() {
            Some(Token::Operator(op)) => Some(op.precedence()),
            Some(Token::Symbol(Symbol::If)) => Some(2),
            Some(Token::Symbol(Symbol::Else)) => Some(0),
            Some(Token::Symbol(Symbol::RightBracket(Bracket::Round))) => Some(1),
            Some(Token::Symbol(Symbol::LeftBracket(_))) => Some(1000),
            None => Some(0),
            _ => {
                self.new_issue(Level::Error, "expected binary operator");
                return None;
            }
        }
    }

    pub fn parse_assignment(&mut self) -> Option<Node<Assignment>> {
        let pos = self.peek_source_pos_or_end(0);
        let ident = try_opt!(self.parse_ident());
        try_opt!(self.parse_symbol(Symbol::Equals));
        let expr = try_opt!(self.parse_expression());

        Some((Assignment {
            ident: ident,
            expr: expr,
        }, pos))
    }

    pub fn parse_symbol(&mut self, symbol: Symbol) -> Option<()> {
        match self.next_token() {
            Some(Token::Symbol(x)) if x == symbol => Some(()),
            _ => {
                self.new_issue(Level::Error, format!("expected {}", symbol));
                None
            },
        }
    }

    pub fn parse_ident(&mut self) -> Option<Node<Identifier>> {
        match expect_value!(self, Token::Ident) {
            None => {
                self.new_issue(Level::Error, "expected identifier");
                None
            }
            x => x,
        }
    }

    pub fn parse_function_def(&mut self) -> Option<Node<FunctionDef>> {
        let pos = self.peek_source_pos_or_end(0);
        let ident = try_opt!(self.parse_ident());
        let func = try_opt!(self.parse_function());
        Some((FunctionDef {
            ident: ident,
            func: func,
        }, pos))
    }

    pub fn parse_closure(&mut self) -> Option<Node<Closure>> {
        let pos = self.peek_source_pos_or_end(0);
        try_opt!(self.parse_symbol(Symbol::Backslash));
        Some((try_opt!(self.parse_function()).0, pos))
    }

    fn parse_function(&mut self) -> Option<Node<Function>> {
        let pos = self.peek_source_pos_or_end(0);
        let (brace, brace_idx) = find_next_skip_brackets!(self, Token::Symbol(Symbol::LeftBracket(Bracket::Curly)));
        if brace.is_none() {
            self.new_issue_at(Level::Error, self.end_source_pos(), "expected `{`");
            return None;
        }
        let idx = self.index();
        self.enter_subsection(idx, brace_idx-1);
        let args = try_opt!(self.parse_arg_list(true));
        self.integrate_subsection();

        let block = try_opt!(self.parse_block());

        Some((Function {
            args: args,
            block: block,
        }, pos))
    }

    pub fn parse_block(&mut self) -> Option<Node<Block>> {
        let pos = self.peek_source_pos_or_end(0);
        try_opt!(self.parse_symbol(Symbol::LeftBracket(Bracket::Curly)));
        let (brace, brace_idx) = find_next_skip_brackets!(self, Token::Symbol(Symbol::RightBracket(Bracket::Curly)));
        if brace.is_none() {
            self.new_issue_at(Level::Error, self.end_source_pos(), "expected `}`");
            return None;
        }
        let mut stmts = Vec::new();
        let idx = self.index();
        self.enter_subsection(idx, brace_idx-1);
        loop {
            let (semi, semi_idx) = find_next_skip_brackets!(self, Token::Symbol(Symbol::Semicolon));
            if semi_idx - 1 == self.index() {
                break;
            }
            let idx = self.index();
            self.enter_subsection(idx, semi_idx-1);
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
        Some((stmts, pos))
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
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
    pub fn parse_item(&mut self) -> Option<Item> {
        try_opt!(self.parse_ident());

        match self.next_token() {
            Some(Token::Symbol(Symbol::Equals)) => {
                self.seek(-2);
                let (semi, semi_idx) = find_next_skip_brackets!(self, Token::Symbol(Symbol::Semicolon));
                if semi.is_none() {
                    self.new_issue_at(Level::Error, self.end_source_pos(), "expected `;`");
                    return None;
                }
                let idx = self.index();
                self.enter_subsection(idx, semi_idx-1);
                let assign = try_opt!(self.parse_assignment());
                self.integrate_subsection();
                Some(Item::Assignment(assign))
            },
            _ => {
                self.seek(-1);
                let (brace, _) = find_next_skip_brackets!(self, Token::Symbol(Symbol::LeftBracket(Bracket::Curly)));
                if brace.is_some() {
                    self.seek(-1);
                    Some(Item::FunctionDef(try_opt!(self.parse_function_def())))
                } else {
                    self.new_issue(Level::Error, "expected assignment or function definition");
                    None
                }
            }
        }
    }

    pub fn parse_arg_list(&mut self, def_type: bool) -> Option<Node<ArgumentList>> {
        let pos = self.peek_source_pos_or_end(0);
        let mut args = Vec::new();
        if self.len() == 0 {
            return Some((args, pos))
        }
        loop {
            let (comma, comma_idx) = find_next_skip_brackets!(self, Token::Symbol(Symbol::Comma));
            let idx = self.index();
            self.enter_subsection(idx, comma_idx-1);
            let arg = try_opt!(self.parse_arg(def_type));
            args.push(arg);
            self.integrate_subsection();
            if comma.is_none() {
                break;
            }
        }
        self.seek(-1); // no comma on the last one
        Some((args, pos))
    }

    // if def_type, expression only args are not allowed, if not then identifier only args are
    // parsed as rhs expressions rather than the lhs
    pub fn parse_arg(&mut self, def_type: bool) -> Option<Node<Argument>> {
        let pos = self.peek_source_pos_or_end(0);
        let (one, two) = (self.next(), self.next());
        match (one.token(), two.token()) {
            (Some(Token::Ident(id)), Some(Token::Symbol(Symbol::Equals))) => {
                let expr = try_opt!(self.parse_expression());
                Some(((Some((id, one.pos().unwrap())), Some(expr)), pos))
            }
            (Some(Token::Ident(id)), None) => {
                if !def_type { //it's actually an expression containing a single identifier
                    self.seek(-2);
                    let expr = try_opt!(self.parse_expression());
                    Some(((None, Some(expr)), pos))
                } else {
                    Some(((Some((id, one.pos().unwrap())), None), pos))
                }
            }
            _ => {
                self.seek(-1);
                if def_type {
                    self.new_issue(Level::Error, "expression only arguments are not allowed in definitions");
                    return None
                } else {
                    self.seek(-1);
                    let expr = try_opt!(self.parse_expression());
                    Some(((None, Some(expr)), pos))
                }
            }
        }
    }

    pub fn parse_function_call(&mut self) -> Option<Node<FunctionCall>> {
        let pos = self.peek_source_pos_or_end(0);
        let ident = try_opt!(self.parse_ident());
        match self.next_token() {
            Some(Token::Symbol(Symbol::LeftBracket(brace))) => {
                let ty = match brace {
                    Bracket::Round => CallType::Explicit,
                    Bracket::Square => CallType::Implicit,
                    Bracket::Curly => CallType::Partial,
                };

                let (end, end_idx) = find_next_skip_brackets!(self, Token::Symbol(Symbol::RightBracket(brace)));
                if end.is_none() {
                    self.new_issue_at(Level::Error, self.end_source_pos(),
                                      format!("expected `{}`", Symbol::RightBracket(brace)));
                    return None;
                }
                let idx = self.index();
                self.enter_subsection(idx, end_idx-1);
                let args = try_opt!(self.parse_arg_list(false));
                self.integrate_subsection();
                self.seek(1); //consume closing brace
                Some((FunctionCall {
                    ident: ident,
                    args: args,
                    ty: ty,
                }, pos))
            },
            _ => {
                self.new_issue(Level::Error, "expected `(`, `[`, or `{`");
                return None;
            }
        }
    }
}

pub type Node<T> = PW<T>;

#[derive(Clone, Debug)]
pub enum Item {
    Assignment(Node<Assignment>),
    FunctionDef(Node<FunctionDef>),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Assignment(Node<Assignment>),
    Expression(Expression),
}

pub type Ident = Identifier;

pub type Root = Vec<Item>;

pub type Block = Vec<Statement>;

#[derive(Clone, Debug)]
pub struct FunctionDef {
    ident: Node<Ident>,
    func: Node<Function>,
}

pub type Closure = Function;

#[derive(Clone, Debug)]
pub struct Function {
    args: Node<ArgumentList>,
    block: Node<Block>,
}

#[derive(Clone, Debug)]
pub struct FunctionCall {
    ident: Node<Ident>,
    args: Node<ArgumentList>,
    ty: CallType,
}

#[derive(Clone, Debug)]
pub enum CallType {
    Explicit,
    Implicit,
    Partial,
}

pub type Argument = (Option<Node<Ident>>, Option<Expression>); // At most one of the two can be None
pub type ArgumentList = Vec<Node<Argument>>;

#[derive(Clone, Debug)]
pub struct Assignment {
    ident: Node<Ident>,
    expr: Expression,
}

#[derive(Clone, Debug)]
pub struct Conditional {
    cond: Expression,
    then: Expression,
    els: Option<Expression>,
}

#[derive(Clone, Debug)]
pub struct Infix {
    op: Node<Operator>,
    left: Expression,
    right: Expression,
}

#[derive(Clone, Debug)]
pub struct Prefix {
    op: Node<Operator>,
    expr: Expression,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Constant(Node<Number>),
    Infix(Box<Node<Infix>>),
    Prefix(Box<Node<Prefix>>),
    Variable(Node<Ident>),
    Block(Node<Block>),
    FunctionCall(Node<FunctionCall>),
    Conditional(Box<Node<Conditional>>),
    Closure(Node<Closure>),
}
