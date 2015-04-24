use super::lexer::{Number, Operator, SourcePos};
use super::symbol::Identifier;
use std::ops::Deref;
use std::fmt;

pub struct Node<T>(pub T, pub SourcePos);

pub trait NodeImpl<'a> {
    type Item;
    type Pos;
    fn item(&'a self) -> <Self as NodeImpl>::Item;
    fn pos(&'a self) -> <Self as NodeImpl>::Pos;
}

impl<'a, T> NodeImpl<'a> for Node<T> {
    type Item =  &'a T;
    type Pos = SourcePos;
    fn item(&'a self) -> &'a T {
        &self.0
    }
    fn pos(&self) -> SourcePos {
        self.1
    }
}

impl<'a, T> NodeImpl<'a> for Option<Node<T>> where T: Copy {
    type Item =  Option<T>;
    type Pos = Option<SourcePos>;
    fn item(&self) -> Option<T> {
        self.map(|x| x.0)
    }
    fn pos(&self) -> Option<SourcePos> {
        self.map(|x| x.1)
    }
}

impl<T> fmt::Debug for Node<T> where T: fmt::Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple(&format!("Node+{}", self.1))
            .field(&self.0)
            .finish()
    }
}

impl<T> fmt::Display for Node<T> where T: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} @ {}", self.0, self.1)
    }
}

impl<T> Copy for Node<T> where T: Copy { }

impl<T> Clone for Node<T> where T: Clone {
    fn clone(&self) -> Node<T> {
        Node(self.0.clone(), self.1)
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref<'a>(&'a self) -> &'a T {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub enum Item {
    Assignment(Node<Assignment>),
    FunctionDef(Node<FunctionDef>),
}

impl Item {
    pub fn pos(&self) -> SourcePos {
        match self {
            &Item::Assignment(ref x) => x.pos(),
            &Item::FunctionDef(ref x) => x.pos(),
        }
    }
}


#[derive(Clone, Debug)]
pub enum Statement {
    Assignment(Node<Assignment>),
    Expression(Expression),
}

impl Statement {
    pub fn pos(&self) -> SourcePos {
        match self {
            &Statement::Assignment(ref x) => x.pos(),
            &Statement::Expression(ref x) => x.pos(),
        }
    }
}

pub type Root = Vec<Item>;

pub type Block = Vec<Statement>;

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub ident: Node<Identifier>,
    pub func: Node<Function>,
}

impl FunctionDef {
    pub fn ident(&self) -> Identifier { *self.ident.item() }
    pub fn ident_pos(&self) -> SourcePos { self.ident.pos() }
}

impl Deref for FunctionDef {
    type Target = Function;

    fn deref<'a>(&'a self) -> &'a Function {
        &self.func
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub args: Node<ArgumentList>,
    pub block: Node<Block>,
}

impl Function {
    pub fn args(&self) -> &ArgumentList { &self.args }
    pub fn args_pos(&self) -> SourcePos { self.args.pos() }
    pub fn block(&self) -> &Block { &self.block }
    pub fn block_pos(&self) -> SourcePos { self.block.pos() }
}

#[derive(Clone, Debug)]
pub struct FunctionCall {
    pub ident: Node<Identifier>,
    pub args: Node<ArgumentList>,
    pub ty: CallType,
}

impl FunctionCall {
    pub fn ident(&self) -> Identifier { *self.ident.item() }
    pub fn ident_pos(&self) -> SourcePos { self.ident.pos() }
    pub fn args(&self) -> &ArgumentList { &self.args }
    pub fn args_pos(&self) -> SourcePos { self.args.pos() }
    pub fn ty(&self) -> CallType { self.ty.clone() }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CallType {
    Explicit,
    Implicit,
}

// At most one of the two can be None
#[derive(Clone, Debug)]
pub struct Argument(pub Option<Node<Identifier>>, pub Option<Expression>);

impl Argument {
    pub fn ident(&self) -> Option<Identifier> { self.0.map(|x| *x.item()) }
    pub fn ident_pos(&self) -> Option<SourcePos> { self.0.map(|x| x.pos()) }
    pub fn expr(&self) -> &Option<Expression> { &self.1 }
    pub fn expr_pos(&self) -> Option<SourcePos> { self.1.as_ref().map(|x| x.pos()) }
}

pub type ArgumentList = Vec<Node<Argument>>;

#[derive(Clone, Debug)]
pub struct Assignment {
    pub ident: Node<Identifier>,
    pub expr: Expression,
}

impl Assignment {
    pub fn ident(&self) -> Identifier { *self.ident.item() }
    pub fn ident_pos(&self) -> SourcePos { self.ident.pos() }
    pub fn expr(&self) -> &Expression { &self.expr }
    pub fn expr_pos(&self) -> SourcePos { self.expr.pos() }
}

#[derive(Clone, Debug)]
pub struct Conditional {
    pub cond: Expression,
    pub then: Expression,
    pub els: Option<Expression>,
}

impl Conditional {
    pub fn cond(&self) -> &Expression { &self.cond }
    pub fn cond_pos(&self) -> SourcePos { self.cond.pos() }
    pub fn then(&self) -> &Expression { &self.then }
    pub fn then_pos(&self) -> SourcePos { self.then.pos() }
    pub fn els(&self) -> &Option<Expression> { &self.els }
    pub fn els_pos(&self) -> Option<SourcePos> { self.els.as_ref().map(|x| x.pos()) }
}

#[derive(Clone, Debug)]
pub struct Infix {
    pub op: Node<Operator>,
    pub left: Expression,
    pub right: Expression,
}

impl Infix {
    pub fn op(&self) -> Operator { *self.op.item() }
    pub fn op_pos(&self) -> SourcePos { self.op.pos() }
    pub fn left(&self) -> &Expression { &self.left }
    pub fn left_pos(&self) -> SourcePos { self.left.pos() }
    pub fn right(&self) -> &Expression { &self.right }
    pub fn right_pos(&self) -> SourcePos { self.right.pos() }
}

#[derive(Clone, Debug)]
pub struct Prefix {
    pub op: Node<Operator>,
    pub expr: Expression,
}

impl Prefix {
    pub fn op(&self) -> Operator { *self.op.item() }
    pub fn op_pos(&self) -> SourcePos { self.op.pos() }
    pub fn expr(&self) -> &Expression { &self.expr }
    pub fn expr_pos(&self) -> SourcePos { self.expr.pos() }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Constant(Node<Number>),
    Boolean(Node<bool>),
    Infix(Box<Node<Infix>>),
    Prefix(Box<Node<Prefix>>),
    Variable(Node<Identifier>),
    Block(Node<Block>),
    FunctionCall(Node<FunctionCall>),
    Conditional(Box<Node<Conditional>>),
}

impl Expression {
    pub fn pos(&self) -> SourcePos {
        use self::Expression::*;
        match *self {
            Constant(ref x) => x.pos(),
            Boolean(ref x) => x.pos(),
            Infix(ref x) => x.pos(),
            Prefix(ref x) => x.pos(),
            Variable(ref x) => x.pos(),
            Block(ref x) => x.pos(),
            FunctionCall(ref x) => x.pos(),
            Conditional(ref x) => x.pos(),
        }
    }
}
